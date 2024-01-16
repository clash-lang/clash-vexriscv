// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "VVexRiscv.h"
#include "verilated.h"
#include "interface.h"

#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <netinet/tcp.h>

typedef struct {
	int server_socket, client_handle;
	struct sockaddr_in server_addr;
	struct sockaddr_storage server_storage;

	uint32_t timer;
	socklen_t addr_size;
	uint32_t self_sleep;
	uint32_t check_new_connections_timer;
	uint8_t rx_buffer[100];
	int32_t rx_buffer_size;
	int32_t rx_buffer_remaining;
} vexr_jtag_bridge_data;

extern "C" {
	VVexRiscv* vexr_init();
	void vexr_shutdown(VVexRiscv *top);
	void vexr_cpu_step_rising_edge(VVexRiscv *top, const INPUT *input);
	void vexr_cpu_step_falling_edge(VVexRiscv *top, OUTPUT *output);
	
	void vexr_jtag_step_rising_edge(VVexRiscv *top, const JTAG_INPUT *input);
	void vexr_jtag_step_falling_edge(VVexRiscv *top, JTAG_OUTPUT *output);

	vexr_jtag_bridge_data *vexr_jtag_bridge_init(uint16_t port);
	void vexr_jtag_bridge_step(vexr_jtag_bridge_data *d, const JTAG_OUTPUT *output, JTAG_INPUT *input, bit *jtag_en);
	void vexr_jtag_bridge_shutdown(vexr_jtag_bridge_data *bridge_data);
}

static bool set_socket_blocking_enabled(int fd, bool blocking);
static void connection_reset(vexr_jtag_bridge_data *bridge_data);

VVexRiscv* vexr_init()
{
	return new VVexRiscv();
}

void vexr_shutdown(VVexRiscv *top)
{
	delete top;
}

void vexr_cpu_step_rising_edge(VVexRiscv *top, const INPUT *input)
{
	// set inputs
	top->reset = input->reset;
	top->timerInterrupt = input->timerInterrupt;
	top->externalInterrupt = input->externalInterrupt;
	top->softwareInterrupt = input->softwareInterrupt;
	top->iBusWishbone_ACK = input->iBusWishbone_ACK;
	top->iBusWishbone_DAT_MISO = input->iBusWishbone_DAT_MISO;
	top->iBusWishbone_ERR = input->iBusWishbone_ERR;
	top->dBusWishbone_ACK = input->dBusWishbone_ACK;
	top->dBusWishbone_DAT_MISO = input->dBusWishbone_DAT_MISO;
	top->dBusWishbone_ERR = input->dBusWishbone_ERR;

	// run one cycle of the simulation
	top->clk = true;
	top->eval();
}

void vexr_cpu_step_falling_edge(VVexRiscv *top, OUTPUT *output)
{
	top->clk = false;
	top->eval();

	// update outputs
	output->iBusWishbone_CYC = top->iBusWishbone_CYC;
	output->iBusWishbone_STB = top->iBusWishbone_STB;
	output->iBusWishbone_WE = top->iBusWishbone_WE;
	output->iBusWishbone_ADR = top->iBusWishbone_ADR;
	output->iBusWishbone_DAT_MOSI = top->iBusWishbone_DAT_MOSI;
	output->iBusWishbone_SEL = top->iBusWishbone_SEL;
	output->iBusWishbone_CTI = top->iBusWishbone_CTI;
	output->iBusWishbone_BTE = top->iBusWishbone_BTE;
	output->dBusWishbone_CYC = top->dBusWishbone_CYC;
	output->dBusWishbone_STB = top->dBusWishbone_STB;
	output->dBusWishbone_WE = top->dBusWishbone_WE;
	output->dBusWishbone_ADR = top->dBusWishbone_ADR;
	output->dBusWishbone_DAT_MOSI = top->dBusWishbone_DAT_MOSI;
	output->dBusWishbone_SEL = top->dBusWishbone_SEL;
	output->dBusWishbone_CTI = top->dBusWishbone_CTI;
	output->dBusWishbone_BTE = top->dBusWishbone_BTE;
}

void vexr_jtag_step_rising_edge(VVexRiscv *top, const JTAG_INPUT *input)
{
	// set inputs
	top->jtag_tms = input->jtag_TMS;
	top->jtag_tdi = input->jtag_TDI;

	top->jtag_tck = true;
	top->eval();
}

void vexr_jtag_step_falling_edge(VVexRiscv *top, JTAG_OUTPUT *output)
{
	top->jtag_tck = false;
	top->eval();

	// update outputs
	output->debug_resetOut = top->debug_resetOut;
	output->jtag_TDO = top->jtag_tdo;
}

vexr_jtag_bridge_data *vexr_jtag_bridge_init(uint16_t port)
{
	vexr_jtag_bridge_data *d = new vexr_jtag_bridge_data;
	d->timer = 0;
	d->self_sleep = 0;
	d->check_new_connections_timer = 0;
	d->rx_buffer_size = 0;
	d->rx_buffer_remaining = 0;

	d->server_socket = socket(PF_INET, SOCK_STREAM, 0);
	assert(d->server_socket != -1);
	int flag = 1;
	setsockopt(
		d->server_socket, /* socket affected */
		IPPROTO_TCP,                /* set option at TCP level */
		TCP_NODELAY,                /* name of option */
		(char *) &flag,             /* the cast is historical cruft */
		sizeof(int)                 /* length of option value */
	);

	set_socket_blocking_enabled(d->server_socket, 0);
	
	d->server_addr.sin_family = AF_INET;
	d->server_addr.sin_port = htons(port);
	d->server_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
	memset(d->server_addr.sin_zero, '\0', sizeof(d->server_addr.sin_zero));

	bind(
		d->server_socket,
		(struct sockaddr *) &d->server_addr,
		sizeof(d->server_addr)
	);

	listen(d->server_socket, 1);

	d->client_handle = -1;
	d->addr_size = sizeof(d->server_storage);

	return d;
}

void vexr_jtag_bridge_step(vexr_jtag_bridge_data *d, const JTAG_OUTPUT *output, JTAG_INPUT *input, bit *jtag_en)
{
	const int WAIT_PERIOD = 83333;
	// We set the input values to their default here
	// so that only the "successful" path has to update them

	input->jtag_TMS = 0;
	input->jtag_TDI = 0;
	*jtag_en = 0;

	if(d->timer != 0) {
		d->timer -= 1;
		return;
	}
	d->check_new_connections_timer++;
	if (d->check_new_connections_timer == 5000) {
		d->check_new_connections_timer = 0;
		int new_client_handle = accept(
			d->server_socket,
			(struct sockaddr *) &d->server_storage,
			&d->addr_size
		);
		if (new_client_handle != -1) {
			if(d->client_handle != -1){
				connection_reset(d);
			}
			d->client_handle = new_client_handle;
		} else {
			if(d->client_handle == -1)
				d->self_sleep = 1000;
		}
	}
	if (d->self_sleep) {
		d->self_sleep--;
	} else if (d->client_handle != -1) {
		int n;

		if (d->rx_buffer_remaining == 0) {
			if (ioctl(d->client_handle, FIONREAD, &n) != 0) {
				connection_reset(d);
			} else if (n >= 1) {
				d->rx_buffer_size = read(
					d->client_handle,
					&d->rx_buffer,
					100
				);
				if (d->rx_buffer_size < 0) {
					connection_reset(d);
				} else {
					d->rx_buffer_remaining = d->rx_buffer_size;
				}
			} else {
				d->self_sleep = 30;
			}
		}

		if (d->rx_buffer_remaining != 0){
			uint8_t buffer = d->rx_buffer[d->rx_buffer_size - (d->rx_buffer_remaining--)];
			input->jtag_TMS = (buffer & 1) != 0;
			input->jtag_TDI = (buffer & 2) != 0;
			*jtag_en = (buffer & 8) != 0;
			if(buffer & 4){
				buffer = (output->jtag_TDO != 0);
				if (-1 == send(d->client_handle, &buffer, 1, 0)) {
					connection_reset(d);
				}
			}
		}
	}
	d->timer = 0; // 3; value used by VexRiscv regression test
}

void vexr_jtag_bridge_shutdown(vexr_jtag_bridge_data *bridge_data)
{
	if (bridge_data->client_handle != -1) {
		shutdown(bridge_data->client_handle, SHUT_RDWR);
		usleep(100);
	}
	if (bridge_data->server_socket != -1) {
		close(bridge_data->server_socket);
		usleep(100);
	}
}


/** Returns true on success, or false if there was an error */
static bool set_socket_blocking_enabled(int fd, bool blocking)
{
   if (fd < 0) return false;

#ifdef WIN32
   unsigned long mode = blocking ? 0 : 1;
   return (ioctlsocket(fd, FIONBIO, &mode) == 0) ? true : false;
#else
   int flags = fcntl(fd, F_GETFL, 0);
   if (flags < 0) return false;
   flags = blocking ? (flags&~O_NONBLOCK) : (flags|O_NONBLOCK);
   return (fcntl(fd, F_SETFL, flags) == 0) ? true : false;
#endif
}

static void connection_reset(vexr_jtag_bridge_data *bridge_data) {
	shutdown(bridge_data->client_handle, SHUT_RDWR);
	bridge_data->client_handle = -1;
}