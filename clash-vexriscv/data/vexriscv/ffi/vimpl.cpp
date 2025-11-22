// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0
#include "V__CPU_NAME__VexRiscv.h"
#include "interface.h"
#include "verilated.h"
#include <string.h>
#include <verilated_vcd_c.h>

// Unique function names for: V__CPU_NAME__VexRiscv
extern "C" {
V__CPU_NAME__VexRiscv *V__CPU_NAME__VexRiscv_init();
VerilatedVcdC *V__CPU_NAME__VexRiscv_init_vcd(V__CPU_NAME__VexRiscv *top,
                                              const char *path);
void V__CPU_NAME__VexRiscv_shutdown(V__CPU_NAME__VexRiscv *top);

void V__CPU_NAME__VexRiscv_init_stage1(VerilatedVcdC *vcd,
                                       uint64_t dom_period_fs,
                                       V__CPU_NAME__VexRiscv *top,
                                       const NON_COMB_INPUT *input,
                                       OUTPUT *output);
void V__CPU_NAME__VexRiscv_init_stage2(V__CPU_NAME__VexRiscv *top,
                                       const COMB_INPUT *input);
void V__CPU_NAME__VexRiscv_step_rising_edge(VerilatedVcdC *vcd,
                                            V__CPU_NAME__VexRiscv *top,
                                            uint64_t time_add,
                                            const NON_COMB_INPUT *input,
                                            OUTPUT *output);
void V__CPU_NAME__VexRiscv_step_falling_edge(VerilatedVcdC *vcd,
                                             V__CPU_NAME__VexRiscv *top,
                                             uint64_t time_add,
                                             const COMB_INPUT *input);
}

V__CPU_NAME__VexRiscv *V__CPU_NAME__VexRiscv_init() {
  VerilatedContext *ctx = new VerilatedContext;
  V__CPU_NAME__VexRiscv *v = new V__CPU_NAME__VexRiscv(ctx);
  v->clk = false;
  return v;
}

VerilatedVcdC *V__CPU_NAME__VexRiscv_init_vcd(V__CPU_NAME__VexRiscv *top,
                                              const char *path) {
  VerilatedVcdC *vcd = new VerilatedVcdC;
  Verilated::traceEverOn(true);
  top->trace(vcd, 99);
  vcd->open(path);
  return vcd;
}

static void set_non_comb_inputs(V__CPU_NAME__VexRiscv *top,
                                const NON_COMB_INPUT *input) {
  top->reset = input->reset;
  top->timerInterrupt = input->timerInterrupt;
  top->externalInterrupt = input->externalInterrupt;
  top->softwareInterrupt = input->softwareInterrupt;
}

static void set_comb_inputs(V__CPU_NAME__VexRiscv *top,
                            const COMB_INPUT *input) {
  top->iBusWishbone_ACK = input->iBusWishbone_ACK;
  top->iBusWishbone_DAT_MISO = input->iBusWishbone_DAT_MISO;
  top->iBusWishbone_ERR = input->iBusWishbone_ERR;
  top->dBusWishbone_ACK = input->dBusWishbone_ACK;
  top->dBusWishbone_DAT_MISO = input->dBusWishbone_DAT_MISO;
  top->dBusWishbone_ERR = input->dBusWishbone_ERR;
  top->jtag_tck = input->jtag_TCK;
  top->jtag_tms = input->jtag_TMS;
  top->jtag_tdi = input->jtag_TDI;
}

static void set_outputs(V__CPU_NAME__VexRiscv *top, OUTPUT *output) {
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
  output->ndmreset = top->ndmreset;
  output->stoptime = top->stoptime;
  output->jtag_TDO = top->jtag_tdo;
}

void V__CPU_NAME__VexRiscv_init_stage1(VerilatedVcdC *vcd,
                                       uint64_t dom_period_fs,
                                       V__CPU_NAME__VexRiscv *top,
                                       const NON_COMB_INPUT *input,
                                       OUTPUT *output) {
  VerilatedContext *ctx = top->contextp();
  set_non_comb_inputs(top, input);
  top->eval();
  if (vcd != NULL) {
    vcd->dump(ctx->time());
  }
  set_outputs(top, output);
  ctx->timeInc(dom_period_fs);
}

void V__CPU_NAME__VexRiscv_init_stage2(V__CPU_NAME__VexRiscv *top,
                                       const COMB_INPUT *input) {
  set_comb_inputs(top, input);
}

void V__CPU_NAME__VexRiscv_step_rising_edge(VerilatedVcdC *vcd,
                                            V__CPU_NAME__VexRiscv *top,
                                            uint64_t time_add,
                                            const NON_COMB_INPUT *input,
                                            OUTPUT *output) {
  VerilatedContext *ctx = top->contextp();
  set_non_comb_inputs(top, input);
  top->clk = true;
  top->eval();
  if (vcd != NULL) {
    vcd->dump(ctx->time());
  }
  set_outputs(top, output);
  ctx->timeInc(time_add);
}

void V__CPU_NAME__VexRiscv_step_falling_edge(VerilatedVcdC *vcd,
                                             V__CPU_NAME__VexRiscv *top,
                                             uint64_t time_add,
                                             const COMB_INPUT *input) {
  VerilatedContext *ctx = top->contextp();
  set_comb_inputs(top, input);
  top->clk = false;
  top->eval();
  if (vcd != NULL) {
    vcd->dump(ctx->time());
  }
  ctx->timeInc(time_add);
}

void V__CPU_NAME__VexRiscv_shutdown(V__CPU_NAME__VexRiscv *top) {
  VerilatedContext *ctx = top->contextp();
  delete top;
  delete ctx;
}
