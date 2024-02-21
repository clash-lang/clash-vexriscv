`timescale 1ns/1ps

module VexRiscv (
  input               timerInterrupt,
  input               externalInterrupt,
  input               softwareInterrupt,
  output              debug_resetOut,
  output              iBusWishbone_CYC,
  output              iBusWishbone_STB,
  input               iBusWishbone_ACK,
  output              iBusWishbone_WE,
  output     [29:0]   iBusWishbone_ADR,
  input      [31:0]   iBusWishbone_DAT_MISO,
  output     [31:0]   iBusWishbone_DAT_MOSI,
  output     [3:0]    iBusWishbone_SEL,
  input               iBusWishbone_ERR,
  output     [2:0]    iBusWishbone_CTI,
  output     [1:0]    iBusWishbone_BTE,
  output              dBusWishbone_CYC,
  output              dBusWishbone_STB,
  input               dBusWishbone_ACK,
  output              dBusWishbone_WE,
  output     [29:0]   dBusWishbone_ADR,
  input      [31:0]   dBusWishbone_DAT_MISO,
  output     [31:0]   dBusWishbone_DAT_MOSI,
  output reg [3:0]    dBusWishbone_SEL,
  input               dBusWishbone_ERR,
  output     [2:0]    dBusWishbone_CTI,
  output     [1:0]    dBusWishbone_BTE,
  input               jtag_tms,
  input               jtag_tdi,
  output              jtag_tdo,
  input               jtag_tck,
  input               clk,
  input               reset
);
  initial begin
    // Specify the dump file name
    $dumpfile("simulation_dump.vcd");

    // Dump all signals to the VCD file
    $dumpvars(1, VexRiscv);
  end

  reg reset_cpu;
  wire reqCpuReset;

  // Handle reset logic in Verilog instead of Haskell
  assign debug_resetOut = 1'b0;
  always @(posedge clk) begin
      reset_cpu <= reset || reqCpuReset;
  end

  VexRiscvInner VexRiscvInner
    ( .timerInterrupt        (timerInterrupt)
    , .externalInterrupt     (externalInterrupt)
    , .softwareInterrupt     (softwareInterrupt)
    , .debug_resetOut        (reqCpuReset)
    , .iBusWishbone_CYC      (iBusWishbone_CYC)
    , .iBusWishbone_STB      (iBusWishbone_STB)
    , .iBusWishbone_ACK      (iBusWishbone_ACK)
    , .iBusWishbone_WE       (iBusWishbone_WE)
    , .iBusWishbone_ADR      (iBusWishbone_ADR)
    , .iBusWishbone_DAT_MISO (iBusWishbone_DAT_MISO)
    , .iBusWishbone_DAT_MOSI (iBusWishbone_DAT_MOSI)
    , .iBusWishbone_SEL      (iBusWishbone_SEL)
    , .iBusWishbone_ERR      (iBusWishbone_ERR)
    , .iBusWishbone_CTI      (iBusWishbone_CTI)
    , .iBusWishbone_BTE      (iBusWishbone_BTE)
    , .dBusWishbone_CYC      (dBusWishbone_CYC)
    , .dBusWishbone_STB      (dBusWishbone_STB)
    , .dBusWishbone_ACK      (dBusWishbone_ACK)
    , .dBusWishbone_WE       (dBusWishbone_WE)
    , .dBusWishbone_ADR      (dBusWishbone_ADR)
    , .dBusWishbone_DAT_MISO (dBusWishbone_DAT_MISO)
    , .dBusWishbone_DAT_MOSI (dBusWishbone_DAT_MOSI)
    , .dBusWishbone_SEL      (dBusWishbone_SEL)
    , .dBusWishbone_ERR      (dBusWishbone_ERR)
    , .dBusWishbone_CTI      (dBusWishbone_CTI)
    , .dBusWishbone_BTE      (dBusWishbone_BTE)
    , .jtag_tms              (jtag_tms)
    , .jtag_tdi              (jtag_tdi)
    , .jtag_tdo              (jtag_tdo)
    , .jtag_tck              (jtag_tck)
    , .clk                   (clk)
    , .reset                 (reset_cpu)
    );

endmodule
