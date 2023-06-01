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

  wire       i_iBusWishbone_ACK;
  reg [31:0] i_iBusWishbone_DAT_MISO;
  wire       i_iBusWishbone_ERR = 1'b0;

  wire       i_dBusWishbone_ACK;
  reg [31:0] i_dBusWishbone_DAT_MISO;
  wire       i_dBusWishbone_ERR = 1'b0;

  reg reset_cpu;
  wire reqCpuReset;

  // Handle reset logic in Verilog instead of Haskell
  assign debug_resetOut = 1'b0;
  always @(posedge clk) begin
      reset_cpu <= reset || reqCpuReset;
  end

  // Memory parameters
  localparam dmem_lower = 'h40000000 / 4;
  localparam imem_lower = 'h20000000 / 4;
  localparam mem_size_bytes = 1048576; // 1 MiB
  localparam mem_size_words = mem_size_bytes / 4;
  localparam mem_addr_bits  = 18; // log2(mem_size_words)
  localparam dmem_upper = dmem_lower + mem_size_words;
  localparam imem_upper = imem_lower + mem_size_words;

  //============================================================
  // Data memory
  //============================================================
  reg [7:0] dmem0[0:mem_size_words-1];
  reg [7:0] dmem1[0:mem_size_words-1];
  reg [7:0] dmem2[0:mem_size_words-1];
  reg [7:0] dmem3[0:mem_size_words-1];

  initial begin
    $readmemh("dmem0.mem", dmem0);
    $readmemh("dmem1.mem", dmem1);
    $readmemh("dmem2.mem", dmem2);
    $readmemh("dmem3.mem", dmem3);
  end

  // A commmand is valid on CYC and STB, and the response is valid on the next cycle
  // provided that CYC and STB are still asserted.
  wire dBus_cmd_valid;
  reg prev_dBus_cmd_valid = 1'b0;
  always @(posedge clk) begin
    prev_dBus_cmd_valid <= dBus_cmd_valid;
  end

  reg dBus_rsp = 1'b0;
  assign dBus_cmd_valid = dBusWishbone_CYC && dBusWishbone_STB && !prev_dBus_cmd_valid;
  assign i_dBusWishbone_ACK = dBus_rsp && !i_dBusWishbone_ERR;
  assign i_dBusWishbone_ERR = !(valid_data_address_d || valid_instr_address_d);
  wire [mem_addr_bits-1:0] dmem_addr;
  assign dmem_addr = dBusWishbone_ADR[mem_addr_bits-1:0];

  wire valid_data_address_d, valid_instr_address_d;
  assign valid_data_address_d = dmem_lower <= dBusWishbone_ADR && dBusWishbone_ADR < dmem_upper;
  assign valid_instr_address_d = imem_lower <= dBusWishbone_ADR && dBusWishbone_ADR < imem_upper;

  always @(posedge clk) begin
    i_dBusWishbone_DAT_MISO[ 7: 0] <= 8'b00000000;
    i_dBusWishbone_DAT_MISO[15: 8] <= 8'b00000000;
    i_dBusWishbone_DAT_MISO[23:16] <= 8'b00000000;
    i_dBusWishbone_DAT_MISO[31:24] <= 8'b00000000;
    dBus_rsp <= dBus_cmd_valid;

    if (dBus_cmd_valid) begin
      if (valid_data_address_d) begin
        // DAT
        i_dBusWishbone_DAT_MISO[ 7: 0] <= dmem0[dmem_addr];
        i_dBusWishbone_DAT_MISO[15: 8] <= dmem1[dmem_addr];
        i_dBusWishbone_DAT_MISO[23:16] <= dmem2[dmem_addr];
        i_dBusWishbone_DAT_MISO[31:24] <= dmem3[dmem_addr];

        if (dBusWishbone_WE) begin
          if (dBusWishbone_SEL[0]) begin
            dmem0[dmem_addr]               <= dBusWishbone_DAT_MOSI[ 7: 0];
            i_dBusWishbone_DAT_MISO[ 7: 0] <= dBusWishbone_DAT_MOSI[ 7: 0];
          end

          if (dBusWishbone_SEL[1]) begin
            dmem1[dmem_addr]               <= dBusWishbone_DAT_MOSI[15: 8];
            i_dBusWishbone_DAT_MISO[15: 8] <= dBusWishbone_DAT_MOSI[15: 8];
          end

          if (dBusWishbone_SEL[2]) begin
            dmem2[dmem_addr]               <= dBusWishbone_DAT_MOSI[23:16];
            i_dBusWishbone_DAT_MISO[23:16] <= dBusWishbone_DAT_MOSI[23:16];
          end

          if (dBusWishbone_SEL[3]) begin
            dmem3[dmem_addr]               <= dBusWishbone_DAT_MOSI[31:24];
            i_dBusWishbone_DAT_MISO[31:24] <= dBusWishbone_DAT_MOSI[31:24];
          end
        end

      else
        if (valid_instr_address_d) begin
          // INSTR
          i_dBusWishbone_DAT_MISO[ 7: 0] <= imem0[dmem_addr];
          i_dBusWishbone_DAT_MISO[15: 8] <= imem1[dmem_addr];
          i_dBusWishbone_DAT_MISO[23:16] <= imem2[dmem_addr];
          i_dBusWishbone_DAT_MISO[31:24] <= imem3[dmem_addr];

          if (dBusWishbone_WE) begin
            if (iBusWishbone_SEL[0]) begin
              imem0[dmem_addr]               <= dBusWishbone_DAT_MOSI[ 7: 0];
              i_dBusWishbone_DAT_MISO[ 7: 0] <= dBusWishbone_DAT_MOSI[ 7: 0];
            end

            if (iBusWishbone_SEL[1]) begin
              imem1[dmem_addr]               <= dBusWishbone_DAT_MOSI[15: 8];
              i_dBusWishbone_DAT_MISO[15: 8] <= dBusWishbone_DAT_MOSI[15: 8];
            end

            if (iBusWishbone_SEL[2]) begin
              imem2[dmem_addr]               <= dBusWishbone_DAT_MOSI[23:16];
              i_dBusWishbone_DAT_MISO[23:16] <= dBusWishbone_DAT_MOSI[23:16];
            end

            if (iBusWishbone_SEL[3]) begin
              imem3[dmem_addr]               <= dBusWishbone_DAT_MOSI[31:24];
              i_dBusWishbone_DAT_MISO[31:24] <= dBusWishbone_DAT_MOSI[31:24];
            end
          end
        end
      end
    end
  end

  //============================================================
  // Instruction memory
  //============================================================
  reg [7:0] imem0[0:mem_size_words-1];
  reg [7:0] imem1[0:mem_size_words-1];
  reg [7:0] imem2[0:mem_size_words-1];
  reg [7:0] imem3[0:mem_size_words-1];

  initial begin
    $readmemh("imem0.mem", imem0);
    $readmemh("imem1.mem", imem1);
    $readmemh("imem2.mem", imem2);
    $readmemh("imem3.mem", imem3);
  end

  // A commmand is valid on CYC and STB, and the response is valid on the next cycle
  // provided that CYC and STB are still asserted.
  wire iBus_cmd_valid;
  reg prev_iBus_cmd_valid;
  reg prev_iBus_rsp_valid;
  always @(posedge clk) begin
    prev_iBus_cmd_valid <= iBus_cmd_valid;
    prev_iBus_rsp_valid <= iBus_rsp_valid;
  end

  wire valid_instr_address_i;
  assign valid_instr_address_i = imem_lower <= iBusWishbone_ADR && iBusWishbone_ADR < imem_upper;

  assign iBus_cmd_valid = iBusWishbone_CYC && iBusWishbone_STB;
  wire iBus_rsp_valid;
  assign iBus_rsp_valid = prev_iBus_cmd_valid && iBus_cmd_valid && !prev_iBus_rsp_valid;
  assign i_iBusWishbone_ACK = iBus_rsp_valid && valid_instr_address_i;
  assign i_iBusWishbone_ERR = !valid_instr_address_i;
  wire [mem_addr_bits-1:0] imem_addr;
  assign imem_addr = iBusWishbone_ADR[mem_addr_bits-1:0];

  always @(posedge clk) begin
      i_iBusWishbone_DAT_MISO[ 7: 0] <= imem0[imem_addr];
      i_iBusWishbone_DAT_MISO[15: 8] <= imem1[imem_addr];
      i_iBusWishbone_DAT_MISO[23:16] <= imem2[imem_addr];
      i_iBusWishbone_DAT_MISO[31:24] <= imem3[imem_addr];
  end

  //============================================================
  // CPU instantiation
  //============================================================
  VexRiscvInner VexRiscvInner
    ( .timerInterrupt        (timerInterrupt)
    , .externalInterrupt     (externalInterrupt)
    , .softwareInterrupt     (softwareInterrupt)
    , .debug_resetOut        (reqCpuReset)

    , .iBusWishbone_CYC      (iBusWishbone_CYC)        // o
    , .iBusWishbone_STB      (iBusWishbone_STB)        // o
    , .iBusWishbone_ACK      (i_iBusWishbone_ACK)      // i
    , .iBusWishbone_WE       (iBusWishbone_WE)         // o
    , .iBusWishbone_ADR      (iBusWishbone_ADR)        // o
    , .iBusWishbone_DAT_MISO (i_iBusWishbone_DAT_MISO) // i
    , .iBusWishbone_DAT_MOSI (iBusWishbone_DAT_MOSI)   // o
    , .iBusWishbone_SEL      (iBusWishbone_SEL)        // o
    , .iBusWishbone_ERR      (i_iBusWishbone_ERR)      // i
    , .iBusWishbone_CTI      (iBusWishbone_CTI)        // o
    , .iBusWishbone_BTE      (iBusWishbone_BTE)        // o

    , .dBusWishbone_CYC      (dBusWishbone_CYC)        // o
    , .dBusWishbone_STB      (dBusWishbone_STB)        // o
    , .dBusWishbone_ACK      (i_dBusWishbone_ACK)      // i
    , .dBusWishbone_WE       (dBusWishbone_WE)         // o
    , .dBusWishbone_ADR      (dBusWishbone_ADR)        // o
    , .dBusWishbone_DAT_MISO (i_dBusWishbone_DAT_MISO) // i
    , .dBusWishbone_DAT_MOSI (dBusWishbone_DAT_MOSI)   // o
    , .dBusWishbone_SEL      (dBusWishbone_SEL)        // o
    , .dBusWishbone_ERR      (i_dBusWishbone_ERR)      // i
    , .dBusWishbone_CTI      (dBusWishbone_CTI)        // o
    , .dBusWishbone_BTE      (dBusWishbone_BTE)        // o

    , .jtag_tms              (jtag_tms)
    , .jtag_tdi              (jtag_tdi)
    , .jtag_tdo              (jtag_tdo)
    , .jtag_tck              (jtag_tck)
    , .clk                   (clk)
    , .reset                 (reset_cpu)
    );

endmodule
