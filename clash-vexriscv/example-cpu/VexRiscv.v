// Generator : SpinalHDL v1.10.2a    git head : a348a60b7e8b6a455c72e1536ec3d74a2ea16935
// Component : VexRiscv

`timescale 1ns/1ps

module VexRiscv (
  input  wire          timerInterrupt,
  input  wire          externalInterrupt,
  input  wire          softwareInterrupt,
  input  wire          jtag_tms,
  input  wire          jtag_tdi,
  output wire          jtag_tdo,
  input  wire          jtag_tck,
  output wire          ndmreset,
  input  wire          reset,
  output reg           stoptime,
  output wire          iBusWishbone_CYC,
  output wire          iBusWishbone_STB,
  input  wire          iBusWishbone_ACK,
  output wire          iBusWishbone_WE,
  output wire [29:0]   iBusWishbone_ADR,
  input  wire [31:0]   iBusWishbone_DAT_MISO,
  output wire [31:0]   iBusWishbone_DAT_MOSI,
  output wire [3:0]    iBusWishbone_SEL,
  input  wire          iBusWishbone_ERR,
  output wire [2:0]    iBusWishbone_CTI,
  output wire [1:0]    iBusWishbone_BTE,
  output wire          dBusWishbone_CYC,
  output wire          dBusWishbone_STB,
  input  wire          dBusWishbone_ACK,
  output wire          dBusWishbone_WE,
  output wire [29:0]   dBusWishbone_ADR,
  input  wire [31:0]   dBusWishbone_DAT_MISO,
  output wire [31:0]   dBusWishbone_DAT_MOSI,
  output wire [3:0]    dBusWishbone_SEL,
  input  wire          dBusWishbone_ERR,
  output wire [2:0]    dBusWishbone_CTI,
  output wire [1:0]    dBusWishbone_BTE,
  input  wire          clk
);
  localparam FpuOpcode_LOAD = 4'd0;
  localparam FpuOpcode_STORE = 4'd1;
  localparam FpuOpcode_MUL = 4'd2;
  localparam FpuOpcode_ADD = 4'd3;
  localparam FpuOpcode_FMA = 4'd4;
  localparam FpuOpcode_I2F = 4'd5;
  localparam FpuOpcode_F2I = 4'd6;
  localparam FpuOpcode_CMP = 4'd7;
  localparam FpuOpcode_DIV = 4'd8;
  localparam FpuOpcode_SQRT = 4'd9;
  localparam FpuOpcode_MIN_MAX = 4'd10;
  localparam FpuOpcode_SGNJ = 4'd11;
  localparam FpuOpcode_FMV_X_W = 4'd12;
  localparam FpuOpcode_FMV_W_X = 4'd13;
  localparam FpuOpcode_FCLASS = 4'd14;
  localparam FpuOpcode_FCVT_X_X = 4'd15;
  localparam BranchCtrlEnum_INC = 2'd0;
  localparam BranchCtrlEnum_B = 2'd1;
  localparam BranchCtrlEnum_JAL = 2'd2;
  localparam BranchCtrlEnum_JALR = 2'd3;
  localparam ShiftCtrlEnum_DISABLE_1 = 2'd0;
  localparam ShiftCtrlEnum_SLL_1 = 2'd1;
  localparam ShiftCtrlEnum_SRL_1 = 2'd2;
  localparam ShiftCtrlEnum_SRA_1 = 2'd3;
  localparam AluBitwiseCtrlEnum_XOR_1 = 2'd0;
  localparam AluBitwiseCtrlEnum_OR_1 = 2'd1;
  localparam AluBitwiseCtrlEnum_AND_1 = 2'd2;
  localparam EnvCtrlEnum_NONE = 2'd0;
  localparam EnvCtrlEnum_XRET = 2'd1;
  localparam EnvCtrlEnum_EBREAK = 2'd2;
  localparam AluCtrlEnum_ADD_SUB = 2'd0;
  localparam AluCtrlEnum_SLT_SLTU = 2'd1;
  localparam AluCtrlEnum_BITWISE = 2'd2;
  localparam Src2CtrlEnum_RS = 2'd0;
  localparam Src2CtrlEnum_IMI = 2'd1;
  localparam Src2CtrlEnum_IMS = 2'd2;
  localparam Src2CtrlEnum_PC = 2'd3;
  localparam Src1CtrlEnum_RS = 2'd0;
  localparam Src1CtrlEnum_IMU = 2'd1;
  localparam Src1CtrlEnum_PC_INCREMENT = 2'd2;
  localparam Src1CtrlEnum_URS1 = 2'd3;
  localparam DebugDmToHartOp_DATA = 2'd0;
  localparam DebugDmToHartOp_EXECUTE = 2'd1;
  localparam DebugDmToHartOp_REG_WRITE = 2'd2;
  localparam DebugDmToHartOp_REG_READ = 2'd3;
  localparam FpuFormat_FLOAT = 1'd0;
  localparam FpuFormat_DOUBLE = 1'd1;
  localparam FpuRoundMode_RNE = 3'd0;
  localparam FpuRoundMode_RTZ = 3'd1;
  localparam FpuRoundMode_RDN = 3'd2;
  localparam FpuRoundMode_RUP = 3'd3;
  localparam FpuRoundMode_RMM = 3'd4;
  localparam CsrPlugin_dcsr_stepLogic_enumDef_BOOT = 2'd0;
  localparam CsrPlugin_dcsr_stepLogic_enumDef_IDLE = 2'd1;
  localparam CsrPlugin_dcsr_stepLogic_enumDef_SINGLE = 2'd2;
  localparam CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1 = 2'd3;

  wire                IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_ready;
  wire                dataCache_1_io_cpu_execute_isValid;
  wire       [31:0]   dataCache_1_io_cpu_execute_address;
  wire                dataCache_1_io_cpu_memory_isValid;
  wire       [31:0]   dataCache_1_io_cpu_memory_address;
  reg                 dataCache_1_io_cpu_memory_mmuRsp_isIoAccess;
  reg                 dataCache_1_io_cpu_writeBack_isValid;
  wire                dataCache_1_io_cpu_writeBack_isUser;
  reg        [31:0]   dataCache_1_io_cpu_writeBack_storeData;
  wire       [31:0]   dataCache_1_io_cpu_writeBack_address;
  wire                dataCache_1_io_cpu_writeBack_fence_SW;
  wire                dataCache_1_io_cpu_writeBack_fence_SR;
  wire                dataCache_1_io_cpu_writeBack_fence_SO;
  wire                dataCache_1_io_cpu_writeBack_fence_SI;
  wire                dataCache_1_io_cpu_writeBack_fence_PW;
  wire                dataCache_1_io_cpu_writeBack_fence_PR;
  wire                dataCache_1_io_cpu_writeBack_fence_PO;
  wire                dataCache_1_io_cpu_writeBack_fence_PI;
  wire       [3:0]    dataCache_1_io_cpu_writeBack_fence_FM;
  wire                dataCache_1_io_cpu_flush_valid;
  wire                dataCache_1_io_cpu_flush_payload_singleLine;
  reg        [31:0]   RegFilePlugin_regFile_spinal_port0;
  reg        [31:0]   RegFilePlugin_regFile_spinal_port1;
  wire                IBusSimplePlugin_rspJoin_rspBuffer_c_io_push_ready;
  wire                IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_valid;
  wire                IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_payload_error;
  wire       [31:0]   IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_payload_inst;
  wire       [0:0]    IBusSimplePlugin_rspJoin_rspBuffer_c_io_occupancy;
  wire       [0:0]    IBusSimplePlugin_rspJoin_rspBuffer_c_io_availability;
  wire                dataCache_1_io_cpu_execute_haltIt;
  wire                dataCache_1_io_cpu_execute_refilling;
  wire                dataCache_1_io_cpu_memory_isWrite;
  wire                dataCache_1_io_cpu_writeBack_haltIt;
  wire       [31:0]   dataCache_1_io_cpu_writeBack_data;
  wire                dataCache_1_io_cpu_writeBack_mmuException;
  wire                dataCache_1_io_cpu_writeBack_unalignedAccess;
  wire                dataCache_1_io_cpu_writeBack_accessError;
  wire                dataCache_1_io_cpu_writeBack_isWrite;
  wire                dataCache_1_io_cpu_writeBack_keepMemRspData;
  wire                dataCache_1_io_cpu_writeBack_exclusiveOk;
  wire                dataCache_1_io_cpu_flush_ready;
  wire                dataCache_1_io_cpu_redo;
  wire                dataCache_1_io_cpu_writesPending;
  wire                dataCache_1_io_mem_cmd_valid;
  wire                dataCache_1_io_mem_cmd_payload_wr;
  wire                dataCache_1_io_mem_cmd_payload_uncached;
  wire       [31:0]   dataCache_1_io_mem_cmd_payload_address;
  wire       [31:0]   dataCache_1_io_mem_cmd_payload_data;
  wire       [3:0]    dataCache_1_io_mem_cmd_payload_mask;
  wire       [1:0]    dataCache_1_io_mem_cmd_payload_size;
  wire                dataCache_1_io_mem_cmd_payload_last;
  wire                reset_buffercc_io_dataOut;
  wire                FpuPlugin_fpu_io_port_0_cmd_ready;
  wire                FpuPlugin_fpu_io_port_0_commit_ready;
  wire                FpuPlugin_fpu_io_port_0_rsp_valid;
  wire       [31:0]   FpuPlugin_fpu_io_port_0_rsp_payload_value;
  wire                FpuPlugin_fpu_io_port_0_rsp_payload_NV;
  wire                FpuPlugin_fpu_io_port_0_rsp_payload_NX;
  wire                FpuPlugin_fpu_io_port_0_completion_valid;
  wire                FpuPlugin_fpu_io_port_0_completion_payload_flags_NX;
  wire                FpuPlugin_fpu_io_port_0_completion_payload_flags_UF;
  wire                FpuPlugin_fpu_io_port_0_completion_payload_flags_OF;
  wire                FpuPlugin_fpu_io_port_0_completion_payload_flags_DZ;
  wire                FpuPlugin_fpu_io_port_0_completion_payload_flags_NV;
  wire                FpuPlugin_fpu_io_port_0_completion_payload_written;
  wire                debugModule_1_io_ctrl_cmd_ready;
  wire                debugModule_1_io_ctrl_rsp_valid;
  wire                debugModule_1_io_ctrl_rsp_payload_error;
  wire       [31:0]   debugModule_1_io_ctrl_rsp_payload_data;
  wire                debugModule_1_io_ndmreset;
  wire                debugModule_1_io_harts_0_resume_cmd_valid;
  wire                debugModule_1_io_harts_0_dmToHart_valid;
  wire       [1:0]    debugModule_1_io_harts_0_dmToHart_payload_op;
  wire       [4:0]    debugModule_1_io_harts_0_dmToHart_payload_address;
  wire       [31:0]   debugModule_1_io_harts_0_dmToHart_payload_data;
  wire       [2:0]    debugModule_1_io_harts_0_dmToHart_payload_size;
  wire                debugModule_1_io_harts_0_haltReq;
  wire                debugModule_1_io_harts_0_ackReset;
  wire                debugTransportModuleJtagTap_1_io_jtag_tdo;
  wire                debugTransportModuleJtagTap_1_io_bus_cmd_valid;
  wire                debugTransportModuleJtagTap_1_io_bus_cmd_payload_write;
  wire       [31:0]   debugTransportModuleJtagTap_1_io_bus_cmd_payload_data;
  wire       [6:0]    debugTransportModuleJtagTap_1_io_bus_cmd_payload_address;
  wire       [51:0]   _zz_memory_MUL_LOW;
  wire       [51:0]   _zz_memory_MUL_LOW_1;
  wire       [51:0]   _zz_memory_MUL_LOW_2;
  wire       [32:0]   _zz_memory_MUL_LOW_3;
  wire       [51:0]   _zz_memory_MUL_LOW_4;
  wire       [49:0]   _zz_memory_MUL_LOW_5;
  wire       [51:0]   _zz_memory_MUL_LOW_6;
  wire       [49:0]   _zz_memory_MUL_LOW_7;
  wire       [31:0]   _zz_decode_FORMAL_PC_NEXT;
  wire       [2:0]    _zz_decode_FORMAL_PC_NEXT_1;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_1;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_2;
  wire                _zz_decode_LEGAL_INSTRUCTION_3;
  wire       [0:0]    _zz_decode_LEGAL_INSTRUCTION_4;
  wire       [21:0]   _zz_decode_LEGAL_INSTRUCTION_5;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_6;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_7;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_8;
  wire                _zz_decode_LEGAL_INSTRUCTION_9;
  wire       [0:0]    _zz_decode_LEGAL_INSTRUCTION_10;
  wire       [15:0]   _zz_decode_LEGAL_INSTRUCTION_11;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_12;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_13;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_14;
  wire                _zz_decode_LEGAL_INSTRUCTION_15;
  wire       [0:0]    _zz_decode_LEGAL_INSTRUCTION_16;
  wire       [9:0]    _zz_decode_LEGAL_INSTRUCTION_17;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_18;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_19;
  wire       [31:0]   _zz_decode_LEGAL_INSTRUCTION_20;
  wire                _zz_decode_LEGAL_INSTRUCTION_21;
  wire       [0:0]    _zz_decode_LEGAL_INSTRUCTION_22;
  wire       [3:0]    _zz_decode_LEGAL_INSTRUCTION_23;
  wire       [2:0]    _zz__zz_IBusSimplePlugin_jump_pcLoad_payload_1;
  reg        [31:0]   _zz_IBusSimplePlugin_jump_pcLoad_payload_4;
  wire       [1:0]    _zz_IBusSimplePlugin_jump_pcLoad_payload_5;
  wire       [31:0]   _zz_IBusSimplePlugin_fetchPc_pc;
  wire       [2:0]    _zz_IBusSimplePlugin_fetchPc_pc_1;
  wire       [31:0]   _zz_IBusSimplePlugin_decodePc_pcPlus;
  wire       [2:0]    _zz_IBusSimplePlugin_decodePc_pcPlus_1;
  wire       [4:0]    _zz_IBusSimplePlugin_decompressor_decompressed_27;
  wire       [0:0]    _zz_IBusSimplePlugin_decompressor_decompressed_28;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_29;
  wire       [31:0]   _zz_IBusSimplePlugin_decompressor_decompressed_30;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_31;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_32;
  wire       [6:0]    _zz_IBusSimplePlugin_decompressor_decompressed_33;
  wire       [4:0]    _zz_IBusSimplePlugin_decompressor_decompressed_34;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_35;
  wire       [4:0]    _zz_IBusSimplePlugin_decompressor_decompressed_36;
  wire       [11:0]   _zz_IBusSimplePlugin_decompressor_decompressed_37;
  wire       [11:0]   _zz_IBusSimplePlugin_decompressor_decompressed_38;
  wire       [11:0]   _zz_IBusSimplePlugin_decompressor_decompressed_39;
  wire       [11:0]   _zz_IBusSimplePlugin_decompressor_decompressed_40;
  wire       [2:0]    _zz_IBusSimplePlugin_pending_next;
  wire       [2:0]    _zz_IBusSimplePlugin_pending_next_1;
  wire       [0:0]    _zz_IBusSimplePlugin_pending_next_2;
  wire       [2:0]    _zz_IBusSimplePlugin_pending_next_3;
  wire       [0:0]    _zz_IBusSimplePlugin_pending_next_4;
  wire       [2:0]    _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter;
  wire       [0:0]    _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter_1;
  wire       [2:0]    _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter_2;
  wire       [0:0]    _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter_3;
  wire       [2:0]    _zz_DBusCachedPlugin_exceptionBus_payload_code;
  wire       [2:0]    _zz_DBusCachedPlugin_exceptionBus_payload_code_1;
  reg        [7:0]    _zz_writeBack_DBusCachedPlugin_rspShifted;
  wire       [1:0]    _zz_writeBack_DBusCachedPlugin_rspShifted_1;
  reg        [7:0]    _zz_writeBack_DBusCachedPlugin_rspShifted_2;
  wire       [0:0]    _zz_writeBack_DBusCachedPlugin_rspShifted_3;
  wire       [2:0]    _zz_CsrPlugin_timeout_counter_valueNext;
  wire       [0:0]    _zz_CsrPlugin_timeout_counter_valueNext_1;
  wire       [0:0]    _zz_when;
  wire       [1:0]    _zz_CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext;
  wire       [0:0]    _zz_CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext_1;
  wire       [63:0]   _zz_CsrPlugin_mcycle;
  wire       [0:0]    _zz_CsrPlugin_mcycle_1;
  wire       [1:0]    _zz__zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code_1;
  wire       [1:0]    _zz__zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code_1_1;
  wire                _zz_when_1;
  wire       [31:0]   _zz__zz_decode_FPU_RSP;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_1;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_2;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_3;
  wire                _zz__zz_decode_FPU_RSP_4;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_5;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_6;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_7;
  wire                _zz__zz_decode_FPU_RSP_8;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_9;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_10;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_11;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_12;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_13;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_14;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_15;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_16;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_17;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_18;
  wire       [34:0]   _zz__zz_decode_FPU_RSP_19;
  wire       [2:0]    _zz__zz_decode_FPU_RSP_20;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_21;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_22;
  wire                _zz__zz_decode_FPU_RSP_23;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_24;
  wire                _zz__zz_decode_FPU_RSP_25;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_26;
  wire                _zz__zz_decode_FPU_RSP_27;
  wire                _zz__zz_decode_FPU_RSP_28;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_29;
  wire                _zz__zz_decode_FPU_RSP_30;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_31;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_32;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_33;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_34;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_35;
  wire       [2:0]    _zz__zz_decode_FPU_RSP_36;
  wire                _zz__zz_decode_FPU_RSP_37;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_38;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_39;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_40;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_41;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_42;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_43;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_44;
  wire       [30:0]   _zz__zz_decode_FPU_RSP_45;
  wire       [4:0]    _zz__zz_decode_FPU_RSP_46;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_47;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_48;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_49;
  wire       [2:0]    _zz__zz_decode_FPU_RSP_50;
  wire                _zz__zz_decode_FPU_RSP_51;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_52;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_53;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_54;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_55;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_56;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_57;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_58;
  wire                _zz__zz_decode_FPU_RSP_59;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_60;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_61;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_62;
  wire       [2:0]    _zz__zz_decode_FPU_RSP_63;
  wire                _zz__zz_decode_FPU_RSP_64;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_65;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_66;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_67;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_68;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_69;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_70;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_71;
  wire       [27:0]   _zz__zz_decode_FPU_RSP_72;
  wire                _zz__zz_decode_FPU_RSP_73;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_74;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_75;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_76;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_77;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_78;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_79;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_80;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_81;
  wire       [25:0]   _zz__zz_decode_FPU_RSP_82;
  wire                _zz__zz_decode_FPU_RSP_83;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_84;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_85;
  wire       [23:0]   _zz__zz_decode_FPU_RSP_86;
  wire                _zz__zz_decode_FPU_RSP_87;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_88;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_89;
  wire                _zz__zz_decode_FPU_RSP_90;
  wire                _zz__zz_decode_FPU_RSP_91;
  wire       [21:0]   _zz__zz_decode_FPU_RSP_92;
  wire                _zz__zz_decode_FPU_RSP_93;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_94;
  wire                _zz__zz_decode_FPU_RSP_95;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_96;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_97;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_98;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_99;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_100;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_101;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_102;
  wire       [17:0]   _zz__zz_decode_FPU_RSP_103;
  wire                _zz__zz_decode_FPU_RSP_104;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_105;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_106;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_107;
  wire                _zz__zz_decode_FPU_RSP_108;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_109;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_110;
  wire                _zz__zz_decode_FPU_RSP_111;
  wire                _zz__zz_decode_FPU_RSP_112;
  wire       [13:0]   _zz__zz_decode_FPU_RSP_113;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_114;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_115;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_116;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_117;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_118;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_119;
  wire                _zz__zz_decode_FPU_RSP_120;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_121;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_122;
  wire       [2:0]    _zz__zz_decode_FPU_RSP_123;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_124;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_125;
  wire                _zz__zz_decode_FPU_RSP_126;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_127;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_128;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_129;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_130;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_131;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_132;
  wire       [9:0]    _zz__zz_decode_FPU_RSP_133;
  wire       [6:0]    _zz__zz_decode_FPU_RSP_134;
  wire                _zz__zz_decode_FPU_RSP_135;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_136;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_137;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_138;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_139;
  wire       [3:0]    _zz__zz_decode_FPU_RSP_140;
  wire                _zz__zz_decode_FPU_RSP_141;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_142;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_143;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_144;
  wire                _zz__zz_decode_FPU_RSP_145;
  wire                _zz__zz_decode_FPU_RSP_146;
  wire                _zz__zz_decode_FPU_RSP_147;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_148;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_149;
  wire                _zz__zz_decode_FPU_RSP_150;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_151;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_152;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_153;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_154;
  wire                _zz__zz_decode_FPU_RSP_155;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_156;
  wire       [6:0]    _zz__zz_decode_FPU_RSP_157;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_158;
  wire                _zz__zz_decode_FPU_RSP_159;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_160;
  wire                _zz__zz_decode_FPU_RSP_161;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_162;
  wire                _zz__zz_decode_FPU_RSP_163;
  wire                _zz__zz_decode_FPU_RSP_164;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_165;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_166;
  wire       [6:0]    _zz__zz_decode_FPU_RSP_167;
  wire                _zz__zz_decode_FPU_RSP_168;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_169;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_170;
  wire       [4:0]    _zz__zz_decode_FPU_RSP_171;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_172;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_173;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_174;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_175;
  wire       [3:0]    _zz__zz_decode_FPU_RSP_176;
  wire                _zz__zz_decode_FPU_RSP_177;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_178;
  wire       [2:0]    _zz__zz_decode_FPU_RSP_179;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_180;
  wire       [31:0]   _zz__zz_decode_FPU_RSP_181;
  wire                _zz__zz_decode_FPU_RSP_182;
  wire                _zz__zz_decode_FPU_RSP_183;
  wire       [1:0]    _zz__zz_decode_FPU_RSP_184;
  wire                _zz__zz_decode_FPU_RSP_185;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_186;
  wire       [0:0]    _zz__zz_decode_FPU_RSP_187;
  wire                _zz__zz_decode_FPU_RSP_188;
  wire                _zz_RegFilePlugin_regFile_port;
  wire                _zz_decode_RegFilePlugin_rs1Data;
  wire                _zz_RegFilePlugin_regFile_port_1;
  wire                _zz_decode_RegFilePlugin_rs2Data;
  wire       [0:0]    _zz__zz_execute_REGFILE_WRITE_DATA;
  wire       [2:0]    _zz__zz_decode_SRC1;
  wire       [4:0]    _zz__zz_decode_SRC1_1;
  wire       [11:0]   _zz__zz_decode_SRC2_2;
  wire       [31:0]   _zz_execute_SrcPlugin_addSub;
  wire       [31:0]   _zz_execute_SrcPlugin_addSub_1;
  wire       [31:0]   _zz_execute_SrcPlugin_addSub_2;
  wire       [31:0]   _zz_execute_SrcPlugin_addSub_3;
  wire       [31:0]   _zz_execute_SrcPlugin_addSub_4;
  wire       [65:0]   _zz_writeBack_MulPlugin_result;
  wire       [65:0]   _zz_writeBack_MulPlugin_result_1;
  wire       [31:0]   _zz__zz_lastStageRegFileWrite_payload_data;
  wire       [31:0]   _zz__zz_lastStageRegFileWrite_payload_data_1;
  wire       [5:0]    _zz_memory_DivPlugin_div_counter_valueNext;
  wire       [0:0]    _zz_memory_DivPlugin_div_counter_valueNext_1;
  wire       [32:0]   _zz_memory_DivPlugin_div_stage_0_remainderMinusDenominator;
  wire       [31:0]   _zz_memory_DivPlugin_div_stage_0_outRemainder;
  wire       [31:0]   _zz_memory_DivPlugin_div_stage_0_outRemainder_1;
  wire       [32:0]   _zz_memory_DivPlugin_div_stage_0_outNumerator;
  wire       [32:0]   _zz_memory_DivPlugin_div_result_1;
  wire       [32:0]   _zz_memory_DivPlugin_div_result_2;
  wire       [32:0]   _zz_memory_DivPlugin_div_result_3;
  wire       [32:0]   _zz_memory_DivPlugin_div_result_4;
  wire       [0:0]    _zz_memory_DivPlugin_div_result_5;
  wire       [32:0]   _zz_memory_DivPlugin_rs1_2;
  wire       [0:0]    _zz_memory_DivPlugin_rs1_3;
  wire       [31:0]   _zz_memory_DivPlugin_rs2_1;
  wire       [0:0]    _zz_memory_DivPlugin_rs2_2;
  wire       [5:0]    _zz_FpuPlugin_pendings;
  wire       [5:0]    _zz_FpuPlugin_pendings_1;
  wire       [5:0]    _zz_FpuPlugin_pendings_2;
  wire       [0:0]    _zz_FpuPlugin_pendings_3;
  wire       [5:0]    _zz_FpuPlugin_pendings_4;
  wire       [0:0]    _zz_FpuPlugin_pendings_5;
  wire       [5:0]    _zz_FpuPlugin_pendings_6;
  wire       [0:0]    _zz_FpuPlugin_pendings_7;
  wire       [31:0]   _zz__zz_execute_to_memory_REGFILE_WRITE_DATA_1;
  wire       [32:0]   _zz__zz_execute_to_memory_REGFILE_WRITE_DATA_1_1;
  wire       [19:0]   _zz__zz_execute_BranchPlugin_branch_src2;
  wire       [11:0]   _zz__zz_execute_BranchPlugin_branch_src2_4;
  reg        [31:0]   _zz__zz_CsrPlugin_csrMapping_readDataInit_5;
  wire       [7:0]    _zz_when_CsrPlugin_l1718;
  wire       [31:0]   writeBack_MEMORY_LOAD_DATA;
  wire       [51:0]   memory_MUL_LOW;
  wire       [31:0]   execute_BRANCH_CALC;
  wire                execute_BRANCH_DO;
  wire       [33:0]   memory_MUL_HH;
  wire       [33:0]   execute_MUL_HH;
  wire       [33:0]   execute_MUL_HL;
  wire       [33:0]   execute_MUL_LH;
  wire       [31:0]   execute_MUL_LL;
  wire       [31:0]   execute_REGFILE_WRITE_DATA;
  wire       [31:0]   memory_MEMORY_STORE_DATA_RF;
  wire       [31:0]   execute_MEMORY_STORE_DATA_RF;
  wire                memory_FPU_COMMIT_LOAD;
  wire                execute_FPU_COMMIT_LOAD;
  wire                decode_FPU_COMMIT_LOAD;
  wire                memory_FPU_FORKED;
  wire                execute_FPU_FORKED;
  wire                decode_FPU_FORKED;
  wire       [31:0]   decode_SRC2;
  wire       [31:0]   decode_SRC1;
  wire                decode_SRC2_FORCE_ZERO;
  wire       [31:0]   decode_RS2;
  wire       [31:0]   memory_RS1;
  wire       [31:0]   decode_RS1;
  wire       [1:0]    decode_BRANCH_CTRL;
  wire       [1:0]    _zz_decode_BRANCH_CTRL;
  wire       [1:0]    _zz_decode_to_execute_BRANCH_CTRL;
  wire       [1:0]    _zz_decode_to_execute_BRANCH_CTRL_1;
  wire       [1:0]    decode_SHIFT_CTRL;
  wire       [1:0]    _zz_decode_SHIFT_CTRL;
  wire       [1:0]    _zz_decode_to_execute_SHIFT_CTRL;
  wire       [1:0]    _zz_decode_to_execute_SHIFT_CTRL_1;
  wire       [3:0]    memory_FPU_OPCODE;
  wire       [3:0]    _zz_memory_FPU_OPCODE;
  wire       [3:0]    _zz_memory_to_writeBack_FPU_OPCODE;
  wire       [3:0]    _zz_memory_to_writeBack_FPU_OPCODE_1;
  wire       [3:0]    execute_FPU_OPCODE;
  wire       [3:0]    _zz_execute_FPU_OPCODE;
  wire       [3:0]    _zz_execute_to_memory_FPU_OPCODE;
  wire       [3:0]    _zz_execute_to_memory_FPU_OPCODE_1;
  wire       [3:0]    _zz_decode_to_execute_FPU_OPCODE;
  wire       [3:0]    _zz_decode_to_execute_FPU_OPCODE_1;
  wire                memory_FPU_RSP;
  wire                execute_FPU_RSP;
  wire                decode_FPU_RSP;
  wire                memory_FPU_COMMIT;
  wire                execute_FPU_COMMIT;
  wire                decode_FPU_COMMIT;
  wire                decode_IS_RS2_SIGNED;
  wire                decode_IS_RS1_SIGNED;
  wire                decode_IS_DIV;
  wire                memory_IS_MUL;
  wire                execute_IS_MUL;
  wire                decode_IS_MUL;
  wire       [1:0]    decode_ALU_BITWISE_CTRL;
  wire       [1:0]    _zz_decode_ALU_BITWISE_CTRL;
  wire       [1:0]    _zz_decode_to_execute_ALU_BITWISE_CTRL;
  wire       [1:0]    _zz_decode_to_execute_ALU_BITWISE_CTRL_1;
  wire                decode_SRC_LESS_UNSIGNED;
  wire       [1:0]    _zz_memory_to_writeBack_ENV_CTRL;
  wire       [1:0]    _zz_memory_to_writeBack_ENV_CTRL_1;
  wire       [1:0]    _zz_execute_to_memory_ENV_CTRL;
  wire       [1:0]    _zz_execute_to_memory_ENV_CTRL_1;
  wire       [1:0]    decode_ENV_CTRL;
  wire       [1:0]    _zz_decode_ENV_CTRL;
  wire       [1:0]    _zz_decode_to_execute_ENV_CTRL;
  wire       [1:0]    _zz_decode_to_execute_ENV_CTRL_1;
  wire                decode_IS_CSR;
  wire                decode_MEMORY_MANAGMENT;
  wire                memory_MEMORY_WR;
  wire                decode_MEMORY_WR;
  wire                execute_BYPASSABLE_MEMORY_STAGE;
  wire                decode_BYPASSABLE_MEMORY_STAGE;
  wire                decode_BYPASSABLE_EXECUTE_STAGE;
  wire       [1:0]    decode_ALU_CTRL;
  wire       [1:0]    _zz_decode_ALU_CTRL;
  wire       [1:0]    _zz_decode_to_execute_ALU_CTRL;
  wire       [1:0]    _zz_decode_to_execute_ALU_CTRL_1;
  wire                decode_CSR_READ_OPCODE;
  wire                decode_CSR_WRITE_OPCODE;
  wire                decode_MEMORY_FORCE_CONSTISTENCY;
  wire       [31:0]   writeBack_FORMAL_PC_NEXT;
  wire       [31:0]   memory_FORMAL_PC_NEXT;
  wire       [31:0]   execute_FORMAL_PC_NEXT;
  wire       [31:0]   decode_FORMAL_PC_NEXT;
  wire       [31:0]   memory_PC;
  wire       [31:0]   memory_BRANCH_CALC;
  wire                memory_BRANCH_DO;
  wire       [31:0]   execute_PC;
  wire       [1:0]    execute_BRANCH_CTRL;
  wire       [1:0]    _zz_execute_BRANCH_CTRL;
  wire                decode_RS2_USE;
  wire                decode_RS1_USE;
  wire                execute_REGFILE_WRITE_VALID;
  wire                execute_BYPASSABLE_EXECUTE_STAGE;
  wire                memory_REGFILE_WRITE_VALID;
  wire                memory_BYPASSABLE_MEMORY_STAGE;
  wire                writeBack_REGFILE_WRITE_VALID;
  wire       [1:0]    execute_SHIFT_CTRL;
  wire       [1:0]    _zz_execute_SHIFT_CTRL;
  reg                 _zz_memory_to_writeBack_FPU_FORKED;
  reg                 _zz_execute_to_memory_FPU_FORKED;
  reg                 _zz_decode_to_execute_FPU_FORKED;
  wire       [3:0]    writeBack_FPU_OPCODE;
  wire       [3:0]    _zz_writeBack_FPU_OPCODE;
  wire       [31:0]   writeBack_RS1;
  wire                writeBack_FPU_COMMIT_LOAD;
  reg                 DBusBypass0_cond;
  wire                writeBack_FPU_COMMIT;
  wire                writeBack_FPU_RSP;
  wire                writeBack_FPU_FORKED;
  wire       [1:0]    decode_FPU_ARG;
  wire       [3:0]    decode_FPU_OPCODE;
  wire       [3:0]    _zz_decode_FPU_OPCODE;
  reg                 decode_FPU_ENABLE;
  wire                execute_IS_RS1_SIGNED;
  wire                execute_IS_DIV;
  wire                execute_IS_RS2_SIGNED;
  reg        [31:0]   _zz_memory_to_writeBack_REGFILE_WRITE_DATA;
  wire       [31:0]   memory_INSTRUCTION;
  wire                memory_IS_DIV;
  wire                writeBack_IS_MUL;
  wire       [33:0]   writeBack_MUL_HH;
  wire       [51:0]   writeBack_MUL_LOW;
  wire       [33:0]   memory_MUL_HL;
  wire       [33:0]   memory_MUL_LH;
  wire       [31:0]   memory_MUL_LL;
  wire                execute_SRC_LESS_UNSIGNED;
  wire                execute_SRC2_FORCE_ZERO;
  wire                execute_SRC_USE_SUB_LESS;
  wire       [31:0]   _zz_decode_to_execute_PC;
  wire       [31:0]   _zz_decode_to_execute_RS2;
  wire       [1:0]    decode_SRC2_CTRL;
  wire       [1:0]    _zz_decode_SRC2_CTRL;
  wire       [31:0]   _zz_decode_to_execute_RS1;
  wire       [1:0]    decode_SRC1_CTRL;
  wire       [1:0]    _zz_decode_SRC1_CTRL;
  wire                decode_SRC_USE_SUB_LESS;
  wire                decode_SRC_ADD_ZERO;
  wire       [31:0]   execute_SRC_ADD_SUB;
  wire                execute_SRC_LESS;
  wire       [1:0]    execute_ALU_CTRL;
  wire       [1:0]    _zz_execute_ALU_CTRL;
  wire       [31:0]   execute_SRC2;
  wire       [1:0]    execute_ALU_BITWISE_CTRL;
  wire       [1:0]    _zz_execute_ALU_BITWISE_CTRL;
  wire       [31:0]   _zz_lastStageRegFileWrite_payload_address;
  wire                _zz_lastStageRegFileWrite_valid;
  reg                 _zz_1;
  wire       [31:0]   decode_INSTRUCTION_ANTICIPATED;
  reg                 decode_REGFILE_WRITE_VALID;
  reg                 decode_LEGAL_INSTRUCTION;
  wire       [1:0]    _zz_decode_BRANCH_CTRL_1;
  wire       [1:0]    _zz_decode_SHIFT_CTRL_1;
  wire       [3:0]    _zz_decode_FPU_OPCODE_1;
  wire                _zz_decode_FPU_ENABLE;
  wire       [1:0]    _zz_decode_ALU_BITWISE_CTRL_1;
  wire       [1:0]    _zz_decode_ENV_CTRL_1;
  wire       [1:0]    _zz_decode_SRC2_CTRL_1;
  wire       [1:0]    _zz_decode_ALU_CTRL_1;
  wire       [1:0]    _zz_decode_SRC1_CTRL_1;
  reg        [31:0]   _zz_execute_to_memory_REGFILE_WRITE_DATA;
  wire                execute_CSR_READ_OPCODE;
  wire                execute_CSR_WRITE_OPCODE;
  wire                execute_IS_CSR;
  wire       [1:0]    memory_ENV_CTRL;
  wire       [1:0]    _zz_memory_ENV_CTRL;
  wire       [1:0]    execute_ENV_CTRL;
  wire       [1:0]    _zz_execute_ENV_CTRL;
  wire       [1:0]    writeBack_ENV_CTRL;
  wire       [1:0]    _zz_writeBack_ENV_CTRL;
  wire       [31:0]   execute_SRC1;
  reg                 CsrPlugin_running_aheadValue;
  reg        [31:0]   _zz_lastStageRegFileWrite_payload_data;
  wire                writeBack_MEMORY_WR;
  wire       [31:0]   writeBack_MEMORY_STORE_DATA_RF;
  wire       [31:0]   writeBack_REGFILE_WRITE_DATA;
  wire                writeBack_MEMORY_ENABLE;
  wire       [31:0]   memory_REGFILE_WRITE_DATA;
  wire                memory_MEMORY_ENABLE;
  wire                execute_MEMORY_FORCE_CONSTISTENCY;
  (* keep , syn_keep *) wire       [31:0]   execute_RS1 /* synthesis syn_keep = 1 */ ;
  wire                execute_MEMORY_MANAGMENT;
  (* keep , syn_keep *) wire       [31:0]   execute_RS2 /* synthesis syn_keep = 1 */ ;
  wire                execute_MEMORY_WR;
  wire       [31:0]   execute_SRC_ADD;
  wire                execute_MEMORY_ENABLE;
  wire       [31:0]   execute_INSTRUCTION;
  wire                decode_MEMORY_ENABLE;
  reg        [31:0]   _zz_memory_to_writeBack_FORMAL_PC_NEXT;
  wire       [31:0]   decode_PC;
  wire       [31:0]   decode_INSTRUCTION;
  wire                decode_IS_RVC;
  wire       [31:0]   writeBack_PC;
  wire       [31:0]   writeBack_INSTRUCTION;
  reg                 decode_arbitration_haltItself;
  reg                 decode_arbitration_haltByOther;
  reg                 decode_arbitration_removeIt;
  wire                decode_arbitration_flushIt;
  reg                 decode_arbitration_flushNext;
  reg                 decode_arbitration_isValid;
  wire                decode_arbitration_isStuck;
  wire                decode_arbitration_isStuckByOthers;
  wire                decode_arbitration_isFlushed;
  wire                decode_arbitration_isMoving;
  wire                decode_arbitration_isFiring;
  reg                 execute_arbitration_haltItself;
  reg                 execute_arbitration_haltByOther;
  reg                 execute_arbitration_removeIt;
  wire                execute_arbitration_flushIt;
  reg                 execute_arbitration_flushNext;
  reg                 execute_arbitration_isValid;
  wire                execute_arbitration_isStuck;
  wire                execute_arbitration_isStuckByOthers;
  wire                execute_arbitration_isFlushed;
  wire                execute_arbitration_isMoving;
  wire                execute_arbitration_isFiring;
  reg                 memory_arbitration_haltItself;
  wire                memory_arbitration_haltByOther;
  reg                 memory_arbitration_removeIt;
  wire                memory_arbitration_flushIt;
  reg                 memory_arbitration_flushNext;
  reg                 memory_arbitration_isValid;
  wire                memory_arbitration_isStuck;
  wire                memory_arbitration_isStuckByOthers;
  wire                memory_arbitration_isFlushed;
  wire                memory_arbitration_isMoving;
  wire                memory_arbitration_isFiring;
  reg                 writeBack_arbitration_haltItself;
  reg                 writeBack_arbitration_haltByOther;
  reg                 writeBack_arbitration_removeIt;
  reg                 writeBack_arbitration_flushIt;
  reg                 writeBack_arbitration_flushNext;
  reg                 writeBack_arbitration_isValid;
  wire                writeBack_arbitration_isStuck;
  wire                writeBack_arbitration_isStuckByOthers;
  wire                writeBack_arbitration_isFlushed;
  wire                writeBack_arbitration_isMoving;
  wire                writeBack_arbitration_isFiring;
  wire       [31:0]   lastStageInstruction /* verilator public */ ;
  wire       [31:0]   lastStagePc /* verilator public */ ;
  wire                lastStageIsValid /* verilator public */ ;
  wire                lastStageIsFiring /* verilator public */ ;
  reg                 IBusSimplePlugin_fetcherHalt;
  wire                IBusSimplePlugin_forceNoDecodeCond;
  reg                 IBusSimplePlugin_incomingInstruction;
  wire                IBusSimplePlugin_pcValids_0;
  wire                IBusSimplePlugin_pcValids_1;
  wire                IBusSimplePlugin_pcValids_2;
  wire                IBusSimplePlugin_pcValids_3;
  wire                iBus_cmd_valid;
  reg                 iBus_cmd_ready;
  wire       [31:0]   iBus_cmd_payload_pc;
  wire                iBus_rsp_valid;
  wire                iBus_rsp_payload_error;
  wire       [31:0]   iBus_rsp_payload_inst;
  wire                IBusSimplePlugin_decodeExceptionPort_valid;
  reg        [3:0]    IBusSimplePlugin_decodeExceptionPort_payload_code;
  wire       [31:0]   IBusSimplePlugin_decodeExceptionPort_payload_badAddr;
  wire                dBus_cmd_valid;
  wire                dBus_cmd_ready;
  wire                dBus_cmd_payload_wr;
  wire                dBus_cmd_payload_uncached;
  wire       [31:0]   dBus_cmd_payload_address;
  wire       [31:0]   dBus_cmd_payload_data;
  wire       [3:0]    dBus_cmd_payload_mask;
  wire       [1:0]    dBus_cmd_payload_size;
  wire                dBus_cmd_payload_last;
  wire                dBus_rsp_valid;
  wire                dBus_rsp_payload_last;
  wire       [31:0]   dBus_rsp_payload_data;
  wire                dBus_rsp_payload_error;
  wire                DBusCachedPlugin_mmuBus_cmd_0_isValid;
  wire                DBusCachedPlugin_mmuBus_cmd_0_isStuck;
  wire       [31:0]   DBusCachedPlugin_mmuBus_cmd_0_virtualAddress;
  wire                DBusCachedPlugin_mmuBus_cmd_0_bypassTranslation;
  wire       [31:0]   DBusCachedPlugin_mmuBus_rsp_physicalAddress;
  wire                DBusCachedPlugin_mmuBus_rsp_isIoAccess;
  wire                DBusCachedPlugin_mmuBus_rsp_isPaging;
  wire                DBusCachedPlugin_mmuBus_rsp_allowRead;
  wire                DBusCachedPlugin_mmuBus_rsp_allowWrite;
  wire                DBusCachedPlugin_mmuBus_rsp_allowExecute;
  wire                DBusCachedPlugin_mmuBus_rsp_exception;
  wire                DBusCachedPlugin_mmuBus_rsp_refilling;
  wire                DBusCachedPlugin_mmuBus_rsp_bypassTranslation;
  wire                DBusCachedPlugin_mmuBus_end;
  wire                DBusCachedPlugin_mmuBus_busy;
  reg                 DBusCachedPlugin_redoBranch_valid;
  wire       [31:0]   DBusCachedPlugin_redoBranch_payload;
  reg                 DBusCachedPlugin_exceptionBus_valid;
  reg        [3:0]    DBusCachedPlugin_exceptionBus_payload_code;
  wire       [31:0]   DBusCachedPlugin_exceptionBus_payload_badAddr;
  wire       [31:0]   CsrPlugin_csrMapping_readDataSignal;
  wire       [31:0]   CsrPlugin_csrMapping_readDataInit;
  wire       [31:0]   CsrPlugin_csrMapping_writeDataSignal;
  reg                 CsrPlugin_csrMapping_allowCsrSignal;
  wire                CsrPlugin_csrMapping_hazardFree;
  reg                 CsrPlugin_csrMapping_doForceFailCsr;
  wire                CsrPlugin_inWfi /* verilator public */ ;
  reg                 CsrPlugin_thirdPartyWake;
  reg                 CsrPlugin_jumpInterface_valid;
  reg        [31:0]   CsrPlugin_jumpInterface_payload;
  wire                CsrPlugin_exceptionPendings_0;
  wire                CsrPlugin_exceptionPendings_1;
  wire                CsrPlugin_exceptionPendings_2;
  wire                CsrPlugin_exceptionPendings_3;
  wire                contextSwitching;
  reg        [1:0]    CsrPlugin_privilege;
  wire                CsrPlugin_forceMachineWire;
  reg                 CsrPlugin_selfException_valid;
  reg        [3:0]    CsrPlugin_selfException_payload_code;
  wire       [31:0]   CsrPlugin_selfException_payload_badAddr;
  reg                 CsrPlugin_allowInterrupts;
  wire                CsrPlugin_allowException;
  wire                CsrPlugin_allowEbreakException;
  reg                 CsrPlugin_xretAwayFromMachine;
  wire                CsrPlugin_injectionPort_valid;
  reg                 CsrPlugin_injectionPort_ready;
  wire       [31:0]   CsrPlugin_injectionPort_payload;
  wire                debugMode;
  wire                debugBus_halted;
  wire                debugBus_running;
  wire                debugBus_unavailable;
  reg                 debugBus_exception;
  wire                debugBus_commit;
  reg                 debugBus_ebreak;
  wire                debugBus_redo;
  wire                debugBus_regSuccess;
  wire                debugBus_ackReset;
  wire                debugBus_haveReset;
  wire                debugBus_resume_cmd_valid;
  reg                 debugBus_resume_rsp_valid;
  wire                debugBus_haltReq;
  wire                debugBus_dmToHart_valid;
  wire       [1:0]    debugBus_dmToHart_payload_op;
  wire       [4:0]    debugBus_dmToHart_payload_address;
  wire       [31:0]   debugBus_dmToHart_payload_data;
  wire       [2:0]    debugBus_dmToHart_payload_size;
  wire                debugBus_hartToDm_valid;
  wire       [3:0]    debugBus_hartToDm_payload_address;
  wire       [31:0]   debugBus_hartToDm_payload_data;
  wire                decodeExceptionPort_valid;
  wire       [3:0]    decodeExceptionPort_payload_code;
  wire       [31:0]   decodeExceptionPort_payload_badAddr;
  wire                FpuPlugin_port_cmd_valid /* verilator public */ ;
  wire                FpuPlugin_port_cmd_ready /* verilator public */ ;
  wire       [3:0]    FpuPlugin_port_cmd_payload_opcode /* verilator public */ ;
  wire       [1:0]    FpuPlugin_port_cmd_payload_arg /* verilator public */ ;
  wire       [4:0]    FpuPlugin_port_cmd_payload_rs1 /* verilator public */ ;
  wire       [4:0]    FpuPlugin_port_cmd_payload_rs2 /* verilator public */ ;
  wire       [4:0]    FpuPlugin_port_cmd_payload_rs3 /* verilator public */ ;
  wire       [4:0]    FpuPlugin_port_cmd_payload_rd /* verilator public */ ;
  wire       [0:0]    FpuPlugin_port_cmd_payload_format /* verilator public */ ;
  wire       [2:0]    FpuPlugin_port_cmd_payload_roundMode /* verilator public */ ;
  wire                FpuPlugin_port_commit_valid /* verilator public */ ;
  wire                FpuPlugin_port_commit_ready /* verilator public */ ;
  wire       [3:0]    FpuPlugin_port_commit_payload_opcode /* verilator public */ ;
  wire       [4:0]    FpuPlugin_port_commit_payload_rd /* verilator public */ ;
  wire                FpuPlugin_port_commit_payload_write /* verilator public */ ;
  wire       [31:0]   FpuPlugin_port_commit_payload_value /* verilator public */ ;
  wire                FpuPlugin_port_rsp_valid /* verilator public */ ;
  reg                 FpuPlugin_port_rsp_ready /* verilator public */ ;
  wire       [31:0]   FpuPlugin_port_rsp_payload_value /* verilator public */ ;
  wire                FpuPlugin_port_rsp_payload_NV /* verilator public */ ;
  wire                FpuPlugin_port_rsp_payload_NX /* verilator public */ ;
  wire                FpuPlugin_port_completion_valid /* verilator public */ ;
  wire                FpuPlugin_port_completion_payload_flags_NX /* verilator public */ ;
  wire                FpuPlugin_port_completion_payload_flags_UF /* verilator public */ ;
  wire                FpuPlugin_port_completion_payload_flags_OF /* verilator public */ ;
  wire                FpuPlugin_port_completion_payload_flags_DZ /* verilator public */ ;
  wire                FpuPlugin_port_completion_payload_flags_NV /* verilator public */ ;
  wire                FpuPlugin_port_completion_payload_written /* verilator public */ ;
  wire                BranchPlugin_jumpInterface_valid;
  wire       [31:0]   BranchPlugin_jumpInterface_payload;
  reg                 BranchPlugin_inDebugNoFetchFlag;
  wire                IBusSimplePlugin_externalFlush;
  wire                IBusSimplePlugin_jump_pcLoad_valid;
  wire       [31:0]   IBusSimplePlugin_jump_pcLoad_payload;
  wire       [2:0]    _zz_IBusSimplePlugin_jump_pcLoad_payload;
  wire       [2:0]    _zz_IBusSimplePlugin_jump_pcLoad_payload_1;
  wire                _zz_IBusSimplePlugin_jump_pcLoad_payload_2;
  wire                _zz_IBusSimplePlugin_jump_pcLoad_payload_3;
  wire                IBusSimplePlugin_fetchPc_output_valid;
  wire                IBusSimplePlugin_fetchPc_output_ready;
  wire       [31:0]   IBusSimplePlugin_fetchPc_output_payload;
  reg        [31:0]   IBusSimplePlugin_fetchPc_pcReg /* verilator public */ ;
  reg                 IBusSimplePlugin_fetchPc_correction;
  reg                 IBusSimplePlugin_fetchPc_correctionReg;
  wire                IBusSimplePlugin_fetchPc_output_fire;
  wire                IBusSimplePlugin_fetchPc_corrected;
  reg                 IBusSimplePlugin_fetchPc_pcRegPropagate;
  reg                 IBusSimplePlugin_fetchPc_booted;
  reg                 IBusSimplePlugin_fetchPc_inc;
  wire                when_Fetcher_l133;
  wire                when_Fetcher_l133_1;
  reg        [31:0]   IBusSimplePlugin_fetchPc_pc;
  reg                 IBusSimplePlugin_fetchPc_flushed;
  wire                when_Fetcher_l160;
  reg                 IBusSimplePlugin_decodePc_flushed;
  reg        [31:0]   IBusSimplePlugin_decodePc_pcReg /* verilator public */ ;
  wire       [31:0]   IBusSimplePlugin_decodePc_pcPlus;
  reg                 IBusSimplePlugin_decodePc_injectedDecode;
  wire                when_Fetcher_l182;
  wire                when_Fetcher_l194;
  wire                IBusSimplePlugin_iBusRsp_redoFetch;
  wire                IBusSimplePlugin_iBusRsp_stages_0_input_valid;
  wire                IBusSimplePlugin_iBusRsp_stages_0_input_ready;
  wire       [31:0]   IBusSimplePlugin_iBusRsp_stages_0_input_payload;
  wire                IBusSimplePlugin_iBusRsp_stages_0_output_valid;
  wire                IBusSimplePlugin_iBusRsp_stages_0_output_ready;
  wire       [31:0]   IBusSimplePlugin_iBusRsp_stages_0_output_payload;
  reg                 IBusSimplePlugin_iBusRsp_stages_0_halt;
  wire                IBusSimplePlugin_iBusRsp_stages_1_input_valid;
  wire                IBusSimplePlugin_iBusRsp_stages_1_input_ready;
  wire       [31:0]   IBusSimplePlugin_iBusRsp_stages_1_input_payload;
  wire                IBusSimplePlugin_iBusRsp_stages_1_output_valid;
  wire                IBusSimplePlugin_iBusRsp_stages_1_output_ready;
  wire       [31:0]   IBusSimplePlugin_iBusRsp_stages_1_output_payload;
  wire                IBusSimplePlugin_iBusRsp_stages_1_halt;
  wire                _zz_IBusSimplePlugin_iBusRsp_stages_0_input_ready;
  wire                _zz_IBusSimplePlugin_iBusRsp_stages_1_input_ready;
  wire                IBusSimplePlugin_iBusRsp_flush;
  wire                _zz_IBusSimplePlugin_iBusRsp_stages_0_output_ready;
  wire                _zz_IBusSimplePlugin_iBusRsp_stages_1_input_valid;
  reg                 _zz_IBusSimplePlugin_iBusRsp_stages_1_input_valid_1;
  reg                 IBusSimplePlugin_iBusRsp_readyForError;
  wire                IBusSimplePlugin_iBusRsp_output_valid;
  wire                IBusSimplePlugin_iBusRsp_output_ready;
  wire       [31:0]   IBusSimplePlugin_iBusRsp_output_payload_pc;
  wire                IBusSimplePlugin_iBusRsp_output_payload_rsp_error;
  wire       [31:0]   IBusSimplePlugin_iBusRsp_output_payload_rsp_inst;
  wire                IBusSimplePlugin_iBusRsp_output_payload_isRvc;
  wire                IBusSimplePlugin_decompressor_input_valid;
  wire                IBusSimplePlugin_decompressor_input_ready;
  wire       [31:0]   IBusSimplePlugin_decompressor_input_payload_pc;
  wire                IBusSimplePlugin_decompressor_input_payload_rsp_error;
  wire       [31:0]   IBusSimplePlugin_decompressor_input_payload_rsp_inst;
  wire                IBusSimplePlugin_decompressor_input_payload_isRvc;
  wire                IBusSimplePlugin_decompressor_output_valid;
  wire                IBusSimplePlugin_decompressor_output_ready;
  wire       [31:0]   IBusSimplePlugin_decompressor_output_payload_pc;
  wire                IBusSimplePlugin_decompressor_output_payload_rsp_error;
  wire       [31:0]   IBusSimplePlugin_decompressor_output_payload_rsp_inst;
  wire                IBusSimplePlugin_decompressor_output_payload_isRvc;
  wire                IBusSimplePlugin_decompressor_flushNext;
  wire                IBusSimplePlugin_decompressor_consumeCurrent;
  reg                 IBusSimplePlugin_decompressor_bufferValid;
  reg        [15:0]   IBusSimplePlugin_decompressor_bufferData;
  wire                IBusSimplePlugin_decompressor_isInputLowRvc;
  wire                IBusSimplePlugin_decompressor_isInputHighRvc;
  reg                 IBusSimplePlugin_decompressor_throw2BytesReg;
  wire                IBusSimplePlugin_decompressor_throw2Bytes;
  wire                IBusSimplePlugin_decompressor_unaligned;
  reg                 IBusSimplePlugin_decompressor_bufferValidLatch;
  reg                 IBusSimplePlugin_decompressor_throw2BytesLatch;
  wire                IBusSimplePlugin_decompressor_bufferValidPatched;
  wire                IBusSimplePlugin_decompressor_throw2BytesPatched;
  wire       [31:0]   IBusSimplePlugin_decompressor_raw;
  wire                IBusSimplePlugin_decompressor_isRvc;
  wire       [15:0]   _zz_IBusSimplePlugin_decompressor_decompressed;
  reg        [31:0]   IBusSimplePlugin_decompressor_decompressed;
  wire       [4:0]    _zz_IBusSimplePlugin_decompressor_decompressed_1;
  wire       [4:0]    _zz_IBusSimplePlugin_decompressor_decompressed_2;
  wire       [11:0]   _zz_IBusSimplePlugin_decompressor_decompressed_3;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_4;
  reg        [11:0]   _zz_IBusSimplePlugin_decompressor_decompressed_5;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_6;
  reg        [9:0]    _zz_IBusSimplePlugin_decompressor_decompressed_7;
  wire       [20:0]   _zz_IBusSimplePlugin_decompressor_decompressed_8;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_9;
  reg        [14:0]   _zz_IBusSimplePlugin_decompressor_decompressed_10;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_11;
  reg        [2:0]    _zz_IBusSimplePlugin_decompressor_decompressed_12;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_13;
  reg        [9:0]    _zz_IBusSimplePlugin_decompressor_decompressed_14;
  wire       [20:0]   _zz_IBusSimplePlugin_decompressor_decompressed_15;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_16;
  reg        [4:0]    _zz_IBusSimplePlugin_decompressor_decompressed_17;
  wire       [12:0]   _zz_IBusSimplePlugin_decompressor_decompressed_18;
  wire       [4:0]    _zz_IBusSimplePlugin_decompressor_decompressed_19;
  wire       [4:0]    _zz_IBusSimplePlugin_decompressor_decompressed_20;
  wire       [4:0]    _zz_IBusSimplePlugin_decompressor_decompressed_21;
  wire       [4:0]    switch_Misc_l44;
  wire                when_Misc_l47;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_22;
  wire       [1:0]    switch_Misc_l241;
  wire       [1:0]    switch_Misc_l241_1;
  reg        [2:0]    _zz_IBusSimplePlugin_decompressor_decompressed_23;
  reg        [2:0]    _zz_IBusSimplePlugin_decompressor_decompressed_24;
  wire                _zz_IBusSimplePlugin_decompressor_decompressed_25;
  reg        [6:0]    _zz_IBusSimplePlugin_decompressor_decompressed_26;
  wire                IBusSimplePlugin_decompressor_output_fire;
  wire                IBusSimplePlugin_decompressor_bufferFill;
  wire                when_Fetcher_l285;
  wire                when_Fetcher_l288;
  wire                when_Fetcher_l293;
  wire                IBusSimplePlugin_injector_decodeInput_valid;
  wire                IBusSimplePlugin_injector_decodeInput_ready;
  wire       [31:0]   IBusSimplePlugin_injector_decodeInput_payload_pc;
  wire                IBusSimplePlugin_injector_decodeInput_payload_rsp_error;
  wire       [31:0]   IBusSimplePlugin_injector_decodeInput_payload_rsp_inst;
  wire                IBusSimplePlugin_injector_decodeInput_payload_isRvc;
  reg                 _zz_IBusSimplePlugin_injector_decodeInput_valid;
  reg        [31:0]   _zz_IBusSimplePlugin_injector_decodeInput_payload_pc;
  reg                 _zz_IBusSimplePlugin_injector_decodeInput_payload_rsp_error;
  reg        [31:0]   _zz_IBusSimplePlugin_injector_decodeInput_payload_rsp_inst;
  reg                 _zz_IBusSimplePlugin_injector_decodeInput_payload_isRvc;
  reg                 IBusSimplePlugin_injector_nextPcCalc_valids_0;
  wire                when_Fetcher_l331;
  reg                 IBusSimplePlugin_injector_nextPcCalc_valids_1;
  wire                when_Fetcher_l331_1;
  reg                 IBusSimplePlugin_injector_nextPcCalc_valids_2;
  wire                when_Fetcher_l331_2;
  reg                 IBusSimplePlugin_injector_nextPcCalc_valids_3;
  wire                when_Fetcher_l331_3;
  reg        [31:0]   IBusSimplePlugin_injector_formal_rawInDecode;
  wire                IBusSimplePlugin_cmd_valid;
  wire                IBusSimplePlugin_cmd_ready;
  wire       [31:0]   IBusSimplePlugin_cmd_payload_pc;
  wire                IBusSimplePlugin_pending_inc;
  wire                IBusSimplePlugin_pending_dec;
  reg        [2:0]    IBusSimplePlugin_pending_value;
  wire       [2:0]    IBusSimplePlugin_pending_next;
  wire                IBusSimplePlugin_cmdFork_canEmit;
  wire                when_IBusSimplePlugin_l305;
  wire                IBusSimplePlugin_cmd_fire;
  wire                IBusSimplePlugin_rspJoin_rspBuffer_output_valid;
  wire                IBusSimplePlugin_rspJoin_rspBuffer_output_ready;
  wire                IBusSimplePlugin_rspJoin_rspBuffer_output_payload_error;
  wire       [31:0]   IBusSimplePlugin_rspJoin_rspBuffer_output_payload_inst;
  reg        [2:0]    IBusSimplePlugin_rspJoin_rspBuffer_discardCounter;
  wire                iBus_rsp_toStream_valid;
  wire                iBus_rsp_toStream_ready;
  wire                iBus_rsp_toStream_payload_error;
  wire       [31:0]   iBus_rsp_toStream_payload_inst;
  wire                IBusSimplePlugin_rspJoin_rspBuffer_flush;
  wire                toplevel_IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_fire;
  wire       [31:0]   IBusSimplePlugin_rspJoin_fetchRsp_pc;
  reg                 IBusSimplePlugin_rspJoin_fetchRsp_rsp_error;
  wire       [31:0]   IBusSimplePlugin_rspJoin_fetchRsp_rsp_inst;
  wire                IBusSimplePlugin_rspJoin_fetchRsp_isRvc;
  wire                when_IBusSimplePlugin_l377;
  wire                IBusSimplePlugin_rspJoin_join_valid;
  wire                IBusSimplePlugin_rspJoin_join_ready;
  wire       [31:0]   IBusSimplePlugin_rspJoin_join_payload_pc;
  wire                IBusSimplePlugin_rspJoin_join_payload_rsp_error;
  wire       [31:0]   IBusSimplePlugin_rspJoin_join_payload_rsp_inst;
  wire                IBusSimplePlugin_rspJoin_join_payload_isRvc;
  reg                 IBusSimplePlugin_rspJoin_exceptionDetected;
  wire                IBusSimplePlugin_rspJoin_join_fire;
  wire                _zz_IBusSimplePlugin_iBusRsp_output_valid;
  wire                when_IBusSimplePlugin_l403;
  reg        [31:0]   DBusCachedPlugin_rspCounter;
  wire                when_DBusCachedPlugin_l353;
  wire       [1:0]    execute_DBusCachedPlugin_size;
  reg        [31:0]   _zz_execute_MEMORY_STORE_DATA_RF;
  wire                toplevel_dataCache_1_io_cpu_flush_isStall;
  wire                when_DBusCachedPlugin_l395;
  wire                when_DBusCachedPlugin_l411;
  wire                when_DBusCachedPlugin_l473;
  wire                when_DBusCachedPlugin_l534;
  wire                when_DBusCachedPlugin_l554;
  wire       [31:0]   writeBack_DBusCachedPlugin_rspData;
  wire       [7:0]    writeBack_DBusCachedPlugin_rspSplits_0;
  wire       [7:0]    writeBack_DBusCachedPlugin_rspSplits_1;
  wire       [7:0]    writeBack_DBusCachedPlugin_rspSplits_2;
  wire       [7:0]    writeBack_DBusCachedPlugin_rspSplits_3;
  reg        [31:0]   writeBack_DBusCachedPlugin_rspShifted;
  wire       [31:0]   writeBack_DBusCachedPlugin_rspRf;
  wire       [1:0]    switch_Misc_l241_2;
  wire                _zz_writeBack_DBusCachedPlugin_rspFormated;
  reg        [31:0]   _zz_writeBack_DBusCachedPlugin_rspFormated_1;
  wire                _zz_writeBack_DBusCachedPlugin_rspFormated_2;
  reg        [31:0]   _zz_writeBack_DBusCachedPlugin_rspFormated_3;
  reg        [31:0]   writeBack_DBusCachedPlugin_rspFormated;
  wire                when_DBusCachedPlugin_l581;
  reg                 when_CsrPlugin_l818;
  reg        [1:0]    _zz_CsrPlugin_privilege;
  reg                 CsrPlugin_running;
  wire                when_CsrPlugin_l711;
  reg                 CsrPlugin_reseting;
  reg                 _zz_debugBus_haveReset;
  reg                 CsrPlugin_running_aheadValue_regNext;
  wire                CsrPlugin_enterHalt;
  reg                 CsrPlugin_doHalt;
  wire                when_CsrPlugin_l729;
  wire                CsrPlugin_forceResume;
  reg                 _zz_CsrPlugin_doResume;
  wire                CsrPlugin_doResume;
  reg                 CsrPlugin_timeout_state;
  reg                 CsrPlugin_timeout_stateRise;
  wire                CsrPlugin_timeout_counter_willIncrement;
  reg                 CsrPlugin_timeout_counter_willClear;
  reg        [2:0]    CsrPlugin_timeout_counter_valueNext;
  reg        [2:0]    CsrPlugin_timeout_counter_value;
  wire                CsrPlugin_timeout_counter_willOverflowIfInc;
  wire                CsrPlugin_timeout_counter_willOverflow;
  wire                when_CsrPlugin_l735;
  reg                 _zz_debugBus_hartToDm_valid;
  reg        [31:0]   CsrPlugin_dataCsrw_value_0;
  wire                when_CsrPlugin_l750;
  wire                CsrPlugin_inject_cmd_valid;
  wire       [1:0]    CsrPlugin_inject_cmd_payload_op;
  wire       [4:0]    CsrPlugin_inject_cmd_payload_address;
  wire       [31:0]   CsrPlugin_inject_cmd_payload_data;
  wire       [2:0]    CsrPlugin_inject_cmd_payload_size;
  wire                CsrPlugin_inject_cmd_toStream_valid;
  reg                 CsrPlugin_inject_cmd_toStream_ready;
  wire       [1:0]    CsrPlugin_inject_cmd_toStream_payload_op;
  wire       [4:0]    CsrPlugin_inject_cmd_toStream_payload_address;
  wire       [31:0]   CsrPlugin_inject_cmd_toStream_payload_data;
  wire       [2:0]    CsrPlugin_inject_cmd_toStream_payload_size;
  wire                CsrPlugin_inject_buffer_valid;
  wire                CsrPlugin_inject_buffer_ready;
  wire       [1:0]    CsrPlugin_inject_buffer_payload_op;
  wire       [4:0]    CsrPlugin_inject_buffer_payload_address;
  wire       [31:0]   CsrPlugin_inject_buffer_payload_data;
  wire       [2:0]    CsrPlugin_inject_buffer_payload_size;
  reg                 CsrPlugin_inject_cmd_toStream_rValid;
  reg        [1:0]    CsrPlugin_inject_cmd_toStream_rData_op;
  reg        [4:0]    CsrPlugin_inject_cmd_toStream_rData_address;
  reg        [31:0]   CsrPlugin_inject_cmd_toStream_rData_data;
  reg        [2:0]    CsrPlugin_inject_cmd_toStream_rData_size;
  wire                when_Stream_l375;
  wire                CsrPlugin_injectionPort_fire;
  reg                 CsrPlugin_inject_pending;
  wire                when_CsrPlugin_l786;
  wire                when_CsrPlugin_l786_1;
  reg        [31:0]   CsrPlugin_dpc;
  reg        [1:0]    CsrPlugin_dcsr_prv;
  reg                 CsrPlugin_dcsr_step;
  wire                CsrPlugin_dcsr_nmip;
  wire                CsrPlugin_dcsr_mprven;
  reg        [2:0]    CsrPlugin_dcsr_cause;
  reg                 CsrPlugin_dcsr_stoptime;
  reg                 CsrPlugin_dcsr_stopcount;
  reg                 CsrPlugin_dcsr_stepie;
  reg                 CsrPlugin_dcsr_ebreakm;
  wire       [3:0]    CsrPlugin_dcsr_xdebugver;
  wire                CsrPlugin_dcsr_stepLogic_wantExit;
  reg                 CsrPlugin_dcsr_stepLogic_wantStart;
  wire                CsrPlugin_dcsr_stepLogic_wantKill;
  reg        [1:0]    CsrPlugin_dcsr_stepLogic_stateReg;
  reg        [1:0]    CsrPlugin_dcsr_stepLogic_stateNext;
  wire                when_CsrPlugin_l812;
  wire                when_CsrPlugin_l830;
  wire                when_CsrPlugin_l862;
  reg        [0:0]    CsrPlugin_trigger_tselect_index;
  wire                CsrPlugin_trigger_tselect_outOfRange;
  reg                 CsrPlugin_trigger_decodeBreak_enabled;
  reg                 CsrPlugin_trigger_decodeBreak_timeout_state;
  reg                 CsrPlugin_trigger_decodeBreak_timeout_stateRise;
  wire                CsrPlugin_trigger_decodeBreak_timeout_counter_willIncrement;
  reg                 CsrPlugin_trigger_decodeBreak_timeout_counter_willClear;
  reg        [1:0]    CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext;
  reg        [1:0]    CsrPlugin_trigger_decodeBreak_timeout_counter_value;
  wire                CsrPlugin_trigger_decodeBreak_timeout_counter_willOverflowIfInc;
  wire                CsrPlugin_trigger_decodeBreak_timeout_counter_willOverflow;
  wire                when_Utils_l669;
  wire                CsrPlugin_trigger_slots_0_selected;
  reg        [31:0]   CsrPlugin_trigger_slots_0_tdata1_read;
  wire       [3:0]    CsrPlugin_trigger_slots_0_tdata1_tpe;
  reg                 CsrPlugin_trigger_slots_0_tdata1_dmode;
  reg                 CsrPlugin_trigger_slots_0_tdata1_execute;
  reg                 CsrPlugin_trigger_slots_0_tdata1_m;
  reg                 CsrPlugin_trigger_slots_0_tdata1_s;
  reg                 CsrPlugin_trigger_slots_0_tdata1_u;
  reg        [3:0]    CsrPlugin_trigger_slots_0_tdata1_action;
  reg                 _zz_CsrPlugin_trigger_slots_0_tdata1_privilegeHit;
  wire                CsrPlugin_trigger_slots_0_tdata1_privilegeHit;
  reg        [31:0]   CsrPlugin_trigger_slots_0_tdata2_value;
  wire                CsrPlugin_trigger_slots_0_tdata2_execute_enabled;
  wire                CsrPlugin_trigger_slots_0_tdata2_execute_hit;
  wire                CsrPlugin_trigger_slots_1_selected;
  reg        [31:0]   CsrPlugin_trigger_slots_1_tdata1_read;
  wire       [3:0]    CsrPlugin_trigger_slots_1_tdata1_tpe;
  reg                 CsrPlugin_trigger_slots_1_tdata1_dmode;
  reg                 CsrPlugin_trigger_slots_1_tdata1_execute;
  reg                 CsrPlugin_trigger_slots_1_tdata1_m;
  reg                 CsrPlugin_trigger_slots_1_tdata1_s;
  reg                 CsrPlugin_trigger_slots_1_tdata1_u;
  reg        [3:0]    CsrPlugin_trigger_slots_1_tdata1_action;
  reg                 _zz_CsrPlugin_trigger_slots_1_tdata1_privilegeHit;
  wire                CsrPlugin_trigger_slots_1_tdata1_privilegeHit;
  reg        [31:0]   CsrPlugin_trigger_slots_1_tdata2_value;
  wire                CsrPlugin_trigger_slots_1_tdata2_execute_enabled;
  wire                CsrPlugin_trigger_slots_1_tdata2_execute_hit;
  wire                when_CsrPlugin_l958;
  wire       [1:0]    CsrPlugin_misa_base;
  wire       [25:0]   CsrPlugin_misa_extensions;
  wire       [1:0]    CsrPlugin_mtvec_mode;
  reg        [29:0]   CsrPlugin_mtvec_base;
  reg        [31:0]   CsrPlugin_mepc;
  reg                 CsrPlugin_mstatus_MIE;
  reg                 CsrPlugin_mstatus_MPIE;
  reg        [1:0]    CsrPlugin_mstatus_MPP;
  reg                 CsrPlugin_mip_MEIP;
  reg                 CsrPlugin_mip_MTIP;
  reg                 CsrPlugin_mip_MSIP;
  reg                 CsrPlugin_mie_MEIE;
  reg                 CsrPlugin_mie_MTIE;
  reg                 CsrPlugin_mie_MSIE;
  reg                 CsrPlugin_mcause_interrupt;
  reg        [3:0]    CsrPlugin_mcause_exceptionCode;
  reg        [31:0]   CsrPlugin_mtval;
  reg        [63:0]   CsrPlugin_mcycle;
  reg        [63:0]   CsrPlugin_minstret;
  wire                _zz_when_CsrPlugin_l1302;
  wire                _zz_when_CsrPlugin_l1302_1;
  wire                _zz_when_CsrPlugin_l1302_2;
  reg                 CsrPlugin_exceptionPortCtrl_exceptionValids_decode;
  reg                 CsrPlugin_exceptionPortCtrl_exceptionValids_execute;
  reg                 CsrPlugin_exceptionPortCtrl_exceptionValids_memory;
  reg                 CsrPlugin_exceptionPortCtrl_exceptionValids_writeBack;
  reg                 CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_decode;
  reg                 CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_execute;
  reg                 CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_memory;
  reg                 CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_writeBack;
  reg        [3:0]    CsrPlugin_exceptionPortCtrl_exceptionContext_code;
  reg        [31:0]   CsrPlugin_exceptionPortCtrl_exceptionContext_badAddr;
  wire       [1:0]    CsrPlugin_exceptionPortCtrl_exceptionTargetPrivilegeUncapped;
  wire       [1:0]    CsrPlugin_exceptionPortCtrl_exceptionTargetPrivilege;
  wire       [1:0]    _zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code;
  wire                _zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code_1;
  wire                when_CsrPlugin_l1259;
  wire                when_CsrPlugin_l1259_1;
  wire                when_CsrPlugin_l1259_2;
  wire                when_CsrPlugin_l1259_3;
  wire                when_CsrPlugin_l1272;
  reg                 CsrPlugin_interrupt_valid;
  reg        [3:0]    CsrPlugin_interrupt_code /* verilator public */ ;
  reg        [1:0]    CsrPlugin_interrupt_targetPrivilege;
  wire                when_CsrPlugin_l1296;
  wire                when_CsrPlugin_l1302;
  wire                when_CsrPlugin_l1302_1;
  wire                when_CsrPlugin_l1302_2;
  wire                when_CsrPlugin_l1315;
  wire                CsrPlugin_exception;
  wire                CsrPlugin_lastStageWasWfi;
  reg                 CsrPlugin_pipelineLiberator_pcValids_0;
  reg                 CsrPlugin_pipelineLiberator_pcValids_1;
  reg                 CsrPlugin_pipelineLiberator_pcValids_2;
  wire                CsrPlugin_pipelineLiberator_active;
  wire                when_CsrPlugin_l1335;
  wire                when_CsrPlugin_l1335_1;
  wire                when_CsrPlugin_l1335_2;
  wire                when_CsrPlugin_l1340;
  reg                 CsrPlugin_pipelineLiberator_done;
  wire                when_CsrPlugin_l1346;
  wire                CsrPlugin_interruptJump /* verilator public */ ;
  reg                 CsrPlugin_hadException /* verilator public */ ;
  reg        [1:0]    CsrPlugin_targetPrivilege;
  reg        [3:0]    CsrPlugin_trapCause;
  reg                 CsrPlugin_trapCauseEbreakDebug;
  wire                when_CsrPlugin_l1373;
  wire                when_CsrPlugin_l1375;
  reg        [1:0]    CsrPlugin_xtvec_mode;
  reg        [29:0]   CsrPlugin_xtvec_base;
  reg                 CsrPlugin_trapEnterDebug;
  wire                when_CsrPlugin_l1389;
  wire                when_CsrPlugin_l1390;
  wire                when_CsrPlugin_l1398;
  wire                when_CsrPlugin_l1428;
  wire                when_CsrPlugin_l1456;
  wire       [1:0]    switch_CsrPlugin_l1460;
  wire                when_CsrPlugin_l1468;
  reg                 execute_CsrPlugin_wfiWake;
  wire                when_CsrPlugin_l1527;
  wire                execute_CsrPlugin_blockedBySideEffects;
  reg                 execute_CsrPlugin_illegalAccess;
  reg                 execute_CsrPlugin_illegalInstruction;
  wire                when_CsrPlugin_l1547;
  wire                when_CsrPlugin_l1548;
  wire                when_CsrPlugin_l1565;
  reg                 execute_CsrPlugin_writeInstruction;
  reg                 execute_CsrPlugin_readInstruction;
  wire                execute_CsrPlugin_writeEnable;
  wire                execute_CsrPlugin_readEnable;
  wire       [31:0]   execute_CsrPlugin_readToWriteData;
  wire                switch_Misc_l241_3;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_writeDataSignal;
  wire                when_CsrPlugin_l1587;
  wire                when_CsrPlugin_l1591;
  wire       [11:0]   execute_CsrPlugin_csrAddress;
  wire       [40:0]   _zz_decode_FPU_RSP;
  wire                _zz_decode_FPU_RSP_1;
  wire                _zz_decode_FPU_RSP_2;
  wire                _zz_decode_FPU_RSP_3;
  wire                _zz_decode_FPU_RSP_4;
  wire                _zz_decode_FPU_RSP_5;
  wire                _zz_decode_FPU_RSP_6;
  wire                _zz_decode_FPU_RSP_7;
  wire                _zz_decode_FPU_RSP_8;
  wire                _zz_decode_FPU_RSP_9;
  wire                _zz_decode_FPU_RSP_10;
  wire                _zz_decode_FPU_RSP_11;
  wire       [1:0]    _zz_decode_SRC1_CTRL_2;
  wire       [1:0]    _zz_decode_ALU_CTRL_2;
  wire       [1:0]    _zz_decode_SRC2_CTRL_2;
  wire       [1:0]    _zz_decode_ENV_CTRL_2;
  wire       [1:0]    _zz_decode_ALU_BITWISE_CTRL_2;
  wire       [3:0]    _zz_decode_FPU_OPCODE_2;
  wire       [1:0]    _zz_decode_SHIFT_CTRL_2;
  wire       [1:0]    _zz_decode_BRANCH_CTRL_2;
  wire                when_RegFilePlugin_l63;
  wire       [4:0]    decode_RegFilePlugin_regFileReadAddress1;
  wire       [4:0]    decode_RegFilePlugin_regFileReadAddress2;
  wire       [31:0]   decode_RegFilePlugin_rs1Data;
  wire       [31:0]   decode_RegFilePlugin_rs2Data;
  reg                 lastStageRegFileWrite_valid /* verilator public */ ;
  reg        [4:0]    lastStageRegFileWrite_payload_address /* verilator public */ ;
  reg        [31:0]   lastStageRegFileWrite_payload_data /* verilator public */ ;
  reg                 _zz_5;
  reg        [31:0]   execute_IntAluPlugin_bitwise;
  reg        [31:0]   _zz_execute_REGFILE_WRITE_DATA;
  reg        [31:0]   _zz_decode_SRC1;
  wire                _zz_decode_SRC2;
  reg        [19:0]   _zz_decode_SRC2_1;
  wire                _zz_decode_SRC2_2;
  reg        [19:0]   _zz_decode_SRC2_3;
  reg        [31:0]   _zz_decode_SRC2_4;
  reg        [31:0]   execute_SrcPlugin_addSub;
  wire                execute_SrcPlugin_less;
  reg                 execute_MulPlugin_aSigned;
  reg                 execute_MulPlugin_bSigned;
  wire       [31:0]   execute_MulPlugin_a;
  wire       [31:0]   execute_MulPlugin_b;
  wire       [1:0]    switch_MulPlugin_l87;
  wire       [15:0]   execute_MulPlugin_aULow;
  wire       [15:0]   execute_MulPlugin_bULow;
  wire       [16:0]   execute_MulPlugin_aSLow;
  wire       [16:0]   execute_MulPlugin_bSLow;
  wire       [16:0]   execute_MulPlugin_aHigh;
  wire       [16:0]   execute_MulPlugin_bHigh;
  wire       [65:0]   writeBack_MulPlugin_result;
  wire                when_MulPlugin_l147;
  wire       [1:0]    switch_MulPlugin_l148;
  reg        [32:0]   memory_DivPlugin_rs1;
  reg        [31:0]   memory_DivPlugin_rs2;
  reg        [64:0]   memory_DivPlugin_accumulator;
  wire                memory_DivPlugin_frontendOk;
  reg                 memory_DivPlugin_div_needRevert;
  reg                 memory_DivPlugin_div_counter_willIncrement;
  reg                 memory_DivPlugin_div_counter_willClear;
  reg        [5:0]    memory_DivPlugin_div_counter_valueNext;
  reg        [5:0]    memory_DivPlugin_div_counter_value;
  wire                memory_DivPlugin_div_counter_willOverflowIfInc;
  wire                memory_DivPlugin_div_counter_willOverflow;
  reg                 memory_DivPlugin_div_done;
  wire                when_MulDivIterativePlugin_l126;
  wire                when_MulDivIterativePlugin_l126_1;
  reg        [31:0]   memory_DivPlugin_div_result;
  wire                when_MulDivIterativePlugin_l128;
  wire                when_MulDivIterativePlugin_l129;
  wire                when_MulDivIterativePlugin_l132;
  wire       [31:0]   _zz_memory_DivPlugin_div_stage_0_remainderShifted;
  wire       [32:0]   memory_DivPlugin_div_stage_0_remainderShifted;
  wire       [32:0]   memory_DivPlugin_div_stage_0_remainderMinusDenominator;
  wire       [31:0]   memory_DivPlugin_div_stage_0_outRemainder;
  wire       [31:0]   memory_DivPlugin_div_stage_0_outNumerator;
  wire                when_MulDivIterativePlugin_l151;
  wire       [31:0]   _zz_memory_DivPlugin_div_result;
  wire                when_MulDivIterativePlugin_l162;
  wire                _zz_memory_DivPlugin_rs2;
  wire                _zz_memory_DivPlugin_rs1;
  reg        [32:0]   _zz_memory_DivPlugin_rs1_1;
  reg        [5:0]    FpuPlugin_pendings;
  wire                FpuPlugin_port_cmd_fire;
  wire                FpuPlugin_port_rsp_fire;
  wire                FpuPlugin_hasPending;
  reg                 FpuPlugin_flags_NX;
  reg                 FpuPlugin_flags_UF;
  reg                 FpuPlugin_flags_OF;
  reg                 FpuPlugin_flags_DZ;
  reg                 FpuPlugin_flags_NV;
  wire                when_FpuPlugin_l215;
  wire                when_FpuPlugin_l216;
  wire                when_FpuPlugin_l217;
  wire                when_FpuPlugin_l218;
  wire                when_FpuPlugin_l219;
  reg        [2:0]    FpuPlugin_rm;
  wire                FpuPlugin_csrActive;
  wire                when_FpuPlugin_l229;
  reg        [1:0]    FpuPlugin_fs;
  wire                FpuPlugin_sd;
  wire                when_FpuPlugin_l234;
  reg                 _zz_when_FpuPlugin_l237;
  reg                 _zz_when_FpuPlugin_l237_1;
  reg                 _zz_when_FpuPlugin_l237_2;
  wire                when_FpuPlugin_l237;
  reg                 FpuPlugin_accessFpuCsr;
  wire                when_FpuPlugin_l253;
  wire                _zz_decode_FPU_FORKED;
  wire                decode_FpuPlugin_trap;
  reg                 decode_FpuPlugin_forked;
  wire                when_FpuPlugin_l268;
  wire                when_FpuPlugin_l268_1;
  wire                decode_FpuPlugin_hazard;
  wire                when_FpuPlugin_l272;
  wire                when_FpuPlugin_l273;
  wire                FpuPlugin_port_cmd_isStall;
  wire       [2:0]    decode_FpuPlugin_iRoundMode;
  wire       [2:0]    decode_FpuPlugin_roundMode;
  wire       [2:0]    _zz_FpuPlugin_port_cmd_payload_roundMode;
  wire       [2:0]    _zz_FpuPlugin_port_cmd_payload_roundMode_1;
  wire                writeBack_FpuPlugin_isRsp;
  wire                writeBack_FpuPlugin_isCommit;
  wire       [31:0]   writeBack_FpuPlugin_storeFormated;
  wire       [31:0]   DBusBypass0_value;
  wire                when_FpuPlugin_l315;
  wire                when_FpuPlugin_l318;
  wire                when_FpuPlugin_l323;
  wire                when_FpuPlugin_l325;
  wire                writeBack_FpuPlugin_commit_valid /* verilator public */ ;
  wire                writeBack_FpuPlugin_commit_ready /* verilator public */ ;
  wire       [3:0]    writeBack_FpuPlugin_commit_payload_opcode /* verilator public */ ;
  wire       [4:0]    writeBack_FpuPlugin_commit_payload_rd /* verilator public */ ;
  wire                writeBack_FpuPlugin_commit_payload_write /* verilator public */ ;
  wire       [31:0]   writeBack_FpuPlugin_commit_payload_value /* verilator public */ ;
  wire                when_FpuPlugin_l339;
  wire                writeBack_FpuPlugin_commit_s2mPipe_valid;
  wire                writeBack_FpuPlugin_commit_s2mPipe_ready;
  wire       [3:0]    writeBack_FpuPlugin_commit_s2mPipe_payload_opcode;
  wire       [4:0]    writeBack_FpuPlugin_commit_s2mPipe_payload_rd;
  wire                writeBack_FpuPlugin_commit_s2mPipe_payload_write;
  wire       [31:0]   writeBack_FpuPlugin_commit_s2mPipe_payload_value;
  reg                 writeBack_FpuPlugin_commit_rValidN;
  reg        [3:0]    writeBack_FpuPlugin_commit_rData_opcode;
  reg        [4:0]    writeBack_FpuPlugin_commit_rData_rd;
  reg                 writeBack_FpuPlugin_commit_rData_write;
  reg        [31:0]   writeBack_FpuPlugin_commit_rData_value;
  wire       [3:0]    _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode;
  reg                 execute_LightShifterPlugin_isActive;
  wire                execute_LightShifterPlugin_isShift;
  reg        [4:0]    execute_LightShifterPlugin_amplitudeReg;
  wire       [4:0]    execute_LightShifterPlugin_amplitude;
  wire       [31:0]   execute_LightShifterPlugin_shiftInput;
  wire                execute_LightShifterPlugin_done;
  wire                when_ShiftPlugins_l169;
  reg        [31:0]   _zz_execute_to_memory_REGFILE_WRITE_DATA_1;
  wire                when_ShiftPlugins_l175;
  wire                when_ShiftPlugins_l184;
  reg                 HazardSimplePlugin_src0Hazard;
  reg                 HazardSimplePlugin_src1Hazard;
  wire                HazardSimplePlugin_writeBackWrites_valid;
  wire       [4:0]    HazardSimplePlugin_writeBackWrites_payload_address;
  wire       [31:0]   HazardSimplePlugin_writeBackWrites_payload_data;
  reg                 HazardSimplePlugin_writeBackBuffer_valid;
  reg        [4:0]    HazardSimplePlugin_writeBackBuffer_payload_address;
  reg        [31:0]   HazardSimplePlugin_writeBackBuffer_payload_data;
  wire                HazardSimplePlugin_addr0Match;
  wire                HazardSimplePlugin_addr1Match;
  wire                when_HazardSimplePlugin_l59;
  wire                when_HazardSimplePlugin_l62;
  wire                when_HazardSimplePlugin_l57;
  wire                when_HazardSimplePlugin_l58;
  wire                when_HazardSimplePlugin_l59_1;
  wire                when_HazardSimplePlugin_l62_1;
  wire                when_HazardSimplePlugin_l57_1;
  wire                when_HazardSimplePlugin_l58_1;
  wire                when_HazardSimplePlugin_l59_2;
  wire                when_HazardSimplePlugin_l62_2;
  wire                when_HazardSimplePlugin_l57_2;
  wire                when_HazardSimplePlugin_l58_2;
  wire                when_HazardSimplePlugin_l105;
  wire                when_HazardSimplePlugin_l108;
  wire                when_HazardSimplePlugin_l113;
  wire                execute_BranchPlugin_eq;
  wire       [2:0]    switch_Misc_l241_4;
  reg                 _zz_execute_BRANCH_DO;
  reg                 _zz_execute_BRANCH_DO_1;
  wire       [31:0]   execute_BranchPlugin_branch_src1;
  wire                _zz_execute_BranchPlugin_branch_src2;
  reg        [10:0]   _zz_execute_BranchPlugin_branch_src2_1;
  wire                _zz_execute_BranchPlugin_branch_src2_2;
  reg        [19:0]   _zz_execute_BranchPlugin_branch_src2_3;
  wire                _zz_execute_BranchPlugin_branch_src2_4;
  reg        [18:0]   _zz_execute_BranchPlugin_branch_src2_5;
  reg        [31:0]   _zz_execute_BranchPlugin_branch_src2_6;
  wire       [31:0]   execute_BranchPlugin_branch_src2;
  wire       [31:0]   execute_BranchPlugin_branchAdder;
  reg                 toplevel_debugModule_1_io_harts_0_dmToHart_regNext_valid;
  reg        [1:0]    toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_op;
  reg        [4:0]    toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_address;
  reg        [31:0]   toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_data;
  reg        [2:0]    toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_size;
  wire                when_Pipeline_l124;
  reg        [31:0]   decode_to_execute_PC;
  wire                when_Pipeline_l124_1;
  reg        [31:0]   execute_to_memory_PC;
  wire                when_Pipeline_l124_2;
  reg        [31:0]   memory_to_writeBack_PC;
  wire                when_Pipeline_l124_3;
  reg        [31:0]   decode_to_execute_INSTRUCTION;
  wire                when_Pipeline_l124_4;
  reg        [31:0]   execute_to_memory_INSTRUCTION;
  wire                when_Pipeline_l124_5;
  reg        [31:0]   memory_to_writeBack_INSTRUCTION;
  wire                when_Pipeline_l124_6;
  reg        [31:0]   decode_to_execute_FORMAL_PC_NEXT;
  wire                when_Pipeline_l124_7;
  reg        [31:0]   execute_to_memory_FORMAL_PC_NEXT;
  wire                when_Pipeline_l124_8;
  reg        [31:0]   memory_to_writeBack_FORMAL_PC_NEXT;
  wire                when_Pipeline_l124_9;
  reg                 decode_to_execute_MEMORY_FORCE_CONSTISTENCY;
  wire                when_Pipeline_l124_10;
  reg                 decode_to_execute_CSR_WRITE_OPCODE;
  wire                when_Pipeline_l124_11;
  reg                 decode_to_execute_CSR_READ_OPCODE;
  wire                when_Pipeline_l124_12;
  reg                 decode_to_execute_SRC_USE_SUB_LESS;
  wire                when_Pipeline_l124_13;
  reg                 decode_to_execute_MEMORY_ENABLE;
  wire                when_Pipeline_l124_14;
  reg                 execute_to_memory_MEMORY_ENABLE;
  wire                when_Pipeline_l124_15;
  reg                 memory_to_writeBack_MEMORY_ENABLE;
  wire                when_Pipeline_l124_16;
  reg        [1:0]    decode_to_execute_ALU_CTRL;
  wire                when_Pipeline_l124_17;
  reg                 decode_to_execute_REGFILE_WRITE_VALID;
  wire                when_Pipeline_l124_18;
  reg                 execute_to_memory_REGFILE_WRITE_VALID;
  wire                when_Pipeline_l124_19;
  reg                 memory_to_writeBack_REGFILE_WRITE_VALID;
  wire                when_Pipeline_l124_20;
  reg                 decode_to_execute_BYPASSABLE_EXECUTE_STAGE;
  wire                when_Pipeline_l124_21;
  reg                 decode_to_execute_BYPASSABLE_MEMORY_STAGE;
  wire                when_Pipeline_l124_22;
  reg                 execute_to_memory_BYPASSABLE_MEMORY_STAGE;
  wire                when_Pipeline_l124_23;
  reg                 decode_to_execute_MEMORY_WR;
  wire                when_Pipeline_l124_24;
  reg                 execute_to_memory_MEMORY_WR;
  wire                when_Pipeline_l124_25;
  reg                 memory_to_writeBack_MEMORY_WR;
  wire                when_Pipeline_l124_26;
  reg                 decode_to_execute_MEMORY_MANAGMENT;
  wire                when_Pipeline_l124_27;
  reg                 decode_to_execute_IS_CSR;
  wire                when_Pipeline_l124_28;
  reg        [1:0]    decode_to_execute_ENV_CTRL;
  wire                when_Pipeline_l124_29;
  reg        [1:0]    execute_to_memory_ENV_CTRL;
  wire                when_Pipeline_l124_30;
  reg        [1:0]    memory_to_writeBack_ENV_CTRL;
  wire                when_Pipeline_l124_31;
  reg                 decode_to_execute_SRC_LESS_UNSIGNED;
  wire                when_Pipeline_l124_32;
  reg        [1:0]    decode_to_execute_ALU_BITWISE_CTRL;
  wire                when_Pipeline_l124_33;
  reg                 decode_to_execute_IS_MUL;
  wire                when_Pipeline_l124_34;
  reg                 execute_to_memory_IS_MUL;
  wire                when_Pipeline_l124_35;
  reg                 memory_to_writeBack_IS_MUL;
  wire                when_Pipeline_l124_36;
  reg                 decode_to_execute_IS_DIV;
  wire                when_Pipeline_l124_37;
  reg                 execute_to_memory_IS_DIV;
  wire                when_Pipeline_l124_38;
  reg                 decode_to_execute_IS_RS1_SIGNED;
  wire                when_Pipeline_l124_39;
  reg                 decode_to_execute_IS_RS2_SIGNED;
  wire                when_Pipeline_l124_40;
  reg                 decode_to_execute_FPU_COMMIT;
  wire                when_Pipeline_l124_41;
  reg                 execute_to_memory_FPU_COMMIT;
  wire                when_Pipeline_l124_42;
  reg                 memory_to_writeBack_FPU_COMMIT;
  wire                when_Pipeline_l124_43;
  reg                 decode_to_execute_FPU_RSP;
  wire                when_Pipeline_l124_44;
  reg                 execute_to_memory_FPU_RSP;
  wire                when_Pipeline_l124_45;
  reg                 memory_to_writeBack_FPU_RSP;
  wire                when_Pipeline_l124_46;
  reg        [3:0]    decode_to_execute_FPU_OPCODE;
  wire                when_Pipeline_l124_47;
  reg        [3:0]    execute_to_memory_FPU_OPCODE;
  wire                when_Pipeline_l124_48;
  reg        [3:0]    memory_to_writeBack_FPU_OPCODE;
  wire                when_Pipeline_l124_49;
  reg        [1:0]    decode_to_execute_SHIFT_CTRL;
  wire                when_Pipeline_l124_50;
  reg        [1:0]    decode_to_execute_BRANCH_CTRL;
  wire                when_Pipeline_l124_51;
  reg        [31:0]   decode_to_execute_RS1;
  wire                when_Pipeline_l124_52;
  reg        [31:0]   execute_to_memory_RS1;
  wire                when_Pipeline_l124_53;
  reg        [31:0]   memory_to_writeBack_RS1;
  wire                when_Pipeline_l124_54;
  reg        [31:0]   decode_to_execute_RS2;
  wire                when_Pipeline_l124_55;
  reg                 decode_to_execute_SRC2_FORCE_ZERO;
  wire                when_Pipeline_l124_56;
  reg        [31:0]   decode_to_execute_SRC1;
  wire                when_Pipeline_l124_57;
  reg        [31:0]   decode_to_execute_SRC2;
  wire                when_Pipeline_l124_58;
  reg                 decode_to_execute_FPU_FORKED;
  wire                when_Pipeline_l124_59;
  reg                 execute_to_memory_FPU_FORKED;
  wire                when_Pipeline_l124_60;
  reg                 memory_to_writeBack_FPU_FORKED;
  wire                when_Pipeline_l124_61;
  reg                 decode_to_execute_FPU_COMMIT_LOAD;
  wire                when_Pipeline_l124_62;
  reg                 execute_to_memory_FPU_COMMIT_LOAD;
  wire                when_Pipeline_l124_63;
  reg                 memory_to_writeBack_FPU_COMMIT_LOAD;
  wire                when_Pipeline_l124_64;
  reg        [31:0]   execute_to_memory_MEMORY_STORE_DATA_RF;
  wire                when_Pipeline_l124_65;
  reg        [31:0]   memory_to_writeBack_MEMORY_STORE_DATA_RF;
  wire                when_Pipeline_l124_66;
  reg        [31:0]   execute_to_memory_REGFILE_WRITE_DATA;
  wire                when_Pipeline_l124_67;
  reg        [31:0]   memory_to_writeBack_REGFILE_WRITE_DATA;
  wire                when_Pipeline_l124_68;
  reg        [31:0]   execute_to_memory_MUL_LL;
  wire                when_Pipeline_l124_69;
  reg        [33:0]   execute_to_memory_MUL_LH;
  wire                when_Pipeline_l124_70;
  reg        [33:0]   execute_to_memory_MUL_HL;
  wire                when_Pipeline_l124_71;
  reg        [33:0]   execute_to_memory_MUL_HH;
  wire                when_Pipeline_l124_72;
  reg        [33:0]   memory_to_writeBack_MUL_HH;
  wire                when_Pipeline_l124_73;
  reg                 execute_to_memory_BRANCH_DO;
  wire                when_Pipeline_l124_74;
  reg        [31:0]   execute_to_memory_BRANCH_CALC;
  wire                when_Pipeline_l124_75;
  reg        [51:0]   memory_to_writeBack_MUL_LOW;
  wire                when_Pipeline_l151;
  wire                when_Pipeline_l154;
  wire                when_Pipeline_l151_1;
  wire                when_Pipeline_l154_1;
  wire                when_Pipeline_l151_2;
  wire                when_Pipeline_l154_2;
  reg        [2:0]    IBusSimplePlugin_injector_port_state;
  wire                when_Fetcher_l373;
  wire                when_Fetcher_l391;
  wire                when_Fetcher_l411;
  wire                when_CsrPlugin_l1669;
  reg                 execute_CsrPlugin_csr_1972;
  wire                when_CsrPlugin_l1669_1;
  reg                 execute_CsrPlugin_csr_1969;
  wire                when_CsrPlugin_l1669_2;
  reg                 execute_CsrPlugin_csr_1968;
  wire                when_CsrPlugin_l1669_3;
  reg                 execute_CsrPlugin_csr_1952;
  wire                when_CsrPlugin_l1669_4;
  reg                 execute_CsrPlugin_csr_1956;
  wire                when_CsrPlugin_l1669_5;
  reg                 execute_CsrPlugin_csr_1953;
  wire                when_CsrPlugin_l1669_6;
  reg                 execute_CsrPlugin_csr_1954;
  wire                when_CsrPlugin_l1669_7;
  reg                 execute_CsrPlugin_csr_768;
  wire                when_CsrPlugin_l1669_8;
  reg                 execute_CsrPlugin_csr_836;
  wire                when_CsrPlugin_l1669_9;
  reg                 execute_CsrPlugin_csr_772;
  wire                when_CsrPlugin_l1669_10;
  reg                 execute_CsrPlugin_csr_773;
  wire                when_CsrPlugin_l1669_11;
  reg                 execute_CsrPlugin_csr_834;
  wire                when_CsrPlugin_l1669_12;
  reg                 execute_CsrPlugin_csr_3;
  wire                when_CsrPlugin_l1669_13;
  reg                 execute_CsrPlugin_csr_2;
  wire                when_CsrPlugin_l1669_14;
  reg                 execute_CsrPlugin_csr_1;
  wire                when_CsrPlugin_l1669_15;
  reg                 execute_CsrPlugin_csr_256;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_1;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_2;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_3;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_4;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_5;
  wire       [1:0]    switch_CsrPlugin_l1031;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_6;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_7;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_8;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_9;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_10;
  wire       [4:0]    _zz_FpuPlugin_flags_NX;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_11;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_12;
  wire       [4:0]    _zz_FpuPlugin_flags_NX_1;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_13;
  reg        [31:0]   _zz_CsrPlugin_csrMapping_readDataInit_14;
  wire                when_CsrPlugin_l1702;
  wire       [11:0]   _zz_when_CsrPlugin_l1709;
  wire                when_CsrPlugin_l1709;
  reg                 when_CsrPlugin_l1719;
  wire                when_CsrPlugin_l1717;
  wire                when_CsrPlugin_l1718;
  wire                when_CsrPlugin_l1725;
  wire                iBus_cmd_m2sPipe_valid;
  wire                iBus_cmd_m2sPipe_ready;
  wire       [31:0]   iBus_cmd_m2sPipe_payload_pc;
  reg                 iBus_cmd_rValid;
  reg        [31:0]   iBus_cmd_rData_pc;
  wire                when_Stream_l375_1;
  reg        [0:0]    _zz_dBusWishbone_ADR;
  wire                _zz_dBusWishbone_CYC;
  wire                _zz_dBus_cmd_ready;
  wire                _zz_dBus_cmd_ready_1;
  wire                _zz_dBus_cmd_ready_2;
  wire                _zz_dBusWishbone_ADR_1;
  reg                 _zz_dBus_rsp_valid;
  reg        [31:0]   dBusWishbone_DAT_MISO_regNext;
  reg                 dBusWishbone_ERR_regNext;
  `ifndef SYNTHESIS
  reg [31:0] decode_BRANCH_CTRL_string;
  reg [31:0] _zz_decode_BRANCH_CTRL_string;
  reg [31:0] _zz_decode_to_execute_BRANCH_CTRL_string;
  reg [31:0] _zz_decode_to_execute_BRANCH_CTRL_1_string;
  reg [71:0] decode_SHIFT_CTRL_string;
  reg [71:0] _zz_decode_SHIFT_CTRL_string;
  reg [71:0] _zz_decode_to_execute_SHIFT_CTRL_string;
  reg [71:0] _zz_decode_to_execute_SHIFT_CTRL_1_string;
  reg [63:0] memory_FPU_OPCODE_string;
  reg [63:0] _zz_memory_FPU_OPCODE_string;
  reg [63:0] _zz_memory_to_writeBack_FPU_OPCODE_string;
  reg [63:0] _zz_memory_to_writeBack_FPU_OPCODE_1_string;
  reg [63:0] execute_FPU_OPCODE_string;
  reg [63:0] _zz_execute_FPU_OPCODE_string;
  reg [63:0] _zz_execute_to_memory_FPU_OPCODE_string;
  reg [63:0] _zz_execute_to_memory_FPU_OPCODE_1_string;
  reg [63:0] _zz_decode_to_execute_FPU_OPCODE_string;
  reg [63:0] _zz_decode_to_execute_FPU_OPCODE_1_string;
  reg [39:0] decode_ALU_BITWISE_CTRL_string;
  reg [39:0] _zz_decode_ALU_BITWISE_CTRL_string;
  reg [39:0] _zz_decode_to_execute_ALU_BITWISE_CTRL_string;
  reg [39:0] _zz_decode_to_execute_ALU_BITWISE_CTRL_1_string;
  reg [47:0] _zz_memory_to_writeBack_ENV_CTRL_string;
  reg [47:0] _zz_memory_to_writeBack_ENV_CTRL_1_string;
  reg [47:0] _zz_execute_to_memory_ENV_CTRL_string;
  reg [47:0] _zz_execute_to_memory_ENV_CTRL_1_string;
  reg [47:0] decode_ENV_CTRL_string;
  reg [47:0] _zz_decode_ENV_CTRL_string;
  reg [47:0] _zz_decode_to_execute_ENV_CTRL_string;
  reg [47:0] _zz_decode_to_execute_ENV_CTRL_1_string;
  reg [63:0] decode_ALU_CTRL_string;
  reg [63:0] _zz_decode_ALU_CTRL_string;
  reg [63:0] _zz_decode_to_execute_ALU_CTRL_string;
  reg [63:0] _zz_decode_to_execute_ALU_CTRL_1_string;
  reg [31:0] execute_BRANCH_CTRL_string;
  reg [31:0] _zz_execute_BRANCH_CTRL_string;
  reg [71:0] execute_SHIFT_CTRL_string;
  reg [71:0] _zz_execute_SHIFT_CTRL_string;
  reg [63:0] writeBack_FPU_OPCODE_string;
  reg [63:0] _zz_writeBack_FPU_OPCODE_string;
  reg [63:0] decode_FPU_OPCODE_string;
  reg [63:0] _zz_decode_FPU_OPCODE_string;
  reg [23:0] decode_SRC2_CTRL_string;
  reg [23:0] _zz_decode_SRC2_CTRL_string;
  reg [95:0] decode_SRC1_CTRL_string;
  reg [95:0] _zz_decode_SRC1_CTRL_string;
  reg [63:0] execute_ALU_CTRL_string;
  reg [63:0] _zz_execute_ALU_CTRL_string;
  reg [39:0] execute_ALU_BITWISE_CTRL_string;
  reg [39:0] _zz_execute_ALU_BITWISE_CTRL_string;
  reg [31:0] _zz_decode_BRANCH_CTRL_1_string;
  reg [71:0] _zz_decode_SHIFT_CTRL_1_string;
  reg [63:0] _zz_decode_FPU_OPCODE_1_string;
  reg [39:0] _zz_decode_ALU_BITWISE_CTRL_1_string;
  reg [47:0] _zz_decode_ENV_CTRL_1_string;
  reg [23:0] _zz_decode_SRC2_CTRL_1_string;
  reg [63:0] _zz_decode_ALU_CTRL_1_string;
  reg [95:0] _zz_decode_SRC1_CTRL_1_string;
  reg [47:0] memory_ENV_CTRL_string;
  reg [47:0] _zz_memory_ENV_CTRL_string;
  reg [47:0] execute_ENV_CTRL_string;
  reg [47:0] _zz_execute_ENV_CTRL_string;
  reg [47:0] writeBack_ENV_CTRL_string;
  reg [47:0] _zz_writeBack_ENV_CTRL_string;
  reg [71:0] debugBus_dmToHart_payload_op_string;
  reg [63:0] FpuPlugin_port_cmd_payload_opcode_string;
  reg [47:0] FpuPlugin_port_cmd_payload_format_string;
  reg [23:0] FpuPlugin_port_cmd_payload_roundMode_string;
  reg [63:0] FpuPlugin_port_commit_payload_opcode_string;
  reg [71:0] CsrPlugin_inject_cmd_payload_op_string;
  reg [71:0] CsrPlugin_inject_cmd_toStream_payload_op_string;
  reg [71:0] CsrPlugin_inject_buffer_payload_op_string;
  reg [71:0] CsrPlugin_inject_cmd_toStream_rData_op_string;
  reg [47:0] CsrPlugin_dcsr_stepLogic_stateReg_string;
  reg [47:0] CsrPlugin_dcsr_stepLogic_stateNext_string;
  reg [95:0] _zz_decode_SRC1_CTRL_2_string;
  reg [63:0] _zz_decode_ALU_CTRL_2_string;
  reg [23:0] _zz_decode_SRC2_CTRL_2_string;
  reg [47:0] _zz_decode_ENV_CTRL_2_string;
  reg [39:0] _zz_decode_ALU_BITWISE_CTRL_2_string;
  reg [63:0] _zz_decode_FPU_OPCODE_2_string;
  reg [71:0] _zz_decode_SHIFT_CTRL_2_string;
  reg [31:0] _zz_decode_BRANCH_CTRL_2_string;
  reg [23:0] _zz_FpuPlugin_port_cmd_payload_roundMode_string;
  reg [23:0] _zz_FpuPlugin_port_cmd_payload_roundMode_1_string;
  reg [63:0] writeBack_FpuPlugin_commit_payload_opcode_string;
  reg [63:0] writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string;
  reg [63:0] writeBack_FpuPlugin_commit_rData_opcode_string;
  reg [63:0] _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string;
  reg [71:0] toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_op_string;
  reg [63:0] decode_to_execute_ALU_CTRL_string;
  reg [47:0] decode_to_execute_ENV_CTRL_string;
  reg [47:0] execute_to_memory_ENV_CTRL_string;
  reg [47:0] memory_to_writeBack_ENV_CTRL_string;
  reg [39:0] decode_to_execute_ALU_BITWISE_CTRL_string;
  reg [63:0] decode_to_execute_FPU_OPCODE_string;
  reg [63:0] execute_to_memory_FPU_OPCODE_string;
  reg [63:0] memory_to_writeBack_FPU_OPCODE_string;
  reg [71:0] decode_to_execute_SHIFT_CTRL_string;
  reg [31:0] decode_to_execute_BRANCH_CTRL_string;
  `endif

  reg [31:0] RegFilePlugin_regFile [0:31] /* verilator public */ ;

  assign _zz_when_1 = (|{decodeExceptionPort_valid,IBusSimplePlugin_decodeExceptionPort_valid});
  assign _zz_memory_MUL_LOW = ($signed(_zz_memory_MUL_LOW_1) + $signed(_zz_memory_MUL_LOW_4));
  assign _zz_memory_MUL_LOW_1 = ($signed(52'h0) + $signed(_zz_memory_MUL_LOW_2));
  assign _zz_memory_MUL_LOW_3 = {1'b0,memory_MUL_LL};
  assign _zz_memory_MUL_LOW_2 = {{19{_zz_memory_MUL_LOW_3[32]}}, _zz_memory_MUL_LOW_3};
  assign _zz_memory_MUL_LOW_5 = ({16'd0,memory_MUL_LH} <<< 5'd16);
  assign _zz_memory_MUL_LOW_4 = {{2{_zz_memory_MUL_LOW_5[49]}}, _zz_memory_MUL_LOW_5};
  assign _zz_memory_MUL_LOW_7 = ({16'd0,memory_MUL_HL} <<< 5'd16);
  assign _zz_memory_MUL_LOW_6 = {{2{_zz_memory_MUL_LOW_7[49]}}, _zz_memory_MUL_LOW_7};
  assign _zz_decode_FORMAL_PC_NEXT_1 = (decode_IS_RVC ? 3'b010 : 3'b100);
  assign _zz_decode_FORMAL_PC_NEXT = {29'd0, _zz_decode_FORMAL_PC_NEXT_1};
  assign _zz__zz_IBusSimplePlugin_jump_pcLoad_payload_1 = (_zz_IBusSimplePlugin_jump_pcLoad_payload - 3'b001);
  assign _zz_IBusSimplePlugin_fetchPc_pc_1 = {IBusSimplePlugin_fetchPc_inc,2'b00};
  assign _zz_IBusSimplePlugin_fetchPc_pc = {29'd0, _zz_IBusSimplePlugin_fetchPc_pc_1};
  assign _zz_IBusSimplePlugin_decodePc_pcPlus_1 = (decode_IS_RVC ? 3'b010 : 3'b100);
  assign _zz_IBusSimplePlugin_decodePc_pcPlus = {29'd0, _zz_IBusSimplePlugin_decodePc_pcPlus_1};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_30 = {{_zz_IBusSimplePlugin_decompressor_decompressed_10,_zz_IBusSimplePlugin_decompressor_decompressed[6 : 2]},12'h0};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_37 = {{{4'b0000,_zz_IBusSimplePlugin_decompressor_decompressed[8 : 7]},_zz_IBusSimplePlugin_decompressor_decompressed[12 : 9]},2'b00};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_38 = {{{4'b0000,_zz_IBusSimplePlugin_decompressor_decompressed[8 : 7]},_zz_IBusSimplePlugin_decompressor_decompressed[12 : 9]},2'b00};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_39 = {{{4'b0000,_zz_IBusSimplePlugin_decompressor_decompressed[8 : 7]},_zz_IBusSimplePlugin_decompressor_decompressed[12 : 9]},2'b00};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_40 = {{{4'b0000,_zz_IBusSimplePlugin_decompressor_decompressed[8 : 7]},_zz_IBusSimplePlugin_decompressor_decompressed[12 : 9]},2'b00};
  assign _zz_IBusSimplePlugin_pending_next = (IBusSimplePlugin_pending_value + _zz_IBusSimplePlugin_pending_next_1);
  assign _zz_IBusSimplePlugin_pending_next_2 = IBusSimplePlugin_pending_inc;
  assign _zz_IBusSimplePlugin_pending_next_1 = {2'd0, _zz_IBusSimplePlugin_pending_next_2};
  assign _zz_IBusSimplePlugin_pending_next_4 = IBusSimplePlugin_pending_dec;
  assign _zz_IBusSimplePlugin_pending_next_3 = {2'd0, _zz_IBusSimplePlugin_pending_next_4};
  assign _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter_1 = (IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_valid && (IBusSimplePlugin_rspJoin_rspBuffer_discardCounter != 3'b000));
  assign _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter = {2'd0, _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter_1};
  assign _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter_3 = IBusSimplePlugin_pending_dec;
  assign _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter_2 = {2'd0, _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter_3};
  assign _zz_DBusCachedPlugin_exceptionBus_payload_code = (writeBack_MEMORY_WR ? 3'b111 : 3'b101);
  assign _zz_DBusCachedPlugin_exceptionBus_payload_code_1 = (writeBack_MEMORY_WR ? 3'b110 : 3'b100);
  assign _zz_CsrPlugin_timeout_counter_valueNext_1 = CsrPlugin_timeout_counter_willIncrement;
  assign _zz_CsrPlugin_timeout_counter_valueNext = {2'd0, _zz_CsrPlugin_timeout_counter_valueNext_1};
  assign _zz_when = 1'b1;
  assign _zz_CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext_1 = CsrPlugin_trigger_decodeBreak_timeout_counter_willIncrement;
  assign _zz_CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext = {1'd0, _zz_CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext_1};
  assign _zz_CsrPlugin_mcycle_1 = ((! debugMode) || (! CsrPlugin_dcsr_stopcount));
  assign _zz_CsrPlugin_mcycle = {63'd0, _zz_CsrPlugin_mcycle_1};
  assign _zz__zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code_1 = (_zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code & (~ _zz__zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code_1_1));
  assign _zz__zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code_1_1 = (_zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code - 2'b01);
  assign _zz__zz_execute_REGFILE_WRITE_DATA = execute_SRC_LESS;
  assign _zz__zz_decode_SRC1 = (decode_IS_RVC ? 3'b010 : 3'b100);
  assign _zz__zz_decode_SRC1_1 = decode_INSTRUCTION[19 : 15];
  assign _zz__zz_decode_SRC2_2 = {decode_INSTRUCTION[31 : 25],decode_INSTRUCTION[11 : 7]};
  assign _zz_execute_SrcPlugin_addSub = ($signed(_zz_execute_SrcPlugin_addSub_1) + $signed(_zz_execute_SrcPlugin_addSub_4));
  assign _zz_execute_SrcPlugin_addSub_1 = ($signed(_zz_execute_SrcPlugin_addSub_2) + $signed(_zz_execute_SrcPlugin_addSub_3));
  assign _zz_execute_SrcPlugin_addSub_2 = execute_SRC1;
  assign _zz_execute_SrcPlugin_addSub_3 = (execute_SRC_USE_SUB_LESS ? (~ execute_SRC2) : execute_SRC2);
  assign _zz_execute_SrcPlugin_addSub_4 = (execute_SRC_USE_SUB_LESS ? 32'h00000001 : 32'h0);
  assign _zz_writeBack_MulPlugin_result = {{14{writeBack_MUL_LOW[51]}}, writeBack_MUL_LOW};
  assign _zz_writeBack_MulPlugin_result_1 = ({32'd0,writeBack_MUL_HH} <<< 6'd32);
  assign _zz__zz_lastStageRegFileWrite_payload_data = writeBack_MUL_LOW[31 : 0];
  assign _zz__zz_lastStageRegFileWrite_payload_data_1 = writeBack_MulPlugin_result[63 : 32];
  assign _zz_memory_DivPlugin_div_counter_valueNext_1 = memory_DivPlugin_div_counter_willIncrement;
  assign _zz_memory_DivPlugin_div_counter_valueNext = {5'd0, _zz_memory_DivPlugin_div_counter_valueNext_1};
  assign _zz_memory_DivPlugin_div_stage_0_remainderMinusDenominator = {1'd0, memory_DivPlugin_rs2};
  assign _zz_memory_DivPlugin_div_stage_0_outRemainder = memory_DivPlugin_div_stage_0_remainderMinusDenominator[31:0];
  assign _zz_memory_DivPlugin_div_stage_0_outRemainder_1 = memory_DivPlugin_div_stage_0_remainderShifted[31:0];
  assign _zz_memory_DivPlugin_div_stage_0_outNumerator = {_zz_memory_DivPlugin_div_stage_0_remainderShifted,(! memory_DivPlugin_div_stage_0_remainderMinusDenominator[32])};
  assign _zz_memory_DivPlugin_div_result_1 = _zz_memory_DivPlugin_div_result_2;
  assign _zz_memory_DivPlugin_div_result_2 = _zz_memory_DivPlugin_div_result_3;
  assign _zz_memory_DivPlugin_div_result_3 = ({memory_DivPlugin_div_needRevert,(memory_DivPlugin_div_needRevert ? (~ _zz_memory_DivPlugin_div_result) : _zz_memory_DivPlugin_div_result)} + _zz_memory_DivPlugin_div_result_4);
  assign _zz_memory_DivPlugin_div_result_5 = memory_DivPlugin_div_needRevert;
  assign _zz_memory_DivPlugin_div_result_4 = {32'd0, _zz_memory_DivPlugin_div_result_5};
  assign _zz_memory_DivPlugin_rs1_3 = _zz_memory_DivPlugin_rs1;
  assign _zz_memory_DivPlugin_rs1_2 = {32'd0, _zz_memory_DivPlugin_rs1_3};
  assign _zz_memory_DivPlugin_rs2_2 = _zz_memory_DivPlugin_rs2;
  assign _zz_memory_DivPlugin_rs2_1 = {31'd0, _zz_memory_DivPlugin_rs2_2};
  assign _zz_FpuPlugin_pendings = (_zz_FpuPlugin_pendings_1 - _zz_FpuPlugin_pendings_4);
  assign _zz_FpuPlugin_pendings_1 = (FpuPlugin_pendings + _zz_FpuPlugin_pendings_2);
  assign _zz_FpuPlugin_pendings_3 = FpuPlugin_port_cmd_fire;
  assign _zz_FpuPlugin_pendings_2 = {5'd0, _zz_FpuPlugin_pendings_3};
  assign _zz_FpuPlugin_pendings_5 = FpuPlugin_port_completion_valid;
  assign _zz_FpuPlugin_pendings_4 = {5'd0, _zz_FpuPlugin_pendings_5};
  assign _zz_FpuPlugin_pendings_7 = FpuPlugin_port_rsp_fire;
  assign _zz_FpuPlugin_pendings_6 = {5'd0, _zz_FpuPlugin_pendings_7};
  assign _zz__zz_execute_to_memory_REGFILE_WRITE_DATA_1 = (_zz__zz_execute_to_memory_REGFILE_WRITE_DATA_1_1 >>> 1'd1);
  assign _zz__zz_execute_to_memory_REGFILE_WRITE_DATA_1_1 = {((execute_SHIFT_CTRL == ShiftCtrlEnum_SRA_1) && execute_LightShifterPlugin_shiftInput[31]),execute_LightShifterPlugin_shiftInput};
  assign _zz__zz_execute_BranchPlugin_branch_src2 = {{{execute_INSTRUCTION[31],execute_INSTRUCTION[19 : 12]},execute_INSTRUCTION[20]},execute_INSTRUCTION[30 : 21]};
  assign _zz__zz_execute_BranchPlugin_branch_src2_4 = {{{execute_INSTRUCTION[31],execute_INSTRUCTION[7]},execute_INSTRUCTION[30 : 25]},execute_INSTRUCTION[11 : 8]};
  assign _zz_when_CsrPlugin_l1718 = (execute_CsrPlugin_csrAddress >>> 3'd4);
  assign _zz_decode_RegFilePlugin_rs1Data = 1'b1;
  assign _zz_decode_RegFilePlugin_rs2Data = 1'b1;
  assign _zz_IBusSimplePlugin_jump_pcLoad_payload_5 = {_zz_IBusSimplePlugin_jump_pcLoad_payload_3,_zz_IBusSimplePlugin_jump_pcLoad_payload_2};
  assign _zz_writeBack_DBusCachedPlugin_rspShifted_1 = dataCache_1_io_cpu_writeBack_address[1 : 0];
  assign _zz_writeBack_DBusCachedPlugin_rspShifted_3 = dataCache_1_io_cpu_writeBack_address[1 : 1];
  assign _zz_decode_LEGAL_INSTRUCTION = 32'h06000073;
  assign _zz_decode_LEGAL_INSTRUCTION_1 = (decode_INSTRUCTION & 32'h0000107f);
  assign _zz_decode_LEGAL_INSTRUCTION_2 = 32'h00001073;
  assign _zz_decode_LEGAL_INSTRUCTION_3 = ((decode_INSTRUCTION & 32'h0000207f) == 32'h00002073);
  assign _zz_decode_LEGAL_INSTRUCTION_4 = ((decode_INSTRUCTION & 32'h0000407f) == 32'h00004063);
  assign _zz_decode_LEGAL_INSTRUCTION_5 = {((decode_INSTRUCTION & 32'h0000207f) == 32'h00002013),{((decode_INSTRUCTION & 32'h0000705b) == 32'h00002003),{((decode_INSTRUCTION & _zz_decode_LEGAL_INSTRUCTION_6) == 32'h00000013),{(_zz_decode_LEGAL_INSTRUCTION_7 == _zz_decode_LEGAL_INSTRUCTION_8),{_zz_decode_LEGAL_INSTRUCTION_9,{_zz_decode_LEGAL_INSTRUCTION_10,_zz_decode_LEGAL_INSTRUCTION_11}}}}}};
  assign _zz_decode_LEGAL_INSTRUCTION_6 = 32'h0000107f;
  assign _zz_decode_LEGAL_INSTRUCTION_7 = (decode_INSTRUCTION & 32'h0000603f);
  assign _zz_decode_LEGAL_INSTRUCTION_8 = 32'h00000023;
  assign _zz_decode_LEGAL_INSTRUCTION_9 = ((decode_INSTRUCTION & 32'h0000207f) == 32'h00000003);
  assign _zz_decode_LEGAL_INSTRUCTION_10 = ((decode_INSTRUCTION & 32'h0000707b) == 32'h00000063);
  assign _zz_decode_LEGAL_INSTRUCTION_11 = {((decode_INSTRUCTION & 32'h0000607f) == 32'h0000000f),{((decode_INSTRUCTION & 32'he600007f) == 32'h00000053),{((decode_INSTRUCTION & _zz_decode_LEGAL_INSTRUCTION_12) == 32'h00000033),{(_zz_decode_LEGAL_INSTRUCTION_13 == _zz_decode_LEGAL_INSTRUCTION_14),{_zz_decode_LEGAL_INSTRUCTION_15,{_zz_decode_LEGAL_INSTRUCTION_16,_zz_decode_LEGAL_INSTRUCTION_17}}}}}};
  assign _zz_decode_LEGAL_INSTRUCTION_12 = 32'hfc00007f;
  assign _zz_decode_LEGAL_INSTRUCTION_13 = (decode_INSTRUCTION & 32'h01f0707f);
  assign _zz_decode_LEGAL_INSTRUCTION_14 = 32'h0000500f;
  assign _zz_decode_LEGAL_INSTRUCTION_15 = ((decode_INSTRUCTION & 32'h7e00507f) == 32'h20000053);
  assign _zz_decode_LEGAL_INSTRUCTION_16 = ((decode_INSTRUCTION & 32'hf600607f) == 32'h20000053);
  assign _zz_decode_LEGAL_INSTRUCTION_17 = {((decode_INSTRUCTION & 32'hbe00705f) == 32'h00005013),{((decode_INSTRUCTION & 32'h7e00607f) == 32'h20000053),{((decode_INSTRUCTION & _zz_decode_LEGAL_INSTRUCTION_18) == 32'h00001013),{(_zz_decode_LEGAL_INSTRUCTION_19 == _zz_decode_LEGAL_INSTRUCTION_20),{_zz_decode_LEGAL_INSTRUCTION_21,{_zz_decode_LEGAL_INSTRUCTION_22,_zz_decode_LEGAL_INSTRUCTION_23}}}}}};
  assign _zz_decode_LEGAL_INSTRUCTION_18 = 32'hfe00305f;
  assign _zz_decode_LEGAL_INSTRUCTION_19 = (decode_INSTRUCTION & 32'hbe00707f);
  assign _zz_decode_LEGAL_INSTRUCTION_20 = 32'h00000033;
  assign _zz_decode_LEGAL_INSTRUCTION_21 = ((decode_INSTRUCTION & 32'hefe0007f) == 32'hc0000053);
  assign _zz_decode_LEGAL_INSTRUCTION_22 = ((decode_INSTRUCTION & 32'hfff0007f) == 32'h58000053);
  assign _zz_decode_LEGAL_INSTRUCTION_23 = {((decode_INSTRUCTION & 32'heff0707f) == 32'he0000053),{((decode_INSTRUCTION & 32'hfff0607f) == 32'he0000053),{((decode_INSTRUCTION & 32'hdfffffff) == 32'h10200073),((decode_INSTRUCTION & 32'hffffffff) == 32'h00100073)}}};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_27 = {_zz_IBusSimplePlugin_decompressor_decompressed_12,_zz_IBusSimplePlugin_decompressor_decompressed[4 : 3]};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_28 = _zz_IBusSimplePlugin_decompressor_decompressed[5];
  assign _zz_IBusSimplePlugin_decompressor_decompressed_29 = _zz_IBusSimplePlugin_decompressor_decompressed[2];
  assign _zz_IBusSimplePlugin_decompressor_decompressed_31 = (_zz_IBusSimplePlugin_decompressor_decompressed[11 : 10] == 2'b01);
  assign _zz_IBusSimplePlugin_decompressor_decompressed_32 = ((_zz_IBusSimplePlugin_decompressor_decompressed[11 : 10] == 2'b11) && (_zz_IBusSimplePlugin_decompressor_decompressed[6 : 5] == 2'b00));
  assign _zz_IBusSimplePlugin_decompressor_decompressed_33 = 7'h0;
  assign _zz_IBusSimplePlugin_decompressor_decompressed_34 = _zz_IBusSimplePlugin_decompressor_decompressed[6 : 2];
  assign _zz_IBusSimplePlugin_decompressor_decompressed_35 = _zz_IBusSimplePlugin_decompressor_decompressed[12];
  assign _zz_IBusSimplePlugin_decompressor_decompressed_36 = _zz_IBusSimplePlugin_decompressor_decompressed[11 : 7];
  assign _zz__zz_decode_FPU_RSP = (decode_INSTRUCTION & 32'h00002034);
  assign _zz__zz_decode_FPU_RSP_1 = 32'h00000024;
  assign _zz__zz_decode_FPU_RSP_2 = (decode_INSTRUCTION & 32'h00000078);
  assign _zz__zz_decode_FPU_RSP_3 = 32'h00000060;
  assign _zz__zz_decode_FPU_RSP_4 = ((decode_INSTRUCTION & 32'h02007054) == 32'h00005010);
  assign _zz__zz_decode_FPU_RSP_5 = {((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_6) == 32'h40001010),((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_7) == 32'h00001010)};
  assign _zz__zz_decode_FPU_RSP_8 = (|{_zz_decode_FPU_RSP_9,(_zz__zz_decode_FPU_RSP_9 == _zz__zz_decode_FPU_RSP_10)});
  assign _zz__zz_decode_FPU_RSP_11 = (|{_zz_decode_FPU_RSP_3,{_zz__zz_decode_FPU_RSP_12,_zz__zz_decode_FPU_RSP_14}});
  assign _zz__zz_decode_FPU_RSP_19 = {1'b0,{(|_zz__zz_decode_FPU_RSP_20),{_zz__zz_decode_FPU_RSP_27,{_zz__zz_decode_FPU_RSP_32,_zz__zz_decode_FPU_RSP_45}}}};
  assign _zz__zz_decode_FPU_RSP_6 = 32'h40003054;
  assign _zz__zz_decode_FPU_RSP_7 = 32'h02007054;
  assign _zz__zz_decode_FPU_RSP_9 = (decode_INSTRUCTION & 32'h20002010);
  assign _zz__zz_decode_FPU_RSP_10 = 32'h20002010;
  assign _zz__zz_decode_FPU_RSP_12 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_13) == 32'h20001010);
  assign _zz__zz_decode_FPU_RSP_14 = {(_zz__zz_decode_FPU_RSP_15 == _zz__zz_decode_FPU_RSP_16),(_zz__zz_decode_FPU_RSP_17 == _zz__zz_decode_FPU_RSP_18)};
  assign _zz__zz_decode_FPU_RSP_20 = {(_zz__zz_decode_FPU_RSP_21 == _zz__zz_decode_FPU_RSP_22),{_zz__zz_decode_FPU_RSP_23,_zz__zz_decode_FPU_RSP_25}};
  assign _zz__zz_decode_FPU_RSP_27 = (|{_zz__zz_decode_FPU_RSP_28,_zz__zz_decode_FPU_RSP_30});
  assign _zz__zz_decode_FPU_RSP_32 = (|{_zz__zz_decode_FPU_RSP_33,_zz__zz_decode_FPU_RSP_36});
  assign _zz__zz_decode_FPU_RSP_45 = {(|_zz__zz_decode_FPU_RSP_46),{_zz__zz_decode_FPU_RSP_59,{_zz__zz_decode_FPU_RSP_62,_zz__zz_decode_FPU_RSP_72}}};
  assign _zz__zz_decode_FPU_RSP_13 = 32'h20001010;
  assign _zz__zz_decode_FPU_RSP_15 = (decode_INSTRUCTION & 32'h28000010);
  assign _zz__zz_decode_FPU_RSP_16 = 32'h08000010;
  assign _zz__zz_decode_FPU_RSP_17 = (decode_INSTRUCTION & 32'ha0100010);
  assign _zz__zz_decode_FPU_RSP_18 = 32'h80000010;
  assign _zz__zz_decode_FPU_RSP_21 = (decode_INSTRUCTION & 32'h60000010);
  assign _zz__zz_decode_FPU_RSP_22 = 32'h60000010;
  assign _zz__zz_decode_FPU_RSP_23 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_24) == 32'h18000010);
  assign _zz__zz_decode_FPU_RSP_25 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_26) == 32'h20000010);
  assign _zz__zz_decode_FPU_RSP_28 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_29) == 32'h80000000);
  assign _zz__zz_decode_FPU_RSP_30 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_31) == 32'h00000040);
  assign _zz__zz_decode_FPU_RSP_33 = (_zz__zz_decode_FPU_RSP_34 == _zz__zz_decode_FPU_RSP_35);
  assign _zz__zz_decode_FPU_RSP_36 = {_zz__zz_decode_FPU_RSP_37,{_zz__zz_decode_FPU_RSP_39,_zz__zz_decode_FPU_RSP_42}};
  assign _zz__zz_decode_FPU_RSP_46 = {_zz_decode_FPU_RSP_10,{_zz__zz_decode_FPU_RSP_47,_zz__zz_decode_FPU_RSP_50}};
  assign _zz__zz_decode_FPU_RSP_59 = (|{_zz__zz_decode_FPU_RSP_60,_zz__zz_decode_FPU_RSP_61});
  assign _zz__zz_decode_FPU_RSP_62 = (|_zz__zz_decode_FPU_RSP_63);
  assign _zz__zz_decode_FPU_RSP_72 = {_zz__zz_decode_FPU_RSP_73,{_zz__zz_decode_FPU_RSP_80,_zz__zz_decode_FPU_RSP_82}};
  assign _zz__zz_decode_FPU_RSP_24 = 32'h18000010;
  assign _zz__zz_decode_FPU_RSP_26 = 32'ha0000010;
  assign _zz__zz_decode_FPU_RSP_29 = 32'h80000004;
  assign _zz__zz_decode_FPU_RSP_31 = 32'h00000050;
  assign _zz__zz_decode_FPU_RSP_34 = (decode_INSTRUCTION & 32'h10001010);
  assign _zz__zz_decode_FPU_RSP_35 = 32'h00001010;
  assign _zz__zz_decode_FPU_RSP_37 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_38) == 32'h00000010);
  assign _zz__zz_decode_FPU_RSP_39 = (_zz__zz_decode_FPU_RSP_40 == _zz__zz_decode_FPU_RSP_41);
  assign _zz__zz_decode_FPU_RSP_42 = (_zz__zz_decode_FPU_RSP_43 == _zz__zz_decode_FPU_RSP_44);
  assign _zz__zz_decode_FPU_RSP_47 = (_zz__zz_decode_FPU_RSP_48 == _zz__zz_decode_FPU_RSP_49);
  assign _zz__zz_decode_FPU_RSP_50 = {_zz__zz_decode_FPU_RSP_51,{_zz__zz_decode_FPU_RSP_53,_zz__zz_decode_FPU_RSP_56}};
  assign _zz__zz_decode_FPU_RSP_60 = _zz_decode_FPU_RSP_10;
  assign _zz__zz_decode_FPU_RSP_61 = _zz_decode_FPU_RSP_6;
  assign _zz__zz_decode_FPU_RSP_63 = {_zz__zz_decode_FPU_RSP_64,{_zz__zz_decode_FPU_RSP_66,_zz__zz_decode_FPU_RSP_69}};
  assign _zz__zz_decode_FPU_RSP_73 = (|{_zz__zz_decode_FPU_RSP_74,_zz__zz_decode_FPU_RSP_77});
  assign _zz__zz_decode_FPU_RSP_80 = (|_zz__zz_decode_FPU_RSP_81);
  assign _zz__zz_decode_FPU_RSP_82 = {_zz__zz_decode_FPU_RSP_83,{_zz__zz_decode_FPU_RSP_84,_zz__zz_decode_FPU_RSP_86}};
  assign _zz__zz_decode_FPU_RSP_38 = 32'h30000010;
  assign _zz__zz_decode_FPU_RSP_40 = (decode_INSTRUCTION & 32'h88000010);
  assign _zz__zz_decode_FPU_RSP_41 = 32'h00000010;
  assign _zz__zz_decode_FPU_RSP_43 = (decode_INSTRUCTION & 32'h50000010);
  assign _zz__zz_decode_FPU_RSP_44 = 32'h00000010;
  assign _zz__zz_decode_FPU_RSP_48 = (decode_INSTRUCTION & 32'h90000010);
  assign _zz__zz_decode_FPU_RSP_49 = 32'h90000010;
  assign _zz__zz_decode_FPU_RSP_51 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_52) == 32'h40000010);
  assign _zz__zz_decode_FPU_RSP_53 = (_zz__zz_decode_FPU_RSP_54 == _zz__zz_decode_FPU_RSP_55);
  assign _zz__zz_decode_FPU_RSP_56 = (_zz__zz_decode_FPU_RSP_57 == _zz__zz_decode_FPU_RSP_58);
  assign _zz__zz_decode_FPU_RSP_64 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_65) == 32'h10000000);
  assign _zz__zz_decode_FPU_RSP_66 = (_zz__zz_decode_FPU_RSP_67 == _zz__zz_decode_FPU_RSP_68);
  assign _zz__zz_decode_FPU_RSP_69 = (_zz__zz_decode_FPU_RSP_70 == _zz__zz_decode_FPU_RSP_71);
  assign _zz__zz_decode_FPU_RSP_74 = (_zz__zz_decode_FPU_RSP_75 == _zz__zz_decode_FPU_RSP_76);
  assign _zz__zz_decode_FPU_RSP_77 = (_zz__zz_decode_FPU_RSP_78 == _zz__zz_decode_FPU_RSP_79);
  assign _zz__zz_decode_FPU_RSP_81 = _zz_decode_FPU_RSP_11;
  assign _zz__zz_decode_FPU_RSP_83 = (|_zz_decode_FPU_RSP_11);
  assign _zz__zz_decode_FPU_RSP_84 = (|_zz__zz_decode_FPU_RSP_85);
  assign _zz__zz_decode_FPU_RSP_86 = {_zz__zz_decode_FPU_RSP_87,{_zz__zz_decode_FPU_RSP_89,_zz__zz_decode_FPU_RSP_92}};
  assign _zz__zz_decode_FPU_RSP_52 = 32'hc0000010;
  assign _zz__zz_decode_FPU_RSP_54 = (decode_INSTRUCTION & 32'h58000010);
  assign _zz__zz_decode_FPU_RSP_55 = 32'h00000010;
  assign _zz__zz_decode_FPU_RSP_57 = (decode_INSTRUCTION & 32'hb0000010);
  assign _zz__zz_decode_FPU_RSP_58 = 32'h00000010;
  assign _zz__zz_decode_FPU_RSP_65 = 32'h10000020;
  assign _zz__zz_decode_FPU_RSP_67 = (decode_INSTRUCTION & 32'h80000020);
  assign _zz__zz_decode_FPU_RSP_68 = 32'h0;
  assign _zz__zz_decode_FPU_RSP_70 = (decode_INSTRUCTION & 32'h00000030);
  assign _zz__zz_decode_FPU_RSP_71 = 32'h0;
  assign _zz__zz_decode_FPU_RSP_75 = (decode_INSTRUCTION & 32'h00000060);
  assign _zz__zz_decode_FPU_RSP_76 = 32'h00000040;
  assign _zz__zz_decode_FPU_RSP_78 = (decode_INSTRUCTION & 32'h0000005c);
  assign _zz__zz_decode_FPU_RSP_79 = 32'h00000004;
  assign _zz__zz_decode_FPU_RSP_85 = ((decode_INSTRUCTION & 32'h02004064) == 32'h02004020);
  assign _zz__zz_decode_FPU_RSP_87 = (|((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_88) == 32'h02000030));
  assign _zz__zz_decode_FPU_RSP_89 = (|{_zz__zz_decode_FPU_RSP_90,_zz__zz_decode_FPU_RSP_91});
  assign _zz__zz_decode_FPU_RSP_92 = {(|_zz__zz_decode_FPU_RSP_93),{(|_zz__zz_decode_FPU_RSP_94),{_zz__zz_decode_FPU_RSP_95,{_zz__zz_decode_FPU_RSP_100,_zz__zz_decode_FPU_RSP_103}}}};
  assign _zz__zz_decode_FPU_RSP_88 = 32'h02004074;
  assign _zz__zz_decode_FPU_RSP_90 = ((decode_INSTRUCTION & 32'h00000034) == 32'h00000034);
  assign _zz__zz_decode_FPU_RSP_91 = ((decode_INSTRUCTION & 32'h02003054) == 32'h00001010);
  assign _zz__zz_decode_FPU_RSP_93 = ((decode_INSTRUCTION & 32'h00001000) == 32'h00001000);
  assign _zz__zz_decode_FPU_RSP_94 = ((decode_INSTRUCTION & 32'h00003000) == 32'h00002000);
  assign _zz__zz_decode_FPU_RSP_95 = (|{(_zz__zz_decode_FPU_RSP_96 == _zz__zz_decode_FPU_RSP_97),(_zz__zz_decode_FPU_RSP_98 == _zz__zz_decode_FPU_RSP_99)});
  assign _zz__zz_decode_FPU_RSP_100 = (|(_zz__zz_decode_FPU_RSP_101 == _zz__zz_decode_FPU_RSP_102));
  assign _zz__zz_decode_FPU_RSP_103 = {(|_zz__zz_decode_FPU_RSP_104),{(|_zz__zz_decode_FPU_RSP_105),{_zz__zz_decode_FPU_RSP_108,{_zz__zz_decode_FPU_RSP_110,_zz__zz_decode_FPU_RSP_113}}}};
  assign _zz__zz_decode_FPU_RSP_96 = (decode_INSTRUCTION & 32'h00002010);
  assign _zz__zz_decode_FPU_RSP_97 = 32'h00002000;
  assign _zz__zz_decode_FPU_RSP_98 = (decode_INSTRUCTION & 32'h00005000);
  assign _zz__zz_decode_FPU_RSP_99 = 32'h00001000;
  assign _zz__zz_decode_FPU_RSP_101 = (decode_INSTRUCTION & 32'h10003070);
  assign _zz__zz_decode_FPU_RSP_102 = 32'h00000070;
  assign _zz__zz_decode_FPU_RSP_104 = ((decode_INSTRUCTION & 32'h10003034) == 32'h10000030);
  assign _zz__zz_decode_FPU_RSP_105 = {((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_106) == 32'h00001070),((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_107) == 32'h00002070)};
  assign _zz__zz_decode_FPU_RSP_108 = (|((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_109) == 32'h00004008));
  assign _zz__zz_decode_FPU_RSP_110 = (|{_zz__zz_decode_FPU_RSP_111,_zz__zz_decode_FPU_RSP_112});
  assign _zz__zz_decode_FPU_RSP_113 = {(|{_zz__zz_decode_FPU_RSP_114,_zz__zz_decode_FPU_RSP_116}),{(|_zz__zz_decode_FPU_RSP_119),{_zz__zz_decode_FPU_RSP_120,{_zz__zz_decode_FPU_RSP_128,_zz__zz_decode_FPU_RSP_133}}}};
  assign _zz__zz_decode_FPU_RSP_106 = 32'h00001070;
  assign _zz__zz_decode_FPU_RSP_107 = 32'h00002070;
  assign _zz__zz_decode_FPU_RSP_109 = 32'h00004048;
  assign _zz__zz_decode_FPU_RSP_111 = ((decode_INSTRUCTION & 32'h00000034) == 32'h00000020);
  assign _zz__zz_decode_FPU_RSP_112 = ((decode_INSTRUCTION & 32'h00000064) == 32'h00000020);
  assign _zz__zz_decode_FPU_RSP_114 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_115) == 32'h00000020);
  assign _zz__zz_decode_FPU_RSP_116 = {_zz_decode_FPU_RSP_4,(_zz__zz_decode_FPU_RSP_117 == _zz__zz_decode_FPU_RSP_118)};
  assign _zz__zz_decode_FPU_RSP_119 = _zz_decode_FPU_RSP_10;
  assign _zz__zz_decode_FPU_RSP_120 = (|{_zz_decode_FPU_RSP_9,{_zz__zz_decode_FPU_RSP_121,_zz__zz_decode_FPU_RSP_123}});
  assign _zz__zz_decode_FPU_RSP_128 = (|{_zz__zz_decode_FPU_RSP_129,_zz__zz_decode_FPU_RSP_130});
  assign _zz__zz_decode_FPU_RSP_133 = {(|_zz__zz_decode_FPU_RSP_134),{_zz__zz_decode_FPU_RSP_147,{_zz__zz_decode_FPU_RSP_152,_zz__zz_decode_FPU_RSP_157}}};
  assign _zz__zz_decode_FPU_RSP_115 = 32'h00000030;
  assign _zz__zz_decode_FPU_RSP_117 = (decode_INSTRUCTION & 32'h00003060);
  assign _zz__zz_decode_FPU_RSP_118 = 32'h00000060;
  assign _zz__zz_decode_FPU_RSP_121 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_122) == 32'h00004020);
  assign _zz__zz_decode_FPU_RSP_123 = {(_zz__zz_decode_FPU_RSP_124 == _zz__zz_decode_FPU_RSP_125),{_zz__zz_decode_FPU_RSP_126,_zz_decode_FPU_RSP_8}};
  assign _zz__zz_decode_FPU_RSP_129 = _zz_decode_FPU_RSP_9;
  assign _zz__zz_decode_FPU_RSP_130 = {_zz_decode_FPU_RSP_8,(_zz__zz_decode_FPU_RSP_131 == _zz__zz_decode_FPU_RSP_132)};
  assign _zz__zz_decode_FPU_RSP_134 = {_zz_decode_FPU_RSP_7,{_zz__zz_decode_FPU_RSP_135,{_zz__zz_decode_FPU_RSP_137,_zz__zz_decode_FPU_RSP_140}}};
  assign _zz__zz_decode_FPU_RSP_147 = (|{_zz_decode_FPU_RSP_3,{_zz__zz_decode_FPU_RSP_148,_zz__zz_decode_FPU_RSP_149}});
  assign _zz__zz_decode_FPU_RSP_152 = (|{_zz__zz_decode_FPU_RSP_153,_zz__zz_decode_FPU_RSP_154});
  assign _zz__zz_decode_FPU_RSP_157 = {(|_zz__zz_decode_FPU_RSP_158),{_zz__zz_decode_FPU_RSP_163,{_zz__zz_decode_FPU_RSP_166,_zz__zz_decode_FPU_RSP_176}}};
  assign _zz__zz_decode_FPU_RSP_122 = 32'h00004020;
  assign _zz__zz_decode_FPU_RSP_124 = (decode_INSTRUCTION & 32'h00000060);
  assign _zz__zz_decode_FPU_RSP_125 = 32'h00000060;
  assign _zz__zz_decode_FPU_RSP_126 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_127) == 32'h00000020);
  assign _zz__zz_decode_FPU_RSP_131 = (decode_INSTRUCTION & 32'h02000060);
  assign _zz__zz_decode_FPU_RSP_132 = 32'h00000020;
  assign _zz__zz_decode_FPU_RSP_135 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_136) == 32'h00000010);
  assign _zz__zz_decode_FPU_RSP_137 = (_zz__zz_decode_FPU_RSP_138 == _zz__zz_decode_FPU_RSP_139);
  assign _zz__zz_decode_FPU_RSP_140 = {_zz__zz_decode_FPU_RSP_141,{_zz__zz_decode_FPU_RSP_143,_zz__zz_decode_FPU_RSP_144}};
  assign _zz__zz_decode_FPU_RSP_148 = _zz_decode_FPU_RSP_2;
  assign _zz__zz_decode_FPU_RSP_149 = {_zz_decode_FPU_RSP_5,_zz__zz_decode_FPU_RSP_150};
  assign _zz__zz_decode_FPU_RSP_153 = _zz_decode_FPU_RSP_3;
  assign _zz__zz_decode_FPU_RSP_154 = {_zz__zz_decode_FPU_RSP_155,_zz_decode_FPU_RSP_5};
  assign _zz__zz_decode_FPU_RSP_158 = {_zz__zz_decode_FPU_RSP_159,_zz__zz_decode_FPU_RSP_161};
  assign _zz__zz_decode_FPU_RSP_163 = (|_zz__zz_decode_FPU_RSP_164);
  assign _zz__zz_decode_FPU_RSP_166 = (|_zz__zz_decode_FPU_RSP_167);
  assign _zz__zz_decode_FPU_RSP_176 = {_zz__zz_decode_FPU_RSP_177,{_zz__zz_decode_FPU_RSP_178,_zz__zz_decode_FPU_RSP_184}};
  assign _zz__zz_decode_FPU_RSP_127 = 32'h02000020;
  assign _zz__zz_decode_FPU_RSP_136 = 32'h00000050;
  assign _zz__zz_decode_FPU_RSP_138 = (decode_INSTRUCTION & 32'h00001030);
  assign _zz__zz_decode_FPU_RSP_139 = 32'h00001030;
  assign _zz__zz_decode_FPU_RSP_141 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_142) == 32'h00002030);
  assign _zz__zz_decode_FPU_RSP_143 = _zz_decode_FPU_RSP_6;
  assign _zz__zz_decode_FPU_RSP_144 = {_zz__zz_decode_FPU_RSP_145,_zz__zz_decode_FPU_RSP_146};
  assign _zz__zz_decode_FPU_RSP_150 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_151) == 32'h00000020);
  assign _zz__zz_decode_FPU_RSP_155 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_156) == 32'h0);
  assign _zz__zz_decode_FPU_RSP_159 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_160) == 32'h00006000);
  assign _zz__zz_decode_FPU_RSP_161 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_162) == 32'h00004010);
  assign _zz__zz_decode_FPU_RSP_164 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_165) == 32'h00002010);
  assign _zz__zz_decode_FPU_RSP_167 = {_zz__zz_decode_FPU_RSP_168,{_zz__zz_decode_FPU_RSP_169,_zz__zz_decode_FPU_RSP_171}};
  assign _zz__zz_decode_FPU_RSP_177 = (|_zz_decode_FPU_RSP_4);
  assign _zz__zz_decode_FPU_RSP_178 = (|_zz__zz_decode_FPU_RSP_179);
  assign _zz__zz_decode_FPU_RSP_184 = {_zz__zz_decode_FPU_RSP_185,_zz__zz_decode_FPU_RSP_188};
  assign _zz__zz_decode_FPU_RSP_142 = 32'h00002030;
  assign _zz__zz_decode_FPU_RSP_145 = ((decode_INSTRUCTION & 32'h00002024) == 32'h00000024);
  assign _zz__zz_decode_FPU_RSP_146 = ((decode_INSTRUCTION & 32'h00000064) == 32'h0);
  assign _zz__zz_decode_FPU_RSP_151 = 32'h00000070;
  assign _zz__zz_decode_FPU_RSP_156 = 32'h00000020;
  assign _zz__zz_decode_FPU_RSP_160 = 32'h00006004;
  assign _zz__zz_decode_FPU_RSP_162 = 32'h00005014;
  assign _zz__zz_decode_FPU_RSP_165 = 32'h00006014;
  assign _zz__zz_decode_FPU_RSP_168 = ((decode_INSTRUCTION & 32'h00000044) == 32'h0);
  assign _zz__zz_decode_FPU_RSP_169 = ((decode_INSTRUCTION & _zz__zz_decode_FPU_RSP_170) == 32'h00000020);
  assign _zz__zz_decode_FPU_RSP_171 = {(_zz__zz_decode_FPU_RSP_172 == _zz__zz_decode_FPU_RSP_173),{_zz_decode_FPU_RSP_4,{_zz__zz_decode_FPU_RSP_174,_zz__zz_decode_FPU_RSP_175}}};
  assign _zz__zz_decode_FPU_RSP_179 = {(_zz__zz_decode_FPU_RSP_180 == _zz__zz_decode_FPU_RSP_181),{_zz__zz_decode_FPU_RSP_182,_zz__zz_decode_FPU_RSP_183}};
  assign _zz__zz_decode_FPU_RSP_185 = (|{_zz_decode_FPU_RSP_3,{_zz__zz_decode_FPU_RSP_186,_zz__zz_decode_FPU_RSP_187}});
  assign _zz__zz_decode_FPU_RSP_188 = (|{_zz_decode_FPU_RSP_2,_zz_decode_FPU_RSP_1});
  assign _zz__zz_decode_FPU_RSP_170 = 32'h00000038;
  assign _zz__zz_decode_FPU_RSP_172 = (decode_INSTRUCTION & 32'h00004050);
  assign _zz__zz_decode_FPU_RSP_173 = 32'h00004000;
  assign _zz__zz_decode_FPU_RSP_174 = ((decode_INSTRUCTION & 32'h00006024) == 32'h00002020);
  assign _zz__zz_decode_FPU_RSP_175 = {((decode_INSTRUCTION & 32'h00005024) == 32'h00001020),((decode_INSTRUCTION & 32'h90000034) == 32'h90000010)};
  assign _zz__zz_decode_FPU_RSP_180 = (decode_INSTRUCTION & 32'h00000044);
  assign _zz__zz_decode_FPU_RSP_181 = 32'h00000040;
  assign _zz__zz_decode_FPU_RSP_182 = ((decode_INSTRUCTION & 32'h00002014) == 32'h00002010);
  assign _zz__zz_decode_FPU_RSP_183 = ((decode_INSTRUCTION & 32'h40004034) == 32'h40000030);
  assign _zz__zz_decode_FPU_RSP_186 = _zz_decode_FPU_RSP_1;
  assign _zz__zz_decode_FPU_RSP_187 = ((decode_INSTRUCTION & 32'h00002014) == 32'h00000004);
  always @(posedge clk) begin
    if(_zz_decode_RegFilePlugin_rs1Data) begin
      RegFilePlugin_regFile_spinal_port0 <= RegFilePlugin_regFile[decode_RegFilePlugin_regFileReadAddress1];
    end
  end

  always @(posedge clk) begin
    if(_zz_decode_RegFilePlugin_rs2Data) begin
      RegFilePlugin_regFile_spinal_port1 <= RegFilePlugin_regFile[decode_RegFilePlugin_regFileReadAddress2];
    end
  end

  always @(posedge clk) begin
    if(_zz_1) begin
      RegFilePlugin_regFile[lastStageRegFileWrite_payload_address] <= lastStageRegFileWrite_payload_data;
    end
  end

  StreamFifoLowLatency IBusSimplePlugin_rspJoin_rspBuffer_c (
    .io_push_valid         (iBus_rsp_toStream_valid                                       ), //i
    .io_push_ready         (IBusSimplePlugin_rspJoin_rspBuffer_c_io_push_ready            ), //o
    .io_push_payload_error (iBus_rsp_toStream_payload_error                               ), //i
    .io_push_payload_inst  (iBus_rsp_toStream_payload_inst[31:0]                          ), //i
    .io_pop_valid          (IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_valid             ), //o
    .io_pop_ready          (IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_ready             ), //i
    .io_pop_payload_error  (IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_payload_error     ), //o
    .io_pop_payload_inst   (IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_payload_inst[31:0]), //o
    .io_flush              (1'b0                                                          ), //i
    .io_occupancy          (IBusSimplePlugin_rspJoin_rspBuffer_c_io_occupancy             ), //o
    .io_availability       (IBusSimplePlugin_rspJoin_rspBuffer_c_io_availability          ), //o
    .clk                   (clk                                                           ), //i
    .reset                 (reset                                                         )  //i
  );
  DataCache dataCache_1 (
    .io_cpu_execute_isValid                 (dataCache_1_io_cpu_execute_isValid               ), //i
    .io_cpu_execute_address                 (dataCache_1_io_cpu_execute_address[31:0]         ), //i
    .io_cpu_execute_haltIt                  (dataCache_1_io_cpu_execute_haltIt                ), //o
    .io_cpu_execute_args_wr                 (execute_MEMORY_WR                                ), //i
    .io_cpu_execute_args_size               (execute_DBusCachedPlugin_size[1:0]               ), //i
    .io_cpu_execute_args_totalyConsistent   (execute_MEMORY_FORCE_CONSTISTENCY                ), //i
    .io_cpu_execute_refilling               (dataCache_1_io_cpu_execute_refilling             ), //o
    .io_cpu_memory_isValid                  (dataCache_1_io_cpu_memory_isValid                ), //i
    .io_cpu_memory_isStuck                  (memory_arbitration_isStuck                       ), //i
    .io_cpu_memory_isWrite                  (dataCache_1_io_cpu_memory_isWrite                ), //o
    .io_cpu_memory_address                  (dataCache_1_io_cpu_memory_address[31:0]          ), //i
    .io_cpu_memory_mmuRsp_physicalAddress   (DBusCachedPlugin_mmuBus_rsp_physicalAddress[31:0]), //i
    .io_cpu_memory_mmuRsp_isIoAccess        (dataCache_1_io_cpu_memory_mmuRsp_isIoAccess      ), //i
    .io_cpu_memory_mmuRsp_isPaging          (DBusCachedPlugin_mmuBus_rsp_isPaging             ), //i
    .io_cpu_memory_mmuRsp_allowRead         (DBusCachedPlugin_mmuBus_rsp_allowRead            ), //i
    .io_cpu_memory_mmuRsp_allowWrite        (DBusCachedPlugin_mmuBus_rsp_allowWrite           ), //i
    .io_cpu_memory_mmuRsp_allowExecute      (DBusCachedPlugin_mmuBus_rsp_allowExecute         ), //i
    .io_cpu_memory_mmuRsp_exception         (DBusCachedPlugin_mmuBus_rsp_exception            ), //i
    .io_cpu_memory_mmuRsp_refilling         (DBusCachedPlugin_mmuBus_rsp_refilling            ), //i
    .io_cpu_memory_mmuRsp_bypassTranslation (DBusCachedPlugin_mmuBus_rsp_bypassTranslation    ), //i
    .io_cpu_writeBack_isValid               (dataCache_1_io_cpu_writeBack_isValid             ), //i
    .io_cpu_writeBack_isStuck               (writeBack_arbitration_isStuck                    ), //i
    .io_cpu_writeBack_isFiring              (writeBack_arbitration_isFiring                   ), //i
    .io_cpu_writeBack_isUser                (dataCache_1_io_cpu_writeBack_isUser              ), //i
    .io_cpu_writeBack_haltIt                (dataCache_1_io_cpu_writeBack_haltIt              ), //o
    .io_cpu_writeBack_isWrite               (dataCache_1_io_cpu_writeBack_isWrite             ), //o
    .io_cpu_writeBack_storeData             (dataCache_1_io_cpu_writeBack_storeData[31:0]     ), //i
    .io_cpu_writeBack_data                  (dataCache_1_io_cpu_writeBack_data[31:0]          ), //o
    .io_cpu_writeBack_address               (dataCache_1_io_cpu_writeBack_address[31:0]       ), //i
    .io_cpu_writeBack_mmuException          (dataCache_1_io_cpu_writeBack_mmuException        ), //o
    .io_cpu_writeBack_unalignedAccess       (dataCache_1_io_cpu_writeBack_unalignedAccess     ), //o
    .io_cpu_writeBack_accessError           (dataCache_1_io_cpu_writeBack_accessError         ), //o
    .io_cpu_writeBack_keepMemRspData        (dataCache_1_io_cpu_writeBack_keepMemRspData      ), //o
    .io_cpu_writeBack_fence_SW              (dataCache_1_io_cpu_writeBack_fence_SW            ), //i
    .io_cpu_writeBack_fence_SR              (dataCache_1_io_cpu_writeBack_fence_SR            ), //i
    .io_cpu_writeBack_fence_SO              (dataCache_1_io_cpu_writeBack_fence_SO            ), //i
    .io_cpu_writeBack_fence_SI              (dataCache_1_io_cpu_writeBack_fence_SI            ), //i
    .io_cpu_writeBack_fence_PW              (dataCache_1_io_cpu_writeBack_fence_PW            ), //i
    .io_cpu_writeBack_fence_PR              (dataCache_1_io_cpu_writeBack_fence_PR            ), //i
    .io_cpu_writeBack_fence_PO              (dataCache_1_io_cpu_writeBack_fence_PO            ), //i
    .io_cpu_writeBack_fence_PI              (dataCache_1_io_cpu_writeBack_fence_PI            ), //i
    .io_cpu_writeBack_fence_FM              (dataCache_1_io_cpu_writeBack_fence_FM[3:0]       ), //i
    .io_cpu_writeBack_exclusiveOk           (dataCache_1_io_cpu_writeBack_exclusiveOk         ), //o
    .io_cpu_redo                            (dataCache_1_io_cpu_redo                          ), //o
    .io_cpu_flush_valid                     (dataCache_1_io_cpu_flush_valid                   ), //i
    .io_cpu_flush_ready                     (dataCache_1_io_cpu_flush_ready                   ), //o
    .io_cpu_flush_payload_singleLine        (dataCache_1_io_cpu_flush_payload_singleLine      ), //i
    .io_cpu_writesPending                   (dataCache_1_io_cpu_writesPending                 ), //o
    .io_mem_cmd_valid                       (dataCache_1_io_mem_cmd_valid                     ), //o
    .io_mem_cmd_ready                       (dBus_cmd_ready                                   ), //i
    .io_mem_cmd_payload_wr                  (dataCache_1_io_mem_cmd_payload_wr                ), //o
    .io_mem_cmd_payload_uncached            (dataCache_1_io_mem_cmd_payload_uncached          ), //o
    .io_mem_cmd_payload_address             (dataCache_1_io_mem_cmd_payload_address[31:0]     ), //o
    .io_mem_cmd_payload_data                (dataCache_1_io_mem_cmd_payload_data[31:0]        ), //o
    .io_mem_cmd_payload_mask                (dataCache_1_io_mem_cmd_payload_mask[3:0]         ), //o
    .io_mem_cmd_payload_size                (dataCache_1_io_mem_cmd_payload_size[1:0]         ), //o
    .io_mem_cmd_payload_last                (dataCache_1_io_mem_cmd_payload_last              ), //o
    .io_mem_rsp_valid                       (dBus_rsp_valid                                   ), //i
    .io_mem_rsp_payload_last                (dBus_rsp_payload_last                            ), //i
    .io_mem_rsp_payload_data                (dBus_rsp_payload_data[31:0]                      ), //i
    .io_mem_rsp_payload_error               (dBus_rsp_payload_error                           ), //i
    .clk                                    (clk                                              ), //i
    .reset                                  (reset                                            )  //i
  );
  (* keep_hierarchy = "TRUE" *) BufferCC reset_buffercc (
    .io_dataIn  (reset                    ), //i
    .io_dataOut (reset_buffercc_io_dataOut), //o
    .clk        (clk                      ), //i
    .reset      (reset                    )  //i
  );
  FpuCore FpuPlugin_fpu (
    .io_port_0_cmd_valid                   (FpuPlugin_port_cmd_valid                           ), //i
    .io_port_0_cmd_ready                   (FpuPlugin_fpu_io_port_0_cmd_ready                  ), //o
    .io_port_0_cmd_payload_opcode          (FpuPlugin_port_cmd_payload_opcode[3:0]             ), //i
    .io_port_0_cmd_payload_arg             (FpuPlugin_port_cmd_payload_arg[1:0]                ), //i
    .io_port_0_cmd_payload_rs1             (FpuPlugin_port_cmd_payload_rs1[4:0]                ), //i
    .io_port_0_cmd_payload_rs2             (FpuPlugin_port_cmd_payload_rs2[4:0]                ), //i
    .io_port_0_cmd_payload_rs3             (FpuPlugin_port_cmd_payload_rs3[4:0]                ), //i
    .io_port_0_cmd_payload_rd              (FpuPlugin_port_cmd_payload_rd[4:0]                 ), //i
    .io_port_0_cmd_payload_format          (FpuPlugin_port_cmd_payload_format                  ), //i
    .io_port_0_cmd_payload_roundMode       (FpuPlugin_port_cmd_payload_roundMode[2:0]          ), //i
    .io_port_0_commit_valid                (FpuPlugin_port_commit_valid                        ), //i
    .io_port_0_commit_ready                (FpuPlugin_fpu_io_port_0_commit_ready               ), //o
    .io_port_0_commit_payload_opcode       (FpuPlugin_port_commit_payload_opcode[3:0]          ), //i
    .io_port_0_commit_payload_rd           (FpuPlugin_port_commit_payload_rd[4:0]              ), //i
    .io_port_0_commit_payload_write        (FpuPlugin_port_commit_payload_write                ), //i
    .io_port_0_commit_payload_value        (FpuPlugin_port_commit_payload_value[31:0]          ), //i
    .io_port_0_rsp_valid                   (FpuPlugin_fpu_io_port_0_rsp_valid                  ), //o
    .io_port_0_rsp_ready                   (FpuPlugin_port_rsp_ready                           ), //i
    .io_port_0_rsp_payload_value           (FpuPlugin_fpu_io_port_0_rsp_payload_value[31:0]    ), //o
    .io_port_0_rsp_payload_NV              (FpuPlugin_fpu_io_port_0_rsp_payload_NV             ), //o
    .io_port_0_rsp_payload_NX              (FpuPlugin_fpu_io_port_0_rsp_payload_NX             ), //o
    .io_port_0_completion_valid            (FpuPlugin_fpu_io_port_0_completion_valid           ), //o
    .io_port_0_completion_payload_flags_NX (FpuPlugin_fpu_io_port_0_completion_payload_flags_NX), //o
    .io_port_0_completion_payload_flags_UF (FpuPlugin_fpu_io_port_0_completion_payload_flags_UF), //o
    .io_port_0_completion_payload_flags_OF (FpuPlugin_fpu_io_port_0_completion_payload_flags_OF), //o
    .io_port_0_completion_payload_flags_DZ (FpuPlugin_fpu_io_port_0_completion_payload_flags_DZ), //o
    .io_port_0_completion_payload_flags_NV (FpuPlugin_fpu_io_port_0_completion_payload_flags_NV), //o
    .io_port_0_completion_payload_written  (FpuPlugin_fpu_io_port_0_completion_payload_written ), //o
    .clk                                   (clk                                                ), //i
    .reset                                 (reset                                              )  //i
  );
  DebugModule debugModule_1 (
    .io_ctrl_cmd_valid                   (debugTransportModuleJtagTap_1_io_bus_cmd_valid               ), //i
    .io_ctrl_cmd_ready                   (debugModule_1_io_ctrl_cmd_ready                              ), //o
    .io_ctrl_cmd_payload_write           (debugTransportModuleJtagTap_1_io_bus_cmd_payload_write       ), //i
    .io_ctrl_cmd_payload_data            (debugTransportModuleJtagTap_1_io_bus_cmd_payload_data[31:0]  ), //i
    .io_ctrl_cmd_payload_address         (debugTransportModuleJtagTap_1_io_bus_cmd_payload_address[6:0]), //i
    .io_ctrl_rsp_valid                   (debugModule_1_io_ctrl_rsp_valid                              ), //o
    .io_ctrl_rsp_payload_error           (debugModule_1_io_ctrl_rsp_payload_error                      ), //o
    .io_ctrl_rsp_payload_data            (debugModule_1_io_ctrl_rsp_payload_data[31:0]                 ), //o
    .io_ndmreset                         (debugModule_1_io_ndmreset                                    ), //o
    .io_harts_0_halted                   (debugBus_halted                                              ), //i
    .io_harts_0_running                  (debugBus_running                                             ), //i
    .io_harts_0_unavailable              (debugBus_unavailable                                         ), //i
    .io_harts_0_exception                (debugBus_exception                                           ), //i
    .io_harts_0_commit                   (debugBus_commit                                              ), //i
    .io_harts_0_ebreak                   (debugBus_ebreak                                              ), //i
    .io_harts_0_redo                     (debugBus_redo                                                ), //i
    .io_harts_0_regSuccess               (debugBus_regSuccess                                          ), //i
    .io_harts_0_ackReset                 (debugModule_1_io_harts_0_ackReset                            ), //o
    .io_harts_0_haveReset                (debugBus_haveReset                                           ), //i
    .io_harts_0_resume_cmd_valid         (debugModule_1_io_harts_0_resume_cmd_valid                    ), //o
    .io_harts_0_resume_rsp_valid         (debugBus_resume_rsp_valid                                    ), //i
    .io_harts_0_haltReq                  (debugModule_1_io_harts_0_haltReq                             ), //o
    .io_harts_0_dmToHart_valid           (debugModule_1_io_harts_0_dmToHart_valid                      ), //o
    .io_harts_0_dmToHart_payload_op      (debugModule_1_io_harts_0_dmToHart_payload_op[1:0]            ), //o
    .io_harts_0_dmToHart_payload_address (debugModule_1_io_harts_0_dmToHart_payload_address[4:0]       ), //o
    .io_harts_0_dmToHart_payload_data    (debugModule_1_io_harts_0_dmToHart_payload_data[31:0]         ), //o
    .io_harts_0_dmToHart_payload_size    (debugModule_1_io_harts_0_dmToHart_payload_size[2:0]          ), //o
    .io_harts_0_hartToDm_valid           (debugBus_hartToDm_valid                                      ), //i
    .io_harts_0_hartToDm_payload_address (debugBus_hartToDm_payload_address[3:0]                       ), //i
    .io_harts_0_hartToDm_payload_data    (debugBus_hartToDm_payload_data[31:0]                         ), //i
    .clk                                 (clk                                                          ), //i
    .reset                               (reset                                                        )  //i
  );
  DebugTransportModuleJtagTap debugTransportModuleJtagTap_1 (
    .io_jtag_tms                (jtag_tms                                                     ), //i
    .io_jtag_tdi                (jtag_tdi                                                     ), //i
    .io_jtag_tdo                (debugTransportModuleJtagTap_1_io_jtag_tdo                    ), //o
    .io_jtag_tck                (jtag_tck                                                     ), //i
    .io_bus_cmd_valid           (debugTransportModuleJtagTap_1_io_bus_cmd_valid               ), //o
    .io_bus_cmd_ready           (debugModule_1_io_ctrl_cmd_ready                              ), //i
    .io_bus_cmd_payload_write   (debugTransportModuleJtagTap_1_io_bus_cmd_payload_write       ), //o
    .io_bus_cmd_payload_data    (debugTransportModuleJtagTap_1_io_bus_cmd_payload_data[31:0]  ), //o
    .io_bus_cmd_payload_address (debugTransportModuleJtagTap_1_io_bus_cmd_payload_address[6:0]), //o
    .io_bus_rsp_valid           (debugModule_1_io_ctrl_rsp_valid                              ), //i
    .io_bus_rsp_payload_error   (debugModule_1_io_ctrl_rsp_payload_error                      ), //i
    .io_bus_rsp_payload_data    (debugModule_1_io_ctrl_rsp_payload_data[31:0]                 ), //i
    .clk                        (clk                                                          ), //i
    .reset                      (reset                                                        )  //i
  );
  always @(*) begin
    case(_zz_IBusSimplePlugin_jump_pcLoad_payload_5)
      2'b00 : _zz_IBusSimplePlugin_jump_pcLoad_payload_4 = DBusCachedPlugin_redoBranch_payload;
      2'b01 : _zz_IBusSimplePlugin_jump_pcLoad_payload_4 = CsrPlugin_jumpInterface_payload;
      default : _zz_IBusSimplePlugin_jump_pcLoad_payload_4 = BranchPlugin_jumpInterface_payload;
    endcase
  end

  always @(*) begin
    case(_zz_writeBack_DBusCachedPlugin_rspShifted_1)
      2'b00 : _zz_writeBack_DBusCachedPlugin_rspShifted = writeBack_DBusCachedPlugin_rspSplits_0;
      2'b01 : _zz_writeBack_DBusCachedPlugin_rspShifted = writeBack_DBusCachedPlugin_rspSplits_1;
      2'b10 : _zz_writeBack_DBusCachedPlugin_rspShifted = writeBack_DBusCachedPlugin_rspSplits_2;
      default : _zz_writeBack_DBusCachedPlugin_rspShifted = writeBack_DBusCachedPlugin_rspSplits_3;
    endcase
  end

  always @(*) begin
    case(_zz_writeBack_DBusCachedPlugin_rspShifted_3)
      1'b0 : _zz_writeBack_DBusCachedPlugin_rspShifted_2 = writeBack_DBusCachedPlugin_rspSplits_1;
      default : _zz_writeBack_DBusCachedPlugin_rspShifted_2 = writeBack_DBusCachedPlugin_rspSplits_3;
    endcase
  end

  always @(*) begin
    case(CsrPlugin_trigger_tselect_index)
      1'b0 : _zz__zz_CsrPlugin_csrMapping_readDataInit_5 = CsrPlugin_trigger_slots_0_tdata1_read;
      default : _zz__zz_CsrPlugin_csrMapping_readDataInit_5 = CsrPlugin_trigger_slots_1_tdata1_read;
    endcase
  end

  `ifndef SYNTHESIS
  always @(*) begin
    case(decode_BRANCH_CTRL)
      BranchCtrlEnum_INC : decode_BRANCH_CTRL_string = "INC ";
      BranchCtrlEnum_B : decode_BRANCH_CTRL_string = "B   ";
      BranchCtrlEnum_JAL : decode_BRANCH_CTRL_string = "JAL ";
      BranchCtrlEnum_JALR : decode_BRANCH_CTRL_string = "JALR";
      default : decode_BRANCH_CTRL_string = "????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_BRANCH_CTRL)
      BranchCtrlEnum_INC : _zz_decode_BRANCH_CTRL_string = "INC ";
      BranchCtrlEnum_B : _zz_decode_BRANCH_CTRL_string = "B   ";
      BranchCtrlEnum_JAL : _zz_decode_BRANCH_CTRL_string = "JAL ";
      BranchCtrlEnum_JALR : _zz_decode_BRANCH_CTRL_string = "JALR";
      default : _zz_decode_BRANCH_CTRL_string = "????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_BRANCH_CTRL)
      BranchCtrlEnum_INC : _zz_decode_to_execute_BRANCH_CTRL_string = "INC ";
      BranchCtrlEnum_B : _zz_decode_to_execute_BRANCH_CTRL_string = "B   ";
      BranchCtrlEnum_JAL : _zz_decode_to_execute_BRANCH_CTRL_string = "JAL ";
      BranchCtrlEnum_JALR : _zz_decode_to_execute_BRANCH_CTRL_string = "JALR";
      default : _zz_decode_to_execute_BRANCH_CTRL_string = "????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_BRANCH_CTRL_1)
      BranchCtrlEnum_INC : _zz_decode_to_execute_BRANCH_CTRL_1_string = "INC ";
      BranchCtrlEnum_B : _zz_decode_to_execute_BRANCH_CTRL_1_string = "B   ";
      BranchCtrlEnum_JAL : _zz_decode_to_execute_BRANCH_CTRL_1_string = "JAL ";
      BranchCtrlEnum_JALR : _zz_decode_to_execute_BRANCH_CTRL_1_string = "JALR";
      default : _zz_decode_to_execute_BRANCH_CTRL_1_string = "????";
    endcase
  end
  always @(*) begin
    case(decode_SHIFT_CTRL)
      ShiftCtrlEnum_DISABLE_1 : decode_SHIFT_CTRL_string = "DISABLE_1";
      ShiftCtrlEnum_SLL_1 : decode_SHIFT_CTRL_string = "SLL_1    ";
      ShiftCtrlEnum_SRL_1 : decode_SHIFT_CTRL_string = "SRL_1    ";
      ShiftCtrlEnum_SRA_1 : decode_SHIFT_CTRL_string = "SRA_1    ";
      default : decode_SHIFT_CTRL_string = "?????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_SHIFT_CTRL)
      ShiftCtrlEnum_DISABLE_1 : _zz_decode_SHIFT_CTRL_string = "DISABLE_1";
      ShiftCtrlEnum_SLL_1 : _zz_decode_SHIFT_CTRL_string = "SLL_1    ";
      ShiftCtrlEnum_SRL_1 : _zz_decode_SHIFT_CTRL_string = "SRL_1    ";
      ShiftCtrlEnum_SRA_1 : _zz_decode_SHIFT_CTRL_string = "SRA_1    ";
      default : _zz_decode_SHIFT_CTRL_string = "?????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_SHIFT_CTRL)
      ShiftCtrlEnum_DISABLE_1 : _zz_decode_to_execute_SHIFT_CTRL_string = "DISABLE_1";
      ShiftCtrlEnum_SLL_1 : _zz_decode_to_execute_SHIFT_CTRL_string = "SLL_1    ";
      ShiftCtrlEnum_SRL_1 : _zz_decode_to_execute_SHIFT_CTRL_string = "SRL_1    ";
      ShiftCtrlEnum_SRA_1 : _zz_decode_to_execute_SHIFT_CTRL_string = "SRA_1    ";
      default : _zz_decode_to_execute_SHIFT_CTRL_string = "?????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_SHIFT_CTRL_1)
      ShiftCtrlEnum_DISABLE_1 : _zz_decode_to_execute_SHIFT_CTRL_1_string = "DISABLE_1";
      ShiftCtrlEnum_SLL_1 : _zz_decode_to_execute_SHIFT_CTRL_1_string = "SLL_1    ";
      ShiftCtrlEnum_SRL_1 : _zz_decode_to_execute_SHIFT_CTRL_1_string = "SRL_1    ";
      ShiftCtrlEnum_SRA_1 : _zz_decode_to_execute_SHIFT_CTRL_1_string = "SRA_1    ";
      default : _zz_decode_to_execute_SHIFT_CTRL_1_string = "?????????";
    endcase
  end
  always @(*) begin
    case(memory_FPU_OPCODE)
      FpuOpcode_LOAD : memory_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : memory_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : memory_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : memory_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : memory_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : memory_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : memory_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : memory_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : memory_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : memory_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : memory_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : memory_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : memory_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : memory_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : memory_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : memory_FPU_OPCODE_string = "FCVT_X_X";
      default : memory_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_memory_FPU_OPCODE)
      FpuOpcode_LOAD : _zz_memory_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : _zz_memory_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : _zz_memory_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : _zz_memory_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : _zz_memory_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : _zz_memory_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : _zz_memory_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : _zz_memory_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : _zz_memory_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : _zz_memory_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_memory_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_memory_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_memory_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_memory_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_memory_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_memory_FPU_OPCODE_string = "FCVT_X_X";
      default : _zz_memory_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_memory_to_writeBack_FPU_OPCODE)
      FpuOpcode_LOAD : _zz_memory_to_writeBack_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : _zz_memory_to_writeBack_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : _zz_memory_to_writeBack_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : _zz_memory_to_writeBack_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : _zz_memory_to_writeBack_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : _zz_memory_to_writeBack_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : _zz_memory_to_writeBack_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : _zz_memory_to_writeBack_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : _zz_memory_to_writeBack_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : _zz_memory_to_writeBack_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_memory_to_writeBack_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_memory_to_writeBack_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_memory_to_writeBack_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_memory_to_writeBack_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_memory_to_writeBack_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_memory_to_writeBack_FPU_OPCODE_string = "FCVT_X_X";
      default : _zz_memory_to_writeBack_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_memory_to_writeBack_FPU_OPCODE_1)
      FpuOpcode_LOAD : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "LOAD    ";
      FpuOpcode_STORE : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "STORE   ";
      FpuOpcode_MUL : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "MUL     ";
      FpuOpcode_ADD : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "ADD     ";
      FpuOpcode_FMA : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "FMA     ";
      FpuOpcode_I2F : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "I2F     ";
      FpuOpcode_F2I : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "F2I     ";
      FpuOpcode_CMP : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "CMP     ";
      FpuOpcode_DIV : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "DIV     ";
      FpuOpcode_SQRT : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "FCVT_X_X";
      default : _zz_memory_to_writeBack_FPU_OPCODE_1_string = "????????";
    endcase
  end
  always @(*) begin
    case(execute_FPU_OPCODE)
      FpuOpcode_LOAD : execute_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : execute_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : execute_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : execute_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : execute_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : execute_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : execute_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : execute_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : execute_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : execute_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : execute_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : execute_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : execute_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : execute_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : execute_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : execute_FPU_OPCODE_string = "FCVT_X_X";
      default : execute_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_execute_FPU_OPCODE)
      FpuOpcode_LOAD : _zz_execute_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : _zz_execute_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : _zz_execute_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : _zz_execute_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : _zz_execute_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : _zz_execute_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : _zz_execute_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : _zz_execute_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : _zz_execute_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : _zz_execute_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_execute_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_execute_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_execute_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_execute_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_execute_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_execute_FPU_OPCODE_string = "FCVT_X_X";
      default : _zz_execute_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_execute_to_memory_FPU_OPCODE)
      FpuOpcode_LOAD : _zz_execute_to_memory_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : _zz_execute_to_memory_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : _zz_execute_to_memory_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : _zz_execute_to_memory_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : _zz_execute_to_memory_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : _zz_execute_to_memory_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : _zz_execute_to_memory_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : _zz_execute_to_memory_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : _zz_execute_to_memory_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : _zz_execute_to_memory_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_execute_to_memory_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_execute_to_memory_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_execute_to_memory_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_execute_to_memory_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_execute_to_memory_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_execute_to_memory_FPU_OPCODE_string = "FCVT_X_X";
      default : _zz_execute_to_memory_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_execute_to_memory_FPU_OPCODE_1)
      FpuOpcode_LOAD : _zz_execute_to_memory_FPU_OPCODE_1_string = "LOAD    ";
      FpuOpcode_STORE : _zz_execute_to_memory_FPU_OPCODE_1_string = "STORE   ";
      FpuOpcode_MUL : _zz_execute_to_memory_FPU_OPCODE_1_string = "MUL     ";
      FpuOpcode_ADD : _zz_execute_to_memory_FPU_OPCODE_1_string = "ADD     ";
      FpuOpcode_FMA : _zz_execute_to_memory_FPU_OPCODE_1_string = "FMA     ";
      FpuOpcode_I2F : _zz_execute_to_memory_FPU_OPCODE_1_string = "I2F     ";
      FpuOpcode_F2I : _zz_execute_to_memory_FPU_OPCODE_1_string = "F2I     ";
      FpuOpcode_CMP : _zz_execute_to_memory_FPU_OPCODE_1_string = "CMP     ";
      FpuOpcode_DIV : _zz_execute_to_memory_FPU_OPCODE_1_string = "DIV     ";
      FpuOpcode_SQRT : _zz_execute_to_memory_FPU_OPCODE_1_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_execute_to_memory_FPU_OPCODE_1_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_execute_to_memory_FPU_OPCODE_1_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_execute_to_memory_FPU_OPCODE_1_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_execute_to_memory_FPU_OPCODE_1_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_execute_to_memory_FPU_OPCODE_1_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_execute_to_memory_FPU_OPCODE_1_string = "FCVT_X_X";
      default : _zz_execute_to_memory_FPU_OPCODE_1_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_FPU_OPCODE)
      FpuOpcode_LOAD : _zz_decode_to_execute_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : _zz_decode_to_execute_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : _zz_decode_to_execute_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : _zz_decode_to_execute_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : _zz_decode_to_execute_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : _zz_decode_to_execute_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : _zz_decode_to_execute_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : _zz_decode_to_execute_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : _zz_decode_to_execute_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : _zz_decode_to_execute_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_decode_to_execute_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_decode_to_execute_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_decode_to_execute_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_decode_to_execute_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_decode_to_execute_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_decode_to_execute_FPU_OPCODE_string = "FCVT_X_X";
      default : _zz_decode_to_execute_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_FPU_OPCODE_1)
      FpuOpcode_LOAD : _zz_decode_to_execute_FPU_OPCODE_1_string = "LOAD    ";
      FpuOpcode_STORE : _zz_decode_to_execute_FPU_OPCODE_1_string = "STORE   ";
      FpuOpcode_MUL : _zz_decode_to_execute_FPU_OPCODE_1_string = "MUL     ";
      FpuOpcode_ADD : _zz_decode_to_execute_FPU_OPCODE_1_string = "ADD     ";
      FpuOpcode_FMA : _zz_decode_to_execute_FPU_OPCODE_1_string = "FMA     ";
      FpuOpcode_I2F : _zz_decode_to_execute_FPU_OPCODE_1_string = "I2F     ";
      FpuOpcode_F2I : _zz_decode_to_execute_FPU_OPCODE_1_string = "F2I     ";
      FpuOpcode_CMP : _zz_decode_to_execute_FPU_OPCODE_1_string = "CMP     ";
      FpuOpcode_DIV : _zz_decode_to_execute_FPU_OPCODE_1_string = "DIV     ";
      FpuOpcode_SQRT : _zz_decode_to_execute_FPU_OPCODE_1_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_decode_to_execute_FPU_OPCODE_1_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_decode_to_execute_FPU_OPCODE_1_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_decode_to_execute_FPU_OPCODE_1_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_decode_to_execute_FPU_OPCODE_1_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_decode_to_execute_FPU_OPCODE_1_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_decode_to_execute_FPU_OPCODE_1_string = "FCVT_X_X";
      default : _zz_decode_to_execute_FPU_OPCODE_1_string = "????????";
    endcase
  end
  always @(*) begin
    case(decode_ALU_BITWISE_CTRL)
      AluBitwiseCtrlEnum_XOR_1 : decode_ALU_BITWISE_CTRL_string = "XOR_1";
      AluBitwiseCtrlEnum_OR_1 : decode_ALU_BITWISE_CTRL_string = "OR_1 ";
      AluBitwiseCtrlEnum_AND_1 : decode_ALU_BITWISE_CTRL_string = "AND_1";
      default : decode_ALU_BITWISE_CTRL_string = "?????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_ALU_BITWISE_CTRL)
      AluBitwiseCtrlEnum_XOR_1 : _zz_decode_ALU_BITWISE_CTRL_string = "XOR_1";
      AluBitwiseCtrlEnum_OR_1 : _zz_decode_ALU_BITWISE_CTRL_string = "OR_1 ";
      AluBitwiseCtrlEnum_AND_1 : _zz_decode_ALU_BITWISE_CTRL_string = "AND_1";
      default : _zz_decode_ALU_BITWISE_CTRL_string = "?????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_ALU_BITWISE_CTRL)
      AluBitwiseCtrlEnum_XOR_1 : _zz_decode_to_execute_ALU_BITWISE_CTRL_string = "XOR_1";
      AluBitwiseCtrlEnum_OR_1 : _zz_decode_to_execute_ALU_BITWISE_CTRL_string = "OR_1 ";
      AluBitwiseCtrlEnum_AND_1 : _zz_decode_to_execute_ALU_BITWISE_CTRL_string = "AND_1";
      default : _zz_decode_to_execute_ALU_BITWISE_CTRL_string = "?????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_ALU_BITWISE_CTRL_1)
      AluBitwiseCtrlEnum_XOR_1 : _zz_decode_to_execute_ALU_BITWISE_CTRL_1_string = "XOR_1";
      AluBitwiseCtrlEnum_OR_1 : _zz_decode_to_execute_ALU_BITWISE_CTRL_1_string = "OR_1 ";
      AluBitwiseCtrlEnum_AND_1 : _zz_decode_to_execute_ALU_BITWISE_CTRL_1_string = "AND_1";
      default : _zz_decode_to_execute_ALU_BITWISE_CTRL_1_string = "?????";
    endcase
  end
  always @(*) begin
    case(_zz_memory_to_writeBack_ENV_CTRL)
      EnvCtrlEnum_NONE : _zz_memory_to_writeBack_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_memory_to_writeBack_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_memory_to_writeBack_ENV_CTRL_string = "EBREAK";
      default : _zz_memory_to_writeBack_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_memory_to_writeBack_ENV_CTRL_1)
      EnvCtrlEnum_NONE : _zz_memory_to_writeBack_ENV_CTRL_1_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_memory_to_writeBack_ENV_CTRL_1_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_memory_to_writeBack_ENV_CTRL_1_string = "EBREAK";
      default : _zz_memory_to_writeBack_ENV_CTRL_1_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_execute_to_memory_ENV_CTRL)
      EnvCtrlEnum_NONE : _zz_execute_to_memory_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_execute_to_memory_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_execute_to_memory_ENV_CTRL_string = "EBREAK";
      default : _zz_execute_to_memory_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_execute_to_memory_ENV_CTRL_1)
      EnvCtrlEnum_NONE : _zz_execute_to_memory_ENV_CTRL_1_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_execute_to_memory_ENV_CTRL_1_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_execute_to_memory_ENV_CTRL_1_string = "EBREAK";
      default : _zz_execute_to_memory_ENV_CTRL_1_string = "??????";
    endcase
  end
  always @(*) begin
    case(decode_ENV_CTRL)
      EnvCtrlEnum_NONE : decode_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : decode_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : decode_ENV_CTRL_string = "EBREAK";
      default : decode_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_ENV_CTRL)
      EnvCtrlEnum_NONE : _zz_decode_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_decode_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_decode_ENV_CTRL_string = "EBREAK";
      default : _zz_decode_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_ENV_CTRL)
      EnvCtrlEnum_NONE : _zz_decode_to_execute_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_decode_to_execute_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_decode_to_execute_ENV_CTRL_string = "EBREAK";
      default : _zz_decode_to_execute_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_ENV_CTRL_1)
      EnvCtrlEnum_NONE : _zz_decode_to_execute_ENV_CTRL_1_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_decode_to_execute_ENV_CTRL_1_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_decode_to_execute_ENV_CTRL_1_string = "EBREAK";
      default : _zz_decode_to_execute_ENV_CTRL_1_string = "??????";
    endcase
  end
  always @(*) begin
    case(decode_ALU_CTRL)
      AluCtrlEnum_ADD_SUB : decode_ALU_CTRL_string = "ADD_SUB ";
      AluCtrlEnum_SLT_SLTU : decode_ALU_CTRL_string = "SLT_SLTU";
      AluCtrlEnum_BITWISE : decode_ALU_CTRL_string = "BITWISE ";
      default : decode_ALU_CTRL_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_ALU_CTRL)
      AluCtrlEnum_ADD_SUB : _zz_decode_ALU_CTRL_string = "ADD_SUB ";
      AluCtrlEnum_SLT_SLTU : _zz_decode_ALU_CTRL_string = "SLT_SLTU";
      AluCtrlEnum_BITWISE : _zz_decode_ALU_CTRL_string = "BITWISE ";
      default : _zz_decode_ALU_CTRL_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_ALU_CTRL)
      AluCtrlEnum_ADD_SUB : _zz_decode_to_execute_ALU_CTRL_string = "ADD_SUB ";
      AluCtrlEnum_SLT_SLTU : _zz_decode_to_execute_ALU_CTRL_string = "SLT_SLTU";
      AluCtrlEnum_BITWISE : _zz_decode_to_execute_ALU_CTRL_string = "BITWISE ";
      default : _zz_decode_to_execute_ALU_CTRL_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_to_execute_ALU_CTRL_1)
      AluCtrlEnum_ADD_SUB : _zz_decode_to_execute_ALU_CTRL_1_string = "ADD_SUB ";
      AluCtrlEnum_SLT_SLTU : _zz_decode_to_execute_ALU_CTRL_1_string = "SLT_SLTU";
      AluCtrlEnum_BITWISE : _zz_decode_to_execute_ALU_CTRL_1_string = "BITWISE ";
      default : _zz_decode_to_execute_ALU_CTRL_1_string = "????????";
    endcase
  end
  always @(*) begin
    case(execute_BRANCH_CTRL)
      BranchCtrlEnum_INC : execute_BRANCH_CTRL_string = "INC ";
      BranchCtrlEnum_B : execute_BRANCH_CTRL_string = "B   ";
      BranchCtrlEnum_JAL : execute_BRANCH_CTRL_string = "JAL ";
      BranchCtrlEnum_JALR : execute_BRANCH_CTRL_string = "JALR";
      default : execute_BRANCH_CTRL_string = "????";
    endcase
  end
  always @(*) begin
    case(_zz_execute_BRANCH_CTRL)
      BranchCtrlEnum_INC : _zz_execute_BRANCH_CTRL_string = "INC ";
      BranchCtrlEnum_B : _zz_execute_BRANCH_CTRL_string = "B   ";
      BranchCtrlEnum_JAL : _zz_execute_BRANCH_CTRL_string = "JAL ";
      BranchCtrlEnum_JALR : _zz_execute_BRANCH_CTRL_string = "JALR";
      default : _zz_execute_BRANCH_CTRL_string = "????";
    endcase
  end
  always @(*) begin
    case(execute_SHIFT_CTRL)
      ShiftCtrlEnum_DISABLE_1 : execute_SHIFT_CTRL_string = "DISABLE_1";
      ShiftCtrlEnum_SLL_1 : execute_SHIFT_CTRL_string = "SLL_1    ";
      ShiftCtrlEnum_SRL_1 : execute_SHIFT_CTRL_string = "SRL_1    ";
      ShiftCtrlEnum_SRA_1 : execute_SHIFT_CTRL_string = "SRA_1    ";
      default : execute_SHIFT_CTRL_string = "?????????";
    endcase
  end
  always @(*) begin
    case(_zz_execute_SHIFT_CTRL)
      ShiftCtrlEnum_DISABLE_1 : _zz_execute_SHIFT_CTRL_string = "DISABLE_1";
      ShiftCtrlEnum_SLL_1 : _zz_execute_SHIFT_CTRL_string = "SLL_1    ";
      ShiftCtrlEnum_SRL_1 : _zz_execute_SHIFT_CTRL_string = "SRL_1    ";
      ShiftCtrlEnum_SRA_1 : _zz_execute_SHIFT_CTRL_string = "SRA_1    ";
      default : _zz_execute_SHIFT_CTRL_string = "?????????";
    endcase
  end
  always @(*) begin
    case(writeBack_FPU_OPCODE)
      FpuOpcode_LOAD : writeBack_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : writeBack_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : writeBack_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : writeBack_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : writeBack_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : writeBack_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : writeBack_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : writeBack_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : writeBack_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : writeBack_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : writeBack_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : writeBack_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : writeBack_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : writeBack_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : writeBack_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : writeBack_FPU_OPCODE_string = "FCVT_X_X";
      default : writeBack_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_writeBack_FPU_OPCODE)
      FpuOpcode_LOAD : _zz_writeBack_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : _zz_writeBack_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : _zz_writeBack_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : _zz_writeBack_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : _zz_writeBack_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : _zz_writeBack_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : _zz_writeBack_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : _zz_writeBack_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : _zz_writeBack_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : _zz_writeBack_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_writeBack_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_writeBack_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_writeBack_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_writeBack_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_writeBack_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_writeBack_FPU_OPCODE_string = "FCVT_X_X";
      default : _zz_writeBack_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(decode_FPU_OPCODE)
      FpuOpcode_LOAD : decode_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : decode_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : decode_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : decode_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : decode_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : decode_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : decode_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : decode_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : decode_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : decode_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : decode_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : decode_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : decode_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : decode_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : decode_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : decode_FPU_OPCODE_string = "FCVT_X_X";
      default : decode_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_FPU_OPCODE)
      FpuOpcode_LOAD : _zz_decode_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : _zz_decode_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : _zz_decode_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : _zz_decode_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : _zz_decode_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : _zz_decode_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : _zz_decode_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : _zz_decode_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : _zz_decode_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : _zz_decode_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_decode_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_decode_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_decode_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_decode_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_decode_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_decode_FPU_OPCODE_string = "FCVT_X_X";
      default : _zz_decode_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(decode_SRC2_CTRL)
      Src2CtrlEnum_RS : decode_SRC2_CTRL_string = "RS ";
      Src2CtrlEnum_IMI : decode_SRC2_CTRL_string = "IMI";
      Src2CtrlEnum_IMS : decode_SRC2_CTRL_string = "IMS";
      Src2CtrlEnum_PC : decode_SRC2_CTRL_string = "PC ";
      default : decode_SRC2_CTRL_string = "???";
    endcase
  end
  always @(*) begin
    case(_zz_decode_SRC2_CTRL)
      Src2CtrlEnum_RS : _zz_decode_SRC2_CTRL_string = "RS ";
      Src2CtrlEnum_IMI : _zz_decode_SRC2_CTRL_string = "IMI";
      Src2CtrlEnum_IMS : _zz_decode_SRC2_CTRL_string = "IMS";
      Src2CtrlEnum_PC : _zz_decode_SRC2_CTRL_string = "PC ";
      default : _zz_decode_SRC2_CTRL_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_SRC1_CTRL)
      Src1CtrlEnum_RS : decode_SRC1_CTRL_string = "RS          ";
      Src1CtrlEnum_IMU : decode_SRC1_CTRL_string = "IMU         ";
      Src1CtrlEnum_PC_INCREMENT : decode_SRC1_CTRL_string = "PC_INCREMENT";
      Src1CtrlEnum_URS1 : decode_SRC1_CTRL_string = "URS1        ";
      default : decode_SRC1_CTRL_string = "????????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_SRC1_CTRL)
      Src1CtrlEnum_RS : _zz_decode_SRC1_CTRL_string = "RS          ";
      Src1CtrlEnum_IMU : _zz_decode_SRC1_CTRL_string = "IMU         ";
      Src1CtrlEnum_PC_INCREMENT : _zz_decode_SRC1_CTRL_string = "PC_INCREMENT";
      Src1CtrlEnum_URS1 : _zz_decode_SRC1_CTRL_string = "URS1        ";
      default : _zz_decode_SRC1_CTRL_string = "????????????";
    endcase
  end
  always @(*) begin
    case(execute_ALU_CTRL)
      AluCtrlEnum_ADD_SUB : execute_ALU_CTRL_string = "ADD_SUB ";
      AluCtrlEnum_SLT_SLTU : execute_ALU_CTRL_string = "SLT_SLTU";
      AluCtrlEnum_BITWISE : execute_ALU_CTRL_string = "BITWISE ";
      default : execute_ALU_CTRL_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_execute_ALU_CTRL)
      AluCtrlEnum_ADD_SUB : _zz_execute_ALU_CTRL_string = "ADD_SUB ";
      AluCtrlEnum_SLT_SLTU : _zz_execute_ALU_CTRL_string = "SLT_SLTU";
      AluCtrlEnum_BITWISE : _zz_execute_ALU_CTRL_string = "BITWISE ";
      default : _zz_execute_ALU_CTRL_string = "????????";
    endcase
  end
  always @(*) begin
    case(execute_ALU_BITWISE_CTRL)
      AluBitwiseCtrlEnum_XOR_1 : execute_ALU_BITWISE_CTRL_string = "XOR_1";
      AluBitwiseCtrlEnum_OR_1 : execute_ALU_BITWISE_CTRL_string = "OR_1 ";
      AluBitwiseCtrlEnum_AND_1 : execute_ALU_BITWISE_CTRL_string = "AND_1";
      default : execute_ALU_BITWISE_CTRL_string = "?????";
    endcase
  end
  always @(*) begin
    case(_zz_execute_ALU_BITWISE_CTRL)
      AluBitwiseCtrlEnum_XOR_1 : _zz_execute_ALU_BITWISE_CTRL_string = "XOR_1";
      AluBitwiseCtrlEnum_OR_1 : _zz_execute_ALU_BITWISE_CTRL_string = "OR_1 ";
      AluBitwiseCtrlEnum_AND_1 : _zz_execute_ALU_BITWISE_CTRL_string = "AND_1";
      default : _zz_execute_ALU_BITWISE_CTRL_string = "?????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_BRANCH_CTRL_1)
      BranchCtrlEnum_INC : _zz_decode_BRANCH_CTRL_1_string = "INC ";
      BranchCtrlEnum_B : _zz_decode_BRANCH_CTRL_1_string = "B   ";
      BranchCtrlEnum_JAL : _zz_decode_BRANCH_CTRL_1_string = "JAL ";
      BranchCtrlEnum_JALR : _zz_decode_BRANCH_CTRL_1_string = "JALR";
      default : _zz_decode_BRANCH_CTRL_1_string = "????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_SHIFT_CTRL_1)
      ShiftCtrlEnum_DISABLE_1 : _zz_decode_SHIFT_CTRL_1_string = "DISABLE_1";
      ShiftCtrlEnum_SLL_1 : _zz_decode_SHIFT_CTRL_1_string = "SLL_1    ";
      ShiftCtrlEnum_SRL_1 : _zz_decode_SHIFT_CTRL_1_string = "SRL_1    ";
      ShiftCtrlEnum_SRA_1 : _zz_decode_SHIFT_CTRL_1_string = "SRA_1    ";
      default : _zz_decode_SHIFT_CTRL_1_string = "?????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_FPU_OPCODE_1)
      FpuOpcode_LOAD : _zz_decode_FPU_OPCODE_1_string = "LOAD    ";
      FpuOpcode_STORE : _zz_decode_FPU_OPCODE_1_string = "STORE   ";
      FpuOpcode_MUL : _zz_decode_FPU_OPCODE_1_string = "MUL     ";
      FpuOpcode_ADD : _zz_decode_FPU_OPCODE_1_string = "ADD     ";
      FpuOpcode_FMA : _zz_decode_FPU_OPCODE_1_string = "FMA     ";
      FpuOpcode_I2F : _zz_decode_FPU_OPCODE_1_string = "I2F     ";
      FpuOpcode_F2I : _zz_decode_FPU_OPCODE_1_string = "F2I     ";
      FpuOpcode_CMP : _zz_decode_FPU_OPCODE_1_string = "CMP     ";
      FpuOpcode_DIV : _zz_decode_FPU_OPCODE_1_string = "DIV     ";
      FpuOpcode_SQRT : _zz_decode_FPU_OPCODE_1_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_decode_FPU_OPCODE_1_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_decode_FPU_OPCODE_1_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_decode_FPU_OPCODE_1_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_decode_FPU_OPCODE_1_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_decode_FPU_OPCODE_1_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_decode_FPU_OPCODE_1_string = "FCVT_X_X";
      default : _zz_decode_FPU_OPCODE_1_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_ALU_BITWISE_CTRL_1)
      AluBitwiseCtrlEnum_XOR_1 : _zz_decode_ALU_BITWISE_CTRL_1_string = "XOR_1";
      AluBitwiseCtrlEnum_OR_1 : _zz_decode_ALU_BITWISE_CTRL_1_string = "OR_1 ";
      AluBitwiseCtrlEnum_AND_1 : _zz_decode_ALU_BITWISE_CTRL_1_string = "AND_1";
      default : _zz_decode_ALU_BITWISE_CTRL_1_string = "?????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_ENV_CTRL_1)
      EnvCtrlEnum_NONE : _zz_decode_ENV_CTRL_1_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_decode_ENV_CTRL_1_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_decode_ENV_CTRL_1_string = "EBREAK";
      default : _zz_decode_ENV_CTRL_1_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_SRC2_CTRL_1)
      Src2CtrlEnum_RS : _zz_decode_SRC2_CTRL_1_string = "RS ";
      Src2CtrlEnum_IMI : _zz_decode_SRC2_CTRL_1_string = "IMI";
      Src2CtrlEnum_IMS : _zz_decode_SRC2_CTRL_1_string = "IMS";
      Src2CtrlEnum_PC : _zz_decode_SRC2_CTRL_1_string = "PC ";
      default : _zz_decode_SRC2_CTRL_1_string = "???";
    endcase
  end
  always @(*) begin
    case(_zz_decode_ALU_CTRL_1)
      AluCtrlEnum_ADD_SUB : _zz_decode_ALU_CTRL_1_string = "ADD_SUB ";
      AluCtrlEnum_SLT_SLTU : _zz_decode_ALU_CTRL_1_string = "SLT_SLTU";
      AluCtrlEnum_BITWISE : _zz_decode_ALU_CTRL_1_string = "BITWISE ";
      default : _zz_decode_ALU_CTRL_1_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_SRC1_CTRL_1)
      Src1CtrlEnum_RS : _zz_decode_SRC1_CTRL_1_string = "RS          ";
      Src1CtrlEnum_IMU : _zz_decode_SRC1_CTRL_1_string = "IMU         ";
      Src1CtrlEnum_PC_INCREMENT : _zz_decode_SRC1_CTRL_1_string = "PC_INCREMENT";
      Src1CtrlEnum_URS1 : _zz_decode_SRC1_CTRL_1_string = "URS1        ";
      default : _zz_decode_SRC1_CTRL_1_string = "????????????";
    endcase
  end
  always @(*) begin
    case(memory_ENV_CTRL)
      EnvCtrlEnum_NONE : memory_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : memory_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : memory_ENV_CTRL_string = "EBREAK";
      default : memory_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_memory_ENV_CTRL)
      EnvCtrlEnum_NONE : _zz_memory_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_memory_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_memory_ENV_CTRL_string = "EBREAK";
      default : _zz_memory_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(execute_ENV_CTRL)
      EnvCtrlEnum_NONE : execute_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : execute_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : execute_ENV_CTRL_string = "EBREAK";
      default : execute_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_execute_ENV_CTRL)
      EnvCtrlEnum_NONE : _zz_execute_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_execute_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_execute_ENV_CTRL_string = "EBREAK";
      default : _zz_execute_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(writeBack_ENV_CTRL)
      EnvCtrlEnum_NONE : writeBack_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : writeBack_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : writeBack_ENV_CTRL_string = "EBREAK";
      default : writeBack_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_writeBack_ENV_CTRL)
      EnvCtrlEnum_NONE : _zz_writeBack_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_writeBack_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_writeBack_ENV_CTRL_string = "EBREAK";
      default : _zz_writeBack_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(debugBus_dmToHart_payload_op)
      DebugDmToHartOp_DATA : debugBus_dmToHart_payload_op_string = "DATA     ";
      DebugDmToHartOp_EXECUTE : debugBus_dmToHart_payload_op_string = "EXECUTE  ";
      DebugDmToHartOp_REG_WRITE : debugBus_dmToHart_payload_op_string = "REG_WRITE";
      DebugDmToHartOp_REG_READ : debugBus_dmToHart_payload_op_string = "REG_READ ";
      default : debugBus_dmToHart_payload_op_string = "?????????";
    endcase
  end
  always @(*) begin
    case(FpuPlugin_port_cmd_payload_opcode)
      FpuOpcode_LOAD : FpuPlugin_port_cmd_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : FpuPlugin_port_cmd_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : FpuPlugin_port_cmd_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : FpuPlugin_port_cmd_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : FpuPlugin_port_cmd_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : FpuPlugin_port_cmd_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : FpuPlugin_port_cmd_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : FpuPlugin_port_cmd_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : FpuPlugin_port_cmd_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : FpuPlugin_port_cmd_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : FpuPlugin_port_cmd_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : FpuPlugin_port_cmd_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : FpuPlugin_port_cmd_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : FpuPlugin_port_cmd_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : FpuPlugin_port_cmd_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : FpuPlugin_port_cmd_payload_opcode_string = "FCVT_X_X";
      default : FpuPlugin_port_cmd_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(FpuPlugin_port_cmd_payload_format)
      FpuFormat_FLOAT : FpuPlugin_port_cmd_payload_format_string = "FLOAT ";
      FpuFormat_DOUBLE : FpuPlugin_port_cmd_payload_format_string = "DOUBLE";
      default : FpuPlugin_port_cmd_payload_format_string = "??????";
    endcase
  end
  always @(*) begin
    case(FpuPlugin_port_cmd_payload_roundMode)
      FpuRoundMode_RNE : FpuPlugin_port_cmd_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : FpuPlugin_port_cmd_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : FpuPlugin_port_cmd_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : FpuPlugin_port_cmd_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : FpuPlugin_port_cmd_payload_roundMode_string = "RMM";
      default : FpuPlugin_port_cmd_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(FpuPlugin_port_commit_payload_opcode)
      FpuOpcode_LOAD : FpuPlugin_port_commit_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : FpuPlugin_port_commit_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : FpuPlugin_port_commit_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : FpuPlugin_port_commit_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : FpuPlugin_port_commit_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : FpuPlugin_port_commit_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : FpuPlugin_port_commit_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : FpuPlugin_port_commit_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : FpuPlugin_port_commit_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : FpuPlugin_port_commit_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : FpuPlugin_port_commit_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : FpuPlugin_port_commit_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : FpuPlugin_port_commit_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : FpuPlugin_port_commit_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : FpuPlugin_port_commit_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : FpuPlugin_port_commit_payload_opcode_string = "FCVT_X_X";
      default : FpuPlugin_port_commit_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(CsrPlugin_inject_cmd_payload_op)
      DebugDmToHartOp_DATA : CsrPlugin_inject_cmd_payload_op_string = "DATA     ";
      DebugDmToHartOp_EXECUTE : CsrPlugin_inject_cmd_payload_op_string = "EXECUTE  ";
      DebugDmToHartOp_REG_WRITE : CsrPlugin_inject_cmd_payload_op_string = "REG_WRITE";
      DebugDmToHartOp_REG_READ : CsrPlugin_inject_cmd_payload_op_string = "REG_READ ";
      default : CsrPlugin_inject_cmd_payload_op_string = "?????????";
    endcase
  end
  always @(*) begin
    case(CsrPlugin_inject_cmd_toStream_payload_op)
      DebugDmToHartOp_DATA : CsrPlugin_inject_cmd_toStream_payload_op_string = "DATA     ";
      DebugDmToHartOp_EXECUTE : CsrPlugin_inject_cmd_toStream_payload_op_string = "EXECUTE  ";
      DebugDmToHartOp_REG_WRITE : CsrPlugin_inject_cmd_toStream_payload_op_string = "REG_WRITE";
      DebugDmToHartOp_REG_READ : CsrPlugin_inject_cmd_toStream_payload_op_string = "REG_READ ";
      default : CsrPlugin_inject_cmd_toStream_payload_op_string = "?????????";
    endcase
  end
  always @(*) begin
    case(CsrPlugin_inject_buffer_payload_op)
      DebugDmToHartOp_DATA : CsrPlugin_inject_buffer_payload_op_string = "DATA     ";
      DebugDmToHartOp_EXECUTE : CsrPlugin_inject_buffer_payload_op_string = "EXECUTE  ";
      DebugDmToHartOp_REG_WRITE : CsrPlugin_inject_buffer_payload_op_string = "REG_WRITE";
      DebugDmToHartOp_REG_READ : CsrPlugin_inject_buffer_payload_op_string = "REG_READ ";
      default : CsrPlugin_inject_buffer_payload_op_string = "?????????";
    endcase
  end
  always @(*) begin
    case(CsrPlugin_inject_cmd_toStream_rData_op)
      DebugDmToHartOp_DATA : CsrPlugin_inject_cmd_toStream_rData_op_string = "DATA     ";
      DebugDmToHartOp_EXECUTE : CsrPlugin_inject_cmd_toStream_rData_op_string = "EXECUTE  ";
      DebugDmToHartOp_REG_WRITE : CsrPlugin_inject_cmd_toStream_rData_op_string = "REG_WRITE";
      DebugDmToHartOp_REG_READ : CsrPlugin_inject_cmd_toStream_rData_op_string = "REG_READ ";
      default : CsrPlugin_inject_cmd_toStream_rData_op_string = "?????????";
    endcase
  end
  always @(*) begin
    case(CsrPlugin_dcsr_stepLogic_stateReg)
      CsrPlugin_dcsr_stepLogic_enumDef_BOOT : CsrPlugin_dcsr_stepLogic_stateReg_string = "BOOT  ";
      CsrPlugin_dcsr_stepLogic_enumDef_IDLE : CsrPlugin_dcsr_stepLogic_stateReg_string = "IDLE  ";
      CsrPlugin_dcsr_stepLogic_enumDef_SINGLE : CsrPlugin_dcsr_stepLogic_stateReg_string = "SINGLE";
      CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1 : CsrPlugin_dcsr_stepLogic_stateReg_string = "WAIT_1";
      default : CsrPlugin_dcsr_stepLogic_stateReg_string = "??????";
    endcase
  end
  always @(*) begin
    case(CsrPlugin_dcsr_stepLogic_stateNext)
      CsrPlugin_dcsr_stepLogic_enumDef_BOOT : CsrPlugin_dcsr_stepLogic_stateNext_string = "BOOT  ";
      CsrPlugin_dcsr_stepLogic_enumDef_IDLE : CsrPlugin_dcsr_stepLogic_stateNext_string = "IDLE  ";
      CsrPlugin_dcsr_stepLogic_enumDef_SINGLE : CsrPlugin_dcsr_stepLogic_stateNext_string = "SINGLE";
      CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1 : CsrPlugin_dcsr_stepLogic_stateNext_string = "WAIT_1";
      default : CsrPlugin_dcsr_stepLogic_stateNext_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_SRC1_CTRL_2)
      Src1CtrlEnum_RS : _zz_decode_SRC1_CTRL_2_string = "RS          ";
      Src1CtrlEnum_IMU : _zz_decode_SRC1_CTRL_2_string = "IMU         ";
      Src1CtrlEnum_PC_INCREMENT : _zz_decode_SRC1_CTRL_2_string = "PC_INCREMENT";
      Src1CtrlEnum_URS1 : _zz_decode_SRC1_CTRL_2_string = "URS1        ";
      default : _zz_decode_SRC1_CTRL_2_string = "????????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_ALU_CTRL_2)
      AluCtrlEnum_ADD_SUB : _zz_decode_ALU_CTRL_2_string = "ADD_SUB ";
      AluCtrlEnum_SLT_SLTU : _zz_decode_ALU_CTRL_2_string = "SLT_SLTU";
      AluCtrlEnum_BITWISE : _zz_decode_ALU_CTRL_2_string = "BITWISE ";
      default : _zz_decode_ALU_CTRL_2_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_SRC2_CTRL_2)
      Src2CtrlEnum_RS : _zz_decode_SRC2_CTRL_2_string = "RS ";
      Src2CtrlEnum_IMI : _zz_decode_SRC2_CTRL_2_string = "IMI";
      Src2CtrlEnum_IMS : _zz_decode_SRC2_CTRL_2_string = "IMS";
      Src2CtrlEnum_PC : _zz_decode_SRC2_CTRL_2_string = "PC ";
      default : _zz_decode_SRC2_CTRL_2_string = "???";
    endcase
  end
  always @(*) begin
    case(_zz_decode_ENV_CTRL_2)
      EnvCtrlEnum_NONE : _zz_decode_ENV_CTRL_2_string = "NONE  ";
      EnvCtrlEnum_XRET : _zz_decode_ENV_CTRL_2_string = "XRET  ";
      EnvCtrlEnum_EBREAK : _zz_decode_ENV_CTRL_2_string = "EBREAK";
      default : _zz_decode_ENV_CTRL_2_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_ALU_BITWISE_CTRL_2)
      AluBitwiseCtrlEnum_XOR_1 : _zz_decode_ALU_BITWISE_CTRL_2_string = "XOR_1";
      AluBitwiseCtrlEnum_OR_1 : _zz_decode_ALU_BITWISE_CTRL_2_string = "OR_1 ";
      AluBitwiseCtrlEnum_AND_1 : _zz_decode_ALU_BITWISE_CTRL_2_string = "AND_1";
      default : _zz_decode_ALU_BITWISE_CTRL_2_string = "?????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_FPU_OPCODE_2)
      FpuOpcode_LOAD : _zz_decode_FPU_OPCODE_2_string = "LOAD    ";
      FpuOpcode_STORE : _zz_decode_FPU_OPCODE_2_string = "STORE   ";
      FpuOpcode_MUL : _zz_decode_FPU_OPCODE_2_string = "MUL     ";
      FpuOpcode_ADD : _zz_decode_FPU_OPCODE_2_string = "ADD     ";
      FpuOpcode_FMA : _zz_decode_FPU_OPCODE_2_string = "FMA     ";
      FpuOpcode_I2F : _zz_decode_FPU_OPCODE_2_string = "I2F     ";
      FpuOpcode_F2I : _zz_decode_FPU_OPCODE_2_string = "F2I     ";
      FpuOpcode_CMP : _zz_decode_FPU_OPCODE_2_string = "CMP     ";
      FpuOpcode_DIV : _zz_decode_FPU_OPCODE_2_string = "DIV     ";
      FpuOpcode_SQRT : _zz_decode_FPU_OPCODE_2_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_decode_FPU_OPCODE_2_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_decode_FPU_OPCODE_2_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_decode_FPU_OPCODE_2_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_decode_FPU_OPCODE_2_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_decode_FPU_OPCODE_2_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_decode_FPU_OPCODE_2_string = "FCVT_X_X";
      default : _zz_decode_FPU_OPCODE_2_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_SHIFT_CTRL_2)
      ShiftCtrlEnum_DISABLE_1 : _zz_decode_SHIFT_CTRL_2_string = "DISABLE_1";
      ShiftCtrlEnum_SLL_1 : _zz_decode_SHIFT_CTRL_2_string = "SLL_1    ";
      ShiftCtrlEnum_SRL_1 : _zz_decode_SHIFT_CTRL_2_string = "SRL_1    ";
      ShiftCtrlEnum_SRA_1 : _zz_decode_SHIFT_CTRL_2_string = "SRA_1    ";
      default : _zz_decode_SHIFT_CTRL_2_string = "?????????";
    endcase
  end
  always @(*) begin
    case(_zz_decode_BRANCH_CTRL_2)
      BranchCtrlEnum_INC : _zz_decode_BRANCH_CTRL_2_string = "INC ";
      BranchCtrlEnum_B : _zz_decode_BRANCH_CTRL_2_string = "B   ";
      BranchCtrlEnum_JAL : _zz_decode_BRANCH_CTRL_2_string = "JAL ";
      BranchCtrlEnum_JALR : _zz_decode_BRANCH_CTRL_2_string = "JALR";
      default : _zz_decode_BRANCH_CTRL_2_string = "????";
    endcase
  end
  always @(*) begin
    case(_zz_FpuPlugin_port_cmd_payload_roundMode)
      FpuRoundMode_RNE : _zz_FpuPlugin_port_cmd_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : _zz_FpuPlugin_port_cmd_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : _zz_FpuPlugin_port_cmd_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : _zz_FpuPlugin_port_cmd_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : _zz_FpuPlugin_port_cmd_payload_roundMode_string = "RMM";
      default : _zz_FpuPlugin_port_cmd_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(_zz_FpuPlugin_port_cmd_payload_roundMode_1)
      FpuRoundMode_RNE : _zz_FpuPlugin_port_cmd_payload_roundMode_1_string = "RNE";
      FpuRoundMode_RTZ : _zz_FpuPlugin_port_cmd_payload_roundMode_1_string = "RTZ";
      FpuRoundMode_RDN : _zz_FpuPlugin_port_cmd_payload_roundMode_1_string = "RDN";
      FpuRoundMode_RUP : _zz_FpuPlugin_port_cmd_payload_roundMode_1_string = "RUP";
      FpuRoundMode_RMM : _zz_FpuPlugin_port_cmd_payload_roundMode_1_string = "RMM";
      default : _zz_FpuPlugin_port_cmd_payload_roundMode_1_string = "???";
    endcase
  end
  always @(*) begin
    case(writeBack_FpuPlugin_commit_payload_opcode)
      FpuOpcode_LOAD : writeBack_FpuPlugin_commit_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : writeBack_FpuPlugin_commit_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : writeBack_FpuPlugin_commit_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : writeBack_FpuPlugin_commit_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : writeBack_FpuPlugin_commit_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : writeBack_FpuPlugin_commit_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : writeBack_FpuPlugin_commit_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : writeBack_FpuPlugin_commit_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : writeBack_FpuPlugin_commit_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : writeBack_FpuPlugin_commit_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : writeBack_FpuPlugin_commit_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : writeBack_FpuPlugin_commit_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : writeBack_FpuPlugin_commit_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : writeBack_FpuPlugin_commit_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : writeBack_FpuPlugin_commit_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : writeBack_FpuPlugin_commit_payload_opcode_string = "FCVT_X_X";
      default : writeBack_FpuPlugin_commit_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(writeBack_FpuPlugin_commit_s2mPipe_payload_opcode)
      FpuOpcode_LOAD : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "FCVT_X_X";
      default : writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(writeBack_FpuPlugin_commit_rData_opcode)
      FpuOpcode_LOAD : writeBack_FpuPlugin_commit_rData_opcode_string = "LOAD    ";
      FpuOpcode_STORE : writeBack_FpuPlugin_commit_rData_opcode_string = "STORE   ";
      FpuOpcode_MUL : writeBack_FpuPlugin_commit_rData_opcode_string = "MUL     ";
      FpuOpcode_ADD : writeBack_FpuPlugin_commit_rData_opcode_string = "ADD     ";
      FpuOpcode_FMA : writeBack_FpuPlugin_commit_rData_opcode_string = "FMA     ";
      FpuOpcode_I2F : writeBack_FpuPlugin_commit_rData_opcode_string = "I2F     ";
      FpuOpcode_F2I : writeBack_FpuPlugin_commit_rData_opcode_string = "F2I     ";
      FpuOpcode_CMP : writeBack_FpuPlugin_commit_rData_opcode_string = "CMP     ";
      FpuOpcode_DIV : writeBack_FpuPlugin_commit_rData_opcode_string = "DIV     ";
      FpuOpcode_SQRT : writeBack_FpuPlugin_commit_rData_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : writeBack_FpuPlugin_commit_rData_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : writeBack_FpuPlugin_commit_rData_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : writeBack_FpuPlugin_commit_rData_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : writeBack_FpuPlugin_commit_rData_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : writeBack_FpuPlugin_commit_rData_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : writeBack_FpuPlugin_commit_rData_opcode_string = "FCVT_X_X";
      default : writeBack_FpuPlugin_commit_rData_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode)
      FpuOpcode_LOAD : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "FCVT_X_X";
      default : _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_op)
      DebugDmToHartOp_DATA : toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_op_string = "DATA     ";
      DebugDmToHartOp_EXECUTE : toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_op_string = "EXECUTE  ";
      DebugDmToHartOp_REG_WRITE : toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_op_string = "REG_WRITE";
      DebugDmToHartOp_REG_READ : toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_op_string = "REG_READ ";
      default : toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_op_string = "?????????";
    endcase
  end
  always @(*) begin
    case(decode_to_execute_ALU_CTRL)
      AluCtrlEnum_ADD_SUB : decode_to_execute_ALU_CTRL_string = "ADD_SUB ";
      AluCtrlEnum_SLT_SLTU : decode_to_execute_ALU_CTRL_string = "SLT_SLTU";
      AluCtrlEnum_BITWISE : decode_to_execute_ALU_CTRL_string = "BITWISE ";
      default : decode_to_execute_ALU_CTRL_string = "????????";
    endcase
  end
  always @(*) begin
    case(decode_to_execute_ENV_CTRL)
      EnvCtrlEnum_NONE : decode_to_execute_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : decode_to_execute_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : decode_to_execute_ENV_CTRL_string = "EBREAK";
      default : decode_to_execute_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(execute_to_memory_ENV_CTRL)
      EnvCtrlEnum_NONE : execute_to_memory_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : execute_to_memory_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : execute_to_memory_ENV_CTRL_string = "EBREAK";
      default : execute_to_memory_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(memory_to_writeBack_ENV_CTRL)
      EnvCtrlEnum_NONE : memory_to_writeBack_ENV_CTRL_string = "NONE  ";
      EnvCtrlEnum_XRET : memory_to_writeBack_ENV_CTRL_string = "XRET  ";
      EnvCtrlEnum_EBREAK : memory_to_writeBack_ENV_CTRL_string = "EBREAK";
      default : memory_to_writeBack_ENV_CTRL_string = "??????";
    endcase
  end
  always @(*) begin
    case(decode_to_execute_ALU_BITWISE_CTRL)
      AluBitwiseCtrlEnum_XOR_1 : decode_to_execute_ALU_BITWISE_CTRL_string = "XOR_1";
      AluBitwiseCtrlEnum_OR_1 : decode_to_execute_ALU_BITWISE_CTRL_string = "OR_1 ";
      AluBitwiseCtrlEnum_AND_1 : decode_to_execute_ALU_BITWISE_CTRL_string = "AND_1";
      default : decode_to_execute_ALU_BITWISE_CTRL_string = "?????";
    endcase
  end
  always @(*) begin
    case(decode_to_execute_FPU_OPCODE)
      FpuOpcode_LOAD : decode_to_execute_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : decode_to_execute_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : decode_to_execute_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : decode_to_execute_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : decode_to_execute_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : decode_to_execute_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : decode_to_execute_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : decode_to_execute_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : decode_to_execute_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : decode_to_execute_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : decode_to_execute_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : decode_to_execute_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : decode_to_execute_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : decode_to_execute_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : decode_to_execute_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : decode_to_execute_FPU_OPCODE_string = "FCVT_X_X";
      default : decode_to_execute_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(execute_to_memory_FPU_OPCODE)
      FpuOpcode_LOAD : execute_to_memory_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : execute_to_memory_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : execute_to_memory_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : execute_to_memory_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : execute_to_memory_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : execute_to_memory_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : execute_to_memory_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : execute_to_memory_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : execute_to_memory_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : execute_to_memory_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : execute_to_memory_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : execute_to_memory_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : execute_to_memory_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : execute_to_memory_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : execute_to_memory_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : execute_to_memory_FPU_OPCODE_string = "FCVT_X_X";
      default : execute_to_memory_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(memory_to_writeBack_FPU_OPCODE)
      FpuOpcode_LOAD : memory_to_writeBack_FPU_OPCODE_string = "LOAD    ";
      FpuOpcode_STORE : memory_to_writeBack_FPU_OPCODE_string = "STORE   ";
      FpuOpcode_MUL : memory_to_writeBack_FPU_OPCODE_string = "MUL     ";
      FpuOpcode_ADD : memory_to_writeBack_FPU_OPCODE_string = "ADD     ";
      FpuOpcode_FMA : memory_to_writeBack_FPU_OPCODE_string = "FMA     ";
      FpuOpcode_I2F : memory_to_writeBack_FPU_OPCODE_string = "I2F     ";
      FpuOpcode_F2I : memory_to_writeBack_FPU_OPCODE_string = "F2I     ";
      FpuOpcode_CMP : memory_to_writeBack_FPU_OPCODE_string = "CMP     ";
      FpuOpcode_DIV : memory_to_writeBack_FPU_OPCODE_string = "DIV     ";
      FpuOpcode_SQRT : memory_to_writeBack_FPU_OPCODE_string = "SQRT    ";
      FpuOpcode_MIN_MAX : memory_to_writeBack_FPU_OPCODE_string = "MIN_MAX ";
      FpuOpcode_SGNJ : memory_to_writeBack_FPU_OPCODE_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : memory_to_writeBack_FPU_OPCODE_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : memory_to_writeBack_FPU_OPCODE_string = "FMV_W_X ";
      FpuOpcode_FCLASS : memory_to_writeBack_FPU_OPCODE_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : memory_to_writeBack_FPU_OPCODE_string = "FCVT_X_X";
      default : memory_to_writeBack_FPU_OPCODE_string = "????????";
    endcase
  end
  always @(*) begin
    case(decode_to_execute_SHIFT_CTRL)
      ShiftCtrlEnum_DISABLE_1 : decode_to_execute_SHIFT_CTRL_string = "DISABLE_1";
      ShiftCtrlEnum_SLL_1 : decode_to_execute_SHIFT_CTRL_string = "SLL_1    ";
      ShiftCtrlEnum_SRL_1 : decode_to_execute_SHIFT_CTRL_string = "SRL_1    ";
      ShiftCtrlEnum_SRA_1 : decode_to_execute_SHIFT_CTRL_string = "SRA_1    ";
      default : decode_to_execute_SHIFT_CTRL_string = "?????????";
    endcase
  end
  always @(*) begin
    case(decode_to_execute_BRANCH_CTRL)
      BranchCtrlEnum_INC : decode_to_execute_BRANCH_CTRL_string = "INC ";
      BranchCtrlEnum_B : decode_to_execute_BRANCH_CTRL_string = "B   ";
      BranchCtrlEnum_JAL : decode_to_execute_BRANCH_CTRL_string = "JAL ";
      BranchCtrlEnum_JALR : decode_to_execute_BRANCH_CTRL_string = "JALR";
      default : decode_to_execute_BRANCH_CTRL_string = "????";
    endcase
  end
  `endif

  always @(*) begin
    CsrPlugin_running_aheadValue = CsrPlugin_running;
    if(CsrPlugin_trigger_decodeBreak_enabled) begin
      if(CsrPlugin_trigger_decodeBreak_timeout_state) begin
        CsrPlugin_running_aheadValue = 1'b0;
      end
    end
    if(when_CsrPlugin_l1390) begin
      if(!when_CsrPlugin_l1398) begin
        CsrPlugin_running_aheadValue = 1'b0;
      end
    end
    if(CsrPlugin_doResume) begin
      CsrPlugin_running_aheadValue = 1'b1;
    end
  end

  assign writeBack_MEMORY_LOAD_DATA = writeBack_DBusCachedPlugin_rspShifted;
  assign memory_MUL_LOW = ($signed(_zz_memory_MUL_LOW) + $signed(_zz_memory_MUL_LOW_6));
  assign execute_BRANCH_CALC = {execute_BranchPlugin_branchAdder[31 : 1],1'b0};
  assign execute_BRANCH_DO = _zz_execute_BRANCH_DO_1;
  assign memory_MUL_HH = execute_to_memory_MUL_HH;
  assign execute_MUL_HH = ($signed(execute_MulPlugin_aHigh) * $signed(execute_MulPlugin_bHigh));
  assign execute_MUL_HL = ($signed(execute_MulPlugin_aHigh) * $signed(execute_MulPlugin_bSLow));
  assign execute_MUL_LH = ($signed(execute_MulPlugin_aSLow) * $signed(execute_MulPlugin_bHigh));
  assign execute_MUL_LL = (execute_MulPlugin_aULow * execute_MulPlugin_bULow);
  assign execute_REGFILE_WRITE_DATA = _zz_execute_REGFILE_WRITE_DATA;
  assign memory_MEMORY_STORE_DATA_RF = execute_to_memory_MEMORY_STORE_DATA_RF;
  assign execute_MEMORY_STORE_DATA_RF = _zz_execute_MEMORY_STORE_DATA_RF;
  assign memory_FPU_COMMIT_LOAD = execute_to_memory_FPU_COMMIT_LOAD;
  assign execute_FPU_COMMIT_LOAD = decode_to_execute_FPU_COMMIT_LOAD;
  assign decode_FPU_COMMIT_LOAD = (decode_FPU_OPCODE == FpuOpcode_LOAD);
  assign memory_FPU_FORKED = execute_to_memory_FPU_FORKED;
  assign execute_FPU_FORKED = decode_to_execute_FPU_FORKED;
  assign decode_FPU_FORKED = (decode_FpuPlugin_forked || (FpuPlugin_port_cmd_fire && (! _zz_decode_FPU_FORKED)));
  assign decode_SRC2 = _zz_decode_SRC2_4;
  assign decode_SRC1 = _zz_decode_SRC1;
  assign decode_SRC2_FORCE_ZERO = (decode_SRC_ADD_ZERO && (! decode_SRC_USE_SUB_LESS));
  assign decode_RS2 = decode_RegFilePlugin_rs2Data;
  assign memory_RS1 = execute_to_memory_RS1;
  assign decode_RS1 = decode_RegFilePlugin_rs1Data;
  assign decode_BRANCH_CTRL = _zz_decode_BRANCH_CTRL;
  assign _zz_decode_to_execute_BRANCH_CTRL = _zz_decode_to_execute_BRANCH_CTRL_1;
  assign decode_SHIFT_CTRL = _zz_decode_SHIFT_CTRL;
  assign _zz_decode_to_execute_SHIFT_CTRL = _zz_decode_to_execute_SHIFT_CTRL_1;
  assign memory_FPU_OPCODE = _zz_memory_FPU_OPCODE;
  assign _zz_memory_to_writeBack_FPU_OPCODE = _zz_memory_to_writeBack_FPU_OPCODE_1;
  assign execute_FPU_OPCODE = _zz_execute_FPU_OPCODE;
  assign _zz_execute_to_memory_FPU_OPCODE = _zz_execute_to_memory_FPU_OPCODE_1;
  assign _zz_decode_to_execute_FPU_OPCODE = _zz_decode_to_execute_FPU_OPCODE_1;
  assign memory_FPU_RSP = execute_to_memory_FPU_RSP;
  assign execute_FPU_RSP = decode_to_execute_FPU_RSP;
  assign decode_FPU_RSP = _zz_decode_FPU_RSP[29];
  assign memory_FPU_COMMIT = execute_to_memory_FPU_COMMIT;
  assign execute_FPU_COMMIT = decode_to_execute_FPU_COMMIT;
  assign decode_FPU_COMMIT = _zz_decode_FPU_RSP[28];
  assign decode_IS_RS2_SIGNED = _zz_decode_FPU_RSP[26];
  assign decode_IS_RS1_SIGNED = _zz_decode_FPU_RSP[25];
  assign decode_IS_DIV = _zz_decode_FPU_RSP[24];
  assign memory_IS_MUL = execute_to_memory_IS_MUL;
  assign execute_IS_MUL = decode_to_execute_IS_MUL;
  assign decode_IS_MUL = _zz_decode_FPU_RSP[23];
  assign decode_ALU_BITWISE_CTRL = _zz_decode_ALU_BITWISE_CTRL;
  assign _zz_decode_to_execute_ALU_BITWISE_CTRL = _zz_decode_to_execute_ALU_BITWISE_CTRL_1;
  assign decode_SRC_LESS_UNSIGNED = _zz_decode_FPU_RSP[19];
  assign _zz_memory_to_writeBack_ENV_CTRL = _zz_memory_to_writeBack_ENV_CTRL_1;
  assign _zz_execute_to_memory_ENV_CTRL = _zz_execute_to_memory_ENV_CTRL_1;
  assign decode_ENV_CTRL = _zz_decode_ENV_CTRL;
  assign _zz_decode_to_execute_ENV_CTRL = _zz_decode_to_execute_ENV_CTRL_1;
  assign decode_IS_CSR = _zz_decode_FPU_RSP[16];
  assign decode_MEMORY_MANAGMENT = _zz_decode_FPU_RSP[15];
  assign memory_MEMORY_WR = execute_to_memory_MEMORY_WR;
  assign decode_MEMORY_WR = _zz_decode_FPU_RSP[12];
  assign execute_BYPASSABLE_MEMORY_STAGE = decode_to_execute_BYPASSABLE_MEMORY_STAGE;
  assign decode_BYPASSABLE_MEMORY_STAGE = _zz_decode_FPU_RSP[11];
  assign decode_BYPASSABLE_EXECUTE_STAGE = _zz_decode_FPU_RSP[10];
  assign decode_ALU_CTRL = _zz_decode_ALU_CTRL;
  assign _zz_decode_to_execute_ALU_CTRL = _zz_decode_to_execute_ALU_CTRL_1;
  assign decode_CSR_READ_OPCODE = (decode_INSTRUCTION[13 : 7] != 7'h20);
  assign decode_CSR_WRITE_OPCODE = (! (((decode_INSTRUCTION[14 : 13] == 2'b01) && (decode_INSTRUCTION[19 : 15] == 5'h0)) || ((decode_INSTRUCTION[14 : 13] == 2'b11) && (decode_INSTRUCTION[19 : 15] == 5'h0))));
  assign decode_MEMORY_FORCE_CONSTISTENCY = 1'b0;
  assign writeBack_FORMAL_PC_NEXT = memory_to_writeBack_FORMAL_PC_NEXT;
  assign memory_FORMAL_PC_NEXT = execute_to_memory_FORMAL_PC_NEXT;
  assign execute_FORMAL_PC_NEXT = decode_to_execute_FORMAL_PC_NEXT;
  assign decode_FORMAL_PC_NEXT = (decode_PC + _zz_decode_FORMAL_PC_NEXT);
  assign memory_PC = execute_to_memory_PC;
  assign memory_BRANCH_CALC = execute_to_memory_BRANCH_CALC;
  assign memory_BRANCH_DO = execute_to_memory_BRANCH_DO;
  assign execute_PC = decode_to_execute_PC;
  assign execute_BRANCH_CTRL = _zz_execute_BRANCH_CTRL;
  assign decode_RS2_USE = _zz_decode_FPU_RSP[14];
  assign decode_RS1_USE = _zz_decode_FPU_RSP[4];
  assign execute_REGFILE_WRITE_VALID = decode_to_execute_REGFILE_WRITE_VALID;
  assign execute_BYPASSABLE_EXECUTE_STAGE = decode_to_execute_BYPASSABLE_EXECUTE_STAGE;
  assign memory_REGFILE_WRITE_VALID = execute_to_memory_REGFILE_WRITE_VALID;
  assign memory_BYPASSABLE_MEMORY_STAGE = execute_to_memory_BYPASSABLE_MEMORY_STAGE;
  assign writeBack_REGFILE_WRITE_VALID = memory_to_writeBack_REGFILE_WRITE_VALID;
  assign execute_SHIFT_CTRL = _zz_execute_SHIFT_CTRL;
  always @(*) begin
    _zz_memory_to_writeBack_FPU_FORKED = memory_FPU_FORKED;
    if(memory_arbitration_isStuck) begin
      _zz_memory_to_writeBack_FPU_FORKED = 1'b0;
    end
  end

  always @(*) begin
    _zz_execute_to_memory_FPU_FORKED = execute_FPU_FORKED;
    if(execute_arbitration_isStuck) begin
      _zz_execute_to_memory_FPU_FORKED = 1'b0;
    end
  end

  always @(*) begin
    _zz_decode_to_execute_FPU_FORKED = decode_FPU_FORKED;
    if(decode_arbitration_isStuck) begin
      _zz_decode_to_execute_FPU_FORKED = 1'b0;
    end
  end

  assign writeBack_FPU_OPCODE = _zz_writeBack_FPU_OPCODE;
  assign writeBack_RS1 = memory_to_writeBack_RS1;
  assign writeBack_FPU_COMMIT_LOAD = memory_to_writeBack_FPU_COMMIT_LOAD;
  always @(*) begin
    DBusBypass0_cond = 1'b0;
    if(writeBack_FpuPlugin_isRsp) begin
      if(writeBack_arbitration_isValid) begin
        DBusBypass0_cond = 1'b1;
      end
    end
  end

  assign writeBack_FPU_COMMIT = memory_to_writeBack_FPU_COMMIT;
  assign writeBack_FPU_RSP = memory_to_writeBack_FPU_RSP;
  assign writeBack_FPU_FORKED = memory_to_writeBack_FPU_FORKED;
  assign decode_FPU_ARG = _zz_decode_FPU_RSP[36 : 35];
  assign decode_FPU_OPCODE = _zz_decode_FPU_OPCODE;
  always @(*) begin
    decode_FPU_ENABLE = _zz_decode_FPU_ENABLE;
    if(when_FpuPlugin_l272) begin
      decode_FPU_ENABLE = 1'b0;
    end
  end

  assign execute_IS_RS1_SIGNED = decode_to_execute_IS_RS1_SIGNED;
  assign execute_IS_DIV = decode_to_execute_IS_DIV;
  assign execute_IS_RS2_SIGNED = decode_to_execute_IS_RS2_SIGNED;
  always @(*) begin
    _zz_memory_to_writeBack_REGFILE_WRITE_DATA = memory_REGFILE_WRITE_DATA;
    if(when_MulDivIterativePlugin_l128) begin
      _zz_memory_to_writeBack_REGFILE_WRITE_DATA = memory_DivPlugin_div_result;
    end
  end

  assign memory_INSTRUCTION = execute_to_memory_INSTRUCTION;
  assign memory_IS_DIV = execute_to_memory_IS_DIV;
  assign writeBack_IS_MUL = memory_to_writeBack_IS_MUL;
  assign writeBack_MUL_HH = memory_to_writeBack_MUL_HH;
  assign writeBack_MUL_LOW = memory_to_writeBack_MUL_LOW;
  assign memory_MUL_HL = execute_to_memory_MUL_HL;
  assign memory_MUL_LH = execute_to_memory_MUL_LH;
  assign memory_MUL_LL = execute_to_memory_MUL_LL;
  assign execute_SRC_LESS_UNSIGNED = decode_to_execute_SRC_LESS_UNSIGNED;
  assign execute_SRC2_FORCE_ZERO = decode_to_execute_SRC2_FORCE_ZERO;
  assign execute_SRC_USE_SUB_LESS = decode_to_execute_SRC_USE_SUB_LESS;
  assign _zz_decode_to_execute_PC = decode_PC;
  assign _zz_decode_to_execute_RS2 = decode_RS2;
  assign decode_SRC2_CTRL = _zz_decode_SRC2_CTRL;
  assign _zz_decode_to_execute_RS1 = decode_RS1;
  assign decode_SRC1_CTRL = _zz_decode_SRC1_CTRL;
  assign decode_SRC_USE_SUB_LESS = _zz_decode_FPU_RSP[2];
  assign decode_SRC_ADD_ZERO = _zz_decode_FPU_RSP[22];
  assign execute_SRC_ADD_SUB = execute_SrcPlugin_addSub;
  assign execute_SRC_LESS = execute_SrcPlugin_less;
  assign execute_ALU_CTRL = _zz_execute_ALU_CTRL;
  assign execute_SRC2 = decode_to_execute_SRC2;
  assign execute_ALU_BITWISE_CTRL = _zz_execute_ALU_BITWISE_CTRL;
  assign _zz_lastStageRegFileWrite_payload_address = writeBack_INSTRUCTION;
  assign _zz_lastStageRegFileWrite_valid = writeBack_REGFILE_WRITE_VALID;
  always @(*) begin
    _zz_1 = 1'b0;
    if(lastStageRegFileWrite_valid) begin
      _zz_1 = 1'b1;
    end
  end

  assign decode_INSTRUCTION_ANTICIPATED = (decode_arbitration_isStuck ? decode_INSTRUCTION : IBusSimplePlugin_decompressor_output_payload_rsp_inst);
  always @(*) begin
    decode_REGFILE_WRITE_VALID = _zz_decode_FPU_RSP[9];
    if(when_RegFilePlugin_l63) begin
      decode_REGFILE_WRITE_VALID = 1'b0;
    end
  end

  always @(*) begin
    decode_LEGAL_INSTRUCTION = (|{((decode_INSTRUCTION & 32'h0000005f) == 32'h00000017),{((decode_INSTRUCTION & 32'h0000007f) == 32'h0000006f),{((decode_INSTRUCTION & _zz_decode_LEGAL_INSTRUCTION) == 32'h00000043),{(_zz_decode_LEGAL_INSTRUCTION_1 == _zz_decode_LEGAL_INSTRUCTION_2),{_zz_decode_LEGAL_INSTRUCTION_3,{_zz_decode_LEGAL_INSTRUCTION_4,_zz_decode_LEGAL_INSTRUCTION_5}}}}}});
    if(decode_FpuPlugin_trap) begin
      decode_LEGAL_INSTRUCTION = 1'b0;
    end
  end

  always @(*) begin
    _zz_execute_to_memory_REGFILE_WRITE_DATA = execute_REGFILE_WRITE_DATA;
    if(when_CsrPlugin_l1587) begin
      _zz_execute_to_memory_REGFILE_WRITE_DATA = CsrPlugin_csrMapping_readDataSignal;
    end
    if(when_ShiftPlugins_l169) begin
      _zz_execute_to_memory_REGFILE_WRITE_DATA = _zz_execute_to_memory_REGFILE_WRITE_DATA_1;
    end
  end

  assign execute_CSR_READ_OPCODE = decode_to_execute_CSR_READ_OPCODE;
  assign execute_CSR_WRITE_OPCODE = decode_to_execute_CSR_WRITE_OPCODE;
  assign execute_IS_CSR = decode_to_execute_IS_CSR;
  assign memory_ENV_CTRL = _zz_memory_ENV_CTRL;
  assign execute_ENV_CTRL = _zz_execute_ENV_CTRL;
  assign writeBack_ENV_CTRL = _zz_writeBack_ENV_CTRL;
  assign execute_SRC1 = decode_to_execute_SRC1;
  always @(*) begin
    _zz_lastStageRegFileWrite_payload_data = writeBack_REGFILE_WRITE_DATA;
    if(when_DBusCachedPlugin_l581) begin
      _zz_lastStageRegFileWrite_payload_data = writeBack_DBusCachedPlugin_rspFormated;
    end
    if(when_MulPlugin_l147) begin
      case(switch_MulPlugin_l148)
        2'b00 : begin
          _zz_lastStageRegFileWrite_payload_data = _zz__zz_lastStageRegFileWrite_payload_data;
        end
        default : begin
          _zz_lastStageRegFileWrite_payload_data = _zz__zz_lastStageRegFileWrite_payload_data_1;
        end
      endcase
    end
    if(writeBack_FpuPlugin_isRsp) begin
      if(writeBack_arbitration_isValid) begin
        _zz_lastStageRegFileWrite_payload_data = FpuPlugin_port_rsp_payload_value[31 : 0];
      end
    end
  end

  assign writeBack_MEMORY_WR = memory_to_writeBack_MEMORY_WR;
  assign writeBack_MEMORY_STORE_DATA_RF = memory_to_writeBack_MEMORY_STORE_DATA_RF;
  assign writeBack_REGFILE_WRITE_DATA = memory_to_writeBack_REGFILE_WRITE_DATA;
  assign writeBack_MEMORY_ENABLE = memory_to_writeBack_MEMORY_ENABLE;
  assign memory_REGFILE_WRITE_DATA = execute_to_memory_REGFILE_WRITE_DATA;
  assign memory_MEMORY_ENABLE = execute_to_memory_MEMORY_ENABLE;
  assign execute_MEMORY_FORCE_CONSTISTENCY = decode_to_execute_MEMORY_FORCE_CONSTISTENCY;
  assign execute_RS1 = decode_to_execute_RS1;
  assign execute_MEMORY_MANAGMENT = decode_to_execute_MEMORY_MANAGMENT;
  assign execute_RS2 = decode_to_execute_RS2;
  assign execute_MEMORY_WR = decode_to_execute_MEMORY_WR;
  assign execute_SRC_ADD = execute_SrcPlugin_addSub;
  assign execute_MEMORY_ENABLE = decode_to_execute_MEMORY_ENABLE;
  assign execute_INSTRUCTION = decode_to_execute_INSTRUCTION;
  assign decode_MEMORY_ENABLE = _zz_decode_FPU_RSP[3];
  always @(*) begin
    _zz_memory_to_writeBack_FORMAL_PC_NEXT = memory_FORMAL_PC_NEXT;
    if(BranchPlugin_jumpInterface_valid) begin
      _zz_memory_to_writeBack_FORMAL_PC_NEXT = BranchPlugin_jumpInterface_payload;
    end
  end

  assign decode_PC = IBusSimplePlugin_decodePc_pcReg;
  assign decode_INSTRUCTION = IBusSimplePlugin_injector_decodeInput_payload_rsp_inst;
  assign decode_IS_RVC = IBusSimplePlugin_injector_decodeInput_payload_isRvc;
  assign writeBack_PC = memory_to_writeBack_PC;
  assign writeBack_INSTRUCTION = memory_to_writeBack_INSTRUCTION;
  always @(*) begin
    decode_arbitration_haltItself = 1'b0;
    if(when_DBusCachedPlugin_l353) begin
      decode_arbitration_haltItself = 1'b1;
    end
    if(when_FpuPlugin_l273) begin
      decode_arbitration_haltItself = 1'b1;
    end
    if(FpuPlugin_port_cmd_isStall) begin
      decode_arbitration_haltItself = 1'b1;
    end
    case(IBusSimplePlugin_injector_port_state)
      3'b010 : begin
        decode_arbitration_haltItself = 1'b1;
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    decode_arbitration_haltByOther = 1'b0;
    case(CsrPlugin_dcsr_stepLogic_stateReg)
      CsrPlugin_dcsr_stepLogic_enumDef_IDLE : begin
      end
      CsrPlugin_dcsr_stepLogic_enumDef_SINGLE : begin
      end
      CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1 : begin
        if(decode_arbitration_isValid) begin
          decode_arbitration_haltByOther = 1'b1;
        end
      end
      default : begin
      end
    endcase
    if(CsrPlugin_trigger_decodeBreak_enabled) begin
      decode_arbitration_haltByOther = 1'b1;
    end
    if(CsrPlugin_pipelineLiberator_active) begin
      decode_arbitration_haltByOther = 1'b1;
    end
    if(when_CsrPlugin_l1527) begin
      decode_arbitration_haltByOther = 1'b1;
    end
    if(when_HazardSimplePlugin_l113) begin
      decode_arbitration_haltByOther = 1'b1;
    end
  end

  always @(*) begin
    decode_arbitration_removeIt = 1'b0;
    if(CsrPlugin_trigger_decodeBreak_enabled) begin
      if(CsrPlugin_trigger_decodeBreak_timeout_state) begin
        decode_arbitration_removeIt = 1'b1;
      end
    end
    if(_zz_when_1) begin
      decode_arbitration_removeIt = 1'b1;
    end
    if(decode_arbitration_isFlushed) begin
      decode_arbitration_removeIt = 1'b1;
    end
  end

  assign decode_arbitration_flushIt = 1'b0;
  always @(*) begin
    decode_arbitration_flushNext = 1'b0;
    if(CsrPlugin_trigger_decodeBreak_enabled) begin
      if(CsrPlugin_trigger_decodeBreak_timeout_state) begin
        decode_arbitration_flushNext = 1'b1;
      end
    end
    if(_zz_when_1) begin
      decode_arbitration_flushNext = 1'b1;
    end
  end

  always @(*) begin
    execute_arbitration_haltItself = 1'b0;
    if(when_DBusCachedPlugin_l395) begin
      execute_arbitration_haltItself = 1'b1;
    end
    if(when_CsrPlugin_l1591) begin
      if(execute_CsrPlugin_blockedBySideEffects) begin
        execute_arbitration_haltItself = 1'b1;
      end
    end
    if(when_ShiftPlugins_l169) begin
      if(when_ShiftPlugins_l184) begin
        execute_arbitration_haltItself = 1'b1;
      end
    end
  end

  always @(*) begin
    execute_arbitration_haltByOther = 1'b0;
    if(when_DBusCachedPlugin_l411) begin
      execute_arbitration_haltByOther = 1'b1;
    end
    if(when_FpuPlugin_l229) begin
      execute_arbitration_haltByOther = 1'b1;
    end
  end

  always @(*) begin
    execute_arbitration_removeIt = 1'b0;
    if(CsrPlugin_selfException_valid) begin
      execute_arbitration_removeIt = 1'b1;
    end
    if(execute_arbitration_isFlushed) begin
      execute_arbitration_removeIt = 1'b1;
    end
  end

  assign execute_arbitration_flushIt = 1'b0;
  always @(*) begin
    execute_arbitration_flushNext = 1'b0;
    if(CsrPlugin_selfException_valid) begin
      execute_arbitration_flushNext = 1'b1;
    end
  end

  always @(*) begin
    memory_arbitration_haltItself = 1'b0;
    if(when_MulDivIterativePlugin_l128) begin
      if(when_MulDivIterativePlugin_l129) begin
        memory_arbitration_haltItself = 1'b1;
      end
    end
  end

  assign memory_arbitration_haltByOther = 1'b0;
  always @(*) begin
    memory_arbitration_removeIt = 1'b0;
    if(memory_arbitration_isFlushed) begin
      memory_arbitration_removeIt = 1'b1;
    end
  end

  assign memory_arbitration_flushIt = 1'b0;
  always @(*) begin
    memory_arbitration_flushNext = 1'b0;
    if(BranchPlugin_jumpInterface_valid) begin
      memory_arbitration_flushNext = 1'b1;
    end
  end

  always @(*) begin
    writeBack_arbitration_haltItself = 1'b0;
    if(when_DBusCachedPlugin_l554) begin
      writeBack_arbitration_haltItself = 1'b1;
    end
  end

  always @(*) begin
    writeBack_arbitration_haltByOther = 1'b0;
    if(writeBack_FpuPlugin_isRsp) begin
      if(when_FpuPlugin_l323) begin
        writeBack_arbitration_haltByOther = 1'b1;
      end
    end
    if(when_FpuPlugin_l339) begin
      writeBack_arbitration_haltByOther = 1'b1;
    end
  end

  always @(*) begin
    writeBack_arbitration_removeIt = 1'b0;
    if(DBusCachedPlugin_exceptionBus_valid) begin
      writeBack_arbitration_removeIt = 1'b1;
    end
    if(writeBack_arbitration_isFlushed) begin
      writeBack_arbitration_removeIt = 1'b1;
    end
  end

  always @(*) begin
    writeBack_arbitration_flushIt = 1'b0;
    if(DBusCachedPlugin_redoBranch_valid) begin
      writeBack_arbitration_flushIt = 1'b1;
    end
    if(CsrPlugin_doResume) begin
      writeBack_arbitration_flushIt = 1'b1;
    end
  end

  always @(*) begin
    writeBack_arbitration_flushNext = 1'b0;
    if(DBusCachedPlugin_redoBranch_valid) begin
      writeBack_arbitration_flushNext = 1'b1;
    end
    if(DBusCachedPlugin_exceptionBus_valid) begin
      writeBack_arbitration_flushNext = 1'b1;
    end
    if(when_CsrPlugin_l1390) begin
      writeBack_arbitration_flushNext = 1'b1;
    end
    if(when_CsrPlugin_l1456) begin
      writeBack_arbitration_flushNext = 1'b1;
    end
  end

  assign lastStageInstruction = writeBack_INSTRUCTION;
  assign lastStagePc = writeBack_PC;
  assign lastStageIsValid = writeBack_arbitration_isValid;
  assign lastStageIsFiring = writeBack_arbitration_isFiring;
  always @(*) begin
    IBusSimplePlugin_fetcherHalt = 1'b0;
    if(when_CsrPlugin_l711) begin
      IBusSimplePlugin_fetcherHalt = 1'b1;
    end
    if(when_CsrPlugin_l1272) begin
      IBusSimplePlugin_fetcherHalt = 1'b1;
    end
    if(when_CsrPlugin_l1390) begin
      IBusSimplePlugin_fetcherHalt = 1'b1;
    end
    if(when_CsrPlugin_l1456) begin
      IBusSimplePlugin_fetcherHalt = 1'b1;
    end
  end

  assign IBusSimplePlugin_forceNoDecodeCond = 1'b0;
  always @(*) begin
    IBusSimplePlugin_incomingInstruction = 1'b0;
    if(IBusSimplePlugin_iBusRsp_stages_1_input_valid) begin
      IBusSimplePlugin_incomingInstruction = 1'b1;
    end
    if(IBusSimplePlugin_injector_decodeInput_valid) begin
      IBusSimplePlugin_incomingInstruction = 1'b1;
    end
  end

  always @(*) begin
    CsrPlugin_csrMapping_allowCsrSignal = 1'b0;
    if(when_CsrPlugin_l1702) begin
      CsrPlugin_csrMapping_allowCsrSignal = 1'b1;
    end
    if(when_CsrPlugin_l1709) begin
      CsrPlugin_csrMapping_allowCsrSignal = 1'b1;
    end
  end

  always @(*) begin
    CsrPlugin_csrMapping_doForceFailCsr = 1'b0;
    if(when_FpuPlugin_l253) begin
      CsrPlugin_csrMapping_doForceFailCsr = 1'b1;
    end
  end

  assign CsrPlugin_csrMapping_readDataSignal = CsrPlugin_csrMapping_readDataInit;
  assign CsrPlugin_inWfi = 1'b0;
  always @(*) begin
    CsrPlugin_thirdPartyWake = 1'b0;
    if(when_CsrPlugin_l862) begin
      CsrPlugin_thirdPartyWake = 1'b1;
    end
    if(decode_FpuPlugin_forked) begin
      CsrPlugin_thirdPartyWake = 1'b1;
    end
  end

  always @(*) begin
    CsrPlugin_jumpInterface_valid = 1'b0;
    if(when_CsrPlugin_l1390) begin
      CsrPlugin_jumpInterface_valid = 1'b1;
    end
    if(when_CsrPlugin_l1456) begin
      CsrPlugin_jumpInterface_valid = 1'b1;
    end
    if(CsrPlugin_doResume) begin
      CsrPlugin_jumpInterface_valid = 1'b1;
    end
  end

  always @(*) begin
    CsrPlugin_jumpInterface_payload = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    if(when_CsrPlugin_l1390) begin
      CsrPlugin_jumpInterface_payload = {CsrPlugin_xtvec_base,2'b00};
    end
    if(when_CsrPlugin_l1456) begin
      case(switch_CsrPlugin_l1460)
        2'b11 : begin
          CsrPlugin_jumpInterface_payload = CsrPlugin_mepc;
        end
        default : begin
        end
      endcase
    end
    if(CsrPlugin_doResume) begin
      CsrPlugin_jumpInterface_payload = CsrPlugin_dpc;
    end
  end

  assign CsrPlugin_forceMachineWire = 1'b0;
  always @(*) begin
    CsrPlugin_allowInterrupts = 1'b1;
    if(debugMode) begin
      CsrPlugin_allowInterrupts = 1'b0;
    end
  end

  assign CsrPlugin_allowException = 1'b1;
  assign CsrPlugin_allowEbreakException = 1'b1;
  always @(*) begin
    CsrPlugin_xretAwayFromMachine = 1'b0;
    if(when_CsrPlugin_l1456) begin
      case(switch_CsrPlugin_l1460)
        2'b11 : begin
          if(when_CsrPlugin_l1468) begin
            CsrPlugin_xretAwayFromMachine = 1'b1;
          end
        end
        default : begin
        end
      endcase
    end
  end

  always @(*) begin
    BranchPlugin_inDebugNoFetchFlag = 1'b0;
    if(debugMode) begin
      BranchPlugin_inDebugNoFetchFlag = 1'b1;
    end
  end

  assign IBusSimplePlugin_externalFlush = (|{writeBack_arbitration_flushNext,{memory_arbitration_flushNext,{execute_arbitration_flushNext,decode_arbitration_flushNext}}});
  assign IBusSimplePlugin_jump_pcLoad_valid = (|{BranchPlugin_jumpInterface_valid,{CsrPlugin_jumpInterface_valid,DBusCachedPlugin_redoBranch_valid}});
  assign _zz_IBusSimplePlugin_jump_pcLoad_payload = {BranchPlugin_jumpInterface_valid,{CsrPlugin_jumpInterface_valid,DBusCachedPlugin_redoBranch_valid}};
  assign _zz_IBusSimplePlugin_jump_pcLoad_payload_1 = (_zz_IBusSimplePlugin_jump_pcLoad_payload & (~ _zz__zz_IBusSimplePlugin_jump_pcLoad_payload_1));
  assign _zz_IBusSimplePlugin_jump_pcLoad_payload_2 = _zz_IBusSimplePlugin_jump_pcLoad_payload_1[1];
  assign _zz_IBusSimplePlugin_jump_pcLoad_payload_3 = _zz_IBusSimplePlugin_jump_pcLoad_payload_1[2];
  assign IBusSimplePlugin_jump_pcLoad_payload = _zz_IBusSimplePlugin_jump_pcLoad_payload_4;
  always @(*) begin
    IBusSimplePlugin_fetchPc_correction = 1'b0;
    if(IBusSimplePlugin_jump_pcLoad_valid) begin
      IBusSimplePlugin_fetchPc_correction = 1'b1;
    end
  end

  assign IBusSimplePlugin_fetchPc_output_fire = (IBusSimplePlugin_fetchPc_output_valid && IBusSimplePlugin_fetchPc_output_ready);
  assign IBusSimplePlugin_fetchPc_corrected = (IBusSimplePlugin_fetchPc_correction || IBusSimplePlugin_fetchPc_correctionReg);
  always @(*) begin
    IBusSimplePlugin_fetchPc_pcRegPropagate = 1'b0;
    if(IBusSimplePlugin_iBusRsp_stages_1_input_ready) begin
      IBusSimplePlugin_fetchPc_pcRegPropagate = 1'b1;
    end
  end

  assign when_Fetcher_l133 = (IBusSimplePlugin_fetchPc_correction || IBusSimplePlugin_fetchPc_pcRegPropagate);
  assign when_Fetcher_l133_1 = ((! IBusSimplePlugin_fetchPc_output_valid) && IBusSimplePlugin_fetchPc_output_ready);
  always @(*) begin
    IBusSimplePlugin_fetchPc_pc = (IBusSimplePlugin_fetchPc_pcReg + _zz_IBusSimplePlugin_fetchPc_pc);
    if(IBusSimplePlugin_fetchPc_inc) begin
      IBusSimplePlugin_fetchPc_pc[1] = 1'b0;
    end
    if(IBusSimplePlugin_jump_pcLoad_valid) begin
      IBusSimplePlugin_fetchPc_pc = IBusSimplePlugin_jump_pcLoad_payload;
    end
    IBusSimplePlugin_fetchPc_pc[0] = 1'b0;
  end

  always @(*) begin
    IBusSimplePlugin_fetchPc_flushed = 1'b0;
    if(IBusSimplePlugin_jump_pcLoad_valid) begin
      IBusSimplePlugin_fetchPc_flushed = 1'b1;
    end
  end

  assign when_Fetcher_l160 = (IBusSimplePlugin_fetchPc_booted && ((IBusSimplePlugin_fetchPc_output_ready || IBusSimplePlugin_fetchPc_correction) || IBusSimplePlugin_fetchPc_pcRegPropagate));
  assign IBusSimplePlugin_fetchPc_output_valid = ((! IBusSimplePlugin_fetcherHalt) && IBusSimplePlugin_fetchPc_booted);
  assign IBusSimplePlugin_fetchPc_output_payload = IBusSimplePlugin_fetchPc_pc;
  always @(*) begin
    IBusSimplePlugin_decodePc_flushed = 1'b0;
    if(when_Fetcher_l194) begin
      IBusSimplePlugin_decodePc_flushed = 1'b1;
    end
  end

  assign IBusSimplePlugin_decodePc_pcPlus = (IBusSimplePlugin_decodePc_pcReg + _zz_IBusSimplePlugin_decodePc_pcPlus);
  always @(*) begin
    IBusSimplePlugin_decodePc_injectedDecode = 1'b0;
    if(when_Fetcher_l373) begin
      IBusSimplePlugin_decodePc_injectedDecode = 1'b1;
    end
  end

  assign when_Fetcher_l182 = (decode_arbitration_isFiring && (! IBusSimplePlugin_decodePc_injectedDecode));
  assign when_Fetcher_l194 = (IBusSimplePlugin_jump_pcLoad_valid && ((! decode_arbitration_isStuck) || decode_arbitration_removeIt));
  assign IBusSimplePlugin_iBusRsp_redoFetch = 1'b0;
  assign IBusSimplePlugin_iBusRsp_stages_0_input_valid = IBusSimplePlugin_fetchPc_output_valid;
  assign IBusSimplePlugin_fetchPc_output_ready = IBusSimplePlugin_iBusRsp_stages_0_input_ready;
  assign IBusSimplePlugin_iBusRsp_stages_0_input_payload = IBusSimplePlugin_fetchPc_output_payload;
  always @(*) begin
    IBusSimplePlugin_iBusRsp_stages_0_halt = 1'b0;
    if(when_IBusSimplePlugin_l305) begin
      IBusSimplePlugin_iBusRsp_stages_0_halt = 1'b1;
    end
  end

  assign _zz_IBusSimplePlugin_iBusRsp_stages_0_input_ready = (! IBusSimplePlugin_iBusRsp_stages_0_halt);
  assign IBusSimplePlugin_iBusRsp_stages_0_input_ready = (IBusSimplePlugin_iBusRsp_stages_0_output_ready && _zz_IBusSimplePlugin_iBusRsp_stages_0_input_ready);
  assign IBusSimplePlugin_iBusRsp_stages_0_output_valid = (IBusSimplePlugin_iBusRsp_stages_0_input_valid && _zz_IBusSimplePlugin_iBusRsp_stages_0_input_ready);
  assign IBusSimplePlugin_iBusRsp_stages_0_output_payload = IBusSimplePlugin_iBusRsp_stages_0_input_payload;
  assign IBusSimplePlugin_iBusRsp_stages_1_halt = 1'b0;
  assign _zz_IBusSimplePlugin_iBusRsp_stages_1_input_ready = (! IBusSimplePlugin_iBusRsp_stages_1_halt);
  assign IBusSimplePlugin_iBusRsp_stages_1_input_ready = (IBusSimplePlugin_iBusRsp_stages_1_output_ready && _zz_IBusSimplePlugin_iBusRsp_stages_1_input_ready);
  assign IBusSimplePlugin_iBusRsp_stages_1_output_valid = (IBusSimplePlugin_iBusRsp_stages_1_input_valid && _zz_IBusSimplePlugin_iBusRsp_stages_1_input_ready);
  assign IBusSimplePlugin_iBusRsp_stages_1_output_payload = IBusSimplePlugin_iBusRsp_stages_1_input_payload;
  assign IBusSimplePlugin_iBusRsp_flush = (IBusSimplePlugin_externalFlush || IBusSimplePlugin_iBusRsp_redoFetch);
  assign IBusSimplePlugin_iBusRsp_stages_0_output_ready = _zz_IBusSimplePlugin_iBusRsp_stages_0_output_ready;
  assign _zz_IBusSimplePlugin_iBusRsp_stages_0_output_ready = ((1'b0 && (! _zz_IBusSimplePlugin_iBusRsp_stages_1_input_valid)) || IBusSimplePlugin_iBusRsp_stages_1_input_ready);
  assign _zz_IBusSimplePlugin_iBusRsp_stages_1_input_valid = _zz_IBusSimplePlugin_iBusRsp_stages_1_input_valid_1;
  assign IBusSimplePlugin_iBusRsp_stages_1_input_valid = _zz_IBusSimplePlugin_iBusRsp_stages_1_input_valid;
  assign IBusSimplePlugin_iBusRsp_stages_1_input_payload = IBusSimplePlugin_fetchPc_pcReg;
  always @(*) begin
    IBusSimplePlugin_iBusRsp_readyForError = 1'b1;
    if(IBusSimplePlugin_injector_decodeInput_valid) begin
      IBusSimplePlugin_iBusRsp_readyForError = 1'b0;
    end
  end

  assign IBusSimplePlugin_decompressor_input_valid = (IBusSimplePlugin_iBusRsp_output_valid && (! IBusSimplePlugin_iBusRsp_redoFetch));
  assign IBusSimplePlugin_decompressor_input_payload_pc = IBusSimplePlugin_iBusRsp_output_payload_pc;
  assign IBusSimplePlugin_decompressor_input_payload_rsp_error = IBusSimplePlugin_iBusRsp_output_payload_rsp_error;
  assign IBusSimplePlugin_decompressor_input_payload_rsp_inst = IBusSimplePlugin_iBusRsp_output_payload_rsp_inst;
  assign IBusSimplePlugin_decompressor_input_payload_isRvc = IBusSimplePlugin_iBusRsp_output_payload_isRvc;
  assign IBusSimplePlugin_iBusRsp_output_ready = IBusSimplePlugin_decompressor_input_ready;
  assign IBusSimplePlugin_decompressor_flushNext = 1'b0;
  assign IBusSimplePlugin_decompressor_consumeCurrent = 1'b0;
  assign IBusSimplePlugin_decompressor_isInputLowRvc = (IBusSimplePlugin_decompressor_input_payload_rsp_inst[1 : 0] != 2'b11);
  assign IBusSimplePlugin_decompressor_isInputHighRvc = (IBusSimplePlugin_decompressor_input_payload_rsp_inst[17 : 16] != 2'b11);
  assign IBusSimplePlugin_decompressor_throw2Bytes = (IBusSimplePlugin_decompressor_throw2BytesReg || IBusSimplePlugin_decompressor_input_payload_pc[1]);
  assign IBusSimplePlugin_decompressor_unaligned = (IBusSimplePlugin_decompressor_throw2Bytes || IBusSimplePlugin_decompressor_bufferValid);
  assign IBusSimplePlugin_decompressor_bufferValidPatched = (IBusSimplePlugin_decompressor_input_valid ? IBusSimplePlugin_decompressor_bufferValid : IBusSimplePlugin_decompressor_bufferValidLatch);
  assign IBusSimplePlugin_decompressor_throw2BytesPatched = (IBusSimplePlugin_decompressor_input_valid ? IBusSimplePlugin_decompressor_throw2Bytes : IBusSimplePlugin_decompressor_throw2BytesLatch);
  assign IBusSimplePlugin_decompressor_raw = (IBusSimplePlugin_decompressor_bufferValidPatched ? {IBusSimplePlugin_decompressor_input_payload_rsp_inst[15 : 0],IBusSimplePlugin_decompressor_bufferData} : {IBusSimplePlugin_decompressor_input_payload_rsp_inst[31 : 16],(IBusSimplePlugin_decompressor_throw2BytesPatched ? IBusSimplePlugin_decompressor_input_payload_rsp_inst[31 : 16] : IBusSimplePlugin_decompressor_input_payload_rsp_inst[15 : 0])});
  assign IBusSimplePlugin_decompressor_isRvc = (IBusSimplePlugin_decompressor_raw[1 : 0] != 2'b11);
  assign _zz_IBusSimplePlugin_decompressor_decompressed = IBusSimplePlugin_decompressor_raw[15 : 0];
  always @(*) begin
    IBusSimplePlugin_decompressor_decompressed = 32'h0;
    case(switch_Misc_l44)
      5'h0 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{{{{{2'b00,_zz_IBusSimplePlugin_decompressor_decompressed[10 : 7]},_zz_IBusSimplePlugin_decompressor_decompressed[12 : 11]},_zz_IBusSimplePlugin_decompressor_decompressed[5]},_zz_IBusSimplePlugin_decompressor_decompressed[6]},2'b00},5'h02},3'b000},_zz_IBusSimplePlugin_decompressor_decompressed_2},7'h13};
        if(when_Misc_l47) begin
          IBusSimplePlugin_decompressor_decompressed = 32'h0;
        end
      end
      5'h02 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{_zz_IBusSimplePlugin_decompressor_decompressed_3,_zz_IBusSimplePlugin_decompressor_decompressed_1},3'b010},_zz_IBusSimplePlugin_decompressor_decompressed_2},7'h03};
      end
      5'h03 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{_zz_IBusSimplePlugin_decompressor_decompressed_3,_zz_IBusSimplePlugin_decompressor_decompressed_1},3'b010},_zz_IBusSimplePlugin_decompressor_decompressed_2},7'h07};
      end
      5'h06 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{_zz_IBusSimplePlugin_decompressor_decompressed_3[11 : 5],_zz_IBusSimplePlugin_decompressor_decompressed_2},_zz_IBusSimplePlugin_decompressor_decompressed_1},3'b010},_zz_IBusSimplePlugin_decompressor_decompressed_3[4 : 0]},7'h23};
      end
      5'h07 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{_zz_IBusSimplePlugin_decompressor_decompressed_3[11 : 5],_zz_IBusSimplePlugin_decompressor_decompressed_2},_zz_IBusSimplePlugin_decompressor_decompressed_1},3'b010},_zz_IBusSimplePlugin_decompressor_decompressed_3[4 : 0]},7'h27};
      end
      5'h08 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{_zz_IBusSimplePlugin_decompressor_decompressed_5,_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},3'b000},_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},7'h13};
      end
      5'h09 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{_zz_IBusSimplePlugin_decompressor_decompressed_8[20],_zz_IBusSimplePlugin_decompressor_decompressed_8[10 : 1]},_zz_IBusSimplePlugin_decompressor_decompressed_8[11]},_zz_IBusSimplePlugin_decompressor_decompressed_8[19 : 12]},_zz_IBusSimplePlugin_decompressor_decompressed_20},7'h6f};
      end
      5'h0a : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{_zz_IBusSimplePlugin_decompressor_decompressed_5,5'h0},3'b000},_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},7'h13};
      end
      5'h0b : begin
        IBusSimplePlugin_decompressor_decompressed = ((_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7] == 5'h02) ? {{{{{{{{_zz_IBusSimplePlugin_decompressor_decompressed_27,_zz_IBusSimplePlugin_decompressor_decompressed_28},_zz_IBusSimplePlugin_decompressor_decompressed_29},_zz_IBusSimplePlugin_decompressor_decompressed[6]},4'b0000},_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},3'b000},_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},7'h13} : {{_zz_IBusSimplePlugin_decompressor_decompressed_30[31 : 12],_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},7'h37});
      end
      5'h0c : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{((_zz_IBusSimplePlugin_decompressor_decompressed[11 : 10] == 2'b10) ? _zz_IBusSimplePlugin_decompressor_decompressed_26 : {{1'b0,(_zz_IBusSimplePlugin_decompressor_decompressed_31 || _zz_IBusSimplePlugin_decompressor_decompressed_32)},5'h0}),(((! _zz_IBusSimplePlugin_decompressor_decompressed[11]) || _zz_IBusSimplePlugin_decompressor_decompressed_22) ? _zz_IBusSimplePlugin_decompressor_decompressed[6 : 2] : _zz_IBusSimplePlugin_decompressor_decompressed_2)},_zz_IBusSimplePlugin_decompressor_decompressed_1},_zz_IBusSimplePlugin_decompressor_decompressed_24},_zz_IBusSimplePlugin_decompressor_decompressed_1},(_zz_IBusSimplePlugin_decompressor_decompressed_22 ? 7'h13 : 7'h33)};
      end
      5'h0d : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{_zz_IBusSimplePlugin_decompressor_decompressed_15[20],_zz_IBusSimplePlugin_decompressor_decompressed_15[10 : 1]},_zz_IBusSimplePlugin_decompressor_decompressed_15[11]},_zz_IBusSimplePlugin_decompressor_decompressed_15[19 : 12]},_zz_IBusSimplePlugin_decompressor_decompressed_19},7'h6f};
      end
      5'h0e : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{{{_zz_IBusSimplePlugin_decompressor_decompressed_18[12],_zz_IBusSimplePlugin_decompressor_decompressed_18[10 : 5]},_zz_IBusSimplePlugin_decompressor_decompressed_19},_zz_IBusSimplePlugin_decompressor_decompressed_1},3'b000},_zz_IBusSimplePlugin_decompressor_decompressed_18[4 : 1]},_zz_IBusSimplePlugin_decompressor_decompressed_18[11]},7'h63};
      end
      5'h0f : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{{{_zz_IBusSimplePlugin_decompressor_decompressed_18[12],_zz_IBusSimplePlugin_decompressor_decompressed_18[10 : 5]},_zz_IBusSimplePlugin_decompressor_decompressed_19},_zz_IBusSimplePlugin_decompressor_decompressed_1},3'b001},_zz_IBusSimplePlugin_decompressor_decompressed_18[4 : 1]},_zz_IBusSimplePlugin_decompressor_decompressed_18[11]},7'h63};
      end
      5'h10 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{7'h0,_zz_IBusSimplePlugin_decompressor_decompressed[6 : 2]},_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},3'b001},_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},7'h13};
      end
      5'h12 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{{{{4'b0000,_zz_IBusSimplePlugin_decompressor_decompressed[3 : 2]},_zz_IBusSimplePlugin_decompressor_decompressed[12]},_zz_IBusSimplePlugin_decompressor_decompressed[6 : 4]},2'b00},_zz_IBusSimplePlugin_decompressor_decompressed_21},3'b010},_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},7'h03};
      end
      5'h13 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{{{{4'b0000,_zz_IBusSimplePlugin_decompressor_decompressed[3 : 2]},_zz_IBusSimplePlugin_decompressor_decompressed[12]},_zz_IBusSimplePlugin_decompressor_decompressed[6 : 4]},2'b00},_zz_IBusSimplePlugin_decompressor_decompressed_21},3'b010},_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},7'h07};
      end
      5'h14 : begin
        IBusSimplePlugin_decompressor_decompressed = ((_zz_IBusSimplePlugin_decompressor_decompressed[12 : 2] == 11'h400) ? 32'h00100073 : ((_zz_IBusSimplePlugin_decompressor_decompressed[6 : 2] == 5'h0) ? {{{{12'h0,_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},3'b000},(_zz_IBusSimplePlugin_decompressor_decompressed[12] ? _zz_IBusSimplePlugin_decompressor_decompressed_20 : _zz_IBusSimplePlugin_decompressor_decompressed_19)},7'h67} : {{{{{_zz_IBusSimplePlugin_decompressor_decompressed_33,_zz_IBusSimplePlugin_decompressor_decompressed_34},(_zz_IBusSimplePlugin_decompressor_decompressed_35 ? _zz_IBusSimplePlugin_decompressor_decompressed_36 : _zz_IBusSimplePlugin_decompressor_decompressed_19)},3'b000},_zz_IBusSimplePlugin_decompressor_decompressed[11 : 7]},7'h33}));
      end
      5'h16 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{_zz_IBusSimplePlugin_decompressor_decompressed_37[11 : 5],_zz_IBusSimplePlugin_decompressor_decompressed[6 : 2]},_zz_IBusSimplePlugin_decompressor_decompressed_21},3'b010},_zz_IBusSimplePlugin_decompressor_decompressed_38[4 : 0]},7'h23};
      end
      5'h17 : begin
        IBusSimplePlugin_decompressor_decompressed = {{{{{_zz_IBusSimplePlugin_decompressor_decompressed_39[11 : 5],_zz_IBusSimplePlugin_decompressor_decompressed[6 : 2]},_zz_IBusSimplePlugin_decompressor_decompressed_21},3'b010},_zz_IBusSimplePlugin_decompressor_decompressed_40[4 : 0]},7'h27};
      end
      default : begin
      end
    endcase
  end

  assign _zz_IBusSimplePlugin_decompressor_decompressed_1 = {2'b01,_zz_IBusSimplePlugin_decompressor_decompressed[9 : 7]};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_2 = {2'b01,_zz_IBusSimplePlugin_decompressor_decompressed[4 : 2]};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_3 = {{{{5'h0,_zz_IBusSimplePlugin_decompressor_decompressed[5]},_zz_IBusSimplePlugin_decompressor_decompressed[12 : 10]},_zz_IBusSimplePlugin_decompressor_decompressed[6]},2'b00};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_4 = _zz_IBusSimplePlugin_decompressor_decompressed[12];
  always @(*) begin
    _zz_IBusSimplePlugin_decompressor_decompressed_5[11] = _zz_IBusSimplePlugin_decompressor_decompressed_4;
    _zz_IBusSimplePlugin_decompressor_decompressed_5[10] = _zz_IBusSimplePlugin_decompressor_decompressed_4;
    _zz_IBusSimplePlugin_decompressor_decompressed_5[9] = _zz_IBusSimplePlugin_decompressor_decompressed_4;
    _zz_IBusSimplePlugin_decompressor_decompressed_5[8] = _zz_IBusSimplePlugin_decompressor_decompressed_4;
    _zz_IBusSimplePlugin_decompressor_decompressed_5[7] = _zz_IBusSimplePlugin_decompressor_decompressed_4;
    _zz_IBusSimplePlugin_decompressor_decompressed_5[6] = _zz_IBusSimplePlugin_decompressor_decompressed_4;
    _zz_IBusSimplePlugin_decompressor_decompressed_5[5] = _zz_IBusSimplePlugin_decompressor_decompressed_4;
    _zz_IBusSimplePlugin_decompressor_decompressed_5[4 : 0] = _zz_IBusSimplePlugin_decompressor_decompressed[6 : 2];
  end

  assign _zz_IBusSimplePlugin_decompressor_decompressed_6 = _zz_IBusSimplePlugin_decompressor_decompressed[12];
  always @(*) begin
    _zz_IBusSimplePlugin_decompressor_decompressed_7[9] = _zz_IBusSimplePlugin_decompressor_decompressed_6;
    _zz_IBusSimplePlugin_decompressor_decompressed_7[8] = _zz_IBusSimplePlugin_decompressor_decompressed_6;
    _zz_IBusSimplePlugin_decompressor_decompressed_7[7] = _zz_IBusSimplePlugin_decompressor_decompressed_6;
    _zz_IBusSimplePlugin_decompressor_decompressed_7[6] = _zz_IBusSimplePlugin_decompressor_decompressed_6;
    _zz_IBusSimplePlugin_decompressor_decompressed_7[5] = _zz_IBusSimplePlugin_decompressor_decompressed_6;
    _zz_IBusSimplePlugin_decompressor_decompressed_7[4] = _zz_IBusSimplePlugin_decompressor_decompressed_6;
    _zz_IBusSimplePlugin_decompressor_decompressed_7[3] = _zz_IBusSimplePlugin_decompressor_decompressed_6;
    _zz_IBusSimplePlugin_decompressor_decompressed_7[2] = _zz_IBusSimplePlugin_decompressor_decompressed_6;
    _zz_IBusSimplePlugin_decompressor_decompressed_7[1] = _zz_IBusSimplePlugin_decompressor_decompressed_6;
    _zz_IBusSimplePlugin_decompressor_decompressed_7[0] = _zz_IBusSimplePlugin_decompressor_decompressed_6;
  end

  assign _zz_IBusSimplePlugin_decompressor_decompressed_8 = {{{{{{{{_zz_IBusSimplePlugin_decompressor_decompressed_7,_zz_IBusSimplePlugin_decompressor_decompressed[8]},_zz_IBusSimplePlugin_decompressor_decompressed[10 : 9]},_zz_IBusSimplePlugin_decompressor_decompressed[6]},_zz_IBusSimplePlugin_decompressor_decompressed[7]},_zz_IBusSimplePlugin_decompressor_decompressed[2]},_zz_IBusSimplePlugin_decompressor_decompressed[11]},_zz_IBusSimplePlugin_decompressor_decompressed[5 : 3]},1'b0};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_9 = _zz_IBusSimplePlugin_decompressor_decompressed[12];
  always @(*) begin
    _zz_IBusSimplePlugin_decompressor_decompressed_10[14] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[13] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[12] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[11] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[10] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[9] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[8] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[7] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[6] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[5] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[4] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[3] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[2] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[1] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
    _zz_IBusSimplePlugin_decompressor_decompressed_10[0] = _zz_IBusSimplePlugin_decompressor_decompressed_9;
  end

  assign _zz_IBusSimplePlugin_decompressor_decompressed_11 = _zz_IBusSimplePlugin_decompressor_decompressed[12];
  always @(*) begin
    _zz_IBusSimplePlugin_decompressor_decompressed_12[2] = _zz_IBusSimplePlugin_decompressor_decompressed_11;
    _zz_IBusSimplePlugin_decompressor_decompressed_12[1] = _zz_IBusSimplePlugin_decompressor_decompressed_11;
    _zz_IBusSimplePlugin_decompressor_decompressed_12[0] = _zz_IBusSimplePlugin_decompressor_decompressed_11;
  end

  assign _zz_IBusSimplePlugin_decompressor_decompressed_13 = _zz_IBusSimplePlugin_decompressor_decompressed[12];
  always @(*) begin
    _zz_IBusSimplePlugin_decompressor_decompressed_14[9] = _zz_IBusSimplePlugin_decompressor_decompressed_13;
    _zz_IBusSimplePlugin_decompressor_decompressed_14[8] = _zz_IBusSimplePlugin_decompressor_decompressed_13;
    _zz_IBusSimplePlugin_decompressor_decompressed_14[7] = _zz_IBusSimplePlugin_decompressor_decompressed_13;
    _zz_IBusSimplePlugin_decompressor_decompressed_14[6] = _zz_IBusSimplePlugin_decompressor_decompressed_13;
    _zz_IBusSimplePlugin_decompressor_decompressed_14[5] = _zz_IBusSimplePlugin_decompressor_decompressed_13;
    _zz_IBusSimplePlugin_decompressor_decompressed_14[4] = _zz_IBusSimplePlugin_decompressor_decompressed_13;
    _zz_IBusSimplePlugin_decompressor_decompressed_14[3] = _zz_IBusSimplePlugin_decompressor_decompressed_13;
    _zz_IBusSimplePlugin_decompressor_decompressed_14[2] = _zz_IBusSimplePlugin_decompressor_decompressed_13;
    _zz_IBusSimplePlugin_decompressor_decompressed_14[1] = _zz_IBusSimplePlugin_decompressor_decompressed_13;
    _zz_IBusSimplePlugin_decompressor_decompressed_14[0] = _zz_IBusSimplePlugin_decompressor_decompressed_13;
  end

  assign _zz_IBusSimplePlugin_decompressor_decompressed_15 = {{{{{{{{_zz_IBusSimplePlugin_decompressor_decompressed_14,_zz_IBusSimplePlugin_decompressor_decompressed[8]},_zz_IBusSimplePlugin_decompressor_decompressed[10 : 9]},_zz_IBusSimplePlugin_decompressor_decompressed[6]},_zz_IBusSimplePlugin_decompressor_decompressed[7]},_zz_IBusSimplePlugin_decompressor_decompressed[2]},_zz_IBusSimplePlugin_decompressor_decompressed[11]},_zz_IBusSimplePlugin_decompressor_decompressed[5 : 3]},1'b0};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_16 = _zz_IBusSimplePlugin_decompressor_decompressed[12];
  always @(*) begin
    _zz_IBusSimplePlugin_decompressor_decompressed_17[4] = _zz_IBusSimplePlugin_decompressor_decompressed_16;
    _zz_IBusSimplePlugin_decompressor_decompressed_17[3] = _zz_IBusSimplePlugin_decompressor_decompressed_16;
    _zz_IBusSimplePlugin_decompressor_decompressed_17[2] = _zz_IBusSimplePlugin_decompressor_decompressed_16;
    _zz_IBusSimplePlugin_decompressor_decompressed_17[1] = _zz_IBusSimplePlugin_decompressor_decompressed_16;
    _zz_IBusSimplePlugin_decompressor_decompressed_17[0] = _zz_IBusSimplePlugin_decompressor_decompressed_16;
  end

  assign _zz_IBusSimplePlugin_decompressor_decompressed_18 = {{{{{_zz_IBusSimplePlugin_decompressor_decompressed_17,_zz_IBusSimplePlugin_decompressor_decompressed[6 : 5]},_zz_IBusSimplePlugin_decompressor_decompressed[2]},_zz_IBusSimplePlugin_decompressor_decompressed[11 : 10]},_zz_IBusSimplePlugin_decompressor_decompressed[4 : 3]},1'b0};
  assign _zz_IBusSimplePlugin_decompressor_decompressed_19 = 5'h0;
  assign _zz_IBusSimplePlugin_decompressor_decompressed_20 = 5'h01;
  assign _zz_IBusSimplePlugin_decompressor_decompressed_21 = 5'h02;
  assign switch_Misc_l44 = {_zz_IBusSimplePlugin_decompressor_decompressed[1 : 0],_zz_IBusSimplePlugin_decompressor_decompressed[15 : 13]};
  assign when_Misc_l47 = (_zz_IBusSimplePlugin_decompressor_decompressed[12 : 2] == 11'h0);
  assign _zz_IBusSimplePlugin_decompressor_decompressed_22 = (_zz_IBusSimplePlugin_decompressor_decompressed[11 : 10] != 2'b11);
  assign switch_Misc_l241 = _zz_IBusSimplePlugin_decompressor_decompressed[11 : 10];
  assign switch_Misc_l241_1 = _zz_IBusSimplePlugin_decompressor_decompressed[6 : 5];
  always @(*) begin
    case(switch_Misc_l241_1)
      2'b00 : begin
        _zz_IBusSimplePlugin_decompressor_decompressed_23 = 3'b000;
      end
      2'b01 : begin
        _zz_IBusSimplePlugin_decompressor_decompressed_23 = 3'b100;
      end
      2'b10 : begin
        _zz_IBusSimplePlugin_decompressor_decompressed_23 = 3'b110;
      end
      default : begin
        _zz_IBusSimplePlugin_decompressor_decompressed_23 = 3'b111;
      end
    endcase
  end

  always @(*) begin
    case(switch_Misc_l241)
      2'b00 : begin
        _zz_IBusSimplePlugin_decompressor_decompressed_24 = 3'b101;
      end
      2'b01 : begin
        _zz_IBusSimplePlugin_decompressor_decompressed_24 = 3'b101;
      end
      2'b10 : begin
        _zz_IBusSimplePlugin_decompressor_decompressed_24 = 3'b111;
      end
      default : begin
        _zz_IBusSimplePlugin_decompressor_decompressed_24 = _zz_IBusSimplePlugin_decompressor_decompressed_23;
      end
    endcase
  end

  assign _zz_IBusSimplePlugin_decompressor_decompressed_25 = _zz_IBusSimplePlugin_decompressor_decompressed[12];
  always @(*) begin
    _zz_IBusSimplePlugin_decompressor_decompressed_26[6] = _zz_IBusSimplePlugin_decompressor_decompressed_25;
    _zz_IBusSimplePlugin_decompressor_decompressed_26[5] = _zz_IBusSimplePlugin_decompressor_decompressed_25;
    _zz_IBusSimplePlugin_decompressor_decompressed_26[4] = _zz_IBusSimplePlugin_decompressor_decompressed_25;
    _zz_IBusSimplePlugin_decompressor_decompressed_26[3] = _zz_IBusSimplePlugin_decompressor_decompressed_25;
    _zz_IBusSimplePlugin_decompressor_decompressed_26[2] = _zz_IBusSimplePlugin_decompressor_decompressed_25;
    _zz_IBusSimplePlugin_decompressor_decompressed_26[1] = _zz_IBusSimplePlugin_decompressor_decompressed_25;
    _zz_IBusSimplePlugin_decompressor_decompressed_26[0] = _zz_IBusSimplePlugin_decompressor_decompressed_25;
  end

  assign IBusSimplePlugin_decompressor_output_valid = (IBusSimplePlugin_decompressor_input_valid && (! ((IBusSimplePlugin_decompressor_throw2Bytes && (! IBusSimplePlugin_decompressor_bufferValid)) && (! IBusSimplePlugin_decompressor_isInputHighRvc))));
  assign IBusSimplePlugin_decompressor_output_payload_pc = IBusSimplePlugin_decompressor_input_payload_pc;
  assign IBusSimplePlugin_decompressor_output_payload_isRvc = IBusSimplePlugin_decompressor_isRvc;
  assign IBusSimplePlugin_decompressor_output_payload_rsp_inst = (IBusSimplePlugin_decompressor_isRvc ? IBusSimplePlugin_decompressor_decompressed : IBusSimplePlugin_decompressor_raw);
  assign IBusSimplePlugin_decompressor_input_ready = (IBusSimplePlugin_decompressor_output_ready && (((! IBusSimplePlugin_iBusRsp_stages_1_input_valid) || IBusSimplePlugin_decompressor_flushNext) || ((! (IBusSimplePlugin_decompressor_bufferValid && IBusSimplePlugin_decompressor_isInputHighRvc)) && (! (((! IBusSimplePlugin_decompressor_unaligned) && IBusSimplePlugin_decompressor_isInputLowRvc) && IBusSimplePlugin_decompressor_isInputHighRvc)))));
  assign IBusSimplePlugin_decompressor_output_fire = (IBusSimplePlugin_decompressor_output_valid && IBusSimplePlugin_decompressor_output_ready);
  assign IBusSimplePlugin_decompressor_bufferFill = (((((! IBusSimplePlugin_decompressor_unaligned) && IBusSimplePlugin_decompressor_isInputLowRvc) && (! IBusSimplePlugin_decompressor_isInputHighRvc)) || (IBusSimplePlugin_decompressor_bufferValid && (! IBusSimplePlugin_decompressor_isInputHighRvc))) || ((IBusSimplePlugin_decompressor_throw2Bytes && (! IBusSimplePlugin_decompressor_isRvc)) && (! IBusSimplePlugin_decompressor_isInputHighRvc)));
  assign when_Fetcher_l285 = (IBusSimplePlugin_decompressor_output_ready && IBusSimplePlugin_decompressor_input_valid);
  assign when_Fetcher_l288 = (IBusSimplePlugin_decompressor_output_ready && IBusSimplePlugin_decompressor_input_valid);
  assign when_Fetcher_l293 = (IBusSimplePlugin_externalFlush || IBusSimplePlugin_decompressor_consumeCurrent);
  assign IBusSimplePlugin_decompressor_output_ready = ((1'b0 && (! IBusSimplePlugin_injector_decodeInput_valid)) || IBusSimplePlugin_injector_decodeInput_ready);
  assign IBusSimplePlugin_injector_decodeInput_valid = _zz_IBusSimplePlugin_injector_decodeInput_valid;
  assign IBusSimplePlugin_injector_decodeInput_payload_pc = _zz_IBusSimplePlugin_injector_decodeInput_payload_pc;
  assign IBusSimplePlugin_injector_decodeInput_payload_rsp_error = _zz_IBusSimplePlugin_injector_decodeInput_payload_rsp_error;
  assign IBusSimplePlugin_injector_decodeInput_payload_rsp_inst = _zz_IBusSimplePlugin_injector_decodeInput_payload_rsp_inst;
  assign IBusSimplePlugin_injector_decodeInput_payload_isRvc = _zz_IBusSimplePlugin_injector_decodeInput_payload_isRvc;
  assign when_Fetcher_l331 = (! 1'b0);
  assign when_Fetcher_l331_1 = (! execute_arbitration_isStuck);
  assign when_Fetcher_l331_2 = (! memory_arbitration_isStuck);
  assign when_Fetcher_l331_3 = (! writeBack_arbitration_isStuck);
  assign IBusSimplePlugin_pcValids_0 = IBusSimplePlugin_injector_nextPcCalc_valids_0;
  assign IBusSimplePlugin_pcValids_1 = IBusSimplePlugin_injector_nextPcCalc_valids_1;
  assign IBusSimplePlugin_pcValids_2 = IBusSimplePlugin_injector_nextPcCalc_valids_2;
  assign IBusSimplePlugin_pcValids_3 = IBusSimplePlugin_injector_nextPcCalc_valids_3;
  assign IBusSimplePlugin_injector_decodeInput_ready = (! decode_arbitration_isStuck);
  always @(*) begin
    decode_arbitration_isValid = IBusSimplePlugin_injector_decodeInput_valid;
    case(IBusSimplePlugin_injector_port_state)
      3'b010 : begin
        decode_arbitration_isValid = 1'b1;
      end
      3'b011 : begin
        decode_arbitration_isValid = 1'b1;
      end
      default : begin
      end
    endcase
    if(IBusSimplePlugin_forceNoDecodeCond) begin
      decode_arbitration_isValid = 1'b0;
    end
  end

  assign iBus_cmd_valid = IBusSimplePlugin_cmd_valid;
  assign IBusSimplePlugin_cmd_ready = iBus_cmd_ready;
  assign iBus_cmd_payload_pc = IBusSimplePlugin_cmd_payload_pc;
  assign IBusSimplePlugin_pending_next = (_zz_IBusSimplePlugin_pending_next - _zz_IBusSimplePlugin_pending_next_3);
  assign IBusSimplePlugin_cmdFork_canEmit = (IBusSimplePlugin_iBusRsp_stages_0_output_ready && (IBusSimplePlugin_pending_value != 3'b111));
  assign when_IBusSimplePlugin_l305 = (IBusSimplePlugin_iBusRsp_stages_0_input_valid && ((! IBusSimplePlugin_cmdFork_canEmit) || (! IBusSimplePlugin_cmd_ready)));
  assign IBusSimplePlugin_cmd_valid = (IBusSimplePlugin_iBusRsp_stages_0_input_valid && IBusSimplePlugin_cmdFork_canEmit);
  assign IBusSimplePlugin_cmd_fire = (IBusSimplePlugin_cmd_valid && IBusSimplePlugin_cmd_ready);
  assign IBusSimplePlugin_pending_inc = IBusSimplePlugin_cmd_fire;
  assign IBusSimplePlugin_cmd_payload_pc = {IBusSimplePlugin_iBusRsp_stages_0_input_payload[31 : 2],2'b00};
  assign iBus_rsp_toStream_valid = iBus_rsp_valid;
  assign iBus_rsp_toStream_payload_error = iBus_rsp_payload_error;
  assign iBus_rsp_toStream_payload_inst = iBus_rsp_payload_inst;
  assign iBus_rsp_toStream_ready = IBusSimplePlugin_rspJoin_rspBuffer_c_io_push_ready;
  assign IBusSimplePlugin_rspJoin_rspBuffer_flush = ((IBusSimplePlugin_rspJoin_rspBuffer_discardCounter != 3'b000) || IBusSimplePlugin_iBusRsp_flush);
  assign IBusSimplePlugin_rspJoin_rspBuffer_output_valid = (IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_valid && (IBusSimplePlugin_rspJoin_rspBuffer_discardCounter == 3'b000));
  assign IBusSimplePlugin_rspJoin_rspBuffer_output_payload_error = IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_payload_error;
  assign IBusSimplePlugin_rspJoin_rspBuffer_output_payload_inst = IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_payload_inst;
  assign IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_ready = (IBusSimplePlugin_rspJoin_rspBuffer_output_ready || IBusSimplePlugin_rspJoin_rspBuffer_flush);
  assign toplevel_IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_fire = (IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_valid && IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_ready);
  assign IBusSimplePlugin_pending_dec = toplevel_IBusSimplePlugin_rspJoin_rspBuffer_c_io_pop_fire;
  assign IBusSimplePlugin_rspJoin_fetchRsp_isRvc = 1'b0;
  assign IBusSimplePlugin_rspJoin_fetchRsp_pc = IBusSimplePlugin_iBusRsp_stages_1_output_payload;
  always @(*) begin
    IBusSimplePlugin_rspJoin_fetchRsp_rsp_error = IBusSimplePlugin_rspJoin_rspBuffer_output_payload_error;
    if(when_IBusSimplePlugin_l377) begin
      IBusSimplePlugin_rspJoin_fetchRsp_rsp_error = 1'b0;
    end
  end

  assign IBusSimplePlugin_rspJoin_fetchRsp_rsp_inst = IBusSimplePlugin_rspJoin_rspBuffer_output_payload_inst;
  assign when_IBusSimplePlugin_l377 = (! IBusSimplePlugin_rspJoin_rspBuffer_output_valid);
  always @(*) begin
    IBusSimplePlugin_rspJoin_exceptionDetected = 1'b0;
    if(when_IBusSimplePlugin_l403) begin
      IBusSimplePlugin_rspJoin_exceptionDetected = 1'b1;
    end
  end

  assign IBusSimplePlugin_rspJoin_join_valid = (IBusSimplePlugin_iBusRsp_stages_1_output_valid && IBusSimplePlugin_rspJoin_rspBuffer_output_valid);
  assign IBusSimplePlugin_rspJoin_join_payload_pc = IBusSimplePlugin_rspJoin_fetchRsp_pc;
  assign IBusSimplePlugin_rspJoin_join_payload_rsp_error = IBusSimplePlugin_rspJoin_fetchRsp_rsp_error;
  assign IBusSimplePlugin_rspJoin_join_payload_rsp_inst = IBusSimplePlugin_rspJoin_fetchRsp_rsp_inst;
  assign IBusSimplePlugin_rspJoin_join_payload_isRvc = IBusSimplePlugin_rspJoin_fetchRsp_isRvc;
  assign IBusSimplePlugin_rspJoin_join_fire = (IBusSimplePlugin_rspJoin_join_valid && IBusSimplePlugin_rspJoin_join_ready);
  assign IBusSimplePlugin_iBusRsp_stages_1_output_ready = (IBusSimplePlugin_iBusRsp_stages_1_output_valid ? IBusSimplePlugin_rspJoin_join_fire : IBusSimplePlugin_rspJoin_join_ready);
  assign IBusSimplePlugin_rspJoin_rspBuffer_output_ready = IBusSimplePlugin_rspJoin_join_fire;
  assign _zz_IBusSimplePlugin_iBusRsp_output_valid = (! IBusSimplePlugin_rspJoin_exceptionDetected);
  assign IBusSimplePlugin_rspJoin_join_ready = (IBusSimplePlugin_iBusRsp_output_ready && _zz_IBusSimplePlugin_iBusRsp_output_valid);
  assign IBusSimplePlugin_iBusRsp_output_valid = (IBusSimplePlugin_rspJoin_join_valid && _zz_IBusSimplePlugin_iBusRsp_output_valid);
  assign IBusSimplePlugin_iBusRsp_output_payload_pc = IBusSimplePlugin_rspJoin_join_payload_pc;
  assign IBusSimplePlugin_iBusRsp_output_payload_rsp_error = IBusSimplePlugin_rspJoin_join_payload_rsp_error;
  assign IBusSimplePlugin_iBusRsp_output_payload_rsp_inst = IBusSimplePlugin_rspJoin_join_payload_rsp_inst;
  assign IBusSimplePlugin_iBusRsp_output_payload_isRvc = IBusSimplePlugin_rspJoin_join_payload_isRvc;
  always @(*) begin
    IBusSimplePlugin_decodeExceptionPort_payload_code = 4'bxxxx;
    if(when_IBusSimplePlugin_l403) begin
      IBusSimplePlugin_decodeExceptionPort_payload_code = 4'b0001;
    end
  end

  assign IBusSimplePlugin_decodeExceptionPort_payload_badAddr = {IBusSimplePlugin_rspJoin_join_payload_pc[31 : 2],2'b00};
  assign when_IBusSimplePlugin_l403 = (IBusSimplePlugin_rspJoin_join_valid && IBusSimplePlugin_rspJoin_join_payload_rsp_error);
  assign IBusSimplePlugin_decodeExceptionPort_valid = (IBusSimplePlugin_rspJoin_exceptionDetected && IBusSimplePlugin_iBusRsp_readyForError);
  assign dBus_cmd_valid = dataCache_1_io_mem_cmd_valid;
  assign dBus_cmd_payload_wr = dataCache_1_io_mem_cmd_payload_wr;
  assign dBus_cmd_payload_uncached = dataCache_1_io_mem_cmd_payload_uncached;
  assign dBus_cmd_payload_address = dataCache_1_io_mem_cmd_payload_address;
  assign dBus_cmd_payload_data = dataCache_1_io_mem_cmd_payload_data;
  assign dBus_cmd_payload_mask = dataCache_1_io_mem_cmd_payload_mask;
  assign dBus_cmd_payload_size = dataCache_1_io_mem_cmd_payload_size;
  assign dBus_cmd_payload_last = dataCache_1_io_mem_cmd_payload_last;
  assign when_DBusCachedPlugin_l353 = ((DBusCachedPlugin_mmuBus_busy && decode_arbitration_isValid) && decode_MEMORY_ENABLE);
  assign execute_DBusCachedPlugin_size = execute_INSTRUCTION[13 : 12];
  assign dataCache_1_io_cpu_execute_isValid = (execute_arbitration_isValid && execute_MEMORY_ENABLE);
  assign dataCache_1_io_cpu_execute_address = execute_SRC_ADD;
  always @(*) begin
    case(execute_DBusCachedPlugin_size)
      2'b00 : begin
        _zz_execute_MEMORY_STORE_DATA_RF = {{{execute_RS2[7 : 0],execute_RS2[7 : 0]},execute_RS2[7 : 0]},execute_RS2[7 : 0]};
      end
      2'b01 : begin
        _zz_execute_MEMORY_STORE_DATA_RF = {execute_RS2[15 : 0],execute_RS2[15 : 0]};
      end
      default : begin
        _zz_execute_MEMORY_STORE_DATA_RF = execute_RS2[31 : 0];
      end
    endcase
  end

  assign dataCache_1_io_cpu_flush_valid = (execute_arbitration_isValid && execute_MEMORY_MANAGMENT);
  assign dataCache_1_io_cpu_flush_payload_singleLine = (execute_INSTRUCTION[19 : 15] != 5'h0);
  assign toplevel_dataCache_1_io_cpu_flush_isStall = (dataCache_1_io_cpu_flush_valid && (! dataCache_1_io_cpu_flush_ready));
  assign when_DBusCachedPlugin_l395 = (toplevel_dataCache_1_io_cpu_flush_isStall || dataCache_1_io_cpu_execute_haltIt);
  assign when_DBusCachedPlugin_l411 = (dataCache_1_io_cpu_execute_refilling && execute_arbitration_isValid);
  assign dataCache_1_io_cpu_memory_isValid = (memory_arbitration_isValid && memory_MEMORY_ENABLE);
  assign dataCache_1_io_cpu_memory_address = memory_REGFILE_WRITE_DATA;
  assign DBusCachedPlugin_mmuBus_cmd_0_isValid = dataCache_1_io_cpu_memory_isValid;
  assign DBusCachedPlugin_mmuBus_cmd_0_isStuck = memory_arbitration_isStuck;
  assign DBusCachedPlugin_mmuBus_cmd_0_virtualAddress = dataCache_1_io_cpu_memory_address;
  assign DBusCachedPlugin_mmuBus_cmd_0_bypassTranslation = 1'b0;
  assign DBusCachedPlugin_mmuBus_end = ((! memory_arbitration_isStuck) || memory_arbitration_removeIt);
  always @(*) begin
    dataCache_1_io_cpu_memory_mmuRsp_isIoAccess = DBusCachedPlugin_mmuBus_rsp_isIoAccess;
    if(when_DBusCachedPlugin_l473) begin
      dataCache_1_io_cpu_memory_mmuRsp_isIoAccess = 1'b1;
    end
  end

  assign when_DBusCachedPlugin_l473 = (1'b0 && (! dataCache_1_io_cpu_memory_isWrite));
  always @(*) begin
    dataCache_1_io_cpu_writeBack_isValid = (writeBack_arbitration_isValid && writeBack_MEMORY_ENABLE);
    if(writeBack_arbitration_haltByOther) begin
      dataCache_1_io_cpu_writeBack_isValid = 1'b0;
    end
  end

  assign dataCache_1_io_cpu_writeBack_isUser = (CsrPlugin_privilege == 2'b00);
  assign dataCache_1_io_cpu_writeBack_address = writeBack_REGFILE_WRITE_DATA;
  always @(*) begin
    dataCache_1_io_cpu_writeBack_storeData[31 : 0] = writeBack_MEMORY_STORE_DATA_RF;
    if(DBusBypass0_cond) begin
      dataCache_1_io_cpu_writeBack_storeData[31 : 0] = DBusBypass0_value;
    end
  end

  always @(*) begin
    DBusCachedPlugin_redoBranch_valid = 1'b0;
    if(when_DBusCachedPlugin_l534) begin
      if(dataCache_1_io_cpu_redo) begin
        DBusCachedPlugin_redoBranch_valid = 1'b1;
      end
    end
  end

  assign DBusCachedPlugin_redoBranch_payload = writeBack_PC;
  always @(*) begin
    DBusCachedPlugin_exceptionBus_valid = 1'b0;
    if(when_DBusCachedPlugin_l534) begin
      if(dataCache_1_io_cpu_writeBack_accessError) begin
        DBusCachedPlugin_exceptionBus_valid = 1'b1;
      end
      if(dataCache_1_io_cpu_writeBack_mmuException) begin
        DBusCachedPlugin_exceptionBus_valid = 1'b1;
      end
      if(dataCache_1_io_cpu_writeBack_unalignedAccess) begin
        DBusCachedPlugin_exceptionBus_valid = 1'b1;
      end
      if(dataCache_1_io_cpu_redo) begin
        DBusCachedPlugin_exceptionBus_valid = 1'b0;
      end
    end
  end

  assign DBusCachedPlugin_exceptionBus_payload_badAddr = writeBack_REGFILE_WRITE_DATA;
  always @(*) begin
    DBusCachedPlugin_exceptionBus_payload_code = 4'bxxxx;
    if(when_DBusCachedPlugin_l534) begin
      if(dataCache_1_io_cpu_writeBack_accessError) begin
        DBusCachedPlugin_exceptionBus_payload_code = {1'd0, _zz_DBusCachedPlugin_exceptionBus_payload_code};
      end
      if(dataCache_1_io_cpu_writeBack_mmuException) begin
        DBusCachedPlugin_exceptionBus_payload_code = (writeBack_MEMORY_WR ? 4'b1111 : 4'b1101);
      end
      if(dataCache_1_io_cpu_writeBack_unalignedAccess) begin
        DBusCachedPlugin_exceptionBus_payload_code = {1'd0, _zz_DBusCachedPlugin_exceptionBus_payload_code_1};
      end
    end
  end

  assign when_DBusCachedPlugin_l534 = (writeBack_arbitration_isValid && writeBack_MEMORY_ENABLE);
  assign when_DBusCachedPlugin_l554 = (dataCache_1_io_cpu_writeBack_isValid && dataCache_1_io_cpu_writeBack_haltIt);
  assign writeBack_DBusCachedPlugin_rspData = dataCache_1_io_cpu_writeBack_data;
  assign writeBack_DBusCachedPlugin_rspSplits_0 = writeBack_DBusCachedPlugin_rspData[7 : 0];
  assign writeBack_DBusCachedPlugin_rspSplits_1 = writeBack_DBusCachedPlugin_rspData[15 : 8];
  assign writeBack_DBusCachedPlugin_rspSplits_2 = writeBack_DBusCachedPlugin_rspData[23 : 16];
  assign writeBack_DBusCachedPlugin_rspSplits_3 = writeBack_DBusCachedPlugin_rspData[31 : 24];
  always @(*) begin
    writeBack_DBusCachedPlugin_rspShifted[7 : 0] = _zz_writeBack_DBusCachedPlugin_rspShifted;
    writeBack_DBusCachedPlugin_rspShifted[15 : 8] = _zz_writeBack_DBusCachedPlugin_rspShifted_2;
    writeBack_DBusCachedPlugin_rspShifted[23 : 16] = writeBack_DBusCachedPlugin_rspSplits_2;
    writeBack_DBusCachedPlugin_rspShifted[31 : 24] = writeBack_DBusCachedPlugin_rspSplits_3;
  end

  assign writeBack_DBusCachedPlugin_rspRf = writeBack_DBusCachedPlugin_rspShifted[31 : 0];
  assign switch_Misc_l241_2 = writeBack_INSTRUCTION[13 : 12];
  assign _zz_writeBack_DBusCachedPlugin_rspFormated = (writeBack_DBusCachedPlugin_rspRf[7] && (! writeBack_INSTRUCTION[14]));
  always @(*) begin
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[31] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[30] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[29] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[28] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[27] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[26] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[25] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[24] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[23] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[22] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[21] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[20] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[19] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[18] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[17] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[16] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[15] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[14] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[13] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[12] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[11] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[10] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[9] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[8] = _zz_writeBack_DBusCachedPlugin_rspFormated;
    _zz_writeBack_DBusCachedPlugin_rspFormated_1[7 : 0] = writeBack_DBusCachedPlugin_rspRf[7 : 0];
  end

  assign _zz_writeBack_DBusCachedPlugin_rspFormated_2 = (writeBack_DBusCachedPlugin_rspRf[15] && (! writeBack_INSTRUCTION[14]));
  always @(*) begin
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[31] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[30] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[29] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[28] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[27] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[26] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[25] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[24] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[23] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[22] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[21] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[20] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[19] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[18] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[17] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[16] = _zz_writeBack_DBusCachedPlugin_rspFormated_2;
    _zz_writeBack_DBusCachedPlugin_rspFormated_3[15 : 0] = writeBack_DBusCachedPlugin_rspRf[15 : 0];
  end

  always @(*) begin
    case(switch_Misc_l241_2)
      2'b00 : begin
        writeBack_DBusCachedPlugin_rspFormated = _zz_writeBack_DBusCachedPlugin_rspFormated_1;
      end
      2'b01 : begin
        writeBack_DBusCachedPlugin_rspFormated = _zz_writeBack_DBusCachedPlugin_rspFormated_3;
      end
      default : begin
        writeBack_DBusCachedPlugin_rspFormated = writeBack_DBusCachedPlugin_rspRf;
      end
    endcase
  end

  assign when_DBusCachedPlugin_l581 = (writeBack_arbitration_isValid && writeBack_MEMORY_ENABLE);
  assign DBusCachedPlugin_mmuBus_rsp_physicalAddress = DBusCachedPlugin_mmuBus_cmd_0_virtualAddress;
  assign DBusCachedPlugin_mmuBus_rsp_allowRead = 1'b1;
  assign DBusCachedPlugin_mmuBus_rsp_allowWrite = 1'b1;
  assign DBusCachedPlugin_mmuBus_rsp_allowExecute = 1'b1;
  assign DBusCachedPlugin_mmuBus_rsp_isIoAccess = 1'b1;
  assign DBusCachedPlugin_mmuBus_rsp_isPaging = 1'b0;
  assign DBusCachedPlugin_mmuBus_rsp_exception = 1'b0;
  assign DBusCachedPlugin_mmuBus_rsp_refilling = 1'b0;
  assign DBusCachedPlugin_mmuBus_busy = 1'b0;
  always @(*) begin
    when_CsrPlugin_l818 = 1'b0;
    if(CsrPlugin_trigger_decodeBreak_enabled) begin
      if(CsrPlugin_trigger_decodeBreak_timeout_state) begin
        when_CsrPlugin_l818 = 1'b1;
      end
    end
    if(when_CsrPlugin_l1390) begin
      when_CsrPlugin_l818 = 1'b1;
    end
  end

  always @(*) begin
    CsrPlugin_privilege = _zz_CsrPlugin_privilege;
    if(CsrPlugin_forceMachineWire) begin
      CsrPlugin_privilege = 2'b11;
    end
  end

  assign debugMode = (! CsrPlugin_running);
  assign when_CsrPlugin_l711 = (! CsrPlugin_running);
  always @(*) begin
    debugBus_resume_rsp_valid = 1'b0;
    if(CsrPlugin_doResume) begin
      debugBus_resume_rsp_valid = 1'b1;
    end
  end

  assign debugBus_running = CsrPlugin_running;
  assign debugBus_halted = (! CsrPlugin_running);
  assign debugBus_unavailable = reset_buffercc_io_dataOut;
  assign debugBus_haveReset = _zz_debugBus_haveReset;
  assign CsrPlugin_enterHalt = ((! CsrPlugin_running_aheadValue) && CsrPlugin_running_aheadValue_regNext);
  assign when_CsrPlugin_l729 = ((debugBus_haltReq && debugBus_running) && (! debugMode));
  assign CsrPlugin_forceResume = 1'b0;
  assign CsrPlugin_doResume = (CsrPlugin_forceResume || _zz_CsrPlugin_doResume);
  always @(*) begin
    CsrPlugin_timeout_stateRise = 1'b0;
    if(CsrPlugin_timeout_counter_willOverflow) begin
      CsrPlugin_timeout_stateRise = (! CsrPlugin_timeout_state);
    end
    if(when_CsrPlugin_l735) begin
      CsrPlugin_timeout_stateRise = 1'b0;
    end
    if(CsrPlugin_inject_cmd_valid) begin
      CsrPlugin_timeout_stateRise = 1'b0;
    end
    case(CsrPlugin_dcsr_stepLogic_stateReg)
      CsrPlugin_dcsr_stepLogic_enumDef_IDLE : begin
      end
      CsrPlugin_dcsr_stepLogic_enumDef_SINGLE : begin
        CsrPlugin_timeout_stateRise = 1'b0;
      end
      CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1 : begin
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    CsrPlugin_timeout_counter_willClear = 1'b0;
    if(when_CsrPlugin_l735) begin
      CsrPlugin_timeout_counter_willClear = 1'b1;
    end
    if(CsrPlugin_inject_cmd_valid) begin
      CsrPlugin_timeout_counter_willClear = 1'b1;
    end
    case(CsrPlugin_dcsr_stepLogic_stateReg)
      CsrPlugin_dcsr_stepLogic_enumDef_IDLE : begin
      end
      CsrPlugin_dcsr_stepLogic_enumDef_SINGLE : begin
        CsrPlugin_timeout_counter_willClear = 1'b1;
      end
      CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1 : begin
      end
      default : begin
      end
    endcase
  end

  assign CsrPlugin_timeout_counter_willOverflowIfInc = (CsrPlugin_timeout_counter_value == 3'b110);
  assign CsrPlugin_timeout_counter_willOverflow = (CsrPlugin_timeout_counter_willOverflowIfInc && CsrPlugin_timeout_counter_willIncrement);
  always @(*) begin
    if(CsrPlugin_timeout_counter_willOverflow) begin
      CsrPlugin_timeout_counter_valueNext = 3'b000;
    end else begin
      CsrPlugin_timeout_counter_valueNext = (CsrPlugin_timeout_counter_value + _zz_CsrPlugin_timeout_counter_valueNext);
    end
    if(CsrPlugin_timeout_counter_willClear) begin
      CsrPlugin_timeout_counter_valueNext = 3'b000;
    end
  end

  assign CsrPlugin_timeout_counter_willIncrement = 1'b1;
  assign when_CsrPlugin_l735 = (|{writeBack_arbitration_isValid,{memory_arbitration_isValid,execute_arbitration_isValid}});
  always @(*) begin
    _zz_debugBus_hartToDm_valid = 1'b0;
    if(execute_CsrPlugin_csr_1972) begin
      if(execute_CsrPlugin_writeEnable) begin
        _zz_debugBus_hartToDm_valid = 1'b1;
      end
    end
  end

  assign debugBus_hartToDm_valid = _zz_debugBus_hartToDm_valid;
  assign debugBus_hartToDm_payload_address = 4'b0000;
  assign debugBus_hartToDm_payload_data = execute_SRC1;
  assign when_CsrPlugin_l750 = (debugBus_dmToHart_valid && (debugBus_dmToHart_payload_op == DebugDmToHartOp_DATA));
  assign CsrPlugin_inject_cmd_valid = (debugBus_dmToHart_valid && (((debugBus_dmToHart_payload_op == DebugDmToHartOp_EXECUTE) || (debugBus_dmToHart_payload_op == DebugDmToHartOp_REG_READ)) || (debugBus_dmToHart_payload_op == DebugDmToHartOp_REG_WRITE)));
  assign CsrPlugin_inject_cmd_payload_op = debugBus_dmToHart_payload_op;
  assign CsrPlugin_inject_cmd_payload_address = debugBus_dmToHart_payload_address;
  assign CsrPlugin_inject_cmd_payload_data = debugBus_dmToHart_payload_data;
  assign CsrPlugin_inject_cmd_payload_size = debugBus_dmToHart_payload_size;
  assign CsrPlugin_inject_cmd_toStream_valid = CsrPlugin_inject_cmd_valid;
  assign CsrPlugin_inject_cmd_toStream_payload_op = CsrPlugin_inject_cmd_payload_op;
  assign CsrPlugin_inject_cmd_toStream_payload_address = CsrPlugin_inject_cmd_payload_address;
  assign CsrPlugin_inject_cmd_toStream_payload_data = CsrPlugin_inject_cmd_payload_data;
  assign CsrPlugin_inject_cmd_toStream_payload_size = CsrPlugin_inject_cmd_payload_size;
  always @(*) begin
    CsrPlugin_inject_cmd_toStream_ready = CsrPlugin_inject_buffer_ready;
    if(when_Stream_l375) begin
      CsrPlugin_inject_cmd_toStream_ready = 1'b1;
    end
  end

  assign when_Stream_l375 = (! CsrPlugin_inject_buffer_valid);
  assign CsrPlugin_inject_buffer_valid = CsrPlugin_inject_cmd_toStream_rValid;
  assign CsrPlugin_inject_buffer_payload_op = CsrPlugin_inject_cmd_toStream_rData_op;
  assign CsrPlugin_inject_buffer_payload_address = CsrPlugin_inject_cmd_toStream_rData_address;
  assign CsrPlugin_inject_buffer_payload_data = CsrPlugin_inject_cmd_toStream_rData_data;
  assign CsrPlugin_inject_buffer_payload_size = CsrPlugin_inject_cmd_toStream_rData_size;
  assign CsrPlugin_injectionPort_valid = (CsrPlugin_inject_buffer_valid && (CsrPlugin_inject_buffer_payload_op == DebugDmToHartOp_EXECUTE));
  assign CsrPlugin_injectionPort_payload = CsrPlugin_inject_buffer_payload_data;
  assign CsrPlugin_injectionPort_fire = (CsrPlugin_injectionPort_valid && CsrPlugin_injectionPort_ready);
  assign CsrPlugin_inject_buffer_ready = CsrPlugin_injectionPort_fire;
  assign debugBus_regSuccess = 1'b0;
  assign when_CsrPlugin_l786 = (CsrPlugin_inject_cmd_valid && (debugBus_dmToHart_payload_op == DebugDmToHartOp_EXECUTE));
  assign when_CsrPlugin_l786_1 = (((debugBus_exception || debugBus_commit) || debugBus_ebreak) || debugBus_redo);
  assign debugBus_redo = (CsrPlugin_inject_pending && CsrPlugin_timeout_state);
  assign CsrPlugin_dcsr_nmip = 1'b0;
  assign CsrPlugin_dcsr_mprven = 1'b1;
  assign CsrPlugin_dcsr_xdebugver = 4'b0100;
  assign CsrPlugin_dcsr_stepLogic_wantExit = 1'b0;
  always @(*) begin
    CsrPlugin_dcsr_stepLogic_wantStart = 1'b0;
    case(CsrPlugin_dcsr_stepLogic_stateReg)
      CsrPlugin_dcsr_stepLogic_enumDef_IDLE : begin
      end
      CsrPlugin_dcsr_stepLogic_enumDef_SINGLE : begin
      end
      CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1 : begin
      end
      default : begin
        CsrPlugin_dcsr_stepLogic_wantStart = 1'b1;
      end
    endcase
  end

  assign CsrPlugin_dcsr_stepLogic_wantKill = 1'b0;
  always @(*) begin
    CsrPlugin_dcsr_stepLogic_stateNext = CsrPlugin_dcsr_stepLogic_stateReg;
    case(CsrPlugin_dcsr_stepLogic_stateReg)
      CsrPlugin_dcsr_stepLogic_enumDef_IDLE : begin
        if(when_CsrPlugin_l812) begin
          CsrPlugin_dcsr_stepLogic_stateNext = CsrPlugin_dcsr_stepLogic_enumDef_SINGLE;
        end
      end
      CsrPlugin_dcsr_stepLogic_enumDef_SINGLE : begin
        if(when_CsrPlugin_l818) begin
          CsrPlugin_dcsr_stepLogic_stateNext = CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1;
        end
        if(decode_arbitration_isFiring) begin
          CsrPlugin_dcsr_stepLogic_stateNext = CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1;
        end
      end
      CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1 : begin
        if(when_CsrPlugin_l830) begin
          CsrPlugin_dcsr_stepLogic_stateNext = CsrPlugin_dcsr_stepLogic_enumDef_SINGLE;
        end
      end
      default : begin
      end
    endcase
    if(CsrPlugin_enterHalt) begin
      CsrPlugin_dcsr_stepLogic_stateNext = CsrPlugin_dcsr_stepLogic_enumDef_IDLE;
    end
    if(CsrPlugin_dcsr_stepLogic_wantStart) begin
      CsrPlugin_dcsr_stepLogic_stateNext = CsrPlugin_dcsr_stepLogic_enumDef_IDLE;
    end
    if(CsrPlugin_dcsr_stepLogic_wantKill) begin
      CsrPlugin_dcsr_stepLogic_stateNext = CsrPlugin_dcsr_stepLogic_enumDef_BOOT;
    end
  end

  assign when_CsrPlugin_l812 = (CsrPlugin_dcsr_step && debugBus_resume_rsp_valid);
  assign when_CsrPlugin_l830 = ((! CsrPlugin_doHalt) && CsrPlugin_timeout_state);
  assign when_CsrPlugin_l862 = ((debugMode || CsrPlugin_dcsr_step) || debugBus_haltReq);
  assign CsrPlugin_trigger_tselect_outOfRange = 1'b0;
  always @(*) begin
    CsrPlugin_trigger_decodeBreak_enabled = 1'b0;
    if(CsrPlugin_trigger_slots_0_tdata2_execute_hit) begin
      CsrPlugin_trigger_decodeBreak_enabled = 1'b1;
    end
    if(CsrPlugin_trigger_slots_1_tdata2_execute_hit) begin
      CsrPlugin_trigger_decodeBreak_enabled = 1'b1;
    end
    if(when_CsrPlugin_l958) begin
      CsrPlugin_trigger_decodeBreak_enabled = 1'b0;
    end
  end

  always @(*) begin
    CsrPlugin_trigger_decodeBreak_timeout_stateRise = 1'b0;
    if(CsrPlugin_trigger_decodeBreak_timeout_counter_willOverflow) begin
      CsrPlugin_trigger_decodeBreak_timeout_stateRise = (! CsrPlugin_trigger_decodeBreak_timeout_state);
    end
    if(when_Utils_l669) begin
      CsrPlugin_trigger_decodeBreak_timeout_stateRise = 1'b0;
    end
  end

  always @(*) begin
    CsrPlugin_trigger_decodeBreak_timeout_counter_willClear = 1'b0;
    if(when_Utils_l669) begin
      CsrPlugin_trigger_decodeBreak_timeout_counter_willClear = 1'b1;
    end
  end

  assign CsrPlugin_trigger_decodeBreak_timeout_counter_willOverflowIfInc = (CsrPlugin_trigger_decodeBreak_timeout_counter_value == 2'b10);
  assign CsrPlugin_trigger_decodeBreak_timeout_counter_willOverflow = (CsrPlugin_trigger_decodeBreak_timeout_counter_willOverflowIfInc && CsrPlugin_trigger_decodeBreak_timeout_counter_willIncrement);
  always @(*) begin
    if(CsrPlugin_trigger_decodeBreak_timeout_counter_willOverflow) begin
      CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext = 2'b00;
    end else begin
      CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext = (CsrPlugin_trigger_decodeBreak_timeout_counter_value + _zz_CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext);
    end
    if(CsrPlugin_trigger_decodeBreak_timeout_counter_willClear) begin
      CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext = 2'b00;
    end
  end

  assign CsrPlugin_trigger_decodeBreak_timeout_counter_willIncrement = 1'b1;
  assign when_Utils_l669 = ((! CsrPlugin_trigger_decodeBreak_enabled) || (|{writeBack_arbitration_isValid,{memory_arbitration_isValid,execute_arbitration_isValid}}));
  assign CsrPlugin_trigger_slots_0_selected = (CsrPlugin_trigger_tselect_index == 1'b0);
  always @(*) begin
    CsrPlugin_trigger_slots_0_tdata1_read = 32'h0;
    if(CsrPlugin_trigger_slots_0_selected) begin
      CsrPlugin_trigger_slots_0_tdata1_read[2 : 2] = CsrPlugin_trigger_slots_0_tdata1_execute;
      CsrPlugin_trigger_slots_0_tdata1_read[3 : 3] = CsrPlugin_trigger_slots_0_tdata1_u;
      CsrPlugin_trigger_slots_0_tdata1_read[4 : 4] = CsrPlugin_trigger_slots_0_tdata1_s;
      CsrPlugin_trigger_slots_0_tdata1_read[6 : 6] = CsrPlugin_trigger_slots_0_tdata1_m;
      CsrPlugin_trigger_slots_0_tdata1_read[27 : 27] = CsrPlugin_trigger_slots_0_tdata1_dmode;
      CsrPlugin_trigger_slots_0_tdata1_read[15 : 12] = CsrPlugin_trigger_slots_0_tdata1_action;
    end
    if(CsrPlugin_trigger_slots_0_selected) begin
      CsrPlugin_trigger_slots_0_tdata1_read[31 : 28] = CsrPlugin_trigger_slots_0_tdata1_tpe;
    end
    if(CsrPlugin_trigger_slots_0_selected) begin
      CsrPlugin_trigger_slots_0_tdata1_read[25 : 20] = 6'h1f;
    end
  end

  assign CsrPlugin_trigger_slots_0_tdata1_tpe = 4'b0010;
  always @(*) begin
    case(CsrPlugin_privilege)
      2'b00 : begin
        _zz_CsrPlugin_trigger_slots_0_tdata1_privilegeHit = CsrPlugin_trigger_slots_0_tdata1_u;
      end
      2'b01 : begin
        _zz_CsrPlugin_trigger_slots_0_tdata1_privilegeHit = CsrPlugin_trigger_slots_0_tdata1_s;
      end
      2'b11 : begin
        _zz_CsrPlugin_trigger_slots_0_tdata1_privilegeHit = CsrPlugin_trigger_slots_0_tdata1_m;
      end
      default : begin
        _zz_CsrPlugin_trigger_slots_0_tdata1_privilegeHit = 1'b0;
      end
    endcase
  end

  assign CsrPlugin_trigger_slots_0_tdata1_privilegeHit = ((! debugMode) && _zz_CsrPlugin_trigger_slots_0_tdata1_privilegeHit);
  assign CsrPlugin_trigger_slots_0_tdata2_execute_enabled = ((((! debugMode) && (CsrPlugin_trigger_slots_0_tdata1_action == 4'b0001)) && CsrPlugin_trigger_slots_0_tdata1_execute) && CsrPlugin_trigger_slots_0_tdata1_privilegeHit);
  assign CsrPlugin_trigger_slots_0_tdata2_execute_hit = (CsrPlugin_trigger_slots_0_tdata2_execute_enabled && (CsrPlugin_trigger_slots_0_tdata2_value == decode_PC));
  assign CsrPlugin_trigger_slots_1_selected = (CsrPlugin_trigger_tselect_index == 1'b1);
  always @(*) begin
    CsrPlugin_trigger_slots_1_tdata1_read = 32'h0;
    if(CsrPlugin_trigger_slots_1_selected) begin
      CsrPlugin_trigger_slots_1_tdata1_read[2 : 2] = CsrPlugin_trigger_slots_1_tdata1_execute;
      CsrPlugin_trigger_slots_1_tdata1_read[3 : 3] = CsrPlugin_trigger_slots_1_tdata1_u;
      CsrPlugin_trigger_slots_1_tdata1_read[4 : 4] = CsrPlugin_trigger_slots_1_tdata1_s;
      CsrPlugin_trigger_slots_1_tdata1_read[6 : 6] = CsrPlugin_trigger_slots_1_tdata1_m;
      CsrPlugin_trigger_slots_1_tdata1_read[27 : 27] = CsrPlugin_trigger_slots_1_tdata1_dmode;
      CsrPlugin_trigger_slots_1_tdata1_read[15 : 12] = CsrPlugin_trigger_slots_1_tdata1_action;
    end
    if(CsrPlugin_trigger_slots_1_selected) begin
      CsrPlugin_trigger_slots_1_tdata1_read[31 : 28] = CsrPlugin_trigger_slots_1_tdata1_tpe;
    end
    if(CsrPlugin_trigger_slots_1_selected) begin
      CsrPlugin_trigger_slots_1_tdata1_read[25 : 20] = 6'h1f;
    end
  end

  assign CsrPlugin_trigger_slots_1_tdata1_tpe = 4'b0010;
  always @(*) begin
    case(CsrPlugin_privilege)
      2'b00 : begin
        _zz_CsrPlugin_trigger_slots_1_tdata1_privilegeHit = CsrPlugin_trigger_slots_1_tdata1_u;
      end
      2'b01 : begin
        _zz_CsrPlugin_trigger_slots_1_tdata1_privilegeHit = CsrPlugin_trigger_slots_1_tdata1_s;
      end
      2'b11 : begin
        _zz_CsrPlugin_trigger_slots_1_tdata1_privilegeHit = CsrPlugin_trigger_slots_1_tdata1_m;
      end
      default : begin
        _zz_CsrPlugin_trigger_slots_1_tdata1_privilegeHit = 1'b0;
      end
    endcase
  end

  assign CsrPlugin_trigger_slots_1_tdata1_privilegeHit = ((! debugMode) && _zz_CsrPlugin_trigger_slots_1_tdata1_privilegeHit);
  assign CsrPlugin_trigger_slots_1_tdata2_execute_enabled = ((((! debugMode) && (CsrPlugin_trigger_slots_1_tdata1_action == 4'b0001)) && CsrPlugin_trigger_slots_1_tdata1_execute) && CsrPlugin_trigger_slots_1_tdata1_privilegeHit);
  assign CsrPlugin_trigger_slots_1_tdata2_execute_hit = (CsrPlugin_trigger_slots_1_tdata2_execute_enabled && (CsrPlugin_trigger_slots_1_tdata2_value == decode_PC));
  assign when_CsrPlugin_l958 = (! decode_arbitration_isValid);
  assign CsrPlugin_misa_base = 2'b01;
  assign CsrPlugin_misa_extensions = 26'h0000042;
  assign CsrPlugin_mtvec_mode = 2'b00;
  assign _zz_when_CsrPlugin_l1302 = (CsrPlugin_mip_MTIP && CsrPlugin_mie_MTIE);
  assign _zz_when_CsrPlugin_l1302_1 = (CsrPlugin_mip_MSIP && CsrPlugin_mie_MSIE);
  assign _zz_when_CsrPlugin_l1302_2 = (CsrPlugin_mip_MEIP && CsrPlugin_mie_MEIE);
  assign CsrPlugin_exceptionPortCtrl_exceptionTargetPrivilegeUncapped = 2'b11;
  assign CsrPlugin_exceptionPortCtrl_exceptionTargetPrivilege = ((CsrPlugin_privilege < CsrPlugin_exceptionPortCtrl_exceptionTargetPrivilegeUncapped) ? CsrPlugin_exceptionPortCtrl_exceptionTargetPrivilegeUncapped : CsrPlugin_privilege);
  assign _zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code = {decodeExceptionPort_valid,IBusSimplePlugin_decodeExceptionPort_valid};
  assign _zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code_1 = _zz__zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code_1[0];
  always @(*) begin
    CsrPlugin_exceptionPortCtrl_exceptionValids_decode = CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_decode;
    if(_zz_when_1) begin
      CsrPlugin_exceptionPortCtrl_exceptionValids_decode = 1'b1;
    end
    if(decode_arbitration_isFlushed) begin
      CsrPlugin_exceptionPortCtrl_exceptionValids_decode = 1'b0;
    end
  end

  always @(*) begin
    CsrPlugin_exceptionPortCtrl_exceptionValids_execute = CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_execute;
    if(CsrPlugin_selfException_valid) begin
      CsrPlugin_exceptionPortCtrl_exceptionValids_execute = 1'b1;
    end
    if(execute_arbitration_isFlushed) begin
      CsrPlugin_exceptionPortCtrl_exceptionValids_execute = 1'b0;
    end
  end

  always @(*) begin
    CsrPlugin_exceptionPortCtrl_exceptionValids_memory = CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_memory;
    if(memory_arbitration_isFlushed) begin
      CsrPlugin_exceptionPortCtrl_exceptionValids_memory = 1'b0;
    end
  end

  always @(*) begin
    CsrPlugin_exceptionPortCtrl_exceptionValids_writeBack = CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_writeBack;
    if(DBusCachedPlugin_exceptionBus_valid) begin
      CsrPlugin_exceptionPortCtrl_exceptionValids_writeBack = 1'b1;
    end
    if(writeBack_arbitration_isFlushed) begin
      CsrPlugin_exceptionPortCtrl_exceptionValids_writeBack = 1'b0;
    end
  end

  assign when_CsrPlugin_l1259 = (! decode_arbitration_isStuck);
  assign when_CsrPlugin_l1259_1 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1259_2 = (! memory_arbitration_isStuck);
  assign when_CsrPlugin_l1259_3 = (! writeBack_arbitration_isStuck);
  assign when_CsrPlugin_l1272 = (|{CsrPlugin_exceptionPortCtrl_exceptionValids_writeBack,{CsrPlugin_exceptionPortCtrl_exceptionValids_memory,{CsrPlugin_exceptionPortCtrl_exceptionValids_execute,CsrPlugin_exceptionPortCtrl_exceptionValids_decode}}});
  assign CsrPlugin_exceptionPendings_0 = CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_decode;
  assign CsrPlugin_exceptionPendings_1 = CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_execute;
  assign CsrPlugin_exceptionPendings_2 = CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_memory;
  assign CsrPlugin_exceptionPendings_3 = CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_writeBack;
  assign when_CsrPlugin_l1296 = (CsrPlugin_mstatus_MIE || (CsrPlugin_privilege < 2'b11));
  assign when_CsrPlugin_l1302 = ((_zz_when_CsrPlugin_l1302 && 1'b1) && (! 1'b0));
  assign when_CsrPlugin_l1302_1 = ((_zz_when_CsrPlugin_l1302_1 && 1'b1) && (! 1'b0));
  assign when_CsrPlugin_l1302_2 = ((_zz_when_CsrPlugin_l1302_2 && 1'b1) && (! 1'b0));
  assign when_CsrPlugin_l1315 = (CsrPlugin_dcsr_step && (! CsrPlugin_dcsr_stepie));
  assign CsrPlugin_exception = (CsrPlugin_exceptionPortCtrl_exceptionValids_writeBack && CsrPlugin_allowException);
  assign CsrPlugin_lastStageWasWfi = 1'b0;
  assign CsrPlugin_pipelineLiberator_active = ((CsrPlugin_interrupt_valid && CsrPlugin_allowInterrupts) && decode_arbitration_isValid);
  assign when_CsrPlugin_l1335 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1335_1 = (! memory_arbitration_isStuck);
  assign when_CsrPlugin_l1335_2 = (! writeBack_arbitration_isStuck);
  assign when_CsrPlugin_l1340 = ((! CsrPlugin_pipelineLiberator_active) || decode_arbitration_removeIt);
  always @(*) begin
    CsrPlugin_pipelineLiberator_done = CsrPlugin_pipelineLiberator_pcValids_2;
    if(when_CsrPlugin_l1346) begin
      CsrPlugin_pipelineLiberator_done = 1'b0;
    end
    if(CsrPlugin_hadException) begin
      CsrPlugin_pipelineLiberator_done = 1'b0;
    end
  end

  assign when_CsrPlugin_l1346 = (|{CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_writeBack,{CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_memory,CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_execute}});
  assign CsrPlugin_interruptJump = ((CsrPlugin_interrupt_valid && CsrPlugin_pipelineLiberator_done) && CsrPlugin_allowInterrupts);
  assign debugBus_commit = (debugMode && writeBack_arbitration_isFiring);
  always @(*) begin
    debugBus_exception = (debugMode && CsrPlugin_hadException);
    if(when_CsrPlugin_l1390) begin
      if(!when_CsrPlugin_l1398) begin
        if(!when_CsrPlugin_l1428) begin
          debugBus_exception = (! CsrPlugin_trapCauseEbreakDebug);
        end
      end
    end
  end

  always @(*) begin
    debugBus_ebreak = 1'b0;
    if(when_CsrPlugin_l1390) begin
      if(!when_CsrPlugin_l1398) begin
        if(!when_CsrPlugin_l1428) begin
          debugBus_ebreak = CsrPlugin_trapCauseEbreakDebug;
        end
      end
    end
  end

  always @(*) begin
    CsrPlugin_targetPrivilege = CsrPlugin_interrupt_targetPrivilege;
    if(CsrPlugin_hadException) begin
      CsrPlugin_targetPrivilege = CsrPlugin_exceptionPortCtrl_exceptionTargetPrivilege;
    end
  end

  always @(*) begin
    CsrPlugin_trapCause = CsrPlugin_interrupt_code;
    if(CsrPlugin_hadException) begin
      CsrPlugin_trapCause = CsrPlugin_exceptionPortCtrl_exceptionContext_code;
    end
  end

  always @(*) begin
    CsrPlugin_trapCauseEbreakDebug = 1'b0;
    if(CsrPlugin_hadException) begin
      if(when_CsrPlugin_l1373) begin
        if(debugMode) begin
          CsrPlugin_trapCauseEbreakDebug = 1'b1;
        end
        if(when_CsrPlugin_l1375) begin
          CsrPlugin_trapCauseEbreakDebug = 1'b1;
        end
      end
    end
  end

  assign when_CsrPlugin_l1373 = (CsrPlugin_exceptionPortCtrl_exceptionContext_code == 4'b0011);
  assign when_CsrPlugin_l1375 = ((CsrPlugin_privilege == 2'b11) && CsrPlugin_dcsr_ebreakm);
  always @(*) begin
    CsrPlugin_xtvec_mode = 2'bxx;
    case(CsrPlugin_targetPrivilege)
      2'b11 : begin
        CsrPlugin_xtvec_mode = CsrPlugin_mtvec_mode;
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    CsrPlugin_xtvec_base = 30'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    case(CsrPlugin_targetPrivilege)
      2'b11 : begin
        CsrPlugin_xtvec_base = CsrPlugin_mtvec_base;
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    CsrPlugin_trapEnterDebug = 1'b0;
    if(when_CsrPlugin_l1389) begin
      CsrPlugin_trapEnterDebug = 1'b1;
    end
  end

  assign when_CsrPlugin_l1389 = (((CsrPlugin_doHalt || CsrPlugin_trapCauseEbreakDebug) || ((! CsrPlugin_hadException) && CsrPlugin_doHalt)) || (! CsrPlugin_running));
  assign when_CsrPlugin_l1390 = (CsrPlugin_hadException || CsrPlugin_interruptJump);
  assign when_CsrPlugin_l1398 = (! CsrPlugin_trapEnterDebug);
  assign when_CsrPlugin_l1428 = (! debugMode);
  assign when_CsrPlugin_l1456 = (writeBack_arbitration_isValid && (writeBack_ENV_CTRL == EnvCtrlEnum_XRET));
  assign switch_CsrPlugin_l1460 = writeBack_INSTRUCTION[29 : 28];
  assign when_CsrPlugin_l1468 = (CsrPlugin_mstatus_MPP < 2'b11);
  assign contextSwitching = CsrPlugin_jumpInterface_valid;
  assign when_CsrPlugin_l1527 = (|{(writeBack_arbitration_isValid && (writeBack_ENV_CTRL == EnvCtrlEnum_XRET)),{(memory_arbitration_isValid && (memory_ENV_CTRL == EnvCtrlEnum_XRET)),(execute_arbitration_isValid && (execute_ENV_CTRL == EnvCtrlEnum_XRET))}});
  assign execute_CsrPlugin_blockedBySideEffects = ((|{writeBack_arbitration_isValid,memory_arbitration_isValid}) || 1'b0);
  always @(*) begin
    execute_CsrPlugin_illegalAccess = 1'b1;
    if(execute_CsrPlugin_csr_1972) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_1969) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_1968) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_1952) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_1956) begin
      if(execute_CSR_READ_OPCODE) begin
        execute_CsrPlugin_illegalAccess = 1'b0;
      end
    end
    if(execute_CsrPlugin_csr_1953) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_1954) begin
      if(execute_CSR_WRITE_OPCODE) begin
        execute_CsrPlugin_illegalAccess = 1'b0;
      end
    end
    if(execute_CsrPlugin_csr_768) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_836) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_772) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_773) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_834) begin
      if(execute_CSR_READ_OPCODE) begin
        execute_CsrPlugin_illegalAccess = 1'b0;
      end
    end
    if(execute_CsrPlugin_csr_3) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_2) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_1) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(execute_CsrPlugin_csr_256) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(CsrPlugin_csrMapping_allowCsrSignal) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
    if(when_CsrPlugin_l1719) begin
      execute_CsrPlugin_illegalAccess = 1'b1;
    end
    if(when_CsrPlugin_l1725) begin
      execute_CsrPlugin_illegalAccess = 1'b0;
    end
  end

  always @(*) begin
    execute_CsrPlugin_illegalInstruction = 1'b0;
    if(when_CsrPlugin_l1547) begin
      if(when_CsrPlugin_l1548) begin
        execute_CsrPlugin_illegalInstruction = 1'b1;
      end
    end
  end

  always @(*) begin
    CsrPlugin_selfException_valid = 1'b0;
    if(when_CsrPlugin_l1565) begin
      CsrPlugin_selfException_valid = 1'b1;
    end
  end

  always @(*) begin
    CsrPlugin_selfException_payload_code = 4'bxxxx;
    if(when_CsrPlugin_l1565) begin
      CsrPlugin_selfException_payload_code = 4'b0011;
    end
  end

  assign CsrPlugin_selfException_payload_badAddr = execute_INSTRUCTION;
  assign when_CsrPlugin_l1547 = (execute_arbitration_isValid && (execute_ENV_CTRL == EnvCtrlEnum_XRET));
  assign when_CsrPlugin_l1548 = (CsrPlugin_privilege < execute_INSTRUCTION[29 : 28]);
  assign when_CsrPlugin_l1565 = ((execute_arbitration_isValid && (execute_ENV_CTRL == EnvCtrlEnum_EBREAK)) && CsrPlugin_allowEbreakException);
  always @(*) begin
    execute_CsrPlugin_writeInstruction = ((execute_arbitration_isValid && execute_IS_CSR) && execute_CSR_WRITE_OPCODE);
    if(when_CsrPlugin_l1719) begin
      execute_CsrPlugin_writeInstruction = 1'b0;
    end
  end

  always @(*) begin
    execute_CsrPlugin_readInstruction = ((execute_arbitration_isValid && execute_IS_CSR) && execute_CSR_READ_OPCODE);
    if(when_CsrPlugin_l1719) begin
      execute_CsrPlugin_readInstruction = 1'b0;
    end
  end

  assign execute_CsrPlugin_writeEnable = (execute_CsrPlugin_writeInstruction && (! execute_arbitration_isStuck));
  assign execute_CsrPlugin_readEnable = (execute_CsrPlugin_readInstruction && (! execute_arbitration_isStuck));
  assign CsrPlugin_csrMapping_hazardFree = (! execute_CsrPlugin_blockedBySideEffects);
  assign execute_CsrPlugin_readToWriteData = CsrPlugin_csrMapping_readDataSignal;
  assign switch_Misc_l241_3 = execute_INSTRUCTION[13];
  always @(*) begin
    case(switch_Misc_l241_3)
      1'b0 : begin
        _zz_CsrPlugin_csrMapping_writeDataSignal = execute_SRC1;
      end
      default : begin
        _zz_CsrPlugin_csrMapping_writeDataSignal = (execute_INSTRUCTION[12] ? (execute_CsrPlugin_readToWriteData & (~ execute_SRC1)) : (execute_CsrPlugin_readToWriteData | execute_SRC1));
      end
    endcase
  end

  assign CsrPlugin_csrMapping_writeDataSignal = _zz_CsrPlugin_csrMapping_writeDataSignal;
  assign when_CsrPlugin_l1587 = (execute_arbitration_isValid && execute_IS_CSR);
  assign when_CsrPlugin_l1591 = (execute_arbitration_isValid && (execute_IS_CSR || 1'b0));
  assign execute_CsrPlugin_csrAddress = execute_INSTRUCTION[31 : 20];
  assign _zz_decode_FPU_RSP_1 = ((decode_INSTRUCTION & 32'h00004050) == 32'h00004050);
  assign _zz_decode_FPU_RSP_2 = ((decode_INSTRUCTION & 32'h00000014) == 32'h00000014);
  assign _zz_decode_FPU_RSP_3 = ((decode_INSTRUCTION & 32'h00000008) == 32'h00000008);
  assign _zz_decode_FPU_RSP_4 = ((decode_INSTRUCTION & 32'h00000058) == 32'h0);
  assign _zz_decode_FPU_RSP_5 = ((decode_INSTRUCTION & 32'h00002004) == 32'h00000004);
  assign _zz_decode_FPU_RSP_6 = ((decode_INSTRUCTION & 32'h90000010) == 32'h80000010);
  assign _zz_decode_FPU_RSP_7 = ((decode_INSTRUCTION & 32'h00000028) == 32'h00000028);
  assign _zz_decode_FPU_RSP_8 = ((decode_INSTRUCTION & 32'h00000070) == 32'h00000010);
  assign _zz_decode_FPU_RSP_9 = ((decode_INSTRUCTION & 32'h00000004) == 32'h00000004);
  assign _zz_decode_FPU_RSP_10 = ((decode_INSTRUCTION & 32'h00000020) == 32'h00000020);
  assign _zz_decode_FPU_RSP_11 = ((decode_INSTRUCTION & 32'h00001000) == 32'h0);
  assign _zz_decode_FPU_RSP = {(|{_zz_decode_FPU_RSP_7,(_zz__zz_decode_FPU_RSP == _zz__zz_decode_FPU_RSP_1)}),{(|(_zz__zz_decode_FPU_RSP_2 == _zz__zz_decode_FPU_RSP_3)),{(|_zz__zz_decode_FPU_RSP_4),{(|_zz__zz_decode_FPU_RSP_5),{_zz__zz_decode_FPU_RSP_8,{_zz__zz_decode_FPU_RSP_11,_zz__zz_decode_FPU_RSP_19}}}}}};
  assign _zz_decode_SRC1_CTRL_2 = _zz_decode_FPU_RSP[1 : 0];
  assign _zz_decode_SRC1_CTRL_1 = _zz_decode_SRC1_CTRL_2;
  assign _zz_decode_ALU_CTRL_2 = _zz_decode_FPU_RSP[6 : 5];
  assign _zz_decode_ALU_CTRL_1 = _zz_decode_ALU_CTRL_2;
  assign _zz_decode_SRC2_CTRL_2 = _zz_decode_FPU_RSP[8 : 7];
  assign _zz_decode_SRC2_CTRL_1 = _zz_decode_SRC2_CTRL_2;
  assign _zz_decode_ENV_CTRL_2 = _zz_decode_FPU_RSP[18 : 17];
  assign _zz_decode_ENV_CTRL_1 = _zz_decode_ENV_CTRL_2;
  assign _zz_decode_ALU_BITWISE_CTRL_2 = _zz_decode_FPU_RSP[21 : 20];
  assign _zz_decode_ALU_BITWISE_CTRL_1 = _zz_decode_ALU_BITWISE_CTRL_2;
  assign _zz_decode_FPU_ENABLE = _zz_decode_FPU_RSP[27];
  assign _zz_decode_FPU_OPCODE_2 = _zz_decode_FPU_RSP[33 : 30];
  assign _zz_decode_FPU_OPCODE_1 = _zz_decode_FPU_OPCODE_2;
  assign _zz_decode_SHIFT_CTRL_2 = _zz_decode_FPU_RSP[38 : 37];
  assign _zz_decode_SHIFT_CTRL_1 = _zz_decode_SHIFT_CTRL_2;
  assign _zz_decode_BRANCH_CTRL_2 = _zz_decode_FPU_RSP[40 : 39];
  assign _zz_decode_BRANCH_CTRL_1 = _zz_decode_BRANCH_CTRL_2;
  assign decodeExceptionPort_valid = (decode_arbitration_isValid && (! decode_LEGAL_INSTRUCTION));
  assign decodeExceptionPort_payload_code = 4'b0010;
  assign decodeExceptionPort_payload_badAddr = decode_INSTRUCTION;
  assign when_RegFilePlugin_l63 = (decode_INSTRUCTION[11 : 7] == 5'h0);
  assign decode_RegFilePlugin_regFileReadAddress1 = decode_INSTRUCTION_ANTICIPATED[19 : 15];
  assign decode_RegFilePlugin_regFileReadAddress2 = decode_INSTRUCTION_ANTICIPATED[24 : 20];
  assign decode_RegFilePlugin_rs1Data = RegFilePlugin_regFile_spinal_port0;
  assign decode_RegFilePlugin_rs2Data = RegFilePlugin_regFile_spinal_port1;
  always @(*) begin
    lastStageRegFileWrite_valid = (_zz_lastStageRegFileWrite_valid && writeBack_arbitration_isFiring);
    if(_zz_5) begin
      lastStageRegFileWrite_valid = 1'b1;
    end
  end

  always @(*) begin
    lastStageRegFileWrite_payload_address = _zz_lastStageRegFileWrite_payload_address[11 : 7];
    if(_zz_5) begin
      lastStageRegFileWrite_payload_address = 5'h0;
    end
  end

  always @(*) begin
    lastStageRegFileWrite_payload_data = _zz_lastStageRegFileWrite_payload_data;
    if(_zz_5) begin
      lastStageRegFileWrite_payload_data = 32'h0;
    end
  end

  always @(*) begin
    case(execute_ALU_BITWISE_CTRL)
      AluBitwiseCtrlEnum_AND_1 : begin
        execute_IntAluPlugin_bitwise = (execute_SRC1 & execute_SRC2);
      end
      AluBitwiseCtrlEnum_OR_1 : begin
        execute_IntAluPlugin_bitwise = (execute_SRC1 | execute_SRC2);
      end
      default : begin
        execute_IntAluPlugin_bitwise = (execute_SRC1 ^ execute_SRC2);
      end
    endcase
  end

  always @(*) begin
    case(execute_ALU_CTRL)
      AluCtrlEnum_BITWISE : begin
        _zz_execute_REGFILE_WRITE_DATA = execute_IntAluPlugin_bitwise;
      end
      AluCtrlEnum_SLT_SLTU : begin
        _zz_execute_REGFILE_WRITE_DATA = {31'd0, _zz__zz_execute_REGFILE_WRITE_DATA};
      end
      default : begin
        _zz_execute_REGFILE_WRITE_DATA = execute_SRC_ADD_SUB;
      end
    endcase
  end

  always @(*) begin
    case(decode_SRC1_CTRL)
      Src1CtrlEnum_RS : begin
        _zz_decode_SRC1 = _zz_decode_to_execute_RS1;
      end
      Src1CtrlEnum_PC_INCREMENT : begin
        _zz_decode_SRC1 = {29'd0, _zz__zz_decode_SRC1};
      end
      Src1CtrlEnum_IMU : begin
        _zz_decode_SRC1 = {decode_INSTRUCTION[31 : 12],12'h0};
      end
      default : begin
        _zz_decode_SRC1 = {27'd0, _zz__zz_decode_SRC1_1};
      end
    endcase
  end

  assign _zz_decode_SRC2 = decode_INSTRUCTION[31];
  always @(*) begin
    _zz_decode_SRC2_1[19] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[18] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[17] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[16] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[15] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[14] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[13] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[12] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[11] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[10] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[9] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[8] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[7] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[6] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[5] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[4] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[3] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[2] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[1] = _zz_decode_SRC2;
    _zz_decode_SRC2_1[0] = _zz_decode_SRC2;
  end

  assign _zz_decode_SRC2_2 = _zz__zz_decode_SRC2_2[11];
  always @(*) begin
    _zz_decode_SRC2_3[19] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[18] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[17] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[16] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[15] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[14] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[13] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[12] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[11] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[10] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[9] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[8] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[7] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[6] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[5] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[4] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[3] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[2] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[1] = _zz_decode_SRC2_2;
    _zz_decode_SRC2_3[0] = _zz_decode_SRC2_2;
  end

  always @(*) begin
    case(decode_SRC2_CTRL)
      Src2CtrlEnum_RS : begin
        _zz_decode_SRC2_4 = _zz_decode_to_execute_RS2;
      end
      Src2CtrlEnum_IMI : begin
        _zz_decode_SRC2_4 = {_zz_decode_SRC2_1,decode_INSTRUCTION[31 : 20]};
      end
      Src2CtrlEnum_IMS : begin
        _zz_decode_SRC2_4 = {_zz_decode_SRC2_3,{decode_INSTRUCTION[31 : 25],decode_INSTRUCTION[11 : 7]}};
      end
      default : begin
        _zz_decode_SRC2_4 = _zz_decode_to_execute_PC;
      end
    endcase
  end

  always @(*) begin
    execute_SrcPlugin_addSub = _zz_execute_SrcPlugin_addSub;
    if(execute_SRC2_FORCE_ZERO) begin
      execute_SrcPlugin_addSub = execute_SRC1;
    end
  end

  assign execute_SrcPlugin_less = ((execute_SRC1[31] == execute_SRC2[31]) ? execute_SrcPlugin_addSub[31] : (execute_SRC_LESS_UNSIGNED ? execute_SRC2[31] : execute_SRC1[31]));
  assign execute_MulPlugin_a = execute_RS1;
  assign execute_MulPlugin_b = execute_RS2;
  assign switch_MulPlugin_l87 = execute_INSTRUCTION[13 : 12];
  always @(*) begin
    case(switch_MulPlugin_l87)
      2'b01 : begin
        execute_MulPlugin_aSigned = 1'b1;
      end
      2'b10 : begin
        execute_MulPlugin_aSigned = 1'b1;
      end
      default : begin
        execute_MulPlugin_aSigned = 1'b0;
      end
    endcase
  end

  always @(*) begin
    case(switch_MulPlugin_l87)
      2'b01 : begin
        execute_MulPlugin_bSigned = 1'b1;
      end
      2'b10 : begin
        execute_MulPlugin_bSigned = 1'b0;
      end
      default : begin
        execute_MulPlugin_bSigned = 1'b0;
      end
    endcase
  end

  assign execute_MulPlugin_aULow = execute_MulPlugin_a[15 : 0];
  assign execute_MulPlugin_bULow = execute_MulPlugin_b[15 : 0];
  assign execute_MulPlugin_aSLow = {1'b0,execute_MulPlugin_a[15 : 0]};
  assign execute_MulPlugin_bSLow = {1'b0,execute_MulPlugin_b[15 : 0]};
  assign execute_MulPlugin_aHigh = {(execute_MulPlugin_aSigned && execute_MulPlugin_a[31]),execute_MulPlugin_a[31 : 16]};
  assign execute_MulPlugin_bHigh = {(execute_MulPlugin_bSigned && execute_MulPlugin_b[31]),execute_MulPlugin_b[31 : 16]};
  assign writeBack_MulPlugin_result = ($signed(_zz_writeBack_MulPlugin_result) + $signed(_zz_writeBack_MulPlugin_result_1));
  assign when_MulPlugin_l147 = (writeBack_arbitration_isValid && writeBack_IS_MUL);
  assign switch_MulPlugin_l148 = writeBack_INSTRUCTION[13 : 12];
  assign memory_DivPlugin_frontendOk = 1'b1;
  always @(*) begin
    memory_DivPlugin_div_counter_willIncrement = 1'b0;
    if(when_MulDivIterativePlugin_l128) begin
      if(when_MulDivIterativePlugin_l132) begin
        memory_DivPlugin_div_counter_willIncrement = 1'b1;
      end
    end
  end

  always @(*) begin
    memory_DivPlugin_div_counter_willClear = 1'b0;
    if(when_MulDivIterativePlugin_l162) begin
      memory_DivPlugin_div_counter_willClear = 1'b1;
    end
  end

  assign memory_DivPlugin_div_counter_willOverflowIfInc = (memory_DivPlugin_div_counter_value == 6'h21);
  assign memory_DivPlugin_div_counter_willOverflow = (memory_DivPlugin_div_counter_willOverflowIfInc && memory_DivPlugin_div_counter_willIncrement);
  always @(*) begin
    if(memory_DivPlugin_div_counter_willOverflow) begin
      memory_DivPlugin_div_counter_valueNext = 6'h0;
    end else begin
      memory_DivPlugin_div_counter_valueNext = (memory_DivPlugin_div_counter_value + _zz_memory_DivPlugin_div_counter_valueNext);
    end
    if(memory_DivPlugin_div_counter_willClear) begin
      memory_DivPlugin_div_counter_valueNext = 6'h0;
    end
  end

  assign when_MulDivIterativePlugin_l126 = (memory_DivPlugin_div_counter_value == 6'h20);
  assign when_MulDivIterativePlugin_l126_1 = (! memory_arbitration_isStuck);
  assign when_MulDivIterativePlugin_l128 = (memory_arbitration_isValid && memory_IS_DIV);
  assign when_MulDivIterativePlugin_l129 = ((! memory_DivPlugin_frontendOk) || (! memory_DivPlugin_div_done));
  assign when_MulDivIterativePlugin_l132 = (memory_DivPlugin_frontendOk && (! memory_DivPlugin_div_done));
  assign _zz_memory_DivPlugin_div_stage_0_remainderShifted = memory_DivPlugin_rs1[31 : 0];
  assign memory_DivPlugin_div_stage_0_remainderShifted = {memory_DivPlugin_accumulator[31 : 0],_zz_memory_DivPlugin_div_stage_0_remainderShifted[31]};
  assign memory_DivPlugin_div_stage_0_remainderMinusDenominator = (memory_DivPlugin_div_stage_0_remainderShifted - _zz_memory_DivPlugin_div_stage_0_remainderMinusDenominator);
  assign memory_DivPlugin_div_stage_0_outRemainder = ((! memory_DivPlugin_div_stage_0_remainderMinusDenominator[32]) ? _zz_memory_DivPlugin_div_stage_0_outRemainder : _zz_memory_DivPlugin_div_stage_0_outRemainder_1);
  assign memory_DivPlugin_div_stage_0_outNumerator = _zz_memory_DivPlugin_div_stage_0_outNumerator[31:0];
  assign when_MulDivIterativePlugin_l151 = (memory_DivPlugin_div_counter_value == 6'h20);
  assign _zz_memory_DivPlugin_div_result = (memory_INSTRUCTION[13] ? memory_DivPlugin_accumulator[31 : 0] : memory_DivPlugin_rs1[31 : 0]);
  assign when_MulDivIterativePlugin_l162 = (! memory_arbitration_isStuck);
  assign _zz_memory_DivPlugin_rs2 = (execute_RS2[31] && execute_IS_RS2_SIGNED);
  assign _zz_memory_DivPlugin_rs1 = (1'b0 || ((execute_IS_DIV && execute_RS1[31]) && execute_IS_RS1_SIGNED));
  always @(*) begin
    _zz_memory_DivPlugin_rs1_1[32] = (execute_IS_RS1_SIGNED && execute_RS1[31]);
    _zz_memory_DivPlugin_rs1_1[31 : 0] = execute_RS1;
  end

  assign FpuPlugin_port_cmd_ready = FpuPlugin_fpu_io_port_0_cmd_ready;
  assign FpuPlugin_port_commit_ready = FpuPlugin_fpu_io_port_0_commit_ready;
  assign FpuPlugin_port_rsp_valid = FpuPlugin_fpu_io_port_0_rsp_valid;
  assign FpuPlugin_port_rsp_payload_value = FpuPlugin_fpu_io_port_0_rsp_payload_value;
  assign FpuPlugin_port_rsp_payload_NV = FpuPlugin_fpu_io_port_0_rsp_payload_NV;
  assign FpuPlugin_port_rsp_payload_NX = FpuPlugin_fpu_io_port_0_rsp_payload_NX;
  assign FpuPlugin_port_completion_valid = FpuPlugin_fpu_io_port_0_completion_valid;
  assign FpuPlugin_port_completion_payload_flags_NX = FpuPlugin_fpu_io_port_0_completion_payload_flags_NX;
  assign FpuPlugin_port_completion_payload_flags_UF = FpuPlugin_fpu_io_port_0_completion_payload_flags_UF;
  assign FpuPlugin_port_completion_payload_flags_OF = FpuPlugin_fpu_io_port_0_completion_payload_flags_OF;
  assign FpuPlugin_port_completion_payload_flags_DZ = FpuPlugin_fpu_io_port_0_completion_payload_flags_DZ;
  assign FpuPlugin_port_completion_payload_flags_NV = FpuPlugin_fpu_io_port_0_completion_payload_flags_NV;
  assign FpuPlugin_port_completion_payload_written = FpuPlugin_fpu_io_port_0_completion_payload_written;
  assign FpuPlugin_port_cmd_fire = (FpuPlugin_port_cmd_valid && FpuPlugin_port_cmd_ready);
  assign FpuPlugin_port_rsp_fire = (FpuPlugin_port_rsp_valid && FpuPlugin_port_rsp_ready);
  assign FpuPlugin_hasPending = (FpuPlugin_pendings != 6'h0);
  assign when_FpuPlugin_l215 = (FpuPlugin_port_completion_valid && FpuPlugin_port_completion_payload_flags_NV);
  assign when_FpuPlugin_l216 = (FpuPlugin_port_completion_valid && FpuPlugin_port_completion_payload_flags_DZ);
  assign when_FpuPlugin_l217 = (FpuPlugin_port_completion_valid && FpuPlugin_port_completion_payload_flags_OF);
  assign when_FpuPlugin_l218 = (FpuPlugin_port_completion_valid && FpuPlugin_port_completion_payload_flags_UF);
  assign when_FpuPlugin_l219 = (FpuPlugin_port_completion_valid && FpuPlugin_port_completion_payload_flags_NX);
  assign FpuPlugin_csrActive = (execute_arbitration_isValid && execute_IS_CSR);
  assign when_FpuPlugin_l229 = (FpuPlugin_csrActive && FpuPlugin_hasPending);
  assign FpuPlugin_sd = (FpuPlugin_fs == 2'b11);
  assign when_FpuPlugin_l234 = (FpuPlugin_port_completion_valid && (FpuPlugin_port_completion_payload_written || (|{FpuPlugin_port_completion_payload_flags_NV,{FpuPlugin_port_completion_payload_flags_DZ,{FpuPlugin_port_completion_payload_flags_OF,{FpuPlugin_port_completion_payload_flags_UF,FpuPlugin_port_completion_payload_flags_NX}}}})));
  always @(*) begin
    _zz_when_FpuPlugin_l237 = 1'b0;
    if(execute_CsrPlugin_csr_2) begin
      if(execute_CsrPlugin_writeEnable) begin
        _zz_when_FpuPlugin_l237 = 1'b1;
      end
    end
  end

  always @(*) begin
    _zz_when_FpuPlugin_l237_1 = 1'b0;
    if(execute_CsrPlugin_csr_3) begin
      if(execute_CsrPlugin_writeEnable) begin
        _zz_when_FpuPlugin_l237_1 = 1'b1;
      end
    end
  end

  always @(*) begin
    _zz_when_FpuPlugin_l237_2 = 1'b0;
    if(execute_CsrPlugin_csr_1) begin
      if(execute_CsrPlugin_writeEnable) begin
        _zz_when_FpuPlugin_l237_2 = 1'b1;
      end
    end
  end

  assign when_FpuPlugin_l237 = (|{_zz_when_FpuPlugin_l237_2,{_zz_when_FpuPlugin_l237_1,_zz_when_FpuPlugin_l237}});
  always @(*) begin
    FpuPlugin_accessFpuCsr = 1'b0;
    if(execute_CsrPlugin_csr_3) begin
      FpuPlugin_accessFpuCsr = 1'b1;
    end
    if(execute_CsrPlugin_csr_2) begin
      FpuPlugin_accessFpuCsr = 1'b1;
    end
    if(execute_CsrPlugin_csr_1) begin
      FpuPlugin_accessFpuCsr = 1'b1;
    end
  end

  assign when_FpuPlugin_l253 = ((FpuPlugin_accessFpuCsr && (FpuPlugin_fs == 2'b00)) && (! debugMode));
  assign _zz_decode_FPU_FORKED = 1'b0;
  assign decode_FpuPlugin_trap = (((_zz_decode_FPU_ENABLE && (FpuPlugin_fs == 2'b00)) && (! debugMode)) && (! (|{writeBack_arbitration_isValid,{memory_arbitration_isValid,execute_arbitration_isValid}})));
  assign when_FpuPlugin_l268 = (FpuPlugin_port_cmd_fire && (! _zz_decode_FPU_FORKED));
  assign when_FpuPlugin_l268_1 = (! decode_arbitration_isStuck);
  assign decode_FpuPlugin_hazard = ((FpuPlugin_pendings[5] || FpuPlugin_csrActive) || ((FpuPlugin_fs == 2'b00) && (! debugMode)));
  assign when_FpuPlugin_l272 = (! decode_LEGAL_INSTRUCTION);
  assign when_FpuPlugin_l273 = ((decode_arbitration_isValid && decode_FPU_ENABLE) && decode_FpuPlugin_hazard);
  assign FpuPlugin_port_cmd_isStall = (FpuPlugin_port_cmd_valid && (! FpuPlugin_port_cmd_ready));
  assign decode_FpuPlugin_iRoundMode = decode_INSTRUCTION[14 : 12];
  assign decode_FpuPlugin_roundMode = ((decode_INSTRUCTION[14 : 12] == 3'b111) ? FpuPlugin_rm : decode_INSTRUCTION[14 : 12]);
  assign FpuPlugin_port_cmd_valid = (((decode_arbitration_isValid && decode_FPU_ENABLE) && (! decode_FpuPlugin_forked)) && (! decode_FpuPlugin_hazard));
  assign FpuPlugin_port_cmd_payload_opcode = decode_FPU_OPCODE;
  assign FpuPlugin_port_cmd_payload_arg = decode_FPU_ARG;
  assign FpuPlugin_port_cmd_payload_rs1 = decode_INSTRUCTION[19 : 15];
  assign FpuPlugin_port_cmd_payload_rs2 = decode_INSTRUCTION[24 : 20];
  assign FpuPlugin_port_cmd_payload_rs3 = decode_INSTRUCTION[31 : 27];
  assign FpuPlugin_port_cmd_payload_rd = decode_INSTRUCTION[11 : 7];
  assign FpuPlugin_port_cmd_payload_format = FpuFormat_FLOAT;
  assign _zz_FpuPlugin_port_cmd_payload_roundMode_1 = decode_FpuPlugin_roundMode;
  assign _zz_FpuPlugin_port_cmd_payload_roundMode = _zz_FpuPlugin_port_cmd_payload_roundMode_1;
  assign FpuPlugin_port_cmd_payload_roundMode = _zz_FpuPlugin_port_cmd_payload_roundMode;
  assign writeBack_FpuPlugin_isRsp = (writeBack_FPU_FORKED && writeBack_FPU_RSP);
  assign writeBack_FpuPlugin_isCommit = (writeBack_FPU_FORKED && writeBack_FPU_COMMIT);
  assign writeBack_FpuPlugin_storeFormated = FpuPlugin_port_rsp_payload_value;
  always @(*) begin
    FpuPlugin_port_rsp_ready = 1'b0;
    if(writeBack_FpuPlugin_isRsp) begin
      if(!when_FpuPlugin_l323) begin
        if(when_FpuPlugin_l325) begin
          FpuPlugin_port_rsp_ready = 1'b1;
        end
      end
    end
  end

  assign DBusBypass0_value = writeBack_FpuPlugin_storeFormated;
  assign when_FpuPlugin_l315 = ((! writeBack_arbitration_isStuck) && (! writeBack_arbitration_removeIt));
  assign when_FpuPlugin_l318 = (FpuPlugin_port_rsp_payload_NV || FpuPlugin_port_rsp_payload_NX);
  assign when_FpuPlugin_l323 = (! FpuPlugin_port_rsp_valid);
  assign when_FpuPlugin_l325 = (! writeBack_arbitration_haltItself);
  assign writeBack_FpuPlugin_commit_valid = (writeBack_FpuPlugin_isCommit && (! writeBack_arbitration_isStuck));
  assign writeBack_FpuPlugin_commit_payload_value[31 : 0] = (writeBack_FPU_COMMIT_LOAD ? writeBack_MEMORY_LOAD_DATA[31 : 0] : writeBack_RS1);
  assign writeBack_FpuPlugin_commit_payload_write = (writeBack_arbitration_isValid && (! writeBack_arbitration_removeIt));
  assign writeBack_FpuPlugin_commit_payload_opcode = writeBack_FPU_OPCODE;
  assign writeBack_FpuPlugin_commit_payload_rd = writeBack_INSTRUCTION[11 : 7];
  assign when_FpuPlugin_l339 = (writeBack_FpuPlugin_isCommit && (! writeBack_FpuPlugin_commit_ready));
  assign writeBack_FpuPlugin_commit_ready = writeBack_FpuPlugin_commit_rValidN;
  assign writeBack_FpuPlugin_commit_s2mPipe_valid = (writeBack_FpuPlugin_commit_valid || (! writeBack_FpuPlugin_commit_rValidN));
  assign _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode = (writeBack_FpuPlugin_commit_rValidN ? writeBack_FpuPlugin_commit_payload_opcode : writeBack_FpuPlugin_commit_rData_opcode);
  assign writeBack_FpuPlugin_commit_s2mPipe_payload_opcode = _zz_writeBack_FpuPlugin_commit_s2mPipe_payload_opcode;
  assign writeBack_FpuPlugin_commit_s2mPipe_payload_rd = (writeBack_FpuPlugin_commit_rValidN ? writeBack_FpuPlugin_commit_payload_rd : writeBack_FpuPlugin_commit_rData_rd);
  assign writeBack_FpuPlugin_commit_s2mPipe_payload_write = (writeBack_FpuPlugin_commit_rValidN ? writeBack_FpuPlugin_commit_payload_write : writeBack_FpuPlugin_commit_rData_write);
  assign writeBack_FpuPlugin_commit_s2mPipe_payload_value = (writeBack_FpuPlugin_commit_rValidN ? writeBack_FpuPlugin_commit_payload_value : writeBack_FpuPlugin_commit_rData_value);
  assign FpuPlugin_port_commit_valid = writeBack_FpuPlugin_commit_s2mPipe_valid;
  assign writeBack_FpuPlugin_commit_s2mPipe_ready = FpuPlugin_port_commit_ready;
  assign FpuPlugin_port_commit_payload_opcode = writeBack_FpuPlugin_commit_s2mPipe_payload_opcode;
  assign FpuPlugin_port_commit_payload_rd = writeBack_FpuPlugin_commit_s2mPipe_payload_rd;
  assign FpuPlugin_port_commit_payload_write = writeBack_FpuPlugin_commit_s2mPipe_payload_write;
  assign FpuPlugin_port_commit_payload_value = writeBack_FpuPlugin_commit_s2mPipe_payload_value;
  assign execute_LightShifterPlugin_isShift = (execute_SHIFT_CTRL != ShiftCtrlEnum_DISABLE_1);
  assign execute_LightShifterPlugin_amplitude = (execute_LightShifterPlugin_isActive ? execute_LightShifterPlugin_amplitudeReg : execute_SRC2[4 : 0]);
  assign execute_LightShifterPlugin_shiftInput = (execute_LightShifterPlugin_isActive ? memory_REGFILE_WRITE_DATA : execute_SRC1);
  assign execute_LightShifterPlugin_done = (execute_LightShifterPlugin_amplitude[4 : 1] == 4'b0000);
  assign when_ShiftPlugins_l169 = ((execute_arbitration_isValid && execute_LightShifterPlugin_isShift) && (execute_SRC2[4 : 0] != 5'h0));
  always @(*) begin
    case(execute_SHIFT_CTRL)
      ShiftCtrlEnum_SLL_1 : begin
        _zz_execute_to_memory_REGFILE_WRITE_DATA_1 = (execute_LightShifterPlugin_shiftInput <<< 1);
      end
      default : begin
        _zz_execute_to_memory_REGFILE_WRITE_DATA_1 = _zz__zz_execute_to_memory_REGFILE_WRITE_DATA_1;
      end
    endcase
  end

  assign when_ShiftPlugins_l175 = (! execute_arbitration_isStuckByOthers);
  assign when_ShiftPlugins_l184 = (! execute_LightShifterPlugin_done);
  always @(*) begin
    HazardSimplePlugin_src0Hazard = 1'b0;
    if(HazardSimplePlugin_writeBackBuffer_valid) begin
      if(HazardSimplePlugin_addr0Match) begin
        HazardSimplePlugin_src0Hazard = 1'b1;
      end
    end
    if(when_HazardSimplePlugin_l57) begin
      if(when_HazardSimplePlugin_l58) begin
        if(when_HazardSimplePlugin_l59) begin
          HazardSimplePlugin_src0Hazard = 1'b1;
        end
      end
    end
    if(when_HazardSimplePlugin_l57_1) begin
      if(when_HazardSimplePlugin_l58_1) begin
        if(when_HazardSimplePlugin_l59_1) begin
          HazardSimplePlugin_src0Hazard = 1'b1;
        end
      end
    end
    if(when_HazardSimplePlugin_l57_2) begin
      if(when_HazardSimplePlugin_l58_2) begin
        if(when_HazardSimplePlugin_l59_2) begin
          HazardSimplePlugin_src0Hazard = 1'b1;
        end
      end
    end
    if(when_HazardSimplePlugin_l105) begin
      HazardSimplePlugin_src0Hazard = 1'b0;
    end
  end

  always @(*) begin
    HazardSimplePlugin_src1Hazard = 1'b0;
    if(HazardSimplePlugin_writeBackBuffer_valid) begin
      if(HazardSimplePlugin_addr1Match) begin
        HazardSimplePlugin_src1Hazard = 1'b1;
      end
    end
    if(when_HazardSimplePlugin_l57) begin
      if(when_HazardSimplePlugin_l58) begin
        if(when_HazardSimplePlugin_l62) begin
          HazardSimplePlugin_src1Hazard = 1'b1;
        end
      end
    end
    if(when_HazardSimplePlugin_l57_1) begin
      if(when_HazardSimplePlugin_l58_1) begin
        if(when_HazardSimplePlugin_l62_1) begin
          HazardSimplePlugin_src1Hazard = 1'b1;
        end
      end
    end
    if(when_HazardSimplePlugin_l57_2) begin
      if(when_HazardSimplePlugin_l58_2) begin
        if(when_HazardSimplePlugin_l62_2) begin
          HazardSimplePlugin_src1Hazard = 1'b1;
        end
      end
    end
    if(when_HazardSimplePlugin_l108) begin
      HazardSimplePlugin_src1Hazard = 1'b0;
    end
  end

  assign HazardSimplePlugin_writeBackWrites_valid = (_zz_lastStageRegFileWrite_valid && writeBack_arbitration_isFiring);
  assign HazardSimplePlugin_writeBackWrites_payload_address = _zz_lastStageRegFileWrite_payload_address[11 : 7];
  assign HazardSimplePlugin_writeBackWrites_payload_data = _zz_lastStageRegFileWrite_payload_data;
  assign HazardSimplePlugin_addr0Match = (HazardSimplePlugin_writeBackBuffer_payload_address == decode_INSTRUCTION[19 : 15]);
  assign HazardSimplePlugin_addr1Match = (HazardSimplePlugin_writeBackBuffer_payload_address == decode_INSTRUCTION[24 : 20]);
  assign when_HazardSimplePlugin_l59 = (writeBack_INSTRUCTION[11 : 7] == decode_INSTRUCTION[19 : 15]);
  assign when_HazardSimplePlugin_l62 = (writeBack_INSTRUCTION[11 : 7] == decode_INSTRUCTION[24 : 20]);
  assign when_HazardSimplePlugin_l57 = (writeBack_arbitration_isValid && writeBack_REGFILE_WRITE_VALID);
  assign when_HazardSimplePlugin_l58 = (1'b1 || (! 1'b1));
  assign when_HazardSimplePlugin_l59_1 = (memory_INSTRUCTION[11 : 7] == decode_INSTRUCTION[19 : 15]);
  assign when_HazardSimplePlugin_l62_1 = (memory_INSTRUCTION[11 : 7] == decode_INSTRUCTION[24 : 20]);
  assign when_HazardSimplePlugin_l57_1 = (memory_arbitration_isValid && memory_REGFILE_WRITE_VALID);
  assign when_HazardSimplePlugin_l58_1 = (1'b1 || (! memory_BYPASSABLE_MEMORY_STAGE));
  assign when_HazardSimplePlugin_l59_2 = (execute_INSTRUCTION[11 : 7] == decode_INSTRUCTION[19 : 15]);
  assign when_HazardSimplePlugin_l62_2 = (execute_INSTRUCTION[11 : 7] == decode_INSTRUCTION[24 : 20]);
  assign when_HazardSimplePlugin_l57_2 = (execute_arbitration_isValid && execute_REGFILE_WRITE_VALID);
  assign when_HazardSimplePlugin_l58_2 = (1'b1 || (! execute_BYPASSABLE_EXECUTE_STAGE));
  assign when_HazardSimplePlugin_l105 = (! decode_RS1_USE);
  assign when_HazardSimplePlugin_l108 = (! decode_RS2_USE);
  assign when_HazardSimplePlugin_l113 = (decode_arbitration_isValid && (HazardSimplePlugin_src0Hazard || HazardSimplePlugin_src1Hazard));
  assign execute_BranchPlugin_eq = (execute_SRC1 == execute_SRC2);
  assign switch_Misc_l241_4 = execute_INSTRUCTION[14 : 12];
  always @(*) begin
    case(switch_Misc_l241_4)
      3'b000 : begin
        _zz_execute_BRANCH_DO = execute_BranchPlugin_eq;
      end
      3'b001 : begin
        _zz_execute_BRANCH_DO = (! execute_BranchPlugin_eq);
      end
      3'b101 : begin
        _zz_execute_BRANCH_DO = (! execute_SRC_LESS);
      end
      3'b111 : begin
        _zz_execute_BRANCH_DO = (! execute_SRC_LESS);
      end
      default : begin
        _zz_execute_BRANCH_DO = execute_SRC_LESS;
      end
    endcase
  end

  always @(*) begin
    case(execute_BRANCH_CTRL)
      BranchCtrlEnum_INC : begin
        _zz_execute_BRANCH_DO_1 = 1'b0;
      end
      BranchCtrlEnum_JAL : begin
        _zz_execute_BRANCH_DO_1 = 1'b1;
      end
      BranchCtrlEnum_JALR : begin
        _zz_execute_BRANCH_DO_1 = 1'b1;
      end
      default : begin
        _zz_execute_BRANCH_DO_1 = _zz_execute_BRANCH_DO;
      end
    endcase
  end

  assign execute_BranchPlugin_branch_src1 = ((execute_BRANCH_CTRL == BranchCtrlEnum_JALR) ? execute_RS1 : execute_PC);
  assign _zz_execute_BranchPlugin_branch_src2 = _zz__zz_execute_BranchPlugin_branch_src2[19];
  always @(*) begin
    _zz_execute_BranchPlugin_branch_src2_1[10] = _zz_execute_BranchPlugin_branch_src2;
    _zz_execute_BranchPlugin_branch_src2_1[9] = _zz_execute_BranchPlugin_branch_src2;
    _zz_execute_BranchPlugin_branch_src2_1[8] = _zz_execute_BranchPlugin_branch_src2;
    _zz_execute_BranchPlugin_branch_src2_1[7] = _zz_execute_BranchPlugin_branch_src2;
    _zz_execute_BranchPlugin_branch_src2_1[6] = _zz_execute_BranchPlugin_branch_src2;
    _zz_execute_BranchPlugin_branch_src2_1[5] = _zz_execute_BranchPlugin_branch_src2;
    _zz_execute_BranchPlugin_branch_src2_1[4] = _zz_execute_BranchPlugin_branch_src2;
    _zz_execute_BranchPlugin_branch_src2_1[3] = _zz_execute_BranchPlugin_branch_src2;
    _zz_execute_BranchPlugin_branch_src2_1[2] = _zz_execute_BranchPlugin_branch_src2;
    _zz_execute_BranchPlugin_branch_src2_1[1] = _zz_execute_BranchPlugin_branch_src2;
    _zz_execute_BranchPlugin_branch_src2_1[0] = _zz_execute_BranchPlugin_branch_src2;
  end

  assign _zz_execute_BranchPlugin_branch_src2_2 = execute_INSTRUCTION[31];
  always @(*) begin
    _zz_execute_BranchPlugin_branch_src2_3[19] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[18] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[17] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[16] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[15] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[14] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[13] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[12] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[11] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[10] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[9] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[8] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[7] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[6] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[5] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[4] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[3] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[2] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[1] = _zz_execute_BranchPlugin_branch_src2_2;
    _zz_execute_BranchPlugin_branch_src2_3[0] = _zz_execute_BranchPlugin_branch_src2_2;
  end

  assign _zz_execute_BranchPlugin_branch_src2_4 = _zz__zz_execute_BranchPlugin_branch_src2_4[11];
  always @(*) begin
    _zz_execute_BranchPlugin_branch_src2_5[18] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[17] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[16] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[15] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[14] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[13] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[12] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[11] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[10] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[9] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[8] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[7] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[6] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[5] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[4] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[3] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[2] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[1] = _zz_execute_BranchPlugin_branch_src2_4;
    _zz_execute_BranchPlugin_branch_src2_5[0] = _zz_execute_BranchPlugin_branch_src2_4;
  end

  always @(*) begin
    case(execute_BRANCH_CTRL)
      BranchCtrlEnum_JAL : begin
        _zz_execute_BranchPlugin_branch_src2_6 = {{_zz_execute_BranchPlugin_branch_src2_1,{{{execute_INSTRUCTION[31],execute_INSTRUCTION[19 : 12]},execute_INSTRUCTION[20]},execute_INSTRUCTION[30 : 21]}},1'b0};
      end
      BranchCtrlEnum_JALR : begin
        _zz_execute_BranchPlugin_branch_src2_6 = {_zz_execute_BranchPlugin_branch_src2_3,execute_INSTRUCTION[31 : 20]};
      end
      default : begin
        _zz_execute_BranchPlugin_branch_src2_6 = {{_zz_execute_BranchPlugin_branch_src2_5,{{{execute_INSTRUCTION[31],execute_INSTRUCTION[7]},execute_INSTRUCTION[30 : 25]},execute_INSTRUCTION[11 : 8]}},1'b0};
      end
    endcase
  end

  assign execute_BranchPlugin_branch_src2 = _zz_execute_BranchPlugin_branch_src2_6;
  assign execute_BranchPlugin_branchAdder = (execute_BranchPlugin_branch_src1 + execute_BranchPlugin_branch_src2);
  assign BranchPlugin_jumpInterface_valid = ((memory_arbitration_isValid && memory_BRANCH_DO) && (! 1'b0));
  assign BranchPlugin_jumpInterface_payload = memory_BRANCH_CALC;
  assign ndmreset = debugModule_1_io_ndmreset;
  assign jtag_tdo = debugTransportModuleJtagTap_1_io_jtag_tdo;
  assign debugBus_ackReset = debugModule_1_io_harts_0_ackReset;
  assign debugBus_resume_cmd_valid = debugModule_1_io_harts_0_resume_cmd_valid;
  assign debugBus_haltReq = debugModule_1_io_harts_0_haltReq;
  assign debugBus_dmToHart_valid = toplevel_debugModule_1_io_harts_0_dmToHart_regNext_valid;
  assign debugBus_dmToHart_payload_op = toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_op;
  assign debugBus_dmToHart_payload_address = toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_address;
  assign debugBus_dmToHart_payload_data = toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_data;
  assign debugBus_dmToHart_payload_size = toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_size;
  assign when_Pipeline_l124 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_1 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_2 = ((! writeBack_arbitration_isStuck) && (! CsrPlugin_exceptionPortCtrl_exceptionValids_writeBack));
  assign when_Pipeline_l124_3 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_4 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_5 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_6 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_7 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_8 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_9 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_10 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_11 = (! execute_arbitration_isStuck);
  assign _zz_decode_SRC1_CTRL = _zz_decode_SRC1_CTRL_1;
  assign when_Pipeline_l124_12 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_13 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_14 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_15 = (! writeBack_arbitration_isStuck);
  assign _zz_decode_to_execute_ALU_CTRL_1 = decode_ALU_CTRL;
  assign _zz_decode_ALU_CTRL = _zz_decode_ALU_CTRL_1;
  assign when_Pipeline_l124_16 = (! execute_arbitration_isStuck);
  assign _zz_execute_ALU_CTRL = decode_to_execute_ALU_CTRL;
  assign _zz_decode_SRC2_CTRL = _zz_decode_SRC2_CTRL_1;
  assign when_Pipeline_l124_17 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_18 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_19 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_20 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_21 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_22 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_23 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_24 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_25 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_26 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_27 = (! execute_arbitration_isStuck);
  assign _zz_decode_to_execute_ENV_CTRL_1 = decode_ENV_CTRL;
  assign _zz_execute_to_memory_ENV_CTRL_1 = execute_ENV_CTRL;
  assign _zz_memory_to_writeBack_ENV_CTRL_1 = memory_ENV_CTRL;
  assign _zz_decode_ENV_CTRL = _zz_decode_ENV_CTRL_1;
  assign when_Pipeline_l124_28 = (! execute_arbitration_isStuck);
  assign _zz_execute_ENV_CTRL = decode_to_execute_ENV_CTRL;
  assign when_Pipeline_l124_29 = (! memory_arbitration_isStuck);
  assign _zz_memory_ENV_CTRL = execute_to_memory_ENV_CTRL;
  assign when_Pipeline_l124_30 = (! writeBack_arbitration_isStuck);
  assign _zz_writeBack_ENV_CTRL = memory_to_writeBack_ENV_CTRL;
  assign when_Pipeline_l124_31 = (! execute_arbitration_isStuck);
  assign _zz_decode_to_execute_ALU_BITWISE_CTRL_1 = decode_ALU_BITWISE_CTRL;
  assign _zz_decode_ALU_BITWISE_CTRL = _zz_decode_ALU_BITWISE_CTRL_1;
  assign when_Pipeline_l124_32 = (! execute_arbitration_isStuck);
  assign _zz_execute_ALU_BITWISE_CTRL = decode_to_execute_ALU_BITWISE_CTRL;
  assign when_Pipeline_l124_33 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_34 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_35 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_36 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_37 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_38 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_39 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_40 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_41 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_42 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_43 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_44 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_45 = (! writeBack_arbitration_isStuck);
  assign _zz_decode_to_execute_FPU_OPCODE_1 = decode_FPU_OPCODE;
  assign _zz_execute_to_memory_FPU_OPCODE_1 = execute_FPU_OPCODE;
  assign _zz_memory_to_writeBack_FPU_OPCODE_1 = memory_FPU_OPCODE;
  assign _zz_decode_FPU_OPCODE = _zz_decode_FPU_OPCODE_1;
  assign when_Pipeline_l124_46 = (! execute_arbitration_isStuck);
  assign _zz_execute_FPU_OPCODE = decode_to_execute_FPU_OPCODE;
  assign when_Pipeline_l124_47 = (! memory_arbitration_isStuck);
  assign _zz_memory_FPU_OPCODE = execute_to_memory_FPU_OPCODE;
  assign when_Pipeline_l124_48 = (! writeBack_arbitration_isStuck);
  assign _zz_writeBack_FPU_OPCODE = memory_to_writeBack_FPU_OPCODE;
  assign _zz_decode_to_execute_SHIFT_CTRL_1 = decode_SHIFT_CTRL;
  assign _zz_decode_SHIFT_CTRL = _zz_decode_SHIFT_CTRL_1;
  assign when_Pipeline_l124_49 = (! execute_arbitration_isStuck);
  assign _zz_execute_SHIFT_CTRL = decode_to_execute_SHIFT_CTRL;
  assign _zz_decode_to_execute_BRANCH_CTRL_1 = decode_BRANCH_CTRL;
  assign _zz_decode_BRANCH_CTRL = _zz_decode_BRANCH_CTRL_1;
  assign when_Pipeline_l124_50 = (! execute_arbitration_isStuck);
  assign _zz_execute_BRANCH_CTRL = decode_to_execute_BRANCH_CTRL;
  assign when_Pipeline_l124_51 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_52 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_53 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_54 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_55 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_56 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_57 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_58 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_59 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_60 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_61 = (! execute_arbitration_isStuck);
  assign when_Pipeline_l124_62 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_63 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_64 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_65 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_66 = ((! memory_arbitration_isStuck) && (! execute_arbitration_isStuckByOthers));
  assign when_Pipeline_l124_67 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_68 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_69 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_70 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_71 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_72 = (! writeBack_arbitration_isStuck);
  assign when_Pipeline_l124_73 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_74 = (! memory_arbitration_isStuck);
  assign when_Pipeline_l124_75 = (! writeBack_arbitration_isStuck);
  assign decode_arbitration_isFlushed = ((|{writeBack_arbitration_flushNext,{memory_arbitration_flushNext,execute_arbitration_flushNext}}) || (|{writeBack_arbitration_flushIt,{memory_arbitration_flushIt,{execute_arbitration_flushIt,decode_arbitration_flushIt}}}));
  assign execute_arbitration_isFlushed = ((|{writeBack_arbitration_flushNext,memory_arbitration_flushNext}) || (|{writeBack_arbitration_flushIt,{memory_arbitration_flushIt,execute_arbitration_flushIt}}));
  assign memory_arbitration_isFlushed = ((|writeBack_arbitration_flushNext) || (|{writeBack_arbitration_flushIt,memory_arbitration_flushIt}));
  assign writeBack_arbitration_isFlushed = (1'b0 || (|writeBack_arbitration_flushIt));
  assign decode_arbitration_isStuckByOthers = (decode_arbitration_haltByOther || (((1'b0 || execute_arbitration_isStuck) || memory_arbitration_isStuck) || writeBack_arbitration_isStuck));
  assign decode_arbitration_isStuck = (decode_arbitration_haltItself || decode_arbitration_isStuckByOthers);
  assign decode_arbitration_isMoving = ((! decode_arbitration_isStuck) && (! decode_arbitration_removeIt));
  assign decode_arbitration_isFiring = ((decode_arbitration_isValid && (! decode_arbitration_isStuck)) && (! decode_arbitration_removeIt));
  assign execute_arbitration_isStuckByOthers = (execute_arbitration_haltByOther || ((1'b0 || memory_arbitration_isStuck) || writeBack_arbitration_isStuck));
  assign execute_arbitration_isStuck = (execute_arbitration_haltItself || execute_arbitration_isStuckByOthers);
  assign execute_arbitration_isMoving = ((! execute_arbitration_isStuck) && (! execute_arbitration_removeIt));
  assign execute_arbitration_isFiring = ((execute_arbitration_isValid && (! execute_arbitration_isStuck)) && (! execute_arbitration_removeIt));
  assign memory_arbitration_isStuckByOthers = (memory_arbitration_haltByOther || (1'b0 || writeBack_arbitration_isStuck));
  assign memory_arbitration_isStuck = (memory_arbitration_haltItself || memory_arbitration_isStuckByOthers);
  assign memory_arbitration_isMoving = ((! memory_arbitration_isStuck) && (! memory_arbitration_removeIt));
  assign memory_arbitration_isFiring = ((memory_arbitration_isValid && (! memory_arbitration_isStuck)) && (! memory_arbitration_removeIt));
  assign writeBack_arbitration_isStuckByOthers = (writeBack_arbitration_haltByOther || 1'b0);
  assign writeBack_arbitration_isStuck = (writeBack_arbitration_haltItself || writeBack_arbitration_isStuckByOthers);
  assign writeBack_arbitration_isMoving = ((! writeBack_arbitration_isStuck) && (! writeBack_arbitration_removeIt));
  assign writeBack_arbitration_isFiring = ((writeBack_arbitration_isValid && (! writeBack_arbitration_isStuck)) && (! writeBack_arbitration_removeIt));
  assign when_Pipeline_l151 = ((! execute_arbitration_isStuck) || execute_arbitration_removeIt);
  assign when_Pipeline_l154 = ((! decode_arbitration_isStuck) && (! decode_arbitration_removeIt));
  assign when_Pipeline_l151_1 = ((! memory_arbitration_isStuck) || memory_arbitration_removeIt);
  assign when_Pipeline_l154_1 = ((! execute_arbitration_isStuck) && (! execute_arbitration_removeIt));
  assign when_Pipeline_l151_2 = ((! writeBack_arbitration_isStuck) || writeBack_arbitration_removeIt);
  assign when_Pipeline_l154_2 = ((! memory_arbitration_isStuck) && (! memory_arbitration_removeIt));
  always @(*) begin
    CsrPlugin_injectionPort_ready = 1'b0;
    case(IBusSimplePlugin_injector_port_state)
      3'b100 : begin
        CsrPlugin_injectionPort_ready = 1'b1;
      end
      default : begin
      end
    endcase
  end

  assign when_Fetcher_l373 = (IBusSimplePlugin_injector_port_state != 3'b000);
  assign when_Fetcher_l391 = (! decode_arbitration_isStuck);
  assign when_Fetcher_l411 = (IBusSimplePlugin_injector_port_state != 3'b000);
  assign when_CsrPlugin_l1669 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_1 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_2 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_3 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_4 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_5 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_6 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_7 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_8 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_9 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_10 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_11 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_12 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_13 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_14 = (! execute_arbitration_isStuck);
  assign when_CsrPlugin_l1669_15 = (! execute_arbitration_isStuck);
  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit = 32'h0;
    if(execute_CsrPlugin_csr_1972) begin
      _zz_CsrPlugin_csrMapping_readDataInit[31 : 0] = CsrPlugin_dataCsrw_value_0;
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_1 = 32'h0;
    if(execute_CsrPlugin_csr_1969) begin
      _zz_CsrPlugin_csrMapping_readDataInit_1[31 : 0] = CsrPlugin_dpc;
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_2 = 32'h0;
    if(execute_CsrPlugin_csr_1968) begin
      _zz_CsrPlugin_csrMapping_readDataInit_2[3 : 3] = CsrPlugin_dcsr_nmip;
      _zz_CsrPlugin_csrMapping_readDataInit_2[8 : 6] = CsrPlugin_dcsr_cause;
      _zz_CsrPlugin_csrMapping_readDataInit_2[31 : 28] = CsrPlugin_dcsr_xdebugver;
      _zz_CsrPlugin_csrMapping_readDataInit_2[4 : 4] = CsrPlugin_dcsr_mprven;
      _zz_CsrPlugin_csrMapping_readDataInit_2[1 : 0] = CsrPlugin_dcsr_prv;
      _zz_CsrPlugin_csrMapping_readDataInit_2[2 : 2] = CsrPlugin_dcsr_step;
      _zz_CsrPlugin_csrMapping_readDataInit_2[9 : 9] = CsrPlugin_dcsr_stoptime;
      _zz_CsrPlugin_csrMapping_readDataInit_2[10 : 10] = CsrPlugin_dcsr_stopcount;
      _zz_CsrPlugin_csrMapping_readDataInit_2[11 : 11] = CsrPlugin_dcsr_stepie;
      _zz_CsrPlugin_csrMapping_readDataInit_2[15 : 15] = CsrPlugin_dcsr_ebreakm;
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_3 = 32'h0;
    if(execute_CsrPlugin_csr_1952) begin
      _zz_CsrPlugin_csrMapping_readDataInit_3[0 : 0] = CsrPlugin_trigger_tselect_index;
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_4 = 32'h0;
    if(execute_CsrPlugin_csr_1956) begin
      _zz_CsrPlugin_csrMapping_readDataInit_4[0 : 0] = CsrPlugin_trigger_tselect_outOfRange;
      _zz_CsrPlugin_csrMapping_readDataInit_4[2 : 2] = (! CsrPlugin_trigger_tselect_outOfRange);
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_5 = 32'h0;
    if(execute_CsrPlugin_csr_1953) begin
      _zz_CsrPlugin_csrMapping_readDataInit_5[31 : 0] = _zz__zz_CsrPlugin_csrMapping_readDataInit_5;
    end
  end

  assign switch_CsrPlugin_l1031 = CsrPlugin_csrMapping_writeDataSignal[12 : 11];
  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_6 = 32'h0;
    if(execute_CsrPlugin_csr_768) begin
      _zz_CsrPlugin_csrMapping_readDataInit_6[7 : 7] = CsrPlugin_mstatus_MPIE;
      _zz_CsrPlugin_csrMapping_readDataInit_6[3 : 3] = CsrPlugin_mstatus_MIE;
      _zz_CsrPlugin_csrMapping_readDataInit_6[12 : 11] = CsrPlugin_mstatus_MPP;
      _zz_CsrPlugin_csrMapping_readDataInit_6[14 : 13] = FpuPlugin_fs;
      _zz_CsrPlugin_csrMapping_readDataInit_6[31 : 31] = FpuPlugin_sd;
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_7 = 32'h0;
    if(execute_CsrPlugin_csr_836) begin
      _zz_CsrPlugin_csrMapping_readDataInit_7[11 : 11] = CsrPlugin_mip_MEIP;
      _zz_CsrPlugin_csrMapping_readDataInit_7[7 : 7] = CsrPlugin_mip_MTIP;
      _zz_CsrPlugin_csrMapping_readDataInit_7[3 : 3] = CsrPlugin_mip_MSIP;
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_8 = 32'h0;
    if(execute_CsrPlugin_csr_772) begin
      _zz_CsrPlugin_csrMapping_readDataInit_8[11 : 11] = CsrPlugin_mie_MEIE;
      _zz_CsrPlugin_csrMapping_readDataInit_8[7 : 7] = CsrPlugin_mie_MTIE;
      _zz_CsrPlugin_csrMapping_readDataInit_8[3 : 3] = CsrPlugin_mie_MSIE;
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_9 = 32'h0;
    if(execute_CsrPlugin_csr_773) begin
      _zz_CsrPlugin_csrMapping_readDataInit_9[31 : 2] = CsrPlugin_mtvec_base;
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_10 = 32'h0;
    if(execute_CsrPlugin_csr_834) begin
      _zz_CsrPlugin_csrMapping_readDataInit_10[31 : 31] = CsrPlugin_mcause_interrupt;
      _zz_CsrPlugin_csrMapping_readDataInit_10[3 : 0] = CsrPlugin_mcause_exceptionCode;
    end
  end

  assign _zz_FpuPlugin_flags_NX = CsrPlugin_csrMapping_writeDataSignal[4 : 0];
  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_11 = 32'h0;
    if(execute_CsrPlugin_csr_3) begin
      _zz_CsrPlugin_csrMapping_readDataInit_11[7 : 5] = FpuPlugin_rm;
      _zz_CsrPlugin_csrMapping_readDataInit_11[4 : 0] = {FpuPlugin_flags_NV,{FpuPlugin_flags_DZ,{FpuPlugin_flags_OF,{FpuPlugin_flags_UF,FpuPlugin_flags_NX}}}};
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_12 = 32'h0;
    if(execute_CsrPlugin_csr_2) begin
      _zz_CsrPlugin_csrMapping_readDataInit_12[2 : 0] = FpuPlugin_rm;
    end
  end

  assign _zz_FpuPlugin_flags_NX_1 = CsrPlugin_csrMapping_writeDataSignal[4 : 0];
  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_13 = 32'h0;
    if(execute_CsrPlugin_csr_1) begin
      _zz_CsrPlugin_csrMapping_readDataInit_13[4 : 0] = {FpuPlugin_flags_NV,{FpuPlugin_flags_DZ,{FpuPlugin_flags_OF,{FpuPlugin_flags_UF,FpuPlugin_flags_NX}}}};
    end
  end

  always @(*) begin
    _zz_CsrPlugin_csrMapping_readDataInit_14 = 32'h0;
    if(execute_CsrPlugin_csr_256) begin
      _zz_CsrPlugin_csrMapping_readDataInit_14[14 : 13] = FpuPlugin_fs;
      _zz_CsrPlugin_csrMapping_readDataInit_14[31 : 31] = FpuPlugin_sd;
    end
  end

  assign CsrPlugin_csrMapping_readDataInit = ((((_zz_CsrPlugin_csrMapping_readDataInit | _zz_CsrPlugin_csrMapping_readDataInit_1) | (_zz_CsrPlugin_csrMapping_readDataInit_2 | _zz_CsrPlugin_csrMapping_readDataInit_3)) | ((_zz_CsrPlugin_csrMapping_readDataInit_4 | _zz_CsrPlugin_csrMapping_readDataInit_5) | (_zz_CsrPlugin_csrMapping_readDataInit_6 | _zz_CsrPlugin_csrMapping_readDataInit_7))) | (((_zz_CsrPlugin_csrMapping_readDataInit_8 | _zz_CsrPlugin_csrMapping_readDataInit_9) | (_zz_CsrPlugin_csrMapping_readDataInit_10 | _zz_CsrPlugin_csrMapping_readDataInit_11)) | ((_zz_CsrPlugin_csrMapping_readDataInit_12 | _zz_CsrPlugin_csrMapping_readDataInit_13) | _zz_CsrPlugin_csrMapping_readDataInit_14)));
  assign when_CsrPlugin_l1702 = ((execute_arbitration_isValid && execute_IS_CSR) && (({execute_CsrPlugin_csrAddress[11 : 2],2'b00} == 12'h3a0) || ({execute_CsrPlugin_csrAddress[11 : 4],4'b0000} == 12'h3b0)));
  assign _zz_when_CsrPlugin_l1709 = (execute_CsrPlugin_csrAddress & 12'hf60);
  assign when_CsrPlugin_l1709 = (((execute_arbitration_isValid && execute_IS_CSR) && (5'h03 <= execute_CsrPlugin_csrAddress[4 : 0])) && (((_zz_when_CsrPlugin_l1709 == 12'hb00) || (((_zz_when_CsrPlugin_l1709 == 12'hc00) && (! execute_CsrPlugin_writeInstruction)) && (CsrPlugin_privilege == 2'b11))) || ((execute_CsrPlugin_csrAddress & 12'hfe0) == 12'h320)));
  always @(*) begin
    when_CsrPlugin_l1719 = CsrPlugin_csrMapping_doForceFailCsr;
    if(when_CsrPlugin_l1717) begin
      when_CsrPlugin_l1719 = 1'b1;
    end
    if(when_CsrPlugin_l1718) begin
      when_CsrPlugin_l1719 = 1'b1;
    end
  end

  assign when_CsrPlugin_l1717 = (CsrPlugin_privilege < execute_CsrPlugin_csrAddress[9 : 8]);
  assign when_CsrPlugin_l1718 = ((! debugMode) && (_zz_when_CsrPlugin_l1718 == 8'h7b));
  assign when_CsrPlugin_l1725 = ((! execute_arbitration_isValid) || (! execute_IS_CSR));
  always @(*) begin
    iBus_cmd_ready = iBus_cmd_m2sPipe_ready;
    if(when_Stream_l375_1) begin
      iBus_cmd_ready = 1'b1;
    end
  end

  assign when_Stream_l375_1 = (! iBus_cmd_m2sPipe_valid);
  assign iBus_cmd_m2sPipe_valid = iBus_cmd_rValid;
  assign iBus_cmd_m2sPipe_payload_pc = iBus_cmd_rData_pc;
  assign iBusWishbone_ADR = (iBus_cmd_m2sPipe_payload_pc >>> 2'd2);
  assign iBusWishbone_CTI = 3'b000;
  assign iBusWishbone_BTE = 2'b00;
  assign iBusWishbone_SEL = 4'b1111;
  assign iBusWishbone_WE = 1'b0;
  assign iBusWishbone_DAT_MOSI = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
  assign iBusWishbone_CYC = iBus_cmd_m2sPipe_valid;
  assign iBusWishbone_STB = iBus_cmd_m2sPipe_valid;
  assign iBus_cmd_m2sPipe_ready = (iBus_cmd_m2sPipe_valid && (iBusWishbone_ACK || iBusWishbone_ERR));
  assign iBus_rsp_valid = (iBusWishbone_CYC && (iBusWishbone_ACK || iBusWishbone_ERR));
  assign iBus_rsp_payload_inst = iBusWishbone_DAT_MISO;
  assign iBus_rsp_payload_error = iBusWishbone_ERR;
  assign _zz_dBusWishbone_ADR_1 = (dBus_cmd_payload_size == 2'b11);
  assign _zz_dBusWishbone_CYC = dBus_cmd_valid;
  assign _zz_dBus_cmd_ready_1 = dBus_cmd_payload_wr;
  assign _zz_dBus_cmd_ready_2 = ((! _zz_dBusWishbone_ADR_1) || (_zz_dBusWishbone_ADR == 1'b1));
  assign dBus_cmd_ready = (_zz_dBus_cmd_ready && (_zz_dBus_cmd_ready_1 || _zz_dBus_cmd_ready_2));
  assign dBusWishbone_ADR = ((_zz_dBusWishbone_ADR_1 ? {{dBus_cmd_payload_address[31 : 3],_zz_dBusWishbone_ADR},2'b00} : {dBus_cmd_payload_address[31 : 2],2'b00}) >>> 2'd2);
  assign dBusWishbone_CTI = (_zz_dBusWishbone_ADR_1 ? (_zz_dBus_cmd_ready_2 ? 3'b111 : 3'b010) : 3'b000);
  assign dBusWishbone_BTE = 2'b00;
  assign dBusWishbone_SEL = (_zz_dBus_cmd_ready_1 ? dBus_cmd_payload_mask : 4'b1111);
  assign dBusWishbone_WE = _zz_dBus_cmd_ready_1;
  assign dBusWishbone_DAT_MOSI = dBus_cmd_payload_data;
  assign _zz_dBus_cmd_ready = (_zz_dBusWishbone_CYC && (dBusWishbone_ACK || dBusWishbone_ERR));
  assign dBusWishbone_CYC = _zz_dBusWishbone_CYC;
  assign dBusWishbone_STB = _zz_dBusWishbone_CYC;
  assign dBus_rsp_valid = _zz_dBus_rsp_valid;
  assign dBus_rsp_payload_data = dBusWishbone_DAT_MISO_regNext;
  assign dBus_rsp_payload_error = dBusWishbone_ERR_regNext;
  always @(posedge clk or posedge reset) begin
    if(reset) begin
      IBusSimplePlugin_fetchPc_pcReg <= 32'h20000000;
      IBusSimplePlugin_fetchPc_correctionReg <= 1'b0;
      IBusSimplePlugin_fetchPc_booted <= 1'b0;
      IBusSimplePlugin_fetchPc_inc <= 1'b0;
      IBusSimplePlugin_decodePc_pcReg <= 32'h20000000;
      _zz_IBusSimplePlugin_iBusRsp_stages_1_input_valid_1 <= 1'b0;
      IBusSimplePlugin_decompressor_bufferValid <= 1'b0;
      IBusSimplePlugin_decompressor_throw2BytesReg <= 1'b0;
      _zz_IBusSimplePlugin_injector_decodeInput_valid <= 1'b0;
      IBusSimplePlugin_injector_nextPcCalc_valids_0 <= 1'b0;
      IBusSimplePlugin_injector_nextPcCalc_valids_1 <= 1'b0;
      IBusSimplePlugin_injector_nextPcCalc_valids_2 <= 1'b0;
      IBusSimplePlugin_injector_nextPcCalc_valids_3 <= 1'b0;
      IBusSimplePlugin_pending_value <= 3'b000;
      IBusSimplePlugin_rspJoin_rspBuffer_discardCounter <= 3'b000;
      DBusCachedPlugin_rspCounter <= 32'h0;
      _zz_CsrPlugin_privilege <= 2'b11;
      CsrPlugin_running <= 1'b1;
      CsrPlugin_reseting <= 1'b1;
      _zz_debugBus_haveReset <= 1'b0;
      CsrPlugin_running_aheadValue_regNext <= 1'b0;
      CsrPlugin_doHalt <= 1'b0;
      _zz_CsrPlugin_doResume <= 1'b0;
      CsrPlugin_timeout_state <= 1'b0;
      CsrPlugin_timeout_counter_value <= 3'b000;
      CsrPlugin_inject_cmd_toStream_rValid <= 1'b0;
      CsrPlugin_inject_pending <= 1'b0;
      CsrPlugin_dcsr_prv <= 2'b11;
      CsrPlugin_dcsr_step <= 1'b0;
      CsrPlugin_dcsr_cause <= 3'b000;
      CsrPlugin_dcsr_stoptime <= 1'b0;
      CsrPlugin_dcsr_stopcount <= 1'b0;
      CsrPlugin_dcsr_stepie <= 1'b0;
      CsrPlugin_dcsr_ebreakm <= 1'b0;
      CsrPlugin_dcsr_stepLogic_stateReg <= CsrPlugin_dcsr_stepLogic_enumDef_BOOT;
      stoptime <= 1'b0;
      CsrPlugin_trigger_decodeBreak_timeout_state <= 1'b0;
      CsrPlugin_trigger_decodeBreak_timeout_counter_value <= 2'b00;
      CsrPlugin_trigger_slots_0_tdata1_dmode <= 1'b0;
      CsrPlugin_trigger_slots_0_tdata1_execute <= 1'b0;
      CsrPlugin_trigger_slots_0_tdata1_m <= 1'b0;
      CsrPlugin_trigger_slots_0_tdata1_s <= 1'b0;
      CsrPlugin_trigger_slots_0_tdata1_u <= 1'b0;
      CsrPlugin_trigger_slots_0_tdata1_action <= 4'b0000;
      CsrPlugin_trigger_slots_1_tdata1_dmode <= 1'b0;
      CsrPlugin_trigger_slots_1_tdata1_execute <= 1'b0;
      CsrPlugin_trigger_slots_1_tdata1_m <= 1'b0;
      CsrPlugin_trigger_slots_1_tdata1_s <= 1'b0;
      CsrPlugin_trigger_slots_1_tdata1_u <= 1'b0;
      CsrPlugin_trigger_slots_1_tdata1_action <= 4'b0000;
      CsrPlugin_mtvec_base <= 30'h00000008;
      CsrPlugin_mstatus_MIE <= 1'b0;
      CsrPlugin_mstatus_MPIE <= 1'b0;
      CsrPlugin_mstatus_MPP <= 2'b11;
      CsrPlugin_mie_MEIE <= 1'b0;
      CsrPlugin_mie_MTIE <= 1'b0;
      CsrPlugin_mie_MSIE <= 1'b0;
      CsrPlugin_mcycle <= 64'h0;
      CsrPlugin_minstret <= 64'h0;
      CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_decode <= 1'b0;
      CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_execute <= 1'b0;
      CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_memory <= 1'b0;
      CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_writeBack <= 1'b0;
      CsrPlugin_interrupt_valid <= 1'b0;
      CsrPlugin_pipelineLiberator_pcValids_0 <= 1'b0;
      CsrPlugin_pipelineLiberator_pcValids_1 <= 1'b0;
      CsrPlugin_pipelineLiberator_pcValids_2 <= 1'b0;
      CsrPlugin_hadException <= 1'b0;
      execute_CsrPlugin_wfiWake <= 1'b0;
      _zz_5 <= 1'b1;
      memory_DivPlugin_div_counter_value <= 6'h0;
      FpuPlugin_pendings <= 6'h0;
      FpuPlugin_flags_NV <= 1'b0;
      FpuPlugin_flags_DZ <= 1'b0;
      FpuPlugin_flags_OF <= 1'b0;
      FpuPlugin_flags_UF <= 1'b0;
      FpuPlugin_flags_NX <= 1'b0;
      FpuPlugin_rm <= 3'b000;
      FpuPlugin_fs <= 2'b01;
      decode_FpuPlugin_forked <= 1'b0;
      writeBack_FpuPlugin_commit_rValidN <= 1'b1;
      execute_LightShifterPlugin_isActive <= 1'b0;
      HazardSimplePlugin_writeBackBuffer_valid <= 1'b0;
      toplevel_debugModule_1_io_harts_0_dmToHart_regNext_valid <= 1'b0;
      execute_arbitration_isValid <= 1'b0;
      memory_arbitration_isValid <= 1'b0;
      writeBack_arbitration_isValid <= 1'b0;
      IBusSimplePlugin_injector_port_state <= 3'b000;
      decode_to_execute_FPU_FORKED <= 1'b0;
      execute_to_memory_FPU_FORKED <= 1'b0;
      memory_to_writeBack_FPU_FORKED <= 1'b0;
      iBus_cmd_rValid <= 1'b0;
      _zz_dBusWishbone_ADR <= 1'b0;
      _zz_dBus_rsp_valid <= 1'b0;
    end else begin
      if(IBusSimplePlugin_fetchPc_correction) begin
        IBusSimplePlugin_fetchPc_correctionReg <= 1'b1;
      end
      if(IBusSimplePlugin_fetchPc_output_fire) begin
        IBusSimplePlugin_fetchPc_correctionReg <= 1'b0;
      end
      IBusSimplePlugin_fetchPc_booted <= 1'b1;
      if(when_Fetcher_l133) begin
        IBusSimplePlugin_fetchPc_inc <= 1'b0;
      end
      if(IBusSimplePlugin_fetchPc_output_fire) begin
        IBusSimplePlugin_fetchPc_inc <= 1'b1;
      end
      if(when_Fetcher_l133_1) begin
        IBusSimplePlugin_fetchPc_inc <= 1'b0;
      end
      if(when_Fetcher_l160) begin
        IBusSimplePlugin_fetchPc_pcReg <= IBusSimplePlugin_fetchPc_pc;
      end
      if(when_Fetcher_l182) begin
        IBusSimplePlugin_decodePc_pcReg <= IBusSimplePlugin_decodePc_pcPlus;
      end
      if(when_Fetcher_l194) begin
        IBusSimplePlugin_decodePc_pcReg <= IBusSimplePlugin_jump_pcLoad_payload;
      end
      if(IBusSimplePlugin_iBusRsp_flush) begin
        _zz_IBusSimplePlugin_iBusRsp_stages_1_input_valid_1 <= 1'b0;
      end
      if(_zz_IBusSimplePlugin_iBusRsp_stages_0_output_ready) begin
        _zz_IBusSimplePlugin_iBusRsp_stages_1_input_valid_1 <= (IBusSimplePlugin_iBusRsp_stages_0_output_valid && (! 1'b0));
      end
      if(IBusSimplePlugin_decompressor_output_fire) begin
        IBusSimplePlugin_decompressor_throw2BytesReg <= ((((! IBusSimplePlugin_decompressor_unaligned) && IBusSimplePlugin_decompressor_isInputLowRvc) && IBusSimplePlugin_decompressor_isInputHighRvc) || (IBusSimplePlugin_decompressor_bufferValid && IBusSimplePlugin_decompressor_isInputHighRvc));
      end
      if(when_Fetcher_l285) begin
        IBusSimplePlugin_decompressor_bufferValid <= 1'b0;
      end
      if(when_Fetcher_l288) begin
        if(IBusSimplePlugin_decompressor_bufferFill) begin
          IBusSimplePlugin_decompressor_bufferValid <= 1'b1;
        end
      end
      if(when_Fetcher_l293) begin
        IBusSimplePlugin_decompressor_throw2BytesReg <= 1'b0;
        IBusSimplePlugin_decompressor_bufferValid <= 1'b0;
      end
      if(decode_arbitration_removeIt) begin
        _zz_IBusSimplePlugin_injector_decodeInput_valid <= 1'b0;
      end
      if(IBusSimplePlugin_decompressor_output_ready) begin
        _zz_IBusSimplePlugin_injector_decodeInput_valid <= (IBusSimplePlugin_decompressor_output_valid && (! IBusSimplePlugin_externalFlush));
      end
      if(when_Fetcher_l331) begin
        IBusSimplePlugin_injector_nextPcCalc_valids_0 <= 1'b1;
      end
      if(IBusSimplePlugin_decodePc_flushed) begin
        IBusSimplePlugin_injector_nextPcCalc_valids_0 <= 1'b0;
      end
      if(when_Fetcher_l331_1) begin
        IBusSimplePlugin_injector_nextPcCalc_valids_1 <= IBusSimplePlugin_injector_nextPcCalc_valids_0;
      end
      if(IBusSimplePlugin_decodePc_flushed) begin
        IBusSimplePlugin_injector_nextPcCalc_valids_1 <= 1'b0;
      end
      if(when_Fetcher_l331_2) begin
        IBusSimplePlugin_injector_nextPcCalc_valids_2 <= IBusSimplePlugin_injector_nextPcCalc_valids_1;
      end
      if(IBusSimplePlugin_decodePc_flushed) begin
        IBusSimplePlugin_injector_nextPcCalc_valids_2 <= 1'b0;
      end
      if(when_Fetcher_l331_3) begin
        IBusSimplePlugin_injector_nextPcCalc_valids_3 <= IBusSimplePlugin_injector_nextPcCalc_valids_2;
      end
      if(IBusSimplePlugin_decodePc_flushed) begin
        IBusSimplePlugin_injector_nextPcCalc_valids_3 <= 1'b0;
      end
      IBusSimplePlugin_pending_value <= IBusSimplePlugin_pending_next;
      IBusSimplePlugin_rspJoin_rspBuffer_discardCounter <= (IBusSimplePlugin_rspJoin_rspBuffer_discardCounter - _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter);
      if(IBusSimplePlugin_iBusRsp_flush) begin
        IBusSimplePlugin_rspJoin_rspBuffer_discardCounter <= (IBusSimplePlugin_pending_value - _zz_IBusSimplePlugin_rspJoin_rspBuffer_discardCounter_2);
      end
      if(dBus_rsp_valid) begin
        DBusCachedPlugin_rspCounter <= (DBusCachedPlugin_rspCounter + 32'h00000001);
      end
      CsrPlugin_reseting <= 1'b0;
      if(CsrPlugin_reseting) begin
        _zz_debugBus_haveReset <= 1'b1;
      end
      if(debugBus_ackReset) begin
        _zz_debugBus_haveReset <= 1'b0;
      end
      CsrPlugin_running_aheadValue_regNext <= CsrPlugin_running_aheadValue;
      if(when_CsrPlugin_l729) begin
        CsrPlugin_doHalt <= 1'b1;
      end
      if(CsrPlugin_enterHalt) begin
        CsrPlugin_doHalt <= 1'b0;
      end
      if(debugBus_resume_cmd_valid) begin
        _zz_CsrPlugin_doResume <= 1'b1;
      end
      if(debugBus_resume_rsp_valid) begin
        _zz_CsrPlugin_doResume <= 1'b0;
      end
      CsrPlugin_timeout_counter_value <= CsrPlugin_timeout_counter_valueNext;
      if(CsrPlugin_timeout_counter_willOverflow) begin
        CsrPlugin_timeout_state <= 1'b1;
      end
      if(when_CsrPlugin_l735) begin
        CsrPlugin_timeout_state <= 1'b0;
      end
      if(CsrPlugin_inject_cmd_toStream_ready) begin
        CsrPlugin_inject_cmd_toStream_rValid <= CsrPlugin_inject_cmd_toStream_valid;
      end
      if(when_CsrPlugin_l786) begin
        CsrPlugin_inject_pending <= 1'b1;
      end
      if(when_CsrPlugin_l786_1) begin
        CsrPlugin_inject_pending <= 1'b0;
      end
      if(CsrPlugin_inject_cmd_valid) begin
        CsrPlugin_timeout_state <= 1'b0;
      end
      CsrPlugin_dcsr_stepLogic_stateReg <= CsrPlugin_dcsr_stepLogic_stateNext;
      case(CsrPlugin_dcsr_stepLogic_stateReg)
        CsrPlugin_dcsr_stepLogic_enumDef_IDLE : begin
        end
        CsrPlugin_dcsr_stepLogic_enumDef_SINGLE : begin
          CsrPlugin_timeout_state <= 1'b0;
          if(when_CsrPlugin_l818) begin
            CsrPlugin_doHalt <= 1'b1;
          end
        end
        CsrPlugin_dcsr_stepLogic_enumDef_WAIT_1 : begin
          if(!when_CsrPlugin_l830) begin
            if(writeBack_arbitration_isFiring) begin
              CsrPlugin_doHalt <= 1'b1;
            end
          end
        end
        default : begin
        end
      endcase
      stoptime <= (debugMode && CsrPlugin_dcsr_stoptime);
      CsrPlugin_trigger_decodeBreak_timeout_counter_value <= CsrPlugin_trigger_decodeBreak_timeout_counter_valueNext;
      if(CsrPlugin_trigger_decodeBreak_timeout_counter_willOverflow) begin
        CsrPlugin_trigger_decodeBreak_timeout_state <= 1'b1;
      end
      if(when_Utils_l669) begin
        CsrPlugin_trigger_decodeBreak_timeout_state <= 1'b0;
      end
      if(CsrPlugin_trigger_decodeBreak_enabled) begin
        if(CsrPlugin_trigger_decodeBreak_timeout_state) begin
          CsrPlugin_dcsr_cause <= 3'b010;
          CsrPlugin_dcsr_prv <= CsrPlugin_privilege;
          _zz_CsrPlugin_privilege <= 2'b11;
        end
      end
      CsrPlugin_mcycle <= (CsrPlugin_mcycle + _zz_CsrPlugin_mcycle);
      if(writeBack_arbitration_isFiring) begin
        CsrPlugin_minstret <= (CsrPlugin_minstret + 64'h0000000000000001);
      end
      if(when_CsrPlugin_l1259) begin
        CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_decode <= 1'b0;
      end else begin
        CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_decode <= CsrPlugin_exceptionPortCtrl_exceptionValids_decode;
      end
      if(when_CsrPlugin_l1259_1) begin
        CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_execute <= (CsrPlugin_exceptionPortCtrl_exceptionValids_decode && (! decode_arbitration_isStuck));
      end else begin
        CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_execute <= CsrPlugin_exceptionPortCtrl_exceptionValids_execute;
      end
      if(when_CsrPlugin_l1259_2) begin
        CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_memory <= (CsrPlugin_exceptionPortCtrl_exceptionValids_execute && (! execute_arbitration_isStuck));
      end else begin
        CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_memory <= CsrPlugin_exceptionPortCtrl_exceptionValids_memory;
      end
      if(when_CsrPlugin_l1259_3) begin
        CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_writeBack <= (CsrPlugin_exceptionPortCtrl_exceptionValids_memory && (! memory_arbitration_isStuck));
      end else begin
        CsrPlugin_exceptionPortCtrl_exceptionValidsRegs_writeBack <= 1'b0;
      end
      CsrPlugin_interrupt_valid <= 1'b0;
      if(when_CsrPlugin_l1296) begin
        if(when_CsrPlugin_l1302) begin
          CsrPlugin_interrupt_valid <= 1'b1;
        end
        if(when_CsrPlugin_l1302_1) begin
          CsrPlugin_interrupt_valid <= 1'b1;
        end
        if(when_CsrPlugin_l1302_2) begin
          CsrPlugin_interrupt_valid <= 1'b1;
        end
      end
      if(when_CsrPlugin_l1315) begin
        CsrPlugin_interrupt_valid <= 1'b0;
      end
      if(CsrPlugin_doHalt) begin
        CsrPlugin_interrupt_valid <= 1'b1;
      end
      if(CsrPlugin_pipelineLiberator_active) begin
        if(when_CsrPlugin_l1335) begin
          CsrPlugin_pipelineLiberator_pcValids_0 <= 1'b1;
        end
        if(when_CsrPlugin_l1335_1) begin
          CsrPlugin_pipelineLiberator_pcValids_1 <= CsrPlugin_pipelineLiberator_pcValids_0;
        end
        if(when_CsrPlugin_l1335_2) begin
          CsrPlugin_pipelineLiberator_pcValids_2 <= CsrPlugin_pipelineLiberator_pcValids_1;
        end
      end
      if(when_CsrPlugin_l1340) begin
        CsrPlugin_pipelineLiberator_pcValids_0 <= 1'b0;
        CsrPlugin_pipelineLiberator_pcValids_1 <= 1'b0;
        CsrPlugin_pipelineLiberator_pcValids_2 <= 1'b0;
      end
      if(CsrPlugin_interruptJump) begin
        CsrPlugin_interrupt_valid <= 1'b0;
      end
      CsrPlugin_hadException <= CsrPlugin_exception;
      if(when_CsrPlugin_l1390) begin
        if(when_CsrPlugin_l1398) begin
          _zz_CsrPlugin_privilege <= CsrPlugin_targetPrivilege;
          case(CsrPlugin_targetPrivilege)
            2'b11 : begin
              CsrPlugin_mstatus_MIE <= 1'b0;
              CsrPlugin_mstatus_MPIE <= CsrPlugin_mstatus_MIE;
              CsrPlugin_mstatus_MPP <= CsrPlugin_privilege;
            end
            default : begin
            end
          endcase
        end else begin
          if(when_CsrPlugin_l1428) begin
            CsrPlugin_dcsr_cause <= 3'b011;
            if(CsrPlugin_dcsr_step) begin
              CsrPlugin_dcsr_cause <= 3'b100;
            end
            if(CsrPlugin_trapCauseEbreakDebug) begin
              CsrPlugin_dcsr_cause <= 3'b001;
            end
            CsrPlugin_dcsr_prv <= CsrPlugin_privilege;
          end
          _zz_CsrPlugin_privilege <= 2'b11;
        end
      end
      if(when_CsrPlugin_l1456) begin
        case(switch_CsrPlugin_l1460)
          2'b11 : begin
            CsrPlugin_mstatus_MPP <= 2'b00;
            CsrPlugin_mstatus_MIE <= CsrPlugin_mstatus_MPIE;
            CsrPlugin_mstatus_MPIE <= 1'b1;
            _zz_CsrPlugin_privilege <= CsrPlugin_mstatus_MPP;
          end
          default : begin
          end
        endcase
      end
      if(CsrPlugin_doResume) begin
        _zz_CsrPlugin_privilege <= CsrPlugin_dcsr_prv;
      end
      execute_CsrPlugin_wfiWake <= ((|{_zz_when_CsrPlugin_l1302_2,{_zz_when_CsrPlugin_l1302_1,_zz_when_CsrPlugin_l1302}}) || CsrPlugin_thirdPartyWake);
      _zz_5 <= 1'b0;
      memory_DivPlugin_div_counter_value <= memory_DivPlugin_div_counter_valueNext;
      FpuPlugin_pendings <= (_zz_FpuPlugin_pendings - _zz_FpuPlugin_pendings_6);
      if(when_FpuPlugin_l215) begin
        FpuPlugin_flags_NV <= 1'b1;
      end
      if(when_FpuPlugin_l216) begin
        FpuPlugin_flags_DZ <= 1'b1;
      end
      if(when_FpuPlugin_l217) begin
        FpuPlugin_flags_OF <= 1'b1;
      end
      if(when_FpuPlugin_l218) begin
        FpuPlugin_flags_UF <= 1'b1;
      end
      if(when_FpuPlugin_l219) begin
        FpuPlugin_flags_NX <= 1'b1;
      end
      if(when_FpuPlugin_l234) begin
        FpuPlugin_fs <= 2'b11;
      end
      if(when_FpuPlugin_l237) begin
        FpuPlugin_fs <= 2'b11;
      end
      if(when_FpuPlugin_l268) begin
        decode_FpuPlugin_forked <= 1'b1;
      end
      if(when_FpuPlugin_l268_1) begin
        decode_FpuPlugin_forked <= 1'b0;
      end
      if(writeBack_FpuPlugin_isRsp) begin
        if(writeBack_arbitration_isValid) begin
          if(when_FpuPlugin_l315) begin
            if(FpuPlugin_port_rsp_payload_NV) begin
              FpuPlugin_flags_NV <= 1'b1;
            end
            if(FpuPlugin_port_rsp_payload_NX) begin
              FpuPlugin_flags_NX <= 1'b1;
            end
            if(when_FpuPlugin_l318) begin
              FpuPlugin_fs <= 2'b11;
            end
          end
        end
      end
      if(writeBack_FpuPlugin_commit_valid) begin
        writeBack_FpuPlugin_commit_rValidN <= 1'b0;
      end
      if(writeBack_FpuPlugin_commit_s2mPipe_ready) begin
        writeBack_FpuPlugin_commit_rValidN <= 1'b1;
      end
      if(when_ShiftPlugins_l169) begin
        if(when_ShiftPlugins_l175) begin
          execute_LightShifterPlugin_isActive <= 1'b1;
          if(execute_LightShifterPlugin_done) begin
            execute_LightShifterPlugin_isActive <= 1'b0;
          end
        end
      end
      if(execute_arbitration_removeIt) begin
        execute_LightShifterPlugin_isActive <= 1'b0;
      end
      HazardSimplePlugin_writeBackBuffer_valid <= HazardSimplePlugin_writeBackWrites_valid;
      toplevel_debugModule_1_io_harts_0_dmToHart_regNext_valid <= debugModule_1_io_harts_0_dmToHart_valid;
      if(when_Pipeline_l124_58) begin
        decode_to_execute_FPU_FORKED <= _zz_decode_to_execute_FPU_FORKED;
      end
      if(when_Pipeline_l124_59) begin
        execute_to_memory_FPU_FORKED <= _zz_execute_to_memory_FPU_FORKED;
      end
      if(when_Pipeline_l124_60) begin
        memory_to_writeBack_FPU_FORKED <= _zz_memory_to_writeBack_FPU_FORKED;
      end
      if(when_Pipeline_l151) begin
        execute_arbitration_isValid <= 1'b0;
      end
      if(when_Pipeline_l154) begin
        execute_arbitration_isValid <= decode_arbitration_isValid;
      end
      if(when_Pipeline_l151_1) begin
        memory_arbitration_isValid <= 1'b0;
      end
      if(when_Pipeline_l154_1) begin
        memory_arbitration_isValid <= execute_arbitration_isValid;
      end
      if(when_Pipeline_l151_2) begin
        writeBack_arbitration_isValid <= 1'b0;
      end
      if(when_Pipeline_l154_2) begin
        writeBack_arbitration_isValid <= memory_arbitration_isValid;
      end
      case(IBusSimplePlugin_injector_port_state)
        3'b000 : begin
          if(CsrPlugin_injectionPort_valid) begin
            IBusSimplePlugin_injector_port_state <= 3'b001;
          end
        end
        3'b001 : begin
          IBusSimplePlugin_injector_port_state <= 3'b010;
        end
        3'b010 : begin
          IBusSimplePlugin_injector_port_state <= 3'b011;
        end
        3'b011 : begin
          if(when_Fetcher_l391) begin
            IBusSimplePlugin_injector_port_state <= 3'b100;
          end
        end
        3'b100 : begin
          IBusSimplePlugin_injector_port_state <= 3'b000;
        end
        default : begin
        end
      endcase
      if(execute_CsrPlugin_csr_1968) begin
        if(execute_CsrPlugin_writeEnable) begin
          CsrPlugin_dcsr_prv <= CsrPlugin_csrMapping_writeDataSignal[1 : 0];
          CsrPlugin_dcsr_step <= CsrPlugin_csrMapping_writeDataSignal[2];
          CsrPlugin_dcsr_stoptime <= CsrPlugin_csrMapping_writeDataSignal[9];
          CsrPlugin_dcsr_stopcount <= CsrPlugin_csrMapping_writeDataSignal[10];
          CsrPlugin_dcsr_stepie <= CsrPlugin_csrMapping_writeDataSignal[11];
          CsrPlugin_dcsr_ebreakm <= CsrPlugin_csrMapping_writeDataSignal[15];
        end
      end
      if(execute_CsrPlugin_csr_1953) begin
        if(execute_CsrPlugin_writeEnable) begin
          if(CsrPlugin_trigger_slots_0_selected) begin
            CsrPlugin_trigger_slots_0_tdata1_execute <= CsrPlugin_csrMapping_writeDataSignal[2];
            CsrPlugin_trigger_slots_0_tdata1_u <= CsrPlugin_csrMapping_writeDataSignal[3];
            CsrPlugin_trigger_slots_0_tdata1_s <= CsrPlugin_csrMapping_writeDataSignal[4];
            CsrPlugin_trigger_slots_0_tdata1_m <= CsrPlugin_csrMapping_writeDataSignal[6];
            CsrPlugin_trigger_slots_0_tdata1_dmode <= CsrPlugin_csrMapping_writeDataSignal[27];
            CsrPlugin_trigger_slots_0_tdata1_action <= CsrPlugin_csrMapping_writeDataSignal[15 : 12];
          end
          if(CsrPlugin_trigger_slots_1_selected) begin
            CsrPlugin_trigger_slots_1_tdata1_execute <= CsrPlugin_csrMapping_writeDataSignal[2];
            CsrPlugin_trigger_slots_1_tdata1_u <= CsrPlugin_csrMapping_writeDataSignal[3];
            CsrPlugin_trigger_slots_1_tdata1_s <= CsrPlugin_csrMapping_writeDataSignal[4];
            CsrPlugin_trigger_slots_1_tdata1_m <= CsrPlugin_csrMapping_writeDataSignal[6];
            CsrPlugin_trigger_slots_1_tdata1_dmode <= CsrPlugin_csrMapping_writeDataSignal[27];
            CsrPlugin_trigger_slots_1_tdata1_action <= CsrPlugin_csrMapping_writeDataSignal[15 : 12];
          end
        end
      end
      if(execute_CsrPlugin_csr_768) begin
        if(execute_CsrPlugin_writeEnable) begin
          CsrPlugin_mstatus_MPIE <= CsrPlugin_csrMapping_writeDataSignal[7];
          CsrPlugin_mstatus_MIE <= CsrPlugin_csrMapping_writeDataSignal[3];
          case(switch_CsrPlugin_l1031)
            2'b11 : begin
              CsrPlugin_mstatus_MPP <= 2'b11;
            end
            default : begin
            end
          endcase
          FpuPlugin_fs <= CsrPlugin_csrMapping_writeDataSignal[14 : 13];
        end
      end
      if(execute_CsrPlugin_csr_772) begin
        if(execute_CsrPlugin_writeEnable) begin
          CsrPlugin_mie_MEIE <= CsrPlugin_csrMapping_writeDataSignal[11];
          CsrPlugin_mie_MTIE <= CsrPlugin_csrMapping_writeDataSignal[7];
          CsrPlugin_mie_MSIE <= CsrPlugin_csrMapping_writeDataSignal[3];
        end
      end
      if(execute_CsrPlugin_csr_773) begin
        if(execute_CsrPlugin_writeEnable) begin
          CsrPlugin_mtvec_base <= CsrPlugin_csrMapping_writeDataSignal[31 : 2];
        end
      end
      if(execute_CsrPlugin_csr_3) begin
        if(execute_CsrPlugin_writeEnable) begin
          FpuPlugin_rm <= CsrPlugin_csrMapping_writeDataSignal[7 : 5];
          FpuPlugin_flags_NX <= _zz_FpuPlugin_flags_NX[0];
          FpuPlugin_flags_UF <= _zz_FpuPlugin_flags_NX[1];
          FpuPlugin_flags_OF <= _zz_FpuPlugin_flags_NX[2];
          FpuPlugin_flags_DZ <= _zz_FpuPlugin_flags_NX[3];
          FpuPlugin_flags_NV <= _zz_FpuPlugin_flags_NX[4];
        end
      end
      if(execute_CsrPlugin_csr_2) begin
        if(execute_CsrPlugin_writeEnable) begin
          FpuPlugin_rm <= CsrPlugin_csrMapping_writeDataSignal[2 : 0];
        end
      end
      if(execute_CsrPlugin_csr_1) begin
        if(execute_CsrPlugin_writeEnable) begin
          FpuPlugin_flags_NX <= _zz_FpuPlugin_flags_NX_1[0];
          FpuPlugin_flags_UF <= _zz_FpuPlugin_flags_NX_1[1];
          FpuPlugin_flags_OF <= _zz_FpuPlugin_flags_NX_1[2];
          FpuPlugin_flags_DZ <= _zz_FpuPlugin_flags_NX_1[3];
          FpuPlugin_flags_NV <= _zz_FpuPlugin_flags_NX_1[4];
        end
      end
      if(execute_CsrPlugin_csr_256) begin
        if(execute_CsrPlugin_writeEnable) begin
          FpuPlugin_fs <= CsrPlugin_csrMapping_writeDataSignal[14 : 13];
        end
      end
      if(iBus_cmd_ready) begin
        iBus_cmd_rValid <= iBus_cmd_valid;
      end
      if((_zz_dBusWishbone_CYC && _zz_dBus_cmd_ready)) begin
        _zz_dBusWishbone_ADR <= (_zz_dBusWishbone_ADR + 1'b1);
        if(_zz_dBus_cmd_ready_2) begin
          _zz_dBusWishbone_ADR <= 1'b0;
        end
      end
      _zz_dBus_rsp_valid <= ((_zz_dBusWishbone_CYC && (! dBusWishbone_WE)) && (dBusWishbone_ACK || dBusWishbone_ERR));
      CsrPlugin_running <= CsrPlugin_running_aheadValue;
    end
  end

  always @(posedge clk) begin
    if(IBusSimplePlugin_decompressor_input_valid) begin
      IBusSimplePlugin_decompressor_bufferValidLatch <= IBusSimplePlugin_decompressor_bufferValid;
    end
    if(IBusSimplePlugin_decompressor_input_valid) begin
      IBusSimplePlugin_decompressor_throw2BytesLatch <= IBusSimplePlugin_decompressor_throw2Bytes;
    end
    if(when_Fetcher_l288) begin
      IBusSimplePlugin_decompressor_bufferData <= IBusSimplePlugin_decompressor_input_payload_rsp_inst[31 : 16];
    end
    if(IBusSimplePlugin_decompressor_output_ready) begin
      _zz_IBusSimplePlugin_injector_decodeInput_payload_pc <= IBusSimplePlugin_decompressor_output_payload_pc;
      _zz_IBusSimplePlugin_injector_decodeInput_payload_rsp_error <= IBusSimplePlugin_decompressor_output_payload_rsp_error;
      _zz_IBusSimplePlugin_injector_decodeInput_payload_rsp_inst <= IBusSimplePlugin_decompressor_output_payload_rsp_inst;
      _zz_IBusSimplePlugin_injector_decodeInput_payload_isRvc <= IBusSimplePlugin_decompressor_output_payload_isRvc;
    end
    if(IBusSimplePlugin_injector_decodeInput_ready) begin
      IBusSimplePlugin_injector_formal_rawInDecode <= IBusSimplePlugin_decompressor_raw;
    end
    if(when_CsrPlugin_l750) begin
      if(_zz_when[0]) begin
        CsrPlugin_dataCsrw_value_0 <= debugBus_dmToHart_payload_data;
      end
    end
    if(CsrPlugin_inject_cmd_toStream_ready) begin
      CsrPlugin_inject_cmd_toStream_rData_op <= CsrPlugin_inject_cmd_toStream_payload_op;
      CsrPlugin_inject_cmd_toStream_rData_address <= CsrPlugin_inject_cmd_toStream_payload_address;
      CsrPlugin_inject_cmd_toStream_rData_data <= CsrPlugin_inject_cmd_toStream_payload_data;
      CsrPlugin_inject_cmd_toStream_rData_size <= CsrPlugin_inject_cmd_toStream_payload_size;
    end
    if(CsrPlugin_trigger_decodeBreak_enabled) begin
      if(CsrPlugin_trigger_decodeBreak_timeout_state) begin
        CsrPlugin_dpc <= decode_PC;
      end
    end
    CsrPlugin_mip_MEIP <= externalInterrupt;
    CsrPlugin_mip_MTIP <= timerInterrupt;
    CsrPlugin_mip_MSIP <= softwareInterrupt;
    if(_zz_when_1) begin
      CsrPlugin_exceptionPortCtrl_exceptionContext_code <= (_zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code_1 ? IBusSimplePlugin_decodeExceptionPort_payload_code : decodeExceptionPort_payload_code);
      CsrPlugin_exceptionPortCtrl_exceptionContext_badAddr <= (_zz_CsrPlugin_exceptionPortCtrl_exceptionContext_code_1 ? IBusSimplePlugin_decodeExceptionPort_payload_badAddr : decodeExceptionPort_payload_badAddr);
    end
    if(CsrPlugin_selfException_valid) begin
      CsrPlugin_exceptionPortCtrl_exceptionContext_code <= CsrPlugin_selfException_payload_code;
      CsrPlugin_exceptionPortCtrl_exceptionContext_badAddr <= CsrPlugin_selfException_payload_badAddr;
    end
    if(DBusCachedPlugin_exceptionBus_valid) begin
      CsrPlugin_exceptionPortCtrl_exceptionContext_code <= DBusCachedPlugin_exceptionBus_payload_code;
      CsrPlugin_exceptionPortCtrl_exceptionContext_badAddr <= DBusCachedPlugin_exceptionBus_payload_badAddr;
    end
    if(when_CsrPlugin_l1296) begin
      if(when_CsrPlugin_l1302) begin
        CsrPlugin_interrupt_code <= 4'b0111;
        CsrPlugin_interrupt_targetPrivilege <= 2'b11;
      end
      if(when_CsrPlugin_l1302_1) begin
        CsrPlugin_interrupt_code <= 4'b0011;
        CsrPlugin_interrupt_targetPrivilege <= 2'b11;
      end
      if(when_CsrPlugin_l1302_2) begin
        CsrPlugin_interrupt_code <= 4'b1011;
        CsrPlugin_interrupt_targetPrivilege <= 2'b11;
      end
    end
    if(when_CsrPlugin_l1390) begin
      if(when_CsrPlugin_l1398) begin
        case(CsrPlugin_targetPrivilege)
          2'b11 : begin
            CsrPlugin_mcause_interrupt <= (! CsrPlugin_hadException);
            CsrPlugin_mcause_exceptionCode <= CsrPlugin_trapCause;
            CsrPlugin_mepc <= writeBack_PC;
            if(CsrPlugin_hadException) begin
              CsrPlugin_mtval <= CsrPlugin_exceptionPortCtrl_exceptionContext_badAddr;
            end
          end
          default : begin
          end
        endcase
      end else begin
        if(when_CsrPlugin_l1428) begin
          CsrPlugin_dpc <= writeBack_PC;
        end
      end
    end
    if(when_MulDivIterativePlugin_l126) begin
      memory_DivPlugin_div_done <= 1'b1;
    end
    if(when_MulDivIterativePlugin_l126_1) begin
      memory_DivPlugin_div_done <= 1'b0;
    end
    if(when_MulDivIterativePlugin_l128) begin
      if(when_MulDivIterativePlugin_l132) begin
        memory_DivPlugin_rs1[31 : 0] <= memory_DivPlugin_div_stage_0_outNumerator;
        memory_DivPlugin_accumulator[31 : 0] <= memory_DivPlugin_div_stage_0_outRemainder;
        if(when_MulDivIterativePlugin_l151) begin
          memory_DivPlugin_div_result <= _zz_memory_DivPlugin_div_result_1[31:0];
        end
      end
    end
    if(when_MulDivIterativePlugin_l162) begin
      memory_DivPlugin_accumulator <= 65'h0;
      memory_DivPlugin_rs1 <= ((_zz_memory_DivPlugin_rs1 ? (~ _zz_memory_DivPlugin_rs1_1) : _zz_memory_DivPlugin_rs1_1) + _zz_memory_DivPlugin_rs1_2);
      memory_DivPlugin_rs2 <= ((_zz_memory_DivPlugin_rs2 ? (~ execute_RS2) : execute_RS2) + _zz_memory_DivPlugin_rs2_1);
      memory_DivPlugin_div_needRevert <= ((_zz_memory_DivPlugin_rs1 ^ (_zz_memory_DivPlugin_rs2 && (! execute_INSTRUCTION[13]))) && (! (((execute_RS2 == 32'h0) && execute_IS_RS2_SIGNED) && (! execute_INSTRUCTION[13]))));
    end
    if(writeBack_FpuPlugin_commit_ready) begin
      writeBack_FpuPlugin_commit_rData_opcode <= writeBack_FpuPlugin_commit_payload_opcode;
      writeBack_FpuPlugin_commit_rData_rd <= writeBack_FpuPlugin_commit_payload_rd;
      writeBack_FpuPlugin_commit_rData_write <= writeBack_FpuPlugin_commit_payload_write;
      writeBack_FpuPlugin_commit_rData_value <= writeBack_FpuPlugin_commit_payload_value;
    end
    if(when_ShiftPlugins_l169) begin
      if(when_ShiftPlugins_l175) begin
        execute_LightShifterPlugin_amplitudeReg <= (execute_LightShifterPlugin_amplitude - 5'h01);
      end
    end
    HazardSimplePlugin_writeBackBuffer_payload_address <= HazardSimplePlugin_writeBackWrites_payload_address;
    HazardSimplePlugin_writeBackBuffer_payload_data <= HazardSimplePlugin_writeBackWrites_payload_data;
    toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_op <= debugModule_1_io_harts_0_dmToHart_payload_op;
    toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_address <= debugModule_1_io_harts_0_dmToHart_payload_address;
    toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_data <= debugModule_1_io_harts_0_dmToHart_payload_data;
    toplevel_debugModule_1_io_harts_0_dmToHart_regNext_payload_size <= debugModule_1_io_harts_0_dmToHart_payload_size;
    if(when_Pipeline_l124) begin
      decode_to_execute_PC <= _zz_decode_to_execute_PC;
    end
    if(when_Pipeline_l124_1) begin
      execute_to_memory_PC <= execute_PC;
    end
    if(when_Pipeline_l124_2) begin
      memory_to_writeBack_PC <= memory_PC;
    end
    if(when_Pipeline_l124_3) begin
      decode_to_execute_INSTRUCTION <= decode_INSTRUCTION;
    end
    if(when_Pipeline_l124_4) begin
      execute_to_memory_INSTRUCTION <= execute_INSTRUCTION;
    end
    if(when_Pipeline_l124_5) begin
      memory_to_writeBack_INSTRUCTION <= memory_INSTRUCTION;
    end
    if(when_Pipeline_l124_6) begin
      decode_to_execute_FORMAL_PC_NEXT <= decode_FORMAL_PC_NEXT;
    end
    if(when_Pipeline_l124_7) begin
      execute_to_memory_FORMAL_PC_NEXT <= execute_FORMAL_PC_NEXT;
    end
    if(when_Pipeline_l124_8) begin
      memory_to_writeBack_FORMAL_PC_NEXT <= _zz_memory_to_writeBack_FORMAL_PC_NEXT;
    end
    if(when_Pipeline_l124_9) begin
      decode_to_execute_MEMORY_FORCE_CONSTISTENCY <= decode_MEMORY_FORCE_CONSTISTENCY;
    end
    if(when_Pipeline_l124_10) begin
      decode_to_execute_CSR_WRITE_OPCODE <= decode_CSR_WRITE_OPCODE;
    end
    if(when_Pipeline_l124_11) begin
      decode_to_execute_CSR_READ_OPCODE <= decode_CSR_READ_OPCODE;
    end
    if(when_Pipeline_l124_12) begin
      decode_to_execute_SRC_USE_SUB_LESS <= decode_SRC_USE_SUB_LESS;
    end
    if(when_Pipeline_l124_13) begin
      decode_to_execute_MEMORY_ENABLE <= decode_MEMORY_ENABLE;
    end
    if(when_Pipeline_l124_14) begin
      execute_to_memory_MEMORY_ENABLE <= execute_MEMORY_ENABLE;
    end
    if(when_Pipeline_l124_15) begin
      memory_to_writeBack_MEMORY_ENABLE <= memory_MEMORY_ENABLE;
    end
    if(when_Pipeline_l124_16) begin
      decode_to_execute_ALU_CTRL <= _zz_decode_to_execute_ALU_CTRL;
    end
    if(when_Pipeline_l124_17) begin
      decode_to_execute_REGFILE_WRITE_VALID <= decode_REGFILE_WRITE_VALID;
    end
    if(when_Pipeline_l124_18) begin
      execute_to_memory_REGFILE_WRITE_VALID <= execute_REGFILE_WRITE_VALID;
    end
    if(when_Pipeline_l124_19) begin
      memory_to_writeBack_REGFILE_WRITE_VALID <= memory_REGFILE_WRITE_VALID;
    end
    if(when_Pipeline_l124_20) begin
      decode_to_execute_BYPASSABLE_EXECUTE_STAGE <= decode_BYPASSABLE_EXECUTE_STAGE;
    end
    if(when_Pipeline_l124_21) begin
      decode_to_execute_BYPASSABLE_MEMORY_STAGE <= decode_BYPASSABLE_MEMORY_STAGE;
    end
    if(when_Pipeline_l124_22) begin
      execute_to_memory_BYPASSABLE_MEMORY_STAGE <= execute_BYPASSABLE_MEMORY_STAGE;
    end
    if(when_Pipeline_l124_23) begin
      decode_to_execute_MEMORY_WR <= decode_MEMORY_WR;
    end
    if(when_Pipeline_l124_24) begin
      execute_to_memory_MEMORY_WR <= execute_MEMORY_WR;
    end
    if(when_Pipeline_l124_25) begin
      memory_to_writeBack_MEMORY_WR <= memory_MEMORY_WR;
    end
    if(when_Pipeline_l124_26) begin
      decode_to_execute_MEMORY_MANAGMENT <= decode_MEMORY_MANAGMENT;
    end
    if(when_Pipeline_l124_27) begin
      decode_to_execute_IS_CSR <= decode_IS_CSR;
    end
    if(when_Pipeline_l124_28) begin
      decode_to_execute_ENV_CTRL <= _zz_decode_to_execute_ENV_CTRL;
    end
    if(when_Pipeline_l124_29) begin
      execute_to_memory_ENV_CTRL <= _zz_execute_to_memory_ENV_CTRL;
    end
    if(when_Pipeline_l124_30) begin
      memory_to_writeBack_ENV_CTRL <= _zz_memory_to_writeBack_ENV_CTRL;
    end
    if(when_Pipeline_l124_31) begin
      decode_to_execute_SRC_LESS_UNSIGNED <= decode_SRC_LESS_UNSIGNED;
    end
    if(when_Pipeline_l124_32) begin
      decode_to_execute_ALU_BITWISE_CTRL <= _zz_decode_to_execute_ALU_BITWISE_CTRL;
    end
    if(when_Pipeline_l124_33) begin
      decode_to_execute_IS_MUL <= decode_IS_MUL;
    end
    if(when_Pipeline_l124_34) begin
      execute_to_memory_IS_MUL <= execute_IS_MUL;
    end
    if(when_Pipeline_l124_35) begin
      memory_to_writeBack_IS_MUL <= memory_IS_MUL;
    end
    if(when_Pipeline_l124_36) begin
      decode_to_execute_IS_DIV <= decode_IS_DIV;
    end
    if(when_Pipeline_l124_37) begin
      execute_to_memory_IS_DIV <= execute_IS_DIV;
    end
    if(when_Pipeline_l124_38) begin
      decode_to_execute_IS_RS1_SIGNED <= decode_IS_RS1_SIGNED;
    end
    if(when_Pipeline_l124_39) begin
      decode_to_execute_IS_RS2_SIGNED <= decode_IS_RS2_SIGNED;
    end
    if(when_Pipeline_l124_40) begin
      decode_to_execute_FPU_COMMIT <= decode_FPU_COMMIT;
    end
    if(when_Pipeline_l124_41) begin
      execute_to_memory_FPU_COMMIT <= execute_FPU_COMMIT;
    end
    if(when_Pipeline_l124_42) begin
      memory_to_writeBack_FPU_COMMIT <= memory_FPU_COMMIT;
    end
    if(when_Pipeline_l124_43) begin
      decode_to_execute_FPU_RSP <= decode_FPU_RSP;
    end
    if(when_Pipeline_l124_44) begin
      execute_to_memory_FPU_RSP <= execute_FPU_RSP;
    end
    if(when_Pipeline_l124_45) begin
      memory_to_writeBack_FPU_RSP <= memory_FPU_RSP;
    end
    if(when_Pipeline_l124_46) begin
      decode_to_execute_FPU_OPCODE <= _zz_decode_to_execute_FPU_OPCODE;
    end
    if(when_Pipeline_l124_47) begin
      execute_to_memory_FPU_OPCODE <= _zz_execute_to_memory_FPU_OPCODE;
    end
    if(when_Pipeline_l124_48) begin
      memory_to_writeBack_FPU_OPCODE <= _zz_memory_to_writeBack_FPU_OPCODE;
    end
    if(when_Pipeline_l124_49) begin
      decode_to_execute_SHIFT_CTRL <= _zz_decode_to_execute_SHIFT_CTRL;
    end
    if(when_Pipeline_l124_50) begin
      decode_to_execute_BRANCH_CTRL <= _zz_decode_to_execute_BRANCH_CTRL;
    end
    if(when_Pipeline_l124_51) begin
      decode_to_execute_RS1 <= _zz_decode_to_execute_RS1;
    end
    if(when_Pipeline_l124_52) begin
      execute_to_memory_RS1 <= execute_RS1;
    end
    if(when_Pipeline_l124_53) begin
      memory_to_writeBack_RS1 <= memory_RS1;
    end
    if(when_Pipeline_l124_54) begin
      decode_to_execute_RS2 <= _zz_decode_to_execute_RS2;
    end
    if(when_Pipeline_l124_55) begin
      decode_to_execute_SRC2_FORCE_ZERO <= decode_SRC2_FORCE_ZERO;
    end
    if(when_Pipeline_l124_56) begin
      decode_to_execute_SRC1 <= decode_SRC1;
    end
    if(when_Pipeline_l124_57) begin
      decode_to_execute_SRC2 <= decode_SRC2;
    end
    if(when_Pipeline_l124_61) begin
      decode_to_execute_FPU_COMMIT_LOAD <= decode_FPU_COMMIT_LOAD;
    end
    if(when_Pipeline_l124_62) begin
      execute_to_memory_FPU_COMMIT_LOAD <= execute_FPU_COMMIT_LOAD;
    end
    if(when_Pipeline_l124_63) begin
      memory_to_writeBack_FPU_COMMIT_LOAD <= memory_FPU_COMMIT_LOAD;
    end
    if(when_Pipeline_l124_64) begin
      execute_to_memory_MEMORY_STORE_DATA_RF <= execute_MEMORY_STORE_DATA_RF;
    end
    if(when_Pipeline_l124_65) begin
      memory_to_writeBack_MEMORY_STORE_DATA_RF <= memory_MEMORY_STORE_DATA_RF;
    end
    if(when_Pipeline_l124_66) begin
      execute_to_memory_REGFILE_WRITE_DATA <= _zz_execute_to_memory_REGFILE_WRITE_DATA;
    end
    if(when_Pipeline_l124_67) begin
      memory_to_writeBack_REGFILE_WRITE_DATA <= _zz_memory_to_writeBack_REGFILE_WRITE_DATA;
    end
    if(when_Pipeline_l124_68) begin
      execute_to_memory_MUL_LL <= execute_MUL_LL;
    end
    if(when_Pipeline_l124_69) begin
      execute_to_memory_MUL_LH <= execute_MUL_LH;
    end
    if(when_Pipeline_l124_70) begin
      execute_to_memory_MUL_HL <= execute_MUL_HL;
    end
    if(when_Pipeline_l124_71) begin
      execute_to_memory_MUL_HH <= execute_MUL_HH;
    end
    if(when_Pipeline_l124_72) begin
      memory_to_writeBack_MUL_HH <= memory_MUL_HH;
    end
    if(when_Pipeline_l124_73) begin
      execute_to_memory_BRANCH_DO <= execute_BRANCH_DO;
    end
    if(when_Pipeline_l124_74) begin
      execute_to_memory_BRANCH_CALC <= execute_BRANCH_CALC;
    end
    if(when_Pipeline_l124_75) begin
      memory_to_writeBack_MUL_LOW <= memory_MUL_LOW;
    end
    if(when_Fetcher_l411) begin
      _zz_IBusSimplePlugin_injector_decodeInput_payload_rsp_inst <= CsrPlugin_injectionPort_payload;
    end
    if(when_CsrPlugin_l1669) begin
      execute_CsrPlugin_csr_1972 <= (decode_INSTRUCTION[31 : 20] == 12'h7b4);
    end
    if(when_CsrPlugin_l1669_1) begin
      execute_CsrPlugin_csr_1969 <= (decode_INSTRUCTION[31 : 20] == 12'h7b1);
    end
    if(when_CsrPlugin_l1669_2) begin
      execute_CsrPlugin_csr_1968 <= (decode_INSTRUCTION[31 : 20] == 12'h7b0);
    end
    if(when_CsrPlugin_l1669_3) begin
      execute_CsrPlugin_csr_1952 <= (decode_INSTRUCTION[31 : 20] == 12'h7a0);
    end
    if(when_CsrPlugin_l1669_4) begin
      execute_CsrPlugin_csr_1956 <= (decode_INSTRUCTION[31 : 20] == 12'h7a4);
    end
    if(when_CsrPlugin_l1669_5) begin
      execute_CsrPlugin_csr_1953 <= (decode_INSTRUCTION[31 : 20] == 12'h7a1);
    end
    if(when_CsrPlugin_l1669_6) begin
      execute_CsrPlugin_csr_1954 <= (decode_INSTRUCTION[31 : 20] == 12'h7a2);
    end
    if(when_CsrPlugin_l1669_7) begin
      execute_CsrPlugin_csr_768 <= (decode_INSTRUCTION[31 : 20] == 12'h300);
    end
    if(when_CsrPlugin_l1669_8) begin
      execute_CsrPlugin_csr_836 <= (decode_INSTRUCTION[31 : 20] == 12'h344);
    end
    if(when_CsrPlugin_l1669_9) begin
      execute_CsrPlugin_csr_772 <= (decode_INSTRUCTION[31 : 20] == 12'h304);
    end
    if(when_CsrPlugin_l1669_10) begin
      execute_CsrPlugin_csr_773 <= (decode_INSTRUCTION[31 : 20] == 12'h305);
    end
    if(when_CsrPlugin_l1669_11) begin
      execute_CsrPlugin_csr_834 <= (decode_INSTRUCTION[31 : 20] == 12'h342);
    end
    if(when_CsrPlugin_l1669_12) begin
      execute_CsrPlugin_csr_3 <= (decode_INSTRUCTION[31 : 20] == 12'h003);
    end
    if(when_CsrPlugin_l1669_13) begin
      execute_CsrPlugin_csr_2 <= (decode_INSTRUCTION[31 : 20] == 12'h002);
    end
    if(when_CsrPlugin_l1669_14) begin
      execute_CsrPlugin_csr_1 <= (decode_INSTRUCTION[31 : 20] == 12'h001);
    end
    if(when_CsrPlugin_l1669_15) begin
      execute_CsrPlugin_csr_256 <= (decode_INSTRUCTION[31 : 20] == 12'h100);
    end
    if(execute_CsrPlugin_csr_1969) begin
      if(execute_CsrPlugin_writeEnable) begin
        CsrPlugin_dpc <= CsrPlugin_csrMapping_writeDataSignal[31 : 0];
      end
    end
    if(execute_CsrPlugin_csr_1952) begin
      if(execute_CsrPlugin_writeEnable) begin
        CsrPlugin_trigger_tselect_index <= CsrPlugin_csrMapping_writeDataSignal[0 : 0];
      end
    end
    if(execute_CsrPlugin_csr_1954) begin
      if(execute_CsrPlugin_writeEnable) begin
        if(CsrPlugin_trigger_slots_0_selected) begin
          CsrPlugin_trigger_slots_0_tdata2_value <= CsrPlugin_csrMapping_writeDataSignal[31 : 0];
        end
        if(CsrPlugin_trigger_slots_1_selected) begin
          CsrPlugin_trigger_slots_1_tdata2_value <= CsrPlugin_csrMapping_writeDataSignal[31 : 0];
        end
      end
    end
    if(execute_CsrPlugin_csr_836) begin
      if(execute_CsrPlugin_writeEnable) begin
        CsrPlugin_mip_MSIP <= CsrPlugin_csrMapping_writeDataSignal[3];
      end
    end
    if(iBus_cmd_ready) begin
      iBus_cmd_rData_pc <= iBus_cmd_payload_pc;
    end
    dBusWishbone_DAT_MISO_regNext <= dBusWishbone_DAT_MISO;
    dBusWishbone_ERR_regNext <= dBusWishbone_ERR;
  end


endmodule

module DebugTransportModuleJtagTap (
  input  wire          io_jtag_tms,
  input  wire          io_jtag_tdi,
  output wire          io_jtag_tdo,
  input  wire          io_jtag_tck,
  output wire          io_bus_cmd_valid,
  input  wire          io_bus_cmd_ready,
  output wire          io_bus_cmd_payload_write,
  output wire [31:0]   io_bus_cmd_payload_data,
  output wire [6:0]    io_bus_cmd_payload_address,
  input  wire          io_bus_rsp_valid,
  input  wire          io_bus_rsp_payload_error,
  input  wire [31:0]   io_bus_rsp_payload_data,
  input  wire          clk,
  input  wire          reset
);
  localparam DebugCaptureOp_SUCCESS = 2'd0;
  localparam DebugCaptureOp_RESERVED = 2'd1;
  localparam DebugCaptureOp_FAILED = 2'd2;
  localparam DebugCaptureOp_OVERRUN = 2'd3;
  localparam JtagState_RESET = 4'd0;
  localparam JtagState_IDLE = 4'd1;
  localparam JtagState_IR_SELECT = 4'd2;
  localparam JtagState_IR_CAPTURE = 4'd3;
  localparam JtagState_IR_SHIFT = 4'd4;
  localparam JtagState_IR_EXIT1 = 4'd5;
  localparam JtagState_IR_PAUSE = 4'd6;
  localparam JtagState_IR_EXIT2 = 4'd7;
  localparam JtagState_IR_UPDATE = 4'd8;
  localparam JtagState_DR_SELECT = 4'd9;
  localparam JtagState_DR_CAPTURE = 4'd10;
  localparam JtagState_DR_SHIFT = 4'd11;
  localparam JtagState_DR_EXIT1 = 4'd12;
  localparam JtagState_DR_PAUSE = 4'd13;
  localparam JtagState_DR_EXIT2 = 4'd14;
  localparam JtagState_DR_UPDATE = 4'd15;
  localparam DebugUpdateOp_NOP = 2'd0;
  localparam DebugUpdateOp_READ = 2'd1;
  localparam DebugUpdateOp_WRITE = 2'd2;
  localparam DebugUpdateOp_RESERVED = 2'd3;

  wire                logic_jtagLogic_dmiCmd_ccToggle_io_output_valid;
  wire                logic_jtagLogic_dmiCmd_ccToggle_io_output_payload_write;
  wire       [31:0]   logic_jtagLogic_dmiCmd_ccToggle_io_output_payload_data;
  wire       [6:0]    logic_jtagLogic_dmiCmd_ccToggle_io_output_payload_address;
  wire                logic_systemLogic_bus_rsp_ccToggle_io_output_valid;
  wire                logic_systemLogic_bus_rsp_ccToggle_io_output_payload_error;
  wire       [31:0]   logic_systemLogic_bus_rsp_ccToggle_io_output_payload_data;
  wire       [4:0]    _zz_tap_isBypass;
  wire       [1:0]    _zz_tap_instructionShift;
  reg        [1:0]    logic_jtagLogic_dmiStat_value_aheadValue;
  reg        [3:0]    tap_fsm_stateNext;
  reg        [3:0]    tap_fsm_state;
  wire       [3:0]    _zz_tap_fsm_stateNext;
  wire       [3:0]    _zz_tap_fsm_stateNext_1;
  wire       [3:0]    _zz_tap_fsm_stateNext_2;
  wire       [3:0]    _zz_tap_fsm_stateNext_3;
  wire       [3:0]    _zz_tap_fsm_stateNext_4;
  wire       [3:0]    _zz_tap_fsm_stateNext_5;
  wire       [3:0]    _zz_tap_fsm_stateNext_6;
  wire       [3:0]    _zz_tap_fsm_stateNext_7;
  wire       [3:0]    _zz_tap_fsm_stateNext_8;
  wire       [3:0]    _zz_tap_fsm_stateNext_9;
  wire       [3:0]    _zz_tap_fsm_stateNext_10;
  wire       [3:0]    _zz_tap_fsm_stateNext_11;
  wire       [3:0]    _zz_tap_fsm_stateNext_12;
  wire       [3:0]    _zz_tap_fsm_stateNext_13;
  wire       [3:0]    _zz_tap_fsm_stateNext_14;
  wire       [3:0]    _zz_tap_fsm_stateNext_15;
  reg        [4:0]    tap_instruction;
  reg        [4:0]    tap_instructionShift;
  reg                 tap_bypass;
  reg                 tap_tdoUnbufferd;
  reg                 tap_tdoDr;
  wire                tap_tdoIr;
  wire                tap_isBypass;
  reg                 tap_tdoUnbufferd_regNext;
  wire                idcodeArea_ctrl_tdi;
  wire                idcodeArea_ctrl_enable;
  wire                idcodeArea_ctrl_capture;
  wire                idcodeArea_ctrl_shift;
  wire                idcodeArea_ctrl_update;
  wire                idcodeArea_ctrl_reset;
  wire                idcodeArea_ctrl_tdo;
  reg        [31:0]   idcodeArea_shifter;
  wire                when_JtagTap_l121;
  wire                logic_jtagLogic_dmiCmd_valid;
  wire                logic_jtagLogic_dmiCmd_payload_write;
  wire       [31:0]   logic_jtagLogic_dmiCmd_payload_data;
  wire       [6:0]    logic_jtagLogic_dmiCmd_payload_address;
  wire                logic_jtagLogic_dmiRsp_valid;
  wire                logic_jtagLogic_dmiRsp_payload_error;
  wire       [31:0]   logic_jtagLogic_dmiRsp_payload_data;
  wire       [31:0]   logic_jtagLogic_dtmcs_captureData;
  wire       [31:0]   logic_jtagLogic_dtmcs_updateData;
  wire                logic_jtagLogic_dtmcs_captureValid;
  wire                logic_jtagLogic_dtmcs_updateValid;
  wire                logic_jtagLogic_dtmcs_logic_ctrl_tdi;
  wire                logic_jtagLogic_dtmcs_logic_ctrl_enable;
  wire                logic_jtagLogic_dtmcs_logic_ctrl_capture;
  wire                logic_jtagLogic_dtmcs_logic_ctrl_shift;
  wire                logic_jtagLogic_dtmcs_logic_ctrl_update;
  wire                logic_jtagLogic_dtmcs_logic_ctrl_reset;
  wire                logic_jtagLogic_dtmcs_logic_ctrl_tdo;
  reg        [31:0]   logic_jtagLogic_dtmcs_logic_store;
  wire       [1:0]    logic_jtagLogic_dmi_captureData_op;
  wire       [31:0]   logic_jtagLogic_dmi_captureData_data;
  wire       [6:0]    logic_jtagLogic_dmi_captureData_padding;
  wire       [1:0]    logic_jtagLogic_dmi_updateData_op;
  wire       [31:0]   logic_jtagLogic_dmi_updateData_data;
  wire       [6:0]    logic_jtagLogic_dmi_updateData_address;
  wire                logic_jtagLogic_dmi_captureValid;
  wire                logic_jtagLogic_dmi_updateValid;
  wire                logic_jtagLogic_dmi_logic_ctrl_tdi;
  wire                logic_jtagLogic_dmi_logic_ctrl_enable;
  wire                logic_jtagLogic_dmi_logic_ctrl_capture;
  wire                logic_jtagLogic_dmi_logic_ctrl_shift;
  wire                logic_jtagLogic_dmi_logic_ctrl_update;
  wire                logic_jtagLogic_dmi_logic_ctrl_reset;
  wire                logic_jtagLogic_dmi_logic_ctrl_tdo;
  reg        [40:0]   logic_jtagLogic_dmi_logic_store;
  wire       [1:0]    _zz_logic_jtagLogic_dmi_updateData_op;
  reg        [1:0]    logic_jtagLogic_dmiStat_value;
  reg                 logic_jtagLogic_dmiStat_failure;
  reg                 logic_jtagLogic_dmiStat_busy;
  reg                 logic_jtagLogic_dmiStat_clear;
  wire                when_DebugTransportModuleJtag_l30;
  reg                 logic_jtagLogic_pending;
  wire                logic_jtagLogic_trigger_dmiHardReset;
  wire                logic_jtagLogic_trigger_dmiReset;
  reg                 logic_jtagLogic_trigger_dmiCmd;
  reg        [31:0]   logic_jtagLogic_rspLogic_buffer;
  wire                when_DebugTransportModuleJtag_l78;
  wire                logic_systemLogic_bus_cmd_valid;
  wire                logic_systemLogic_bus_cmd_ready;
  wire                logic_systemLogic_bus_cmd_payload_write;
  wire       [31:0]   logic_systemLogic_bus_cmd_payload_data;
  wire       [6:0]    logic_systemLogic_bus_cmd_payload_address;
  wire                logic_systemLogic_bus_rsp_valid;
  wire                logic_systemLogic_bus_rsp_payload_error;
  wire       [31:0]   logic_systemLogic_bus_rsp_payload_data;
  wire                debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_valid;
  reg                 debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_ready;
  wire                debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_payload_write;
  wire       [31:0]   debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_payload_data;
  wire       [6:0]    debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_payload_address;
  wire                logic_systemLogic_cmd_valid;
  wire                logic_systemLogic_cmd_ready;
  wire                logic_systemLogic_cmd_payload_write;
  wire       [31:0]   logic_systemLogic_cmd_payload_data;
  wire       [6:0]    logic_systemLogic_cmd_payload_address;
  reg                 debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rValid;
  wire                debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_fire;
  (* async_reg = "true" *) reg                 debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rData_write;
  (* async_reg = "true" *) reg        [31:0]   debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rData_data;
  (* async_reg = "true" *) reg        [6:0]    debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rData_address;
  wire                when_Stream_l375;
  `ifndef SYNTHESIS
  reg [63:0] logic_jtagLogic_dmiStat_value_aheadValue_string;
  reg [79:0] tap_fsm_stateNext_string;
  reg [79:0] tap_fsm_state_string;
  reg [79:0] _zz_tap_fsm_stateNext_string;
  reg [79:0] _zz_tap_fsm_stateNext_1_string;
  reg [79:0] _zz_tap_fsm_stateNext_2_string;
  reg [79:0] _zz_tap_fsm_stateNext_3_string;
  reg [79:0] _zz_tap_fsm_stateNext_4_string;
  reg [79:0] _zz_tap_fsm_stateNext_5_string;
  reg [79:0] _zz_tap_fsm_stateNext_6_string;
  reg [79:0] _zz_tap_fsm_stateNext_7_string;
  reg [79:0] _zz_tap_fsm_stateNext_8_string;
  reg [79:0] _zz_tap_fsm_stateNext_9_string;
  reg [79:0] _zz_tap_fsm_stateNext_10_string;
  reg [79:0] _zz_tap_fsm_stateNext_11_string;
  reg [79:0] _zz_tap_fsm_stateNext_12_string;
  reg [79:0] _zz_tap_fsm_stateNext_13_string;
  reg [79:0] _zz_tap_fsm_stateNext_14_string;
  reg [79:0] _zz_tap_fsm_stateNext_15_string;
  reg [63:0] logic_jtagLogic_dmi_captureData_op_string;
  reg [63:0] logic_jtagLogic_dmi_updateData_op_string;
  reg [63:0] _zz_logic_jtagLogic_dmi_updateData_op_string;
  reg [63:0] logic_jtagLogic_dmiStat_value_string;
  `endif


  assign _zz_tap_isBypass = tap_instruction;
  assign _zz_tap_instructionShift = 2'b01;
  FlowCCByToggle logic_jtagLogic_dmiCmd_ccToggle (
    .io_input_valid            (logic_jtagLogic_dmiCmd_valid                                  ), //i
    .io_input_payload_write    (logic_jtagLogic_dmiCmd_payload_write                          ), //i
    .io_input_payload_data     (logic_jtagLogic_dmiCmd_payload_data[31:0]                     ), //i
    .io_input_payload_address  (logic_jtagLogic_dmiCmd_payload_address[6:0]                   ), //i
    .io_output_valid           (logic_jtagLogic_dmiCmd_ccToggle_io_output_valid               ), //o
    .io_output_payload_write   (logic_jtagLogic_dmiCmd_ccToggle_io_output_payload_write       ), //o
    .io_output_payload_data    (logic_jtagLogic_dmiCmd_ccToggle_io_output_payload_data[31:0]  ), //o
    .io_output_payload_address (logic_jtagLogic_dmiCmd_ccToggle_io_output_payload_address[6:0]), //o
    .io_jtag_tck               (io_jtag_tck                                                   ), //i
    .clk                       (clk                                                           ), //i
    .reset                     (reset                                                         )  //i
  );
  FlowCCByToggle_1 logic_systemLogic_bus_rsp_ccToggle (
    .io_input_valid          (logic_systemLogic_bus_rsp_valid                                ), //i
    .io_input_payload_error  (logic_systemLogic_bus_rsp_payload_error                        ), //i
    .io_input_payload_data   (logic_systemLogic_bus_rsp_payload_data[31:0]                   ), //i
    .io_output_valid         (logic_systemLogic_bus_rsp_ccToggle_io_output_valid             ), //o
    .io_output_payload_error (logic_systemLogic_bus_rsp_ccToggle_io_output_payload_error     ), //o
    .io_output_payload_data  (logic_systemLogic_bus_rsp_ccToggle_io_output_payload_data[31:0]), //o
    .clk                     (clk                                                            ), //i
    .reset                   (reset                                                          ), //i
    .io_jtag_tck             (io_jtag_tck                                                    )  //i
  );
  initial begin
  `ifndef SYNTHESIS
    tap_fsm_state = {$urandom};
  `endif
  end

  `ifndef SYNTHESIS
  always @(*) begin
    case(logic_jtagLogic_dmiStat_value_aheadValue)
      DebugCaptureOp_SUCCESS : logic_jtagLogic_dmiStat_value_aheadValue_string = "SUCCESS ";
      DebugCaptureOp_RESERVED : logic_jtagLogic_dmiStat_value_aheadValue_string = "RESERVED";
      DebugCaptureOp_FAILED : logic_jtagLogic_dmiStat_value_aheadValue_string = "FAILED  ";
      DebugCaptureOp_OVERRUN : logic_jtagLogic_dmiStat_value_aheadValue_string = "OVERRUN ";
      default : logic_jtagLogic_dmiStat_value_aheadValue_string = "????????";
    endcase
  end
  always @(*) begin
    case(tap_fsm_stateNext)
      JtagState_RESET : tap_fsm_stateNext_string = "RESET     ";
      JtagState_IDLE : tap_fsm_stateNext_string = "IDLE      ";
      JtagState_IR_SELECT : tap_fsm_stateNext_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : tap_fsm_stateNext_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : tap_fsm_stateNext_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : tap_fsm_stateNext_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : tap_fsm_stateNext_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : tap_fsm_stateNext_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : tap_fsm_stateNext_string = "IR_UPDATE ";
      JtagState_DR_SELECT : tap_fsm_stateNext_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : tap_fsm_stateNext_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : tap_fsm_stateNext_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : tap_fsm_stateNext_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : tap_fsm_stateNext_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : tap_fsm_stateNext_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : tap_fsm_stateNext_string = "DR_UPDATE ";
      default : tap_fsm_stateNext_string = "??????????";
    endcase
  end
  always @(*) begin
    case(tap_fsm_state)
      JtagState_RESET : tap_fsm_state_string = "RESET     ";
      JtagState_IDLE : tap_fsm_state_string = "IDLE      ";
      JtagState_IR_SELECT : tap_fsm_state_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : tap_fsm_state_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : tap_fsm_state_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : tap_fsm_state_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : tap_fsm_state_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : tap_fsm_state_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : tap_fsm_state_string = "IR_UPDATE ";
      JtagState_DR_SELECT : tap_fsm_state_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : tap_fsm_state_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : tap_fsm_state_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : tap_fsm_state_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : tap_fsm_state_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : tap_fsm_state_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : tap_fsm_state_string = "DR_UPDATE ";
      default : tap_fsm_state_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext)
      JtagState_RESET : _zz_tap_fsm_stateNext_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_1)
      JtagState_RESET : _zz_tap_fsm_stateNext_1_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_1_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_1_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_1_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_1_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_1_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_1_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_1_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_1_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_1_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_1_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_1_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_1_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_1_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_1_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_1_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_1_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_2)
      JtagState_RESET : _zz_tap_fsm_stateNext_2_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_2_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_2_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_2_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_2_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_2_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_2_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_2_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_2_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_2_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_2_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_2_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_2_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_2_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_2_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_2_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_2_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_3)
      JtagState_RESET : _zz_tap_fsm_stateNext_3_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_3_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_3_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_3_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_3_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_3_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_3_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_3_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_3_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_3_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_3_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_3_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_3_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_3_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_3_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_3_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_3_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_4)
      JtagState_RESET : _zz_tap_fsm_stateNext_4_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_4_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_4_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_4_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_4_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_4_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_4_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_4_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_4_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_4_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_4_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_4_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_4_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_4_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_4_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_4_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_4_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_5)
      JtagState_RESET : _zz_tap_fsm_stateNext_5_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_5_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_5_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_5_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_5_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_5_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_5_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_5_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_5_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_5_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_5_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_5_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_5_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_5_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_5_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_5_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_5_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_6)
      JtagState_RESET : _zz_tap_fsm_stateNext_6_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_6_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_6_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_6_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_6_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_6_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_6_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_6_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_6_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_6_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_6_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_6_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_6_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_6_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_6_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_6_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_6_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_7)
      JtagState_RESET : _zz_tap_fsm_stateNext_7_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_7_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_7_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_7_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_7_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_7_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_7_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_7_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_7_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_7_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_7_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_7_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_7_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_7_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_7_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_7_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_7_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_8)
      JtagState_RESET : _zz_tap_fsm_stateNext_8_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_8_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_8_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_8_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_8_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_8_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_8_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_8_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_8_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_8_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_8_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_8_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_8_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_8_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_8_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_8_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_8_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_9)
      JtagState_RESET : _zz_tap_fsm_stateNext_9_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_9_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_9_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_9_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_9_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_9_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_9_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_9_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_9_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_9_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_9_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_9_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_9_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_9_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_9_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_9_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_9_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_10)
      JtagState_RESET : _zz_tap_fsm_stateNext_10_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_10_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_10_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_10_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_10_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_10_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_10_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_10_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_10_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_10_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_10_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_10_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_10_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_10_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_10_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_10_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_10_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_11)
      JtagState_RESET : _zz_tap_fsm_stateNext_11_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_11_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_11_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_11_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_11_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_11_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_11_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_11_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_11_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_11_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_11_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_11_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_11_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_11_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_11_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_11_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_11_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_12)
      JtagState_RESET : _zz_tap_fsm_stateNext_12_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_12_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_12_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_12_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_12_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_12_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_12_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_12_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_12_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_12_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_12_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_12_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_12_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_12_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_12_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_12_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_12_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_13)
      JtagState_RESET : _zz_tap_fsm_stateNext_13_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_13_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_13_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_13_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_13_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_13_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_13_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_13_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_13_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_13_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_13_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_13_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_13_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_13_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_13_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_13_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_13_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_14)
      JtagState_RESET : _zz_tap_fsm_stateNext_14_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_14_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_14_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_14_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_14_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_14_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_14_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_14_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_14_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_14_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_14_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_14_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_14_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_14_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_14_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_14_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_14_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_tap_fsm_stateNext_15)
      JtagState_RESET : _zz_tap_fsm_stateNext_15_string = "RESET     ";
      JtagState_IDLE : _zz_tap_fsm_stateNext_15_string = "IDLE      ";
      JtagState_IR_SELECT : _zz_tap_fsm_stateNext_15_string = "IR_SELECT ";
      JtagState_IR_CAPTURE : _zz_tap_fsm_stateNext_15_string = "IR_CAPTURE";
      JtagState_IR_SHIFT : _zz_tap_fsm_stateNext_15_string = "IR_SHIFT  ";
      JtagState_IR_EXIT1 : _zz_tap_fsm_stateNext_15_string = "IR_EXIT1  ";
      JtagState_IR_PAUSE : _zz_tap_fsm_stateNext_15_string = "IR_PAUSE  ";
      JtagState_IR_EXIT2 : _zz_tap_fsm_stateNext_15_string = "IR_EXIT2  ";
      JtagState_IR_UPDATE : _zz_tap_fsm_stateNext_15_string = "IR_UPDATE ";
      JtagState_DR_SELECT : _zz_tap_fsm_stateNext_15_string = "DR_SELECT ";
      JtagState_DR_CAPTURE : _zz_tap_fsm_stateNext_15_string = "DR_CAPTURE";
      JtagState_DR_SHIFT : _zz_tap_fsm_stateNext_15_string = "DR_SHIFT  ";
      JtagState_DR_EXIT1 : _zz_tap_fsm_stateNext_15_string = "DR_EXIT1  ";
      JtagState_DR_PAUSE : _zz_tap_fsm_stateNext_15_string = "DR_PAUSE  ";
      JtagState_DR_EXIT2 : _zz_tap_fsm_stateNext_15_string = "DR_EXIT2  ";
      JtagState_DR_UPDATE : _zz_tap_fsm_stateNext_15_string = "DR_UPDATE ";
      default : _zz_tap_fsm_stateNext_15_string = "??????????";
    endcase
  end
  always @(*) begin
    case(logic_jtagLogic_dmi_captureData_op)
      DebugCaptureOp_SUCCESS : logic_jtagLogic_dmi_captureData_op_string = "SUCCESS ";
      DebugCaptureOp_RESERVED : logic_jtagLogic_dmi_captureData_op_string = "RESERVED";
      DebugCaptureOp_FAILED : logic_jtagLogic_dmi_captureData_op_string = "FAILED  ";
      DebugCaptureOp_OVERRUN : logic_jtagLogic_dmi_captureData_op_string = "OVERRUN ";
      default : logic_jtagLogic_dmi_captureData_op_string = "????????";
    endcase
  end
  always @(*) begin
    case(logic_jtagLogic_dmi_updateData_op)
      DebugUpdateOp_NOP : logic_jtagLogic_dmi_updateData_op_string = "NOP     ";
      DebugUpdateOp_READ : logic_jtagLogic_dmi_updateData_op_string = "READ    ";
      DebugUpdateOp_WRITE : logic_jtagLogic_dmi_updateData_op_string = "WRITE   ";
      DebugUpdateOp_RESERVED : logic_jtagLogic_dmi_updateData_op_string = "RESERVED";
      default : logic_jtagLogic_dmi_updateData_op_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_logic_jtagLogic_dmi_updateData_op)
      DebugUpdateOp_NOP : _zz_logic_jtagLogic_dmi_updateData_op_string = "NOP     ";
      DebugUpdateOp_READ : _zz_logic_jtagLogic_dmi_updateData_op_string = "READ    ";
      DebugUpdateOp_WRITE : _zz_logic_jtagLogic_dmi_updateData_op_string = "WRITE   ";
      DebugUpdateOp_RESERVED : _zz_logic_jtagLogic_dmi_updateData_op_string = "RESERVED";
      default : _zz_logic_jtagLogic_dmi_updateData_op_string = "????????";
    endcase
  end
  always @(*) begin
    case(logic_jtagLogic_dmiStat_value)
      DebugCaptureOp_SUCCESS : logic_jtagLogic_dmiStat_value_string = "SUCCESS ";
      DebugCaptureOp_RESERVED : logic_jtagLogic_dmiStat_value_string = "RESERVED";
      DebugCaptureOp_FAILED : logic_jtagLogic_dmiStat_value_string = "FAILED  ";
      DebugCaptureOp_OVERRUN : logic_jtagLogic_dmiStat_value_string = "OVERRUN ";
      default : logic_jtagLogic_dmiStat_value_string = "????????";
    endcase
  end
  `endif

  always @(*) begin
    logic_jtagLogic_dmiStat_value_aheadValue = logic_jtagLogic_dmiStat_value;
    if(when_DebugTransportModuleJtag_l30) begin
      if(logic_jtagLogic_dmiStat_failure) begin
        logic_jtagLogic_dmiStat_value_aheadValue = DebugCaptureOp_FAILED;
      end
      if(logic_jtagLogic_dmiStat_busy) begin
        logic_jtagLogic_dmiStat_value_aheadValue = DebugCaptureOp_OVERRUN;
      end
    end
    if(logic_jtagLogic_dmiStat_clear) begin
      logic_jtagLogic_dmiStat_value_aheadValue = DebugCaptureOp_SUCCESS;
    end
  end

  assign _zz_tap_fsm_stateNext = (io_jtag_tms ? JtagState_RESET : JtagState_IDLE);
  always @(*) begin
    case(tap_fsm_state)
      JtagState_RESET : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext;
      end
      JtagState_IDLE : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_1;
      end
      JtagState_IR_SELECT : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_2;
      end
      JtagState_IR_CAPTURE : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_3;
      end
      JtagState_IR_SHIFT : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_4;
      end
      JtagState_IR_EXIT1 : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_5;
      end
      JtagState_IR_PAUSE : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_6;
      end
      JtagState_IR_EXIT2 : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_7;
      end
      JtagState_IR_UPDATE : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_8;
      end
      JtagState_DR_SELECT : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_9;
      end
      JtagState_DR_CAPTURE : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_10;
      end
      JtagState_DR_SHIFT : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_11;
      end
      JtagState_DR_EXIT1 : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_12;
      end
      JtagState_DR_PAUSE : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_13;
      end
      JtagState_DR_EXIT2 : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_14;
      end
      JtagState_DR_UPDATE : begin
        tap_fsm_stateNext = _zz_tap_fsm_stateNext_15;
      end
      default : begin
      end
    endcase
  end

  assign _zz_tap_fsm_stateNext_1 = (io_jtag_tms ? JtagState_DR_SELECT : JtagState_IDLE);
  assign _zz_tap_fsm_stateNext_2 = (io_jtag_tms ? JtagState_RESET : JtagState_IR_CAPTURE);
  assign _zz_tap_fsm_stateNext_3 = (io_jtag_tms ? JtagState_IR_EXIT1 : JtagState_IR_SHIFT);
  assign _zz_tap_fsm_stateNext_4 = (io_jtag_tms ? JtagState_IR_EXIT1 : JtagState_IR_SHIFT);
  assign _zz_tap_fsm_stateNext_5 = (io_jtag_tms ? JtagState_IR_UPDATE : JtagState_IR_PAUSE);
  assign _zz_tap_fsm_stateNext_6 = (io_jtag_tms ? JtagState_IR_EXIT2 : JtagState_IR_PAUSE);
  assign _zz_tap_fsm_stateNext_7 = (io_jtag_tms ? JtagState_IR_UPDATE : JtagState_IR_SHIFT);
  assign _zz_tap_fsm_stateNext_8 = (io_jtag_tms ? JtagState_DR_SELECT : JtagState_IDLE);
  assign _zz_tap_fsm_stateNext_9 = (io_jtag_tms ? JtagState_IR_SELECT : JtagState_DR_CAPTURE);
  assign _zz_tap_fsm_stateNext_10 = (io_jtag_tms ? JtagState_DR_EXIT1 : JtagState_DR_SHIFT);
  assign _zz_tap_fsm_stateNext_11 = (io_jtag_tms ? JtagState_DR_EXIT1 : JtagState_DR_SHIFT);
  assign _zz_tap_fsm_stateNext_12 = (io_jtag_tms ? JtagState_DR_UPDATE : JtagState_DR_PAUSE);
  assign _zz_tap_fsm_stateNext_13 = (io_jtag_tms ? JtagState_DR_EXIT2 : JtagState_DR_PAUSE);
  assign _zz_tap_fsm_stateNext_14 = (io_jtag_tms ? JtagState_DR_UPDATE : JtagState_DR_SHIFT);
  assign _zz_tap_fsm_stateNext_15 = (io_jtag_tms ? JtagState_DR_SELECT : JtagState_IDLE);
  always @(*) begin
    tap_tdoUnbufferd = tap_bypass;
    case(tap_fsm_state)
      JtagState_IR_SHIFT : begin
        tap_tdoUnbufferd = tap_tdoIr;
      end
      JtagState_DR_SHIFT : begin
        if(tap_isBypass) begin
          tap_tdoUnbufferd = tap_bypass;
        end else begin
          tap_tdoUnbufferd = tap_tdoDr;
        end
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    tap_tdoDr = 1'b0;
    if(idcodeArea_ctrl_enable) begin
      tap_tdoDr = idcodeArea_ctrl_tdo;
    end
    if(logic_jtagLogic_dtmcs_logic_ctrl_enable) begin
      tap_tdoDr = logic_jtagLogic_dtmcs_logic_ctrl_tdo;
    end
    if(logic_jtagLogic_dmi_logic_ctrl_enable) begin
      tap_tdoDr = logic_jtagLogic_dmi_logic_ctrl_tdo;
    end
  end

  assign tap_tdoIr = tap_instructionShift[0];
  assign tap_isBypass = ($signed(_zz_tap_isBypass) == $signed(5'h1f));
  assign io_jtag_tdo = tap_tdoUnbufferd_regNext;
  assign idcodeArea_ctrl_tdo = idcodeArea_shifter[0];
  assign idcodeArea_ctrl_tdi = io_jtag_tdi;
  assign idcodeArea_ctrl_enable = (tap_instruction == 5'h01);
  assign idcodeArea_ctrl_capture = (tap_fsm_state == JtagState_DR_CAPTURE);
  assign idcodeArea_ctrl_shift = (tap_fsm_state == JtagState_DR_SHIFT);
  assign idcodeArea_ctrl_update = (tap_fsm_state == JtagState_DR_UPDATE);
  assign idcodeArea_ctrl_reset = (tap_fsm_state == JtagState_RESET);
  assign when_JtagTap_l121 = (tap_fsm_state == JtagState_RESET);
  assign logic_jtagLogic_dtmcs_captureValid = ((tap_instruction == 5'h10) && (tap_fsm_state == JtagState_DR_CAPTURE));
  assign logic_jtagLogic_dtmcs_updateValid = ((tap_instruction == 5'h10) && (tap_fsm_state == JtagState_DR_UPDATE));
  assign logic_jtagLogic_dtmcs_logic_ctrl_tdo = logic_jtagLogic_dtmcs_logic_store[0];
  assign logic_jtagLogic_dtmcs_updateData = logic_jtagLogic_dtmcs_logic_store;
  assign logic_jtagLogic_dtmcs_logic_ctrl_tdi = io_jtag_tdi;
  assign logic_jtagLogic_dtmcs_logic_ctrl_enable = (tap_instruction == 5'h10);
  assign logic_jtagLogic_dtmcs_logic_ctrl_capture = (tap_fsm_state == JtagState_DR_CAPTURE);
  assign logic_jtagLogic_dtmcs_logic_ctrl_shift = (tap_fsm_state == JtagState_DR_SHIFT);
  assign logic_jtagLogic_dtmcs_logic_ctrl_update = (tap_fsm_state == JtagState_DR_UPDATE);
  assign logic_jtagLogic_dtmcs_logic_ctrl_reset = (tap_fsm_state == JtagState_RESET);
  assign logic_jtagLogic_dmi_captureValid = ((tap_instruction == 5'h11) && (tap_fsm_state == JtagState_DR_CAPTURE));
  assign logic_jtagLogic_dmi_updateValid = ((tap_instruction == 5'h11) && (tap_fsm_state == JtagState_DR_UPDATE));
  assign logic_jtagLogic_dmi_logic_ctrl_tdo = logic_jtagLogic_dmi_logic_store[0];
  assign _zz_logic_jtagLogic_dmi_updateData_op = logic_jtagLogic_dmi_logic_store[1 : 0];
  assign logic_jtagLogic_dmi_updateData_op = _zz_logic_jtagLogic_dmi_updateData_op;
  assign logic_jtagLogic_dmi_updateData_data = logic_jtagLogic_dmi_logic_store[33 : 2];
  assign logic_jtagLogic_dmi_updateData_address = logic_jtagLogic_dmi_logic_store[40 : 34];
  assign logic_jtagLogic_dmi_logic_ctrl_tdi = io_jtag_tdi;
  assign logic_jtagLogic_dmi_logic_ctrl_enable = (tap_instruction == 5'h11);
  assign logic_jtagLogic_dmi_logic_ctrl_capture = (tap_fsm_state == JtagState_DR_CAPTURE);
  assign logic_jtagLogic_dmi_logic_ctrl_shift = (tap_fsm_state == JtagState_DR_SHIFT);
  assign logic_jtagLogic_dmi_logic_ctrl_update = (tap_fsm_state == JtagState_DR_UPDATE);
  assign logic_jtagLogic_dmi_logic_ctrl_reset = (tap_fsm_state == JtagState_RESET);
  always @(*) begin
    logic_jtagLogic_dmiStat_failure = 1'b0;
    if(logic_jtagLogic_dmi_updateValid) begin
      case(logic_jtagLogic_dmi_updateData_op)
        DebugUpdateOp_NOP : begin
        end
        DebugUpdateOp_READ : begin
        end
        DebugUpdateOp_WRITE : begin
        end
        default : begin
          logic_jtagLogic_dmiStat_failure = 1'b1;
        end
      endcase
    end
    if(logic_jtagLogic_dmiRsp_valid) begin
      if(logic_jtagLogic_dmiRsp_payload_error) begin
        logic_jtagLogic_dmiStat_failure = 1'b1;
      end
    end
  end

  always @(*) begin
    logic_jtagLogic_dmiStat_busy = 1'b0;
    if(when_DebugTransportModuleJtag_l78) begin
      logic_jtagLogic_dmiStat_busy = 1'b1;
    end
  end

  always @(*) begin
    logic_jtagLogic_dmiStat_clear = 1'b0;
    if(logic_jtagLogic_trigger_dmiReset) begin
      logic_jtagLogic_dmiStat_clear = 1'b1;
    end
    if(logic_jtagLogic_trigger_dmiHardReset) begin
      logic_jtagLogic_dmiStat_clear = 1'b1;
    end
  end

  assign when_DebugTransportModuleJtag_l30 = (logic_jtagLogic_dmiStat_value == DebugCaptureOp_SUCCESS);
  assign logic_jtagLogic_trigger_dmiHardReset = ((logic_jtagLogic_dtmcs_updateData[17] && logic_jtagLogic_dtmcs_updateValid) || (tap_fsm_state == JtagState_RESET));
  assign logic_jtagLogic_trigger_dmiReset = ((logic_jtagLogic_dtmcs_updateData[16] && logic_jtagLogic_dtmcs_updateValid) || (tap_fsm_state == JtagState_RESET));
  always @(*) begin
    logic_jtagLogic_trigger_dmiCmd = 1'b0;
    if(logic_jtagLogic_dmi_updateValid) begin
      case(logic_jtagLogic_dmi_updateData_op)
        DebugUpdateOp_NOP : begin
        end
        DebugUpdateOp_READ : begin
          logic_jtagLogic_trigger_dmiCmd = 1'b1;
        end
        DebugUpdateOp_WRITE : begin
          logic_jtagLogic_trigger_dmiCmd = 1'b1;
        end
        default : begin
        end
      endcase
    end
  end

  assign logic_jtagLogic_dtmcs_captureData = {{{{17'h0,3'b111},logic_jtagLogic_dmiStat_value},6'h07},4'b0001};
  assign logic_jtagLogic_dmiCmd_valid = logic_jtagLogic_trigger_dmiCmd;
  assign logic_jtagLogic_dmiCmd_payload_write = (logic_jtagLogic_dmi_updateData_op == DebugUpdateOp_WRITE);
  assign logic_jtagLogic_dmiCmd_payload_address = logic_jtagLogic_dmi_updateData_address;
  assign logic_jtagLogic_dmiCmd_payload_data = logic_jtagLogic_dmi_updateData_data;
  assign logic_jtagLogic_dmi_captureData_op = logic_jtagLogic_dmiStat_value_aheadValue;
  assign logic_jtagLogic_dmi_captureData_data = logic_jtagLogic_rspLogic_buffer;
  assign logic_jtagLogic_dmi_captureData_padding = 7'h0;
  assign when_DebugTransportModuleJtag_l78 = (logic_jtagLogic_dmi_captureValid && logic_jtagLogic_pending);
  assign debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_valid = logic_jtagLogic_dmiCmd_ccToggle_io_output_valid;
  assign debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_payload_write = logic_jtagLogic_dmiCmd_ccToggle_io_output_payload_write;
  assign debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_payload_data = logic_jtagLogic_dmiCmd_ccToggle_io_output_payload_data;
  assign debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_payload_address = logic_jtagLogic_dmiCmd_ccToggle_io_output_payload_address;
  assign debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_fire = (debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_valid && debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_ready);
  always @(*) begin
    debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_ready = logic_systemLogic_cmd_ready;
    if(when_Stream_l375) begin
      debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_ready = 1'b1;
    end
  end

  assign when_Stream_l375 = (! logic_systemLogic_cmd_valid);
  assign logic_systemLogic_cmd_valid = debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rValid;
  assign logic_systemLogic_cmd_payload_write = debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rData_write;
  assign logic_systemLogic_cmd_payload_data = debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rData_data;
  assign logic_systemLogic_cmd_payload_address = debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rData_address;
  assign logic_systemLogic_bus_cmd_valid = logic_systemLogic_cmd_valid;
  assign logic_systemLogic_cmd_ready = logic_systemLogic_bus_cmd_ready;
  assign logic_systemLogic_bus_cmd_payload_write = logic_systemLogic_cmd_payload_write;
  assign logic_systemLogic_bus_cmd_payload_data = logic_systemLogic_cmd_payload_data;
  assign logic_systemLogic_bus_cmd_payload_address = logic_systemLogic_cmd_payload_address;
  assign logic_jtagLogic_dmiRsp_valid = logic_systemLogic_bus_rsp_ccToggle_io_output_valid;
  assign logic_jtagLogic_dmiRsp_payload_error = logic_systemLogic_bus_rsp_ccToggle_io_output_payload_error;
  assign logic_jtagLogic_dmiRsp_payload_data = logic_systemLogic_bus_rsp_ccToggle_io_output_payload_data;
  assign io_bus_cmd_valid = logic_systemLogic_bus_cmd_valid;
  assign logic_systemLogic_bus_cmd_ready = io_bus_cmd_ready;
  assign io_bus_cmd_payload_write = logic_systemLogic_bus_cmd_payload_write;
  assign io_bus_cmd_payload_data = logic_systemLogic_bus_cmd_payload_data;
  assign io_bus_cmd_payload_address = logic_systemLogic_bus_cmd_payload_address;
  assign logic_systemLogic_bus_rsp_valid = io_bus_rsp_valid;
  assign logic_systemLogic_bus_rsp_payload_error = io_bus_rsp_payload_error;
  assign logic_systemLogic_bus_rsp_payload_data = io_bus_rsp_payload_data;
  always @(posedge io_jtag_tck) begin
    tap_fsm_state <= tap_fsm_stateNext;
    tap_bypass <= io_jtag_tdi;
    case(tap_fsm_state)
      JtagState_IR_CAPTURE : begin
        tap_instructionShift <= {3'd0, _zz_tap_instructionShift};
      end
      JtagState_IR_SHIFT : begin
        tap_instructionShift <= ({io_jtag_tdi,tap_instructionShift} >>> 1'd1);
      end
      JtagState_IR_UPDATE : begin
        tap_instruction <= tap_instructionShift;
      end
      JtagState_DR_SHIFT : begin
        tap_instructionShift <= ({io_jtag_tdi,tap_instructionShift} >>> 1'd1);
      end
      default : begin
      end
    endcase
    if(idcodeArea_ctrl_enable) begin
      if(idcodeArea_ctrl_shift) begin
        idcodeArea_shifter <= ({idcodeArea_ctrl_tdi,idcodeArea_shifter} >>> 1'd1);
      end
    end
    if(idcodeArea_ctrl_capture) begin
      idcodeArea_shifter <= 32'h10002fff;
    end
    if(when_JtagTap_l121) begin
      tap_instruction <= 5'h01;
    end
    if(logic_jtagLogic_dtmcs_logic_ctrl_enable) begin
      if(logic_jtagLogic_dtmcs_logic_ctrl_capture) begin
        logic_jtagLogic_dtmcs_logic_store <= logic_jtagLogic_dtmcs_captureData;
      end
      if(logic_jtagLogic_dtmcs_logic_ctrl_shift) begin
        logic_jtagLogic_dtmcs_logic_store <= ({logic_jtagLogic_dtmcs_logic_ctrl_tdi,logic_jtagLogic_dtmcs_logic_store} >>> 1'd1);
      end
    end
    if(logic_jtagLogic_dmi_logic_ctrl_enable) begin
      if(logic_jtagLogic_dmi_logic_ctrl_capture) begin
        logic_jtagLogic_dmi_logic_store <= {logic_jtagLogic_dmi_captureData_padding,{logic_jtagLogic_dmi_captureData_data,logic_jtagLogic_dmi_captureData_op}};
      end
      if(logic_jtagLogic_dmi_logic_ctrl_shift) begin
        logic_jtagLogic_dmi_logic_store <= ({logic_jtagLogic_dmi_logic_ctrl_tdi,logic_jtagLogic_dmi_logic_store} >>> 1'd1);
      end
    end
    if(logic_jtagLogic_dmiCmd_valid) begin
      logic_jtagLogic_pending <= 1'b1;
    end
    if(logic_jtagLogic_dmiRsp_valid) begin
      logic_jtagLogic_pending <= 1'b0;
    end
    if(logic_jtagLogic_trigger_dmiHardReset) begin
      logic_jtagLogic_pending <= 1'b0;
    end
    if(logic_jtagLogic_dmiRsp_valid) begin
      logic_jtagLogic_rspLogic_buffer <= logic_jtagLogic_dmiRsp_payload_data;
    end
    logic_jtagLogic_dmiStat_value <= logic_jtagLogic_dmiStat_value_aheadValue;
  end

  always @(negedge io_jtag_tck) begin
    tap_tdoUnbufferd_regNext <= tap_tdoUnbufferd;
  end

  always @(posedge clk or posedge reset) begin
    if(reset) begin
      debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rValid <= 1'b0;
    end else begin
      if(debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_ready) begin
        debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rValid <= debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_valid;
      end
    end
  end

  always @(posedge clk) begin
    if(debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_fire) begin
      debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rData_write <= debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_payload_write;
      debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rData_data <= debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_payload_data;
      debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_rData_address <= debugTransportModuleJtagTap_1_logic_jtagLogic_dmiCmd_ccToggle_io_output_toStream_payload_address;
    end
  end


endmodule

module DebugModule (
  input  wire          io_ctrl_cmd_valid,
  output wire          io_ctrl_cmd_ready,
  input  wire          io_ctrl_cmd_payload_write,
  input  wire [31:0]   io_ctrl_cmd_payload_data,
  input  wire [6:0]    io_ctrl_cmd_payload_address,
  output wire          io_ctrl_rsp_valid,
  output wire          io_ctrl_rsp_payload_error,
  output wire [31:0]   io_ctrl_rsp_payload_data,
  output wire          io_ndmreset,
  input  wire          io_harts_0_halted,
  input  wire          io_harts_0_running,
  input  wire          io_harts_0_unavailable,
  input  wire          io_harts_0_exception,
  input  wire          io_harts_0_commit,
  input  wire          io_harts_0_ebreak,
  input  wire          io_harts_0_redo,
  input  wire          io_harts_0_regSuccess,
  output wire          io_harts_0_ackReset,
  input  wire          io_harts_0_haveReset,
  output reg           io_harts_0_resume_cmd_valid,
  input  wire          io_harts_0_resume_rsp_valid,
  output wire          io_harts_0_haltReq,
  output wire          io_harts_0_dmToHart_valid,
  output wire [1:0]    io_harts_0_dmToHart_payload_op,
  output wire [4:0]    io_harts_0_dmToHart_payload_address,
  output wire [31:0]   io_harts_0_dmToHart_payload_data,
  output wire [2:0]    io_harts_0_dmToHart_payload_size,
  input  wire          io_harts_0_hartToDm_valid,
  input  wire [3:0]    io_harts_0_hartToDm_payload_address,
  input  wire [31:0]   io_harts_0_hartToDm_payload_data,
  input  wire          clk,
  input  wire          reset
);
  localparam DebugDmToHartOp_DATA = 2'd0;
  localparam DebugDmToHartOp_EXECUTE = 2'd1;
  localparam DebugDmToHartOp_REG_WRITE = 2'd2;
  localparam DebugDmToHartOp_REG_READ = 2'd3;
  localparam DebugModuleCmdErr_NONE = 3'd0;
  localparam DebugModuleCmdErr_BUSY = 3'd1;
  localparam DebugModuleCmdErr_NOT_SUPPORTED = 3'd2;
  localparam DebugModuleCmdErr_EXCEPTION = 3'd3;
  localparam DebugModuleCmdErr_HALT_RESUME = 3'd4;
  localparam DebugModuleCmdErr_BUS_1 = 3'd5;
  localparam DebugModuleCmdErr_OTHER = 3'd6;
  localparam logic_command_enumDef_BOOT = 3'd0;
  localparam logic_command_enumDef_IDLE = 3'd1;
  localparam logic_command_enumDef_DECODE = 3'd2;
  localparam logic_command_enumDef_READ_INT_REG = 3'd3;
  localparam logic_command_enumDef_WRITE_INT_REG = 3'd4;
  localparam logic_command_enumDef_WAIT_DONE = 3'd5;
  localparam logic_command_enumDef_POST_EXEC = 3'd6;
  localparam logic_command_enumDef_POST_EXEC_WAIT = 3'd7;

  wire       [31:0]   logic_progbufX_mem_spinal_port1;
  wire       [0:0]    _zz_logic_dmcontrol_haltSet;
  wire       [0:0]    _zz_logic_dmcontrol_haltClear;
  wire       [0:0]    _zz_logic_dmcontrol_resumeReq;
  wire       [0:0]    _zz_logic_dmcontrol_ackhavereset;
  wire       [14:0]   _zz_when_DebugModule_l143;
  wire       [0:0]    _zz_logic_progbufX_mem_port;
  wire       [0:0]    _zz_logic_abstractAuto_trigger;
  wire       [2:0]    _zz_logic_command_access_notSupported;
  wire       [1:0]    _zz_logic_command_access_notSupported_1;
  wire       [31:0]   _zz_logic_toHarts_payload_data;
  wire       [19:0]   _zz_logic_toHarts_payload_data_1;
  wire       [31:0]   _zz_logic_toHarts_payload_data_2;
  wire       [11:0]   _zz_logic_toHarts_payload_data_3;
  reg                 _zz_1;
  wire                factory_readErrorFlag;
  wire                factory_writeErrorFlag;
  wire                factory_cmdToRsp_valid;
  reg                 factory_cmdToRsp_payload_error;
  reg        [31:0]   factory_cmdToRsp_payload_data;
  reg                 factory_rspBuffer_valid;
  reg                 factory_rspBuffer_payload_error;
  reg        [31:0]   factory_rspBuffer_payload_data;
  wire                factory_askWrite;
  wire                factory_askRead;
  wire                factory_doWrite;
  wire                factory_doRead;
  wire                io_ctrl_cmd_fire;
  reg                 dmactive;
  reg                 logic_dmcontrol_ndmreset;
  wire       [9:0]    logic_dmcontrol_hartSelLoNew;
  wire       [9:0]    logic_dmcontrol_hartSelHiNew;
  wire       [19:0]   logic_dmcontrol_hartSelNew;
  reg        [9:0]    logic_dmcontrol_hartSelLo;
  reg        [9:0]    logic_dmcontrol_hartSelHi;
  wire       [19:0]   logic_dmcontrol_hartSel;
  reg                 logic_dmcontrol_haltSet;
  reg                 when_BusSlaveFactory_l377;
  wire                when_BusSlaveFactory_l379;
  reg                 logic_dmcontrol_haltClear;
  reg                 when_BusSlaveFactory_l391;
  wire                when_BusSlaveFactory_l393;
  reg                 logic_dmcontrol_resumeReq;
  reg                 when_BusSlaveFactory_l377_1;
  wire                when_BusSlaveFactory_l379_1;
  reg                 logic_dmcontrol_ackhavereset;
  reg                 when_BusSlaveFactory_l377_2;
  wire                when_BusSlaveFactory_l379_2;
  wire       [1:0]    logic_dmcontrol_hartSelAarsizeLimit;
  reg                 logic_dmcontrol_harts_0_haltReq;
  wire                when_DebugModule_l102;
  reg                 logic_toHarts_valid;
  reg        [1:0]    logic_toHarts_payload_op;
  reg        [4:0]    logic_toHarts_payload_address;
  reg        [31:0]   logic_toHarts_payload_data;
  reg        [2:0]    logic_toHarts_payload_size;
  wire                logic_fromHarts_valid;
  wire       [3:0]    logic_fromHarts_payload_address;
  wire       [31:0]   logic_fromHarts_payload_data;
  wire                logic_harts_0_sel;
  reg                 _zz_logic_harts_0_resumeReady;
  reg                 _zz_logic_harts_0_resumeReady_1;
  wire                logic_harts_0_resumeReady;
  wire                logic_toHarts_takeWhen_valid;
  wire       [1:0]    logic_toHarts_takeWhen_payload_op;
  wire       [4:0]    logic_toHarts_takeWhen_payload_address;
  wire       [31:0]   logic_toHarts_takeWhen_payload_data;
  wire       [2:0]    logic_toHarts_takeWhen_payload_size;
  reg                 _zz_io_harts_0_ackReset;
  wire                logic_selected_running;
  wire                logic_selected_halted;
  wire                logic_selected_commit;
  wire                logic_selected_regSuccess;
  wire                logic_selected_exception;
  wire                logic_selected_ebreak;
  wire                logic_selected_redo;
  reg        [31:0]   logic_haltsum_value;
  wire                when_DebugModule_l143;
  wire       [3:0]    logic_dmstatus_version;
  wire                logic_dmstatus_authenticated;
  wire                logic_dmstatus_anyHalted;
  wire                logic_dmstatus_allHalted;
  wire                logic_dmstatus_anyRunning;
  wire                logic_dmstatus_allRunning;
  wire                logic_dmstatus_anyUnavail;
  wire                logic_dmstatus_allUnavail;
  wire                logic_dmstatus_anyNonExistent;
  wire                logic_dmstatus_anyResumeAck;
  wire                logic_dmstatus_allResumeAck;
  wire                logic_dmstatus_anyHaveReset;
  wire                logic_dmstatus_allHaveReset;
  wire                logic_dmstatus_impebreak;
  wire       [3:0]    logic_hartInfo_dataaddr;
  wire       [3:0]    logic_hartInfo_datasize;
  wire                logic_hartInfo_dataaccess;
  wire       [3:0]    logic_hartInfo_nscratch;
  wire       [2:0]    logic_sbcs_sbversion;
  wire       [2:0]    logic_sbcs_sbaccess;
  wire                logic_progbufX_trigged;
  reg                 logic_dataX_trigged;
  wire                when_DebugModule_l205;
  wire       [3:0]    logic_abstractcs_dataCount;
  reg        [2:0]    logic_abstractcs_cmdErr;
  reg                 when_BusSlaveFactory_l341;
  wire       [2:0]    _zz_logic_abstractcs_cmdErr;
  reg                 logic_abstractcs_busy;
  wire       [4:0]    logic_abstractcs_progBufSize;
  wire                logic_abstractcs_noError;
  reg        [0:0]    logic_abstractAuto_autoexecdata;
  reg        [1:0]    logic_abstractAuto_autoexecProgbuf;
  wire                logic_abstractAuto_trigger;
  wire                logic_command_wantExit;
  reg                 logic_command_wantStart;
  wire                logic_command_wantKill;
  reg        [0:0]    logic_command_executionCounter;
  reg                 logic_command_commandRequest;
  reg        [31:0]   logic_command_data;
  wire       [15:0]   logic_command_access_args_regno;
  wire                logic_command_access_args_write;
  wire                logic_command_access_args_transfer;
  wire                logic_command_access_args_postExec;
  wire                logic_command_access_args_aarpostincrement;
  wire       [2:0]    logic_command_access_args_aarsize;
  wire       [31:0]   _zz_logic_command_access_args_regno;
  wire                logic_command_access_transferFloat;
  wire                logic_command_access_notSupported;
  wire                logic_command_request;
  wire                when_DebugModule_l260;
  wire                when_DebugModule_l263;
  wire                when_DebugModule_l266;
  reg        [2:0]    logic_command_stateReg;
  reg        [2:0]    logic_command_stateNext;
  wire                when_DebugModule_l275;
  wire                when_DebugModule_l276;
  wire       [7:0]    switch_DebugModule_l287;
  wire                when_DebugModule_l296;
  wire                when_DebugModule_l350;
  wire                when_DebugModule_l366;
  wire                when_DebugModule_l370;
  wire                when_StateMachine_l253;
  wire       [31:0]   _zz_factory_cmdToRsp_payload_data;
  reg        [31:0]   _zz_factory_cmdToRsp_payload_data_1;
  `ifndef SYNTHESIS
  reg [71:0] io_harts_0_dmToHart_payload_op_string;
  reg [71:0] logic_toHarts_payload_op_string;
  reg [71:0] logic_toHarts_takeWhen_payload_op_string;
  reg [103:0] logic_abstractcs_cmdErr_string;
  reg [103:0] _zz_logic_abstractcs_cmdErr_string;
  reg [111:0] logic_command_stateReg_string;
  reg [111:0] logic_command_stateNext_string;
  `endif

  (* ram_style = "distributed" *) reg [31:0] logic_progbufX_mem [0:1];

  assign _zz_logic_dmcontrol_haltSet = 1'b1;
  assign _zz_logic_dmcontrol_haltClear = 1'b1;
  assign _zz_logic_dmcontrol_resumeReq = 1'b1;
  assign _zz_logic_dmcontrol_ackhavereset = 1'b1;
  assign _zz_when_DebugModule_l143 = (logic_dmcontrol_hartSel >>> 3'd5);
  assign _zz_logic_progbufX_mem_port = io_ctrl_cmd_payload_address[0:0];
  assign _zz_logic_abstractAuto_trigger = io_ctrl_cmd_payload_address[0:0];
  assign _zz_logic_command_access_notSupported_1 = (logic_command_access_transferFloat ? 2'b00 : logic_dmcontrol_hartSelAarsizeLimit);
  assign _zz_logic_command_access_notSupported = {1'd0, _zz_logic_command_access_notSupported_1};
  assign _zz_logic_toHarts_payload_data_1 = ({15'd0,logic_command_access_args_regno[4 : 0]} <<< 4'd15);
  assign _zz_logic_toHarts_payload_data = {12'd0, _zz_logic_toHarts_payload_data_1};
  assign _zz_logic_toHarts_payload_data_3 = ({7'd0,logic_command_access_args_regno[4 : 0]} <<< 3'd7);
  assign _zz_logic_toHarts_payload_data_2 = {20'd0, _zz_logic_toHarts_payload_data_3};
  always @(posedge clk) begin
    if(_zz_1) begin
      logic_progbufX_mem[_zz_logic_progbufX_mem_port] <= io_ctrl_cmd_payload_data;
    end
  end

  assign logic_progbufX_mem_spinal_port1 = logic_progbufX_mem[logic_command_executionCounter];
  `ifndef SYNTHESIS
  always @(*) begin
    case(io_harts_0_dmToHart_payload_op)
      DebugDmToHartOp_DATA : io_harts_0_dmToHart_payload_op_string = "DATA     ";
      DebugDmToHartOp_EXECUTE : io_harts_0_dmToHart_payload_op_string = "EXECUTE  ";
      DebugDmToHartOp_REG_WRITE : io_harts_0_dmToHart_payload_op_string = "REG_WRITE";
      DebugDmToHartOp_REG_READ : io_harts_0_dmToHart_payload_op_string = "REG_READ ";
      default : io_harts_0_dmToHart_payload_op_string = "?????????";
    endcase
  end
  always @(*) begin
    case(logic_toHarts_payload_op)
      DebugDmToHartOp_DATA : logic_toHarts_payload_op_string = "DATA     ";
      DebugDmToHartOp_EXECUTE : logic_toHarts_payload_op_string = "EXECUTE  ";
      DebugDmToHartOp_REG_WRITE : logic_toHarts_payload_op_string = "REG_WRITE";
      DebugDmToHartOp_REG_READ : logic_toHarts_payload_op_string = "REG_READ ";
      default : logic_toHarts_payload_op_string = "?????????";
    endcase
  end
  always @(*) begin
    case(logic_toHarts_takeWhen_payload_op)
      DebugDmToHartOp_DATA : logic_toHarts_takeWhen_payload_op_string = "DATA     ";
      DebugDmToHartOp_EXECUTE : logic_toHarts_takeWhen_payload_op_string = "EXECUTE  ";
      DebugDmToHartOp_REG_WRITE : logic_toHarts_takeWhen_payload_op_string = "REG_WRITE";
      DebugDmToHartOp_REG_READ : logic_toHarts_takeWhen_payload_op_string = "REG_READ ";
      default : logic_toHarts_takeWhen_payload_op_string = "?????????";
    endcase
  end
  always @(*) begin
    case(logic_abstractcs_cmdErr)
      DebugModuleCmdErr_NONE : logic_abstractcs_cmdErr_string = "NONE         ";
      DebugModuleCmdErr_BUSY : logic_abstractcs_cmdErr_string = "BUSY         ";
      DebugModuleCmdErr_NOT_SUPPORTED : logic_abstractcs_cmdErr_string = "NOT_SUPPORTED";
      DebugModuleCmdErr_EXCEPTION : logic_abstractcs_cmdErr_string = "EXCEPTION    ";
      DebugModuleCmdErr_HALT_RESUME : logic_abstractcs_cmdErr_string = "HALT_RESUME  ";
      DebugModuleCmdErr_BUS_1 : logic_abstractcs_cmdErr_string = "BUS_1        ";
      DebugModuleCmdErr_OTHER : logic_abstractcs_cmdErr_string = "OTHER        ";
      default : logic_abstractcs_cmdErr_string = "?????????????";
    endcase
  end
  always @(*) begin
    case(_zz_logic_abstractcs_cmdErr)
      DebugModuleCmdErr_NONE : _zz_logic_abstractcs_cmdErr_string = "NONE         ";
      DebugModuleCmdErr_BUSY : _zz_logic_abstractcs_cmdErr_string = "BUSY         ";
      DebugModuleCmdErr_NOT_SUPPORTED : _zz_logic_abstractcs_cmdErr_string = "NOT_SUPPORTED";
      DebugModuleCmdErr_EXCEPTION : _zz_logic_abstractcs_cmdErr_string = "EXCEPTION    ";
      DebugModuleCmdErr_HALT_RESUME : _zz_logic_abstractcs_cmdErr_string = "HALT_RESUME  ";
      DebugModuleCmdErr_BUS_1 : _zz_logic_abstractcs_cmdErr_string = "BUS_1        ";
      DebugModuleCmdErr_OTHER : _zz_logic_abstractcs_cmdErr_string = "OTHER        ";
      default : _zz_logic_abstractcs_cmdErr_string = "?????????????";
    endcase
  end
  always @(*) begin
    case(logic_command_stateReg)
      logic_command_enumDef_BOOT : logic_command_stateReg_string = "BOOT          ";
      logic_command_enumDef_IDLE : logic_command_stateReg_string = "IDLE          ";
      logic_command_enumDef_DECODE : logic_command_stateReg_string = "DECODE        ";
      logic_command_enumDef_READ_INT_REG : logic_command_stateReg_string = "READ_INT_REG  ";
      logic_command_enumDef_WRITE_INT_REG : logic_command_stateReg_string = "WRITE_INT_REG ";
      logic_command_enumDef_WAIT_DONE : logic_command_stateReg_string = "WAIT_DONE     ";
      logic_command_enumDef_POST_EXEC : logic_command_stateReg_string = "POST_EXEC     ";
      logic_command_enumDef_POST_EXEC_WAIT : logic_command_stateReg_string = "POST_EXEC_WAIT";
      default : logic_command_stateReg_string = "??????????????";
    endcase
  end
  always @(*) begin
    case(logic_command_stateNext)
      logic_command_enumDef_BOOT : logic_command_stateNext_string = "BOOT          ";
      logic_command_enumDef_IDLE : logic_command_stateNext_string = "IDLE          ";
      logic_command_enumDef_DECODE : logic_command_stateNext_string = "DECODE        ";
      logic_command_enumDef_READ_INT_REG : logic_command_stateNext_string = "READ_INT_REG  ";
      logic_command_enumDef_WRITE_INT_REG : logic_command_stateNext_string = "WRITE_INT_REG ";
      logic_command_enumDef_WAIT_DONE : logic_command_stateNext_string = "WAIT_DONE     ";
      logic_command_enumDef_POST_EXEC : logic_command_stateNext_string = "POST_EXEC     ";
      logic_command_enumDef_POST_EXEC_WAIT : logic_command_stateNext_string = "POST_EXEC_WAIT";
      default : logic_command_stateNext_string = "??????????????";
    endcase
  end
  `endif

  always @(*) begin
    _zz_1 = 1'b0;
    if(logic_progbufX_trigged) begin
      _zz_1 = 1'b1;
    end
  end

  assign factory_readErrorFlag = 1'b0;
  assign factory_writeErrorFlag = 1'b0;
  assign io_ctrl_cmd_ready = 1'b1;
  assign factory_askWrite = (io_ctrl_cmd_valid && io_ctrl_cmd_payload_write);
  assign factory_askRead = (io_ctrl_cmd_valid && (! io_ctrl_cmd_payload_write));
  assign factory_doWrite = (factory_askWrite && io_ctrl_cmd_ready);
  assign factory_doRead = (factory_askRead && io_ctrl_cmd_ready);
  assign io_ctrl_rsp_valid = factory_rspBuffer_valid;
  assign io_ctrl_rsp_payload_error = factory_rspBuffer_payload_error;
  assign io_ctrl_rsp_payload_data = factory_rspBuffer_payload_data;
  assign io_ctrl_cmd_fire = (io_ctrl_cmd_valid && io_ctrl_cmd_ready);
  assign factory_cmdToRsp_valid = io_ctrl_cmd_fire;
  always @(*) begin
    factory_cmdToRsp_payload_error = 1'b0;
    if(logic_progbufX_trigged) begin
      factory_cmdToRsp_payload_error = 1'b0;
    end
    if(when_DebugModule_l205) begin
      factory_cmdToRsp_payload_error = 1'b0;
    end
    case(io_ctrl_cmd_payload_address)
      7'h10 : begin
        factory_cmdToRsp_payload_error = 1'b0;
      end
      7'h40 : begin
        factory_cmdToRsp_payload_error = 1'b0;
      end
      7'h11 : begin
        factory_cmdToRsp_payload_error = 1'b0;
      end
      7'h12 : begin
        factory_cmdToRsp_payload_error = 1'b0;
      end
      7'h38 : begin
        factory_cmdToRsp_payload_error = 1'b0;
      end
      7'h16 : begin
        factory_cmdToRsp_payload_error = 1'b0;
      end
      7'h18 : begin
        factory_cmdToRsp_payload_error = 1'b0;
      end
      7'h17 : begin
        factory_cmdToRsp_payload_error = 1'b0;
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    factory_cmdToRsp_payload_data = 32'h0;
    if(when_DebugModule_l205) begin
      factory_cmdToRsp_payload_data = _zz_factory_cmdToRsp_payload_data;
    end
    case(io_ctrl_cmd_payload_address)
      7'h10 : begin
        factory_cmdToRsp_payload_data[0 : 0] = dmactive;
        factory_cmdToRsp_payload_data[1 : 1] = logic_dmcontrol_ndmreset;
        factory_cmdToRsp_payload_data[25 : 16] = logic_dmcontrol_hartSelLo;
        factory_cmdToRsp_payload_data[15 : 6] = logic_dmcontrol_hartSelHi;
      end
      7'h40 : begin
        factory_cmdToRsp_payload_data[31 : 0] = logic_haltsum_value;
      end
      7'h11 : begin
        factory_cmdToRsp_payload_data[3 : 0] = logic_dmstatus_version;
        factory_cmdToRsp_payload_data[7 : 7] = logic_dmstatus_authenticated;
        factory_cmdToRsp_payload_data[8 : 8] = logic_dmstatus_anyHalted;
        factory_cmdToRsp_payload_data[9 : 9] = logic_dmstatus_allHalted;
        factory_cmdToRsp_payload_data[10 : 10] = logic_dmstatus_anyRunning;
        factory_cmdToRsp_payload_data[11 : 11] = logic_dmstatus_allRunning;
        factory_cmdToRsp_payload_data[12 : 12] = logic_dmstatus_anyUnavail;
        factory_cmdToRsp_payload_data[13 : 13] = logic_dmstatus_allUnavail;
        factory_cmdToRsp_payload_data[14 : 14] = logic_dmstatus_anyNonExistent;
        factory_cmdToRsp_payload_data[15 : 15] = logic_dmstatus_anyNonExistent;
        factory_cmdToRsp_payload_data[16 : 16] = logic_dmstatus_anyResumeAck;
        factory_cmdToRsp_payload_data[17 : 17] = logic_dmstatus_allResumeAck;
        factory_cmdToRsp_payload_data[18 : 18] = logic_dmstatus_anyHaveReset;
        factory_cmdToRsp_payload_data[19 : 19] = logic_dmstatus_allHaveReset;
        factory_cmdToRsp_payload_data[22 : 22] = logic_dmstatus_impebreak;
      end
      7'h12 : begin
        factory_cmdToRsp_payload_data[3 : 0] = logic_hartInfo_dataaddr;
        factory_cmdToRsp_payload_data[15 : 12] = logic_hartInfo_datasize;
        factory_cmdToRsp_payload_data[16 : 16] = logic_hartInfo_dataaccess;
        factory_cmdToRsp_payload_data[23 : 20] = logic_hartInfo_nscratch;
      end
      7'h38 : begin
        factory_cmdToRsp_payload_data[31 : 29] = logic_sbcs_sbversion;
        factory_cmdToRsp_payload_data[19 : 17] = logic_sbcs_sbaccess;
      end
      7'h16 : begin
        factory_cmdToRsp_payload_data[3 : 0] = logic_abstractcs_dataCount;
        factory_cmdToRsp_payload_data[10 : 8] = logic_abstractcs_cmdErr;
        factory_cmdToRsp_payload_data[12 : 12] = logic_abstractcs_busy;
        factory_cmdToRsp_payload_data[28 : 24] = logic_abstractcs_progBufSize;
      end
      7'h18 : begin
        factory_cmdToRsp_payload_data[0 : 0] = logic_abstractAuto_autoexecdata;
        factory_cmdToRsp_payload_data[17 : 16] = logic_abstractAuto_autoexecProgbuf;
      end
      default : begin
      end
    endcase
  end

  assign logic_dmcontrol_hartSelNew = {logic_dmcontrol_hartSelHiNew,logic_dmcontrol_hartSelLoNew};
  assign logic_dmcontrol_hartSel = {logic_dmcontrol_hartSelHi,logic_dmcontrol_hartSelLo};
  always @(*) begin
    logic_dmcontrol_haltSet = 1'b0;
    if(when_BusSlaveFactory_l377) begin
      if(when_BusSlaveFactory_l379) begin
        logic_dmcontrol_haltSet = _zz_logic_dmcontrol_haltSet[0];
      end
    end
  end

  always @(*) begin
    when_BusSlaveFactory_l377 = 1'b0;
    case(io_ctrl_cmd_payload_address)
      7'h10 : begin
        if(factory_doWrite) begin
          when_BusSlaveFactory_l377 = 1'b1;
        end
      end
      default : begin
      end
    endcase
  end

  assign when_BusSlaveFactory_l379 = io_ctrl_cmd_payload_data[31];
  always @(*) begin
    logic_dmcontrol_haltClear = 1'b0;
    if(when_BusSlaveFactory_l391) begin
      if(when_BusSlaveFactory_l393) begin
        logic_dmcontrol_haltClear = _zz_logic_dmcontrol_haltClear[0];
      end
    end
  end

  always @(*) begin
    when_BusSlaveFactory_l391 = 1'b0;
    case(io_ctrl_cmd_payload_address)
      7'h10 : begin
        if(factory_doWrite) begin
          when_BusSlaveFactory_l391 = 1'b1;
        end
      end
      default : begin
      end
    endcase
  end

  assign when_BusSlaveFactory_l393 = (! io_ctrl_cmd_payload_data[31]);
  always @(*) begin
    logic_dmcontrol_resumeReq = 1'b0;
    if(when_BusSlaveFactory_l377_1) begin
      if(when_BusSlaveFactory_l379_1) begin
        logic_dmcontrol_resumeReq = _zz_logic_dmcontrol_resumeReq[0];
      end
    end
    if(logic_dmcontrol_haltSet) begin
      logic_dmcontrol_resumeReq = 1'b0;
    end
  end

  always @(*) begin
    when_BusSlaveFactory_l377_1 = 1'b0;
    case(io_ctrl_cmd_payload_address)
      7'h10 : begin
        if(factory_doWrite) begin
          when_BusSlaveFactory_l377_1 = 1'b1;
        end
      end
      default : begin
      end
    endcase
  end

  assign when_BusSlaveFactory_l379_1 = io_ctrl_cmd_payload_data[30];
  always @(*) begin
    logic_dmcontrol_ackhavereset = 1'b0;
    if(when_BusSlaveFactory_l377_2) begin
      if(when_BusSlaveFactory_l379_2) begin
        logic_dmcontrol_ackhavereset = _zz_logic_dmcontrol_ackhavereset[0];
      end
    end
  end

  always @(*) begin
    when_BusSlaveFactory_l377_2 = 1'b0;
    case(io_ctrl_cmd_payload_address)
      7'h10 : begin
        if(factory_doWrite) begin
          when_BusSlaveFactory_l377_2 = 1'b1;
        end
      end
      default : begin
      end
    endcase
  end

  assign when_BusSlaveFactory_l379_2 = io_ctrl_cmd_payload_data[28];
  assign logic_dmcontrol_hartSelAarsizeLimit = 2'b10;
  assign io_harts_0_haltReq = logic_dmcontrol_harts_0_haltReq;
  always @(*) begin
    io_harts_0_resume_cmd_valid = 1'b0;
    if(when_DebugModule_l102) begin
      io_harts_0_resume_cmd_valid = logic_dmcontrol_resumeReq;
    end
  end

  assign when_DebugModule_l102 = (logic_dmcontrol_hartSelNew == 20'h0);
  assign io_ndmreset = logic_dmcontrol_ndmreset;
  always @(*) begin
    logic_toHarts_valid = 1'b0;
    if(when_DebugModule_l205) begin
      if(io_ctrl_cmd_payload_write) begin
        logic_toHarts_valid = 1'b1;
      end
    end
    if(logic_abstractcs_busy) begin
      logic_toHarts_valid = 1'b0;
    end
    case(logic_command_stateReg)
      logic_command_enumDef_IDLE : begin
      end
      logic_command_enumDef_DECODE : begin
      end
      logic_command_enumDef_READ_INT_REG : begin
        logic_toHarts_valid = 1'b1;
      end
      logic_command_enumDef_WRITE_INT_REG : begin
        logic_toHarts_valid = 1'b1;
      end
      logic_command_enumDef_WAIT_DONE : begin
      end
      logic_command_enumDef_POST_EXEC : begin
        logic_toHarts_valid = 1'b1;
      end
      logic_command_enumDef_POST_EXEC_WAIT : begin
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    logic_toHarts_payload_op = (2'bxx);
    if(when_DebugModule_l205) begin
      logic_toHarts_payload_op = DebugDmToHartOp_DATA;
    end
    case(logic_command_stateReg)
      logic_command_enumDef_IDLE : begin
      end
      logic_command_enumDef_DECODE : begin
      end
      logic_command_enumDef_READ_INT_REG : begin
        logic_toHarts_payload_op = DebugDmToHartOp_EXECUTE;
      end
      logic_command_enumDef_WRITE_INT_REG : begin
        logic_toHarts_payload_op = DebugDmToHartOp_EXECUTE;
      end
      logic_command_enumDef_WAIT_DONE : begin
      end
      logic_command_enumDef_POST_EXEC : begin
        logic_toHarts_payload_op = DebugDmToHartOp_EXECUTE;
      end
      logic_command_enumDef_POST_EXEC_WAIT : begin
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    logic_toHarts_payload_address = 5'bxxxxx;
    if(when_DebugModule_l205) begin
      logic_toHarts_payload_address = 5'h0;
    end
  end

  always @(*) begin
    logic_toHarts_payload_data = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    if(when_DebugModule_l205) begin
      logic_toHarts_payload_data = io_ctrl_cmd_payload_data;
    end
    case(logic_command_stateReg)
      logic_command_enumDef_IDLE : begin
      end
      logic_command_enumDef_DECODE : begin
      end
      logic_command_enumDef_READ_INT_REG : begin
        logic_toHarts_payload_data = (32'h7b401073 | _zz_logic_toHarts_payload_data);
      end
      logic_command_enumDef_WRITE_INT_REG : begin
        logic_toHarts_payload_data = (32'h7b402073 | _zz_logic_toHarts_payload_data_2);
      end
      logic_command_enumDef_WAIT_DONE : begin
      end
      logic_command_enumDef_POST_EXEC : begin
        logic_toHarts_payload_data = logic_progbufX_mem_spinal_port1;
      end
      logic_command_enumDef_POST_EXEC_WAIT : begin
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    logic_toHarts_payload_size = 3'bxxx;
    logic_toHarts_payload_size = logic_command_access_args_aarsize;
  end

  assign logic_fromHarts_valid = (|io_harts_0_hartToDm_valid);
  assign logic_fromHarts_payload_address = io_harts_0_hartToDm_payload_address;
  assign logic_fromHarts_payload_data = io_harts_0_hartToDm_payload_data;
  assign logic_harts_0_sel = (logic_dmcontrol_hartSel == 20'h0);
  assign logic_harts_0_resumeReady = ((! _zz_logic_harts_0_resumeReady) && _zz_logic_harts_0_resumeReady_1);
  assign logic_toHarts_takeWhen_valid = (logic_toHarts_valid && (! ((logic_toHarts_payload_op != DebugDmToHartOp_DATA) && (! logic_harts_0_sel))));
  assign logic_toHarts_takeWhen_payload_op = logic_toHarts_payload_op;
  assign logic_toHarts_takeWhen_payload_address = logic_toHarts_payload_address;
  assign logic_toHarts_takeWhen_payload_data = logic_toHarts_payload_data;
  assign logic_toHarts_takeWhen_payload_size = logic_toHarts_payload_size;
  assign io_harts_0_dmToHart_valid = logic_toHarts_takeWhen_valid;
  assign io_harts_0_dmToHart_payload_op = logic_toHarts_takeWhen_payload_op;
  assign io_harts_0_dmToHart_payload_address = logic_toHarts_takeWhen_payload_address;
  assign io_harts_0_dmToHart_payload_data = logic_toHarts_takeWhen_payload_data;
  assign io_harts_0_dmToHart_payload_size = logic_toHarts_takeWhen_payload_size;
  assign io_harts_0_ackReset = _zz_io_harts_0_ackReset;
  assign logic_selected_running = io_harts_0_running;
  assign logic_selected_halted = io_harts_0_halted;
  assign logic_selected_commit = io_harts_0_commit;
  assign logic_selected_regSuccess = io_harts_0_regSuccess;
  assign logic_selected_exception = io_harts_0_exception;
  assign logic_selected_ebreak = io_harts_0_ebreak;
  assign logic_selected_redo = io_harts_0_redo;
  always @(*) begin
    logic_haltsum_value = 32'h0;
    if(when_DebugModule_l143) begin
      logic_haltsum_value[0] = io_harts_0_halted;
    end
  end

  assign when_DebugModule_l143 = (_zz_when_DebugModule_l143 == 15'h0);
  assign logic_dmstatus_version = 4'b0010;
  assign logic_dmstatus_authenticated = 1'b1;
  assign logic_dmstatus_anyHalted = (|(logic_harts_0_sel && io_harts_0_halted));
  assign logic_dmstatus_allHalted = (&((! logic_harts_0_sel) || io_harts_0_halted));
  assign logic_dmstatus_anyRunning = (|(logic_harts_0_sel && io_harts_0_running));
  assign logic_dmstatus_allRunning = (&((! logic_harts_0_sel) || io_harts_0_running));
  assign logic_dmstatus_anyUnavail = (|(logic_harts_0_sel && io_harts_0_unavailable));
  assign logic_dmstatus_allUnavail = (&((! logic_harts_0_sel) || io_harts_0_unavailable));
  assign logic_dmstatus_anyNonExistent = (20'h00001 <= logic_dmcontrol_hartSel);
  assign logic_dmstatus_anyResumeAck = (|(logic_harts_0_sel && logic_harts_0_resumeReady));
  assign logic_dmstatus_allResumeAck = (&((! logic_harts_0_sel) || logic_harts_0_resumeReady));
  assign logic_dmstatus_anyHaveReset = (|(logic_harts_0_sel && io_harts_0_haveReset));
  assign logic_dmstatus_allHaveReset = (&((! logic_harts_0_sel) || io_harts_0_haveReset));
  assign logic_dmstatus_impebreak = 1'b1;
  assign logic_hartInfo_dataaddr = 4'b0000;
  assign logic_hartInfo_datasize = 4'b0000;
  assign logic_hartInfo_dataaccess = 1'b0;
  assign logic_hartInfo_nscratch = 4'b0000;
  assign logic_sbcs_sbversion = 3'b001;
  assign logic_sbcs_sbaccess = 3'b010;
  assign logic_progbufX_trigged = ((io_ctrl_cmd_valid && io_ctrl_cmd_payload_write) && ((io_ctrl_cmd_payload_address & 7'h70) == 7'h20));
  always @(*) begin
    logic_dataX_trigged = 1'b0;
    if(when_DebugModule_l205) begin
      logic_dataX_trigged = 1'b1;
    end
  end

  assign when_DebugModule_l205 = ((io_ctrl_cmd_valid && (7'h04 <= io_ctrl_cmd_payload_address)) && (io_ctrl_cmd_payload_address < 7'h05));
  assign logic_abstractcs_dataCount = 4'b0001;
  always @(*) begin
    when_BusSlaveFactory_l341 = 1'b0;
    case(io_ctrl_cmd_payload_address)
      7'h16 : begin
        if(factory_doWrite) begin
          when_BusSlaveFactory_l341 = 1'b1;
        end
      end
      default : begin
      end
    endcase
  end

  assign _zz_logic_abstractcs_cmdErr = (logic_abstractcs_cmdErr & (~ io_ctrl_cmd_payload_data[10 : 8]));
  assign logic_abstractcs_progBufSize = 5'h02;
  assign logic_abstractcs_noError = (logic_abstractcs_cmdErr == DebugModuleCmdErr_NONE);
  assign logic_abstractAuto_trigger = ((logic_progbufX_trigged && logic_abstractAuto_autoexecProgbuf[_zz_logic_abstractAuto_trigger]) || (logic_dataX_trigged && logic_abstractAuto_autoexecdata[0]));
  assign logic_command_wantExit = 1'b0;
  always @(*) begin
    logic_command_wantStart = 1'b0;
    case(logic_command_stateReg)
      logic_command_enumDef_IDLE : begin
      end
      logic_command_enumDef_DECODE : begin
      end
      logic_command_enumDef_READ_INT_REG : begin
      end
      logic_command_enumDef_WRITE_INT_REG : begin
      end
      logic_command_enumDef_WAIT_DONE : begin
      end
      logic_command_enumDef_POST_EXEC : begin
      end
      logic_command_enumDef_POST_EXEC_WAIT : begin
      end
      default : begin
        logic_command_wantStart = 1'b1;
      end
    endcase
  end

  assign logic_command_wantKill = 1'b0;
  always @(*) begin
    logic_command_commandRequest = 1'b0;
    case(io_ctrl_cmd_payload_address)
      7'h17 : begin
        if(factory_doWrite) begin
          logic_command_commandRequest = 1'b1;
        end
      end
      default : begin
      end
    endcase
  end

  assign _zz_logic_command_access_args_regno = logic_command_data;
  assign logic_command_access_args_regno = _zz_logic_command_access_args_regno[15 : 0];
  assign logic_command_access_args_write = _zz_logic_command_access_args_regno[16];
  assign logic_command_access_args_transfer = _zz_logic_command_access_args_regno[17];
  assign logic_command_access_args_postExec = _zz_logic_command_access_args_regno[18];
  assign logic_command_access_args_aarpostincrement = _zz_logic_command_access_args_regno[19];
  assign logic_command_access_args_aarsize = _zz_logic_command_access_args_regno[22 : 20];
  assign logic_command_access_transferFloat = logic_command_access_args_regno[5];
  assign logic_command_access_notSupported = (((_zz_logic_command_access_notSupported < logic_command_access_args_aarsize) || logic_command_access_args_aarpostincrement) || (logic_command_access_args_transfer && (logic_command_access_args_regno[15 : 5] != 11'h080)));
  assign logic_command_request = (logic_command_commandRequest || logic_abstractAuto_trigger);
  assign when_DebugModule_l260 = ((logic_command_request && logic_abstractcs_busy) && logic_abstractcs_noError);
  assign when_DebugModule_l263 = (|io_harts_0_exception);
  assign when_DebugModule_l266 = ((logic_abstractcs_busy && (logic_progbufX_trigged || logic_dataX_trigged)) && logic_abstractcs_noError);
  assign logic_dmcontrol_hartSelLoNew = io_ctrl_cmd_payload_data[25 : 16];
  assign logic_dmcontrol_hartSelHiNew = io_ctrl_cmd_payload_data[15 : 6];
  always @(*) begin
    logic_command_stateNext = logic_command_stateReg;
    case(logic_command_stateReg)
      logic_command_enumDef_IDLE : begin
        if(when_DebugModule_l275) begin
          if(!when_DebugModule_l276) begin
            logic_command_stateNext = logic_command_enumDef_DECODE;
          end
        end
      end
      logic_command_enumDef_DECODE : begin
        logic_command_stateNext = logic_command_enumDef_IDLE;
        case(switch_DebugModule_l287)
          8'h0 : begin
            if(!logic_command_access_notSupported) begin
              if(logic_command_access_args_postExec) begin
                logic_command_stateNext = logic_command_enumDef_POST_EXEC;
              end
              if(logic_command_access_args_transfer) begin
                if(when_DebugModule_l296) begin
                  if(logic_command_access_args_write) begin
                    logic_command_stateNext = logic_command_enumDef_WRITE_INT_REG;
                  end else begin
                    logic_command_stateNext = logic_command_enumDef_READ_INT_REG;
                  end
                end
              end
            end
          end
          default : begin
          end
        endcase
      end
      logic_command_enumDef_READ_INT_REG : begin
        logic_command_stateNext = logic_command_enumDef_WAIT_DONE;
      end
      logic_command_enumDef_WRITE_INT_REG : begin
        logic_command_stateNext = logic_command_enumDef_WAIT_DONE;
      end
      logic_command_enumDef_WAIT_DONE : begin
        if(when_DebugModule_l350) begin
          logic_command_stateNext = logic_command_enumDef_IDLE;
          if(logic_command_access_args_postExec) begin
            logic_command_stateNext = logic_command_enumDef_POST_EXEC;
          end
        end
      end
      logic_command_enumDef_POST_EXEC : begin
        logic_command_stateNext = logic_command_enumDef_POST_EXEC_WAIT;
      end
      logic_command_enumDef_POST_EXEC_WAIT : begin
        if(when_DebugModule_l366) begin
          logic_command_stateNext = logic_command_enumDef_IDLE;
        end
        if(when_DebugModule_l370) begin
          logic_command_stateNext = logic_command_enumDef_POST_EXEC;
        end
      end
      default : begin
      end
    endcase
    if(logic_command_wantStart) begin
      logic_command_stateNext = logic_command_enumDef_IDLE;
    end
    if(logic_command_wantKill) begin
      logic_command_stateNext = logic_command_enumDef_BOOT;
    end
  end

  assign when_DebugModule_l275 = (logic_command_request && logic_abstractcs_noError);
  assign when_DebugModule_l276 = (! io_harts_0_halted);
  assign switch_DebugModule_l287 = logic_command_data[31 : 24];
  assign when_DebugModule_l296 = (! logic_command_access_args_regno[5]);
  assign when_DebugModule_l350 = (logic_selected_commit || logic_selected_regSuccess);
  assign when_DebugModule_l366 = ((logic_selected_ebreak || logic_selected_exception) || logic_selected_commit);
  assign when_DebugModule_l370 = (logic_selected_redo || (logic_selected_commit && (logic_command_executionCounter != 1'b1)));
  assign when_StateMachine_l253 = ((! (logic_command_stateReg == logic_command_enumDef_IDLE)) && (logic_command_stateNext == logic_command_enumDef_IDLE));
  assign _zz_factory_cmdToRsp_payload_data = _zz_factory_cmdToRsp_payload_data_1;
  always @(posedge clk or posedge reset) begin
    if(reset) begin
      factory_rspBuffer_valid <= 1'b0;
      dmactive <= 1'b0;
    end else begin
      factory_rspBuffer_valid <= factory_cmdToRsp_valid;
      case(io_ctrl_cmd_payload_address)
        7'h10 : begin
          if(factory_doWrite) begin
            dmactive <= io_ctrl_cmd_payload_data[0];
          end
        end
        default : begin
        end
      endcase
    end
  end

  always @(posedge clk) begin
    factory_rspBuffer_payload_error <= factory_cmdToRsp_payload_error;
    factory_rspBuffer_payload_data <= factory_cmdToRsp_payload_data;
  end

  always @(posedge clk or negedge dmactive) begin
    if(!dmactive) begin
      logic_dmcontrol_ndmreset <= 1'b0;
      logic_dmcontrol_hartSelLo <= 10'h0;
      logic_dmcontrol_hartSelHi <= 10'h0;
      logic_dmcontrol_harts_0_haltReq <= 1'b0;
      _zz_logic_harts_0_resumeReady <= 1'b0;
      _zz_logic_harts_0_resumeReady_1 <= 1'b0;
      logic_abstractcs_cmdErr <= DebugModuleCmdErr_NONE;
      logic_abstractcs_busy <= 1'b0;
      logic_abstractAuto_autoexecdata <= 1'b0;
      logic_abstractAuto_autoexecProgbuf <= 2'b00;
      logic_command_stateReg <= logic_command_enumDef_BOOT;
    end else begin
      if(when_DebugModule_l102) begin
        logic_dmcontrol_harts_0_haltReq <= ((logic_dmcontrol_harts_0_haltReq || logic_dmcontrol_haltSet) && (! logic_dmcontrol_haltClear));
      end
      if(io_harts_0_resume_cmd_valid) begin
        _zz_logic_harts_0_resumeReady <= 1'b1;
      end
      if(io_harts_0_resume_rsp_valid) begin
        _zz_logic_harts_0_resumeReady <= 1'b0;
      end
      if(io_harts_0_resume_cmd_valid) begin
        _zz_logic_harts_0_resumeReady_1 <= 1'b1;
      end
      if(when_BusSlaveFactory_l341) begin
        logic_abstractcs_cmdErr <= _zz_logic_abstractcs_cmdErr;
      end
      if(when_DebugModule_l260) begin
        logic_abstractcs_cmdErr <= DebugModuleCmdErr_BUSY;
      end
      if(when_DebugModule_l263) begin
        logic_abstractcs_cmdErr <= DebugModuleCmdErr_EXCEPTION;
      end
      if(when_DebugModule_l266) begin
        logic_abstractcs_cmdErr <= DebugModuleCmdErr_BUSY;
      end
      case(io_ctrl_cmd_payload_address)
        7'h10 : begin
          if(factory_doWrite) begin
            logic_dmcontrol_ndmreset <= io_ctrl_cmd_payload_data[1];
            logic_dmcontrol_hartSelLo <= io_ctrl_cmd_payload_data[25 : 16];
            logic_dmcontrol_hartSelHi <= io_ctrl_cmd_payload_data[15 : 6];
          end
        end
        7'h18 : begin
          if(factory_doWrite) begin
            logic_abstractAuto_autoexecdata <= io_ctrl_cmd_payload_data[0 : 0];
            logic_abstractAuto_autoexecProgbuf <= io_ctrl_cmd_payload_data[17 : 16];
          end
        end
        default : begin
        end
      endcase
      logic_command_stateReg <= logic_command_stateNext;
      case(logic_command_stateReg)
        logic_command_enumDef_IDLE : begin
          if(when_DebugModule_l275) begin
            if(when_DebugModule_l276) begin
              logic_abstractcs_cmdErr <= DebugModuleCmdErr_HALT_RESUME;
            end else begin
              logic_abstractcs_busy <= 1'b1;
            end
          end
        end
        logic_command_enumDef_DECODE : begin
          case(switch_DebugModule_l287)
            8'h0 : begin
              if(logic_command_access_notSupported) begin
                logic_abstractcs_cmdErr <= DebugModuleCmdErr_NOT_SUPPORTED;
              end
            end
            default : begin
              logic_abstractcs_cmdErr <= DebugModuleCmdErr_NOT_SUPPORTED;
            end
          endcase
        end
        logic_command_enumDef_READ_INT_REG : begin
        end
        logic_command_enumDef_WRITE_INT_REG : begin
        end
        logic_command_enumDef_WAIT_DONE : begin
        end
        logic_command_enumDef_POST_EXEC : begin
        end
        logic_command_enumDef_POST_EXEC_WAIT : begin
        end
        default : begin
        end
      endcase
      if(when_StateMachine_l253) begin
        logic_abstractcs_busy <= 1'b0;
      end
    end
  end

  always @(posedge clk) begin
    _zz_io_harts_0_ackReset <= (logic_harts_0_sel && logic_dmcontrol_ackhavereset);
    case(io_ctrl_cmd_payload_address)
      7'h17 : begin
        if(factory_doWrite) begin
          logic_command_data <= io_ctrl_cmd_payload_data[31 : 0];
        end
      end
      default : begin
      end
    endcase
    case(logic_command_stateReg)
      logic_command_enumDef_IDLE : begin
        logic_command_executionCounter <= 1'b0;
      end
      logic_command_enumDef_DECODE : begin
      end
      logic_command_enumDef_READ_INT_REG : begin
      end
      logic_command_enumDef_WRITE_INT_REG : begin
      end
      logic_command_enumDef_WAIT_DONE : begin
      end
      logic_command_enumDef_POST_EXEC : begin
      end
      logic_command_enumDef_POST_EXEC_WAIT : begin
        if(when_DebugModule_l366) begin
          logic_command_executionCounter <= (logic_command_executionCounter + 1'b1);
        end
      end
      default : begin
      end
    endcase
    if(logic_fromHarts_valid) begin
      _zz_factory_cmdToRsp_payload_data_1 <= logic_fromHarts_payload_data;
    end
  end


endmodule

module FpuCore (
  input  wire          io_port_0_cmd_valid,
  output wire          io_port_0_cmd_ready,
  input  wire [3:0]    io_port_0_cmd_payload_opcode,
  input  wire [1:0]    io_port_0_cmd_payload_arg,
  input  wire [4:0]    io_port_0_cmd_payload_rs1,
  input  wire [4:0]    io_port_0_cmd_payload_rs2,
  input  wire [4:0]    io_port_0_cmd_payload_rs3,
  input  wire [4:0]    io_port_0_cmd_payload_rd,
  input  wire [0:0]    io_port_0_cmd_payload_format,
  input  wire [2:0]    io_port_0_cmd_payload_roundMode,
  input  wire          io_port_0_commit_valid,
  output wire          io_port_0_commit_ready,
  input  wire [3:0]    io_port_0_commit_payload_opcode,
  input  wire [4:0]    io_port_0_commit_payload_rd,
  input  wire          io_port_0_commit_payload_write,
  input  wire [31:0]   io_port_0_commit_payload_value,
  output wire          io_port_0_rsp_valid,
  input  wire          io_port_0_rsp_ready,
  output wire [31:0]   io_port_0_rsp_payload_value,
  output wire          io_port_0_rsp_payload_NV,
  output wire          io_port_0_rsp_payload_NX,
  output wire          io_port_0_completion_valid,
  output wire          io_port_0_completion_payload_flags_NX,
  output wire          io_port_0_completion_payload_flags_UF,
  output wire          io_port_0_completion_payload_flags_OF,
  output wire          io_port_0_completion_payload_flags_DZ,
  output wire          io_port_0_completion_payload_flags_NV,
  output wire          io_port_0_completion_payload_written,
  input  wire          clk,
  input  wire          reset
);
  localparam FpuOpcode_LOAD = 4'd0;
  localparam FpuOpcode_STORE = 4'd1;
  localparam FpuOpcode_MUL = 4'd2;
  localparam FpuOpcode_ADD = 4'd3;
  localparam FpuOpcode_FMA = 4'd4;
  localparam FpuOpcode_I2F = 4'd5;
  localparam FpuOpcode_F2I = 4'd6;
  localparam FpuOpcode_CMP = 4'd7;
  localparam FpuOpcode_DIV = 4'd8;
  localparam FpuOpcode_SQRT = 4'd9;
  localparam FpuOpcode_MIN_MAX = 4'd10;
  localparam FpuOpcode_SGNJ = 4'd11;
  localparam FpuOpcode_FMV_X_W = 4'd12;
  localparam FpuOpcode_FMV_W_X = 4'd13;
  localparam FpuOpcode_FCLASS = 4'd14;
  localparam FpuOpcode_FCVT_X_X = 4'd15;
  localparam FpuFormat_FLOAT = 1'd0;
  localparam FpuFormat_DOUBLE = 1'd1;
  localparam FpuRoundMode_RNE = 3'd0;
  localparam FpuRoundMode_RTZ = 3'd1;
  localparam FpuRoundMode_RDN = 3'd2;
  localparam FpuRoundMode_RUP = 3'd3;
  localparam FpuRoundMode_RMM = 3'd4;

  wire                div_divider_io_input_valid;
  wire       [23:0]   div_divider_io_input_payload_a;
  wire       [23:0]   div_divider_io_input_payload_b;
  wire                sqrt_sqrt_io_input_valid;
  wire       [24:0]   sqrt_sqrt_io_input_payload_a;
  reg        [33:0]   rf_ram_spinal_port0;
  reg        [33:0]   rf_ram_spinal_port1;
  reg        [33:0]   rf_ram_spinal_port2;
  wire       [0:0]    rf_scoreboards_0_target_spinal_port1;
  wire       [0:0]    rf_scoreboards_0_target_spinal_port2;
  wire       [0:0]    rf_scoreboards_0_target_spinal_port3;
  wire       [0:0]    rf_scoreboards_0_target_spinal_port4;
  wire       [0:0]    rf_scoreboards_0_hit_spinal_port1;
  wire       [0:0]    rf_scoreboards_0_hit_spinal_port2;
  wire       [0:0]    rf_scoreboards_0_hit_spinal_port3;
  wire       [0:0]    rf_scoreboards_0_hit_spinal_port4;
  wire       [0:0]    rf_scoreboards_0_hit_spinal_port5;
  wire       [0:0]    rf_scoreboards_0_writes_spinal_port1;
  wire                streamFork_1_io_input_ready;
  wire                streamFork_1_io_outputs_0_valid;
  wire       [3:0]    streamFork_1_io_outputs_0_payload_opcode;
  wire       [4:0]    streamFork_1_io_outputs_0_payload_rd;
  wire                streamFork_1_io_outputs_0_payload_write;
  wire       [31:0]   streamFork_1_io_outputs_0_payload_value;
  wire                streamFork_1_io_outputs_1_valid;
  wire       [3:0]    streamFork_1_io_outputs_1_payload_opcode;
  wire       [4:0]    streamFork_1_io_outputs_1_payload_rd;
  wire                streamFork_1_io_outputs_1_payload_write;
  wire       [31:0]   streamFork_1_io_outputs_1_payload_value;
  wire                cmdArbiter_arbiter_io_inputs_0_ready;
  wire                cmdArbiter_arbiter_io_output_valid;
  wire       [3:0]    cmdArbiter_arbiter_io_output_payload_opcode;
  wire       [1:0]    cmdArbiter_arbiter_io_output_payload_arg;
  wire       [4:0]    cmdArbiter_arbiter_io_output_payload_rs1;
  wire       [4:0]    cmdArbiter_arbiter_io_output_payload_rs2;
  wire       [4:0]    cmdArbiter_arbiter_io_output_payload_rs3;
  wire       [4:0]    cmdArbiter_arbiter_io_output_payload_rd;
  wire       [0:0]    cmdArbiter_arbiter_io_output_payload_format;
  wire       [2:0]    cmdArbiter_arbiter_io_output_payload_roundMode;
  wire       [0:0]    cmdArbiter_arbiter_io_chosenOH;
  wire                div_divider_io_input_ready;
  wire                div_divider_io_output_valid;
  wire       [26:0]   div_divider_io_output_payload_result;
  wire       [24:0]   div_divider_io_output_payload_remain;
  wire                sqrt_sqrt_io_input_ready;
  wire                sqrt_sqrt_io_output_valid;
  wire       [23:0]   sqrt_sqrt_io_output_payload_result;
  wire       [27:0]   sqrt_sqrt_io_output_payload_remain;
  wire                streamArbiter_2_io_inputs_0_ready;
  wire                streamArbiter_2_io_inputs_1_ready;
  wire                streamArbiter_2_io_inputs_2_ready;
  wire                streamArbiter_2_io_inputs_3_ready;
  wire                streamArbiter_2_io_inputs_4_ready;
  wire                streamArbiter_2_io_inputs_5_ready;
  wire                streamArbiter_2_io_output_valid;
  wire       [4:0]    streamArbiter_2_io_output_payload_rd;
  wire       [23:0]   streamArbiter_2_io_output_payload_value_mantissa;
  wire       [8:0]    streamArbiter_2_io_output_payload_value_exponent;
  wire                streamArbiter_2_io_output_payload_value_sign;
  wire                streamArbiter_2_io_output_payload_value_special;
  wire                streamArbiter_2_io_output_payload_scrap;
  wire       [2:0]    streamArbiter_2_io_output_payload_roundMode;
  wire                streamArbiter_2_io_output_payload_NV;
  wire                streamArbiter_2_io_output_payload_DZ;
  wire       [2:0]    streamArbiter_2_io_chosen;
  wire       [5:0]    streamArbiter_2_io_chosenOH;
  wire       [0:0]    _zz_rf_scoreboards_0_target_port;
  wire       [0:0]    _zz_rf_scoreboards_0_hit_port;
  wire       [3:0]    _zz_commitLogic_0_pending_counter;
  wire       [3:0]    _zz_commitLogic_0_pending_counter_1;
  wire       [0:0]    _zz_commitLogic_0_pending_counter_2;
  wire       [3:0]    _zz_commitLogic_0_pending_counter_3;
  wire       [0:0]    _zz_commitLogic_0_pending_counter_4;
  wire       [3:0]    _zz_commitLogic_0_add_counter;
  wire       [3:0]    _zz_commitLogic_0_add_counter_1;
  wire       [0:0]    _zz_commitLogic_0_add_counter_2;
  wire       [3:0]    _zz_commitLogic_0_add_counter_3;
  wire       [0:0]    _zz_commitLogic_0_add_counter_4;
  wire       [3:0]    _zz_commitLogic_0_mul_counter;
  wire       [3:0]    _zz_commitLogic_0_mul_counter_1;
  wire       [0:0]    _zz_commitLogic_0_mul_counter_2;
  wire       [3:0]    _zz_commitLogic_0_mul_counter_3;
  wire       [0:0]    _zz_commitLogic_0_mul_counter_4;
  wire       [3:0]    _zz_commitLogic_0_div_counter;
  wire       [3:0]    _zz_commitLogic_0_div_counter_1;
  wire       [0:0]    _zz_commitLogic_0_div_counter_2;
  wire       [3:0]    _zz_commitLogic_0_div_counter_3;
  wire       [0:0]    _zz_commitLogic_0_div_counter_4;
  wire       [3:0]    _zz_commitLogic_0_sqrt_counter;
  wire       [3:0]    _zz_commitLogic_0_sqrt_counter_1;
  wire       [0:0]    _zz_commitLogic_0_sqrt_counter_2;
  wire       [3:0]    _zz_commitLogic_0_sqrt_counter_3;
  wire       [0:0]    _zz_commitLogic_0_sqrt_counter_4;
  wire       [3:0]    _zz_commitLogic_0_short_counter;
  wire       [3:0]    _zz_commitLogic_0_short_counter_1;
  wire       [0:0]    _zz_commitLogic_0_short_counter_2;
  wire       [3:0]    _zz_commitLogic_0_short_counter_3;
  wire       [0:0]    _zz_commitLogic_0_short_counter_4;
  wire       [0:0]    _zz_rf_scoreboards_0_writes_port;
  wire       [3:0]    _zz_decode_shortPipHit;
  wire                _zz_decode_shortPipHit_1;
  wire       [0:0]    _zz_decode_shortPipHit_2;
  wire       [0:0]    _zz_decode_shortPipHit_3;
  wire       [0:0]    _zz_when;
  wire       [31:0]   _zz_load_s1_fsm_shift_input_1;
  wire       [31:0]   _zz_load_s1_fsm_shift_input_2;
  wire       [31:0]   _zz_load_s1_fsm_shift_input_3;
  wire       [31:0]   _zz_load_s1_fsm_shift_input_4;
  wire       [31:0]   _zz_load_s1_fsm_shift_input_5;
  wire       [31:0]   _zz_load_s0_output_rData_value_2;
  wire       [32:0]   _zz_load_s0_output_rData_value_3;
  wire       [32:0]   _zz_load_s0_output_rData_value_4;
  wire       [32:0]   _zz_load_s0_output_rData_value_5;
  wire       [0:0]    _zz_load_s0_output_rData_value_6;
  wire                _zz__zz_load_s1_fsm_shift_by;
  wire       [0:0]    _zz__zz_load_s1_fsm_shift_by_1;
  wire       [22:0]   _zz__zz_load_s1_fsm_shift_by_2;
  wire                _zz__zz_load_s1_fsm_shift_by_3;
  wire       [0:0]    _zz__zz_load_s1_fsm_shift_by_4;
  wire       [11:0]   _zz__zz_load_s1_fsm_shift_by_5;
  wire                _zz__zz_load_s1_fsm_shift_by_6;
  wire       [0:0]    _zz__zz_load_s1_fsm_shift_by_7;
  wire       [0:0]    _zz__zz_load_s1_fsm_shift_by_8;
  wire       [31:0]   _zz__zz_load_s1_fsm_shift_by_1_1;
  wire       [9:0]    _zz_load_s1_recoded_exponent;
  wire       [9:0]    _zz_load_s1_recoded_exponent_1;
  wire       [9:0]    _zz_load_s1_recoded_exponent_2;
  wire       [8:0]    _zz_load_s1_output_payload_value_exponent;
  wire       [8:0]    _zz_shortPip_f32_exp;
  wire       [8:0]    _zz_shortPip_expInSubnormalRange;
  wire       [32:0]   _zz_shortPip_fsm_shift_input_1;
  wire       [32:0]   _zz_shortPip_fsm_shift_input_2;
  wire       [32:0]   _zz_shortPip_fsm_shift_input_3;
  wire       [32:0]   _zz_shortPip_fsm_shift_input_4;
  wire       [32:0]   _zz_shortPip_fsm_shift_input_5;
  wire       [32:0]   _zz_shortPip_fsm_shift_input_6;
  wire       [8:0]    _zz_shortPip_fsm_shift_by_2;
  wire       [8:0]    _zz_shortPip_fsm_shift_by_3;
  wire       [8:0]    _zz_shortPip_fsm_shift_by_4;
  wire       [8:0]    _zz_shortPip_fsm_shift_by_5;
  wire       [8:0]    _zz_shortPip_fsm_shift_by_6;
  wire       [31:0]   _zz_shortPip_f2i_result;
  wire       [0:0]    _zz_shortPip_f2i_result_1;
  wire       [30:0]   _zz_shortPip_f2i_underflow;
  wire       [30:0]   _zz_shortPip_f2i_underflow_1;
  wire       [47:0]   _zz_mul_sum1_sum;
  wire       [47:0]   _zz_mul_sum1_sum_1;
  wire       [41:0]   _zz_mul_sum1_sum_2;
  wire       [47:0]   _zz_mul_sum2_sum;
  wire       [47:0]   _zz_mul_sum2_sum_1;
  wire       [41:0]   _zz_mul_sum2_sum_2;
  wire       [47:0]   _zz_mul_sum2_sum_3;
  wire       [9:0]    _zz_mul_norm_exp;
  wire       [0:0]    _zz_mul_norm_exp_1;
  wire       [9:0]    _zz_mul_norm_forceUnderflow;
  wire       [9:0]    _zz_mul_norm_output_exponent;
  wire       [10:0]   _zz_div_exponent;
  wire       [10:0]   _zz_div_exponent_1;
  wire       [10:0]   _zz_div_exponent_2;
  wire       [10:0]   _zz_div_exponent_3;
  wire       [10:0]   _zz_div_exponent_4;
  wire       [0:0]    _zz_div_exponent_5;
  wire       [8:0]    _zz_sqrt_exponent;
  wire       [8:0]    _zz_sqrt_exponent_1;
  wire       [7:0]    _zz_sqrt_exponent_2;
  wire       [7:0]    _zz_sqrt_exponent_3;
  wire       [8:0]    _zz_sqrt_exponent_4;
  wire       [0:0]    _zz_sqrt_exponent_5;
  wire       [9:0]    _zz_add_shifter_shiftBy_1;
  wire       [9:0]    _zz_add_shifter_shiftBy_2;
  wire       [9:0]    _zz_add_shifter_shiftBy_3;
  wire       [0:0]    _zz_add_shifter_shiftBy_4;
  wire       [25:0]   _zz_add_shifter_yMantissa_1;
  wire       [25:0]   _zz_add_shifter_yMantissa_2;
  wire       [25:0]   _zz_add_shifter_yMantissa_3;
  wire       [25:0]   _zz_add_shifter_yMantissa_4;
  wire       [25:0]   _zz_add_shifter_yMantissa_5;
  wire       [26:0]   _zz_add_math_xSigned;
  wire       [26:0]   _zz_add_math_xSigned_1;
  wire       [0:0]    _zz_add_math_xSigned_2;
  wire       [26:0]   _zz_add_math_ySigned;
  wire       [26:0]   _zz_add_math_ySigned_1;
  wire       [0:0]    _zz_add_math_ySigned_2;
  wire       [27:0]   _zz_add_math_output_payload_xyMantissa;
  wire       [27:0]   _zz_add_math_output_payload_xyMantissa_1;
  wire       [27:0]   _zz_add_math_output_payload_xyMantissa_2;
  wire       [27:0]   _zz_add_math_output_payload_xyMantissa_3;
  wire                _zz__zz_add_oh_shift;
  wire       [0:0]    _zz__zz_add_oh_shift_1;
  wire       [17:0]   _zz__zz_add_oh_shift_2;
  wire                _zz__zz_add_oh_shift_3;
  wire       [0:0]    _zz__zz_add_oh_shift_4;
  wire       [6:0]    _zz__zz_add_oh_shift_5;
  wire       [26:0]   _zz__zz_add_oh_shift_1_1;
  wire       [9:0]    _zz_add_norm_output_payload_exponent;
  wire       [9:0]    _zz_add_norm_output_payload_exponent_1;
  wire       [5:0]    _zz_add_norm_output_payload_exponent_2;
  wire       [24:0]   _zz_add_result_output_payload_value_mantissa;
  wire       [9:0]    _zz_roundFront_expDif;
  wire       [8:0]    _zz_roundFront_expDif_1;
  wire       [4:0]    _zz_roundFront_exactMask;
  wire                _zz_roundFront_exactMask_1;
  wire       [0:0]    _zz_roundFront_exactMask_2;
  wire       [17:0]   _zz_roundFront_exactMask_3;
  wire       [4:0]    _zz_roundFront_exactMask_4;
  wire                _zz_roundFront_exactMask_5;
  wire       [0:0]    _zz_roundFront_exactMask_6;
  wire       [9:0]    _zz_roundFront_exactMask_7;
  wire       [4:0]    _zz_roundFront_exactMask_8;
  wire                _zz_roundFront_exactMask_9;
  wire       [0:0]    _zz_roundFront_exactMask_10;
  wire       [1:0]    _zz_roundFront_exactMask_11;
  wire       [24:0]   _zz_roundFront_roundAdjusted;
  wire       [23:0]   _zz_roundFront_roundAdjusted_1;
  wire       [24:0]   _zz_roundFront_rneBit;
  wire       [22:0]   _zz_roundFront_rneBit_1;
  wire       [22:0]   _zz_roundBack_adderMantissa;
  wire       [23:0]   _zz_roundBack_adderRightOp;
  wire       [23:0]   _zz_roundBack_adderRightOp_1;
  wire       [31:0]   _zz_roundBack_adder_2;
  wire       [31:0]   _zz_roundBack_adder_3;
  wire       [31:0]   _zz_roundBack_adder_4;
  wire       [31:0]   _zz_roundBack_masked;
  wire       [22:0]   _zz_roundBack_masked_1;
  wire       [23:0]   _zz_roundBack_masked_2;
  wire       [8:0]    _zz_roundBack_borringCase;
  wire       [8:0]    _zz_when_FpuCore_l1616;
  wire       [8:0]    _zz_when_FpuCore_l1638;
  wire       [33:0]   _zz_rf_ram_port;
  reg                 _zz_1;
  reg        [25:0]   add_shifter_yMantissa_5;
  reg        [25:0]   add_shifter_yMantissa_4;
  reg        [25:0]   add_shifter_yMantissa_3;
  reg        [25:0]   add_shifter_yMantissa_2;
  reg        [25:0]   add_shifter_yMantissa_1;
  reg        [32:0]   shortPip_fsm_shift_input_6;
  reg        [32:0]   shortPip_fsm_shift_input_5;
  reg        [32:0]   shortPip_fsm_shift_input_4;
  reg        [32:0]   shortPip_fsm_shift_input_3;
  reg        [32:0]   shortPip_fsm_shift_input_2;
  reg        [32:0]   shortPip_fsm_shift_input_1;
  reg        [31:0]   load_s1_fsm_shift_input_5;
  reg        [31:0]   load_s1_fsm_shift_input_4;
  reg        [31:0]   load_s1_fsm_shift_input_3;
  reg        [31:0]   load_s1_fsm_shift_input_2;
  reg        [31:0]   load_s1_fsm_shift_input_1;
  reg                 _zz_2;
  reg                 _zz_3;
  reg                 _zz_4;
  reg        [5:0]    rf_init_counter;
  wire                rf_init_done;
  wire                when_FpuCore_l163;
  reg                 rf_scoreboards_0_targetWrite_valid;
  reg        [4:0]    rf_scoreboards_0_targetWrite_payload_address;
  reg                 rf_scoreboards_0_targetWrite_payload_data;
  reg                 rf_scoreboards_0_hitWrite_valid;
  reg        [4:0]    rf_scoreboards_0_hitWrite_payload_address;
  reg                 rf_scoreboards_0_hitWrite_payload_data;
  wire                commitFork_load_0_valid;
  reg                 commitFork_load_0_ready;
  wire       [3:0]    commitFork_load_0_payload_opcode;
  wire       [4:0]    commitFork_load_0_payload_rd;
  wire                commitFork_load_0_payload_write;
  wire       [31:0]   commitFork_load_0_payload_value;
  wire                commitFork_commit_0_valid;
  wire                commitFork_commit_0_ready;
  wire       [3:0]    commitFork_commit_0_payload_opcode;
  wire       [4:0]    commitFork_commit_0_payload_rd;
  wire                commitFork_commit_0_payload_write;
  wire       [31:0]   commitFork_commit_0_payload_value;
  wire                FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_valid;
  wire                FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_ready;
  wire       [3:0]    FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode;
  wire       [4:0]    FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_rd;
  wire                FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_write;
  wire       [31:0]   FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_value;
  reg                 FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN;
  reg        [3:0]    FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode;
  reg        [4:0]    FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_rd;
  reg                 FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_write;
  reg        [31:0]   FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_value;
  wire       [3:0]    _zz_payload_opcode;
  reg        [3:0]    commitLogic_0_pending_counter;
  wire                commitLogic_0_pending_full;
  wire                commitLogic_0_pending_notEmpty;
  reg                 commitLogic_0_pending_inc;
  reg                 commitLogic_0_pending_dec;
  reg        [3:0]    commitLogic_0_add_counter;
  wire                commitLogic_0_add_full;
  wire                commitLogic_0_add_notEmpty;
  reg                 commitLogic_0_add_inc;
  reg                 commitLogic_0_add_dec;
  reg        [3:0]    commitLogic_0_mul_counter;
  wire                commitLogic_0_mul_full;
  wire                commitLogic_0_mul_notEmpty;
  reg                 commitLogic_0_mul_inc;
  reg                 commitLogic_0_mul_dec;
  reg        [3:0]    commitLogic_0_div_counter;
  wire                commitLogic_0_div_full;
  wire                commitLogic_0_div_notEmpty;
  reg                 commitLogic_0_div_inc;
  reg                 commitLogic_0_div_dec;
  reg        [3:0]    commitLogic_0_sqrt_counter;
  wire                commitLogic_0_sqrt_full;
  wire                commitLogic_0_sqrt_notEmpty;
  reg                 commitLogic_0_sqrt_inc;
  reg                 commitLogic_0_sqrt_dec;
  reg        [3:0]    commitLogic_0_short_counter;
  wire                commitLogic_0_short_full;
  wire                commitLogic_0_short_notEmpty;
  reg                 commitLogic_0_short_inc;
  reg                 commitLogic_0_short_dec;
  wire                _zz_commitFork_commit_0_ready;
  wire       [3:0]    _zz_commitLogic_0_input_payload_opcode;
  wire                commitLogic_0_input_valid;
  wire       [3:0]    commitLogic_0_input_payload_opcode;
  wire       [4:0]    commitLogic_0_input_payload_rd;
  wire                commitLogic_0_input_payload_write;
  wire       [31:0]   commitLogic_0_input_payload_value;
  wire                when_FpuCore_l208;
  wire                when_FpuCore_l209;
  wire                when_FpuCore_l210;
  wire                when_FpuCore_l211;
  wire                when_FpuCore_l212;
  wire                scheduler_0_input_valid;
  wire                scheduler_0_input_ready;
  wire       [3:0]    scheduler_0_input_payload_opcode;
  wire       [1:0]    scheduler_0_input_payload_arg;
  wire       [4:0]    scheduler_0_input_payload_rs1;
  wire       [4:0]    scheduler_0_input_payload_rs2;
  wire       [4:0]    scheduler_0_input_payload_rs3;
  wire       [4:0]    scheduler_0_input_payload_rd;
  wire       [0:0]    scheduler_0_input_payload_format;
  wire       [2:0]    scheduler_0_input_payload_roundMode;
  reg                 io_port_0_cmd_rValidN;
  reg        [3:0]    io_port_0_cmd_rData_opcode;
  reg        [1:0]    io_port_0_cmd_rData_arg;
  reg        [4:0]    io_port_0_cmd_rData_rs1;
  reg        [4:0]    io_port_0_cmd_rData_rs2;
  reg        [4:0]    io_port_0_cmd_rData_rs3;
  reg        [4:0]    io_port_0_cmd_rData_rd;
  reg        [0:0]    io_port_0_cmd_rData_format;
  reg        [2:0]    io_port_0_cmd_rData_roundMode;
  wire       [3:0]    _zz_scheduler_0_input_payload_opcode;
  wire       [0:0]    _zz_scheduler_0_input_payload_format;
  wire       [2:0]    _zz_scheduler_0_input_payload_roundMode;
  reg                 scheduler_0_useRs1;
  reg                 scheduler_0_useRs2;
  reg                 scheduler_0_useRs3;
  reg                 scheduler_0_useRd;
  wire                scheduler_0_rfHits_0;
  wire                scheduler_0_rfHits_1;
  wire                scheduler_0_rfHits_2;
  wire                scheduler_0_rfHits_3;
  wire                scheduler_0_rfTargets_0;
  wire                scheduler_0_rfTargets_1;
  wire                scheduler_0_rfTargets_2;
  wire                scheduler_0_rfTargets_3;
  wire                scheduler_0_rfBusy_0;
  wire                scheduler_0_rfBusy_1;
  wire                scheduler_0_rfBusy_2;
  wire                scheduler_0_rfBusy_3;
  wire                scheduler_0_hits_0;
  wire                scheduler_0_hits_1;
  wire                scheduler_0_hits_2;
  wire                scheduler_0_hits_3;
  wire                scheduler_0_hazard;
  wire                _zz_scheduler_0_input_ready;
  wire                scheduler_0_output_valid;
  wire                scheduler_0_output_ready;
  wire       [3:0]    scheduler_0_output_payload_opcode;
  wire       [1:0]    scheduler_0_output_payload_arg;
  reg        [4:0]    scheduler_0_output_payload_rs1;
  wire       [4:0]    scheduler_0_output_payload_rs2;
  wire       [4:0]    scheduler_0_output_payload_rs3;
  wire       [4:0]    scheduler_0_output_payload_rd;
  wire       [0:0]    scheduler_0_output_payload_format;
  wire       [2:0]    scheduler_0_output_payload_roundMode;
  wire                when_FpuCore_l258;
  wire                when_FpuCore_l261;
  wire                scheduler_0_output_fire;
  wire                when_FpuCore_l265;
  wire       [3:0]    _zz_io_inputs_0_payload_opcode;
  wire       [0:0]    _zz_io_inputs_0_payload_format;
  wire       [2:0]    _zz_io_inputs_0_payload_roundMode;
  wire                cmdArbiter_output_valid;
  wire                cmdArbiter_output_ready;
  wire       [3:0]    cmdArbiter_output_payload_opcode;
  wire       [4:0]    cmdArbiter_output_payload_rs1;
  wire       [4:0]    cmdArbiter_output_payload_rs2;
  wire       [4:0]    cmdArbiter_output_payload_rs3;
  wire       [4:0]    cmdArbiter_output_payload_rd;
  wire       [1:0]    cmdArbiter_output_payload_arg;
  wire       [2:0]    cmdArbiter_output_payload_roundMode;
  wire                read_s0_valid;
  reg                 read_s0_ready;
  wire       [3:0]    read_s0_payload_opcode;
  wire       [4:0]    read_s0_payload_rs1;
  wire       [4:0]    read_s0_payload_rs2;
  wire       [4:0]    read_s0_payload_rs3;
  wire       [4:0]    read_s0_payload_rd;
  wire       [1:0]    read_s0_payload_arg;
  wire       [2:0]    read_s0_payload_roundMode;
  wire                read_s1_valid;
  wire                read_s1_ready;
  wire       [3:0]    read_s1_payload_opcode;
  wire       [4:0]    read_s1_payload_rs1;
  wire       [4:0]    read_s1_payload_rs2;
  wire       [4:0]    read_s1_payload_rs3;
  wire       [4:0]    read_s1_payload_rd;
  wire       [1:0]    read_s1_payload_arg;
  wire       [2:0]    read_s1_payload_roundMode;
  reg                 read_s0_rValid;
  reg        [3:0]    read_s0_rData_opcode;
  reg        [4:0]    read_s0_rData_rs1;
  reg        [4:0]    read_s0_rData_rs2;
  reg        [4:0]    read_s0_rData_rs3;
  reg        [4:0]    read_s0_rData_rd;
  reg        [1:0]    read_s0_rData_arg;
  reg        [2:0]    read_s0_rData_roundMode;
  wire                when_Stream_l375;
  wire                read_output_valid;
  wire                read_output_ready;
  wire       [3:0]    read_output_payload_opcode;
  wire       [22:0]   read_output_payload_rs1_mantissa;
  wire       [8:0]    read_output_payload_rs1_exponent;
  wire                read_output_payload_rs1_sign;
  wire                read_output_payload_rs1_special;
  wire       [22:0]   read_output_payload_rs2_mantissa;
  wire       [8:0]    read_output_payload_rs2_exponent;
  wire                read_output_payload_rs2_sign;
  wire                read_output_payload_rs2_special;
  wire       [22:0]   read_output_payload_rs3_mantissa;
  wire       [8:0]    read_output_payload_rs3_exponent;
  wire                read_output_payload_rs3_sign;
  wire                read_output_payload_rs3_special;
  wire       [4:0]    read_output_payload_rd;
  wire       [1:0]    read_output_payload_arg;
  wire       [2:0]    read_output_payload_roundMode;
  wire       [4:0]    _zz_read_rs_0_value_mantissa;
  wire                read_output_isStall;
  wire                _zz_read_rs_0_value_mantissa_1;
  wire       [22:0]   read_rs_0_value_mantissa;
  wire       [8:0]    read_rs_0_value_exponent;
  wire                read_rs_0_value_sign;
  wire                read_rs_0_value_special;
  wire       [33:0]   _zz_read_rs_0_value_mantissa_2;
  wire       [4:0]    _zz_read_rs_1_value_mantissa;
  wire                _zz_read_rs_1_value_mantissa_1;
  wire       [22:0]   read_rs_1_value_mantissa;
  wire       [8:0]    read_rs_1_value_exponent;
  wire                read_rs_1_value_sign;
  wire                read_rs_1_value_special;
  wire       [33:0]   _zz_read_rs_1_value_mantissa_2;
  wire       [4:0]    _zz_read_rs_2_value_mantissa;
  wire                _zz_read_rs_2_value_mantissa_1;
  wire       [22:0]   read_rs_2_value_mantissa;
  wire       [8:0]    read_rs_2_value_exponent;
  wire                read_rs_2_value_sign;
  wire                read_rs_2_value_special;
  wire       [33:0]   _zz_read_rs_2_value_mantissa_2;
  wire                decode_input_valid;
  reg                 decode_input_ready;
  wire       [3:0]    decode_input_payload_opcode;
  wire       [22:0]   decode_input_payload_rs1_mantissa;
  wire       [8:0]    decode_input_payload_rs1_exponent;
  wire                decode_input_payload_rs1_sign;
  wire                decode_input_payload_rs1_special;
  wire       [22:0]   decode_input_payload_rs2_mantissa;
  wire       [8:0]    decode_input_payload_rs2_exponent;
  wire                decode_input_payload_rs2_sign;
  wire                decode_input_payload_rs2_special;
  wire       [22:0]   decode_input_payload_rs3_mantissa;
  wire       [8:0]    decode_input_payload_rs3_exponent;
  wire                decode_input_payload_rs3_sign;
  wire                decode_input_payload_rs3_special;
  wire       [4:0]    decode_input_payload_rd;
  wire       [1:0]    decode_input_payload_arg;
  wire       [2:0]    decode_input_payload_roundMode;
  wire                decode_loadHit;
  wire                decode_load_valid;
  wire                decode_load_ready;
  wire       [4:0]    decode_load_payload_rd;
  wire                decode_load_payload_i2f;
  wire       [1:0]    decode_load_payload_arg;
  wire       [2:0]    decode_load_payload_roundMode;
  wire                when_FpuCore_l329;
  wire                decode_shortPipHit;
  wire                decode_shortPip_valid;
  reg                 decode_shortPip_ready;
  wire       [3:0]    decode_shortPip_payload_opcode;
  wire       [22:0]   decode_shortPip_payload_rs1_mantissa;
  wire       [8:0]    decode_shortPip_payload_rs1_exponent;
  wire                decode_shortPip_payload_rs1_sign;
  wire                decode_shortPip_payload_rs1_special;
  wire       [22:0]   decode_shortPip_payload_rs2_mantissa;
  wire       [8:0]    decode_shortPip_payload_rs2_exponent;
  wire                decode_shortPip_payload_rs2_sign;
  wire                decode_shortPip_payload_rs2_special;
  wire       [4:0]    decode_shortPip_payload_rd;
  wire       [31:0]   decode_shortPip_payload_value;
  wire       [1:0]    decode_shortPip_payload_arg;
  wire       [2:0]    decode_shortPip_payload_roundMode;
  wire                when_FpuCore_l335;
  wire                decode_divSqrtHit;
  wire                decode_divSqrt_valid;
  wire                decode_divSqrt_ready;
  wire       [22:0]   decode_divSqrt_payload_rs1_mantissa;
  wire       [8:0]    decode_divSqrt_payload_rs1_exponent;
  wire                decode_divSqrt_payload_rs1_sign;
  wire                decode_divSqrt_payload_rs1_special;
  wire       [22:0]   decode_divSqrt_payload_rs2_mantissa;
  wire       [8:0]    decode_divSqrt_payload_rs2_exponent;
  wire                decode_divSqrt_payload_rs2_sign;
  wire                decode_divSqrt_payload_rs2_special;
  wire       [4:0]    decode_divSqrt_payload_rd;
  wire                decode_divSqrt_payload_div;
  wire       [2:0]    decode_divSqrt_payload_roundMode;
  wire                decode_divHit;
  wire                decode_div_valid;
  wire                decode_div_ready;
  wire       [22:0]   decode_div_payload_rs1_mantissa;
  wire       [8:0]    decode_div_payload_rs1_exponent;
  wire                decode_div_payload_rs1_sign;
  wire                decode_div_payload_rs1_special;
  wire       [22:0]   decode_div_payload_rs2_mantissa;
  wire       [8:0]    decode_div_payload_rs2_exponent;
  wire                decode_div_payload_rs2_sign;
  wire                decode_div_payload_rs2_special;
  wire       [4:0]    decode_div_payload_rd;
  wire       [2:0]    decode_div_payload_roundMode;
  wire                when_FpuCore_l351;
  wire                decode_sqrtHit;
  wire                decode_sqrt_valid;
  wire                decode_sqrt_ready;
  wire       [22:0]   decode_sqrt_payload_rs1_mantissa;
  wire       [8:0]    decode_sqrt_payload_rs1_exponent;
  wire                decode_sqrt_payload_rs1_sign;
  wire                decode_sqrt_payload_rs1_special;
  wire       [4:0]    decode_sqrt_payload_rd;
  wire       [2:0]    decode_sqrt_payload_roundMode;
  wire                when_FpuCore_l359;
  wire                decode_fmaHit;
  wire                decode_mulHit;
  wire                decode_mul_valid;
  reg                 decode_mul_ready;
  reg        [22:0]   decode_mul_payload_rs1_mantissa;
  reg        [8:0]    decode_mul_payload_rs1_exponent;
  reg                 decode_mul_payload_rs1_sign;
  reg                 decode_mul_payload_rs1_special;
  reg        [22:0]   decode_mul_payload_rs2_mantissa;
  reg        [8:0]    decode_mul_payload_rs2_exponent;
  reg                 decode_mul_payload_rs2_sign;
  reg                 decode_mul_payload_rs2_special;
  reg        [22:0]   decode_mul_payload_rs3_mantissa;
  reg        [8:0]    decode_mul_payload_rs3_exponent;
  reg                 decode_mul_payload_rs3_sign;
  reg                 decode_mul_payload_rs3_special;
  reg        [4:0]    decode_mul_payload_rd;
  reg                 decode_mul_payload_add;
  reg                 decode_mul_payload_divSqrt;
  reg                 decode_mul_payload_msb1;
  reg                 decode_mul_payload_msb2;
  reg        [2:0]    decode_mul_payload_roundMode;
  wire                decode_divSqrtToMul_valid;
  wire                decode_divSqrtToMul_ready;
  wire       [22:0]   decode_divSqrtToMul_payload_rs1_mantissa;
  wire       [8:0]    decode_divSqrtToMul_payload_rs1_exponent;
  wire                decode_divSqrtToMul_payload_rs1_sign;
  wire                decode_divSqrtToMul_payload_rs1_special;
  wire       [22:0]   decode_divSqrtToMul_payload_rs2_mantissa;
  wire       [8:0]    decode_divSqrtToMul_payload_rs2_exponent;
  wire                decode_divSqrtToMul_payload_rs2_sign;
  wire                decode_divSqrtToMul_payload_rs2_special;
  wire       [22:0]   decode_divSqrtToMul_payload_rs3_mantissa;
  wire       [8:0]    decode_divSqrtToMul_payload_rs3_exponent;
  wire                decode_divSqrtToMul_payload_rs3_sign;
  wire                decode_divSqrtToMul_payload_rs3_special;
  wire       [4:0]    decode_divSqrtToMul_payload_rd;
  wire                decode_divSqrtToMul_payload_add;
  wire                decode_divSqrtToMul_payload_divSqrt;
  wire                decode_divSqrtToMul_payload_msb1;
  wire                decode_divSqrtToMul_payload_msb2;
  wire       [2:0]    decode_divSqrtToMul_payload_roundMode;
  wire                when_FpuCore_l375;
  wire                when_FpuCore_l380;
  wire                decode_addHit;
  wire                decode_add_valid;
  wire                decode_add_ready;
  reg        [24:0]   decode_add_payload_rs1_mantissa;
  reg        [8:0]    decode_add_payload_rs1_exponent;
  reg                 decode_add_payload_rs1_sign;
  reg                 decode_add_payload_rs1_special;
  reg        [24:0]   decode_add_payload_rs2_mantissa;
  reg        [8:0]    decode_add_payload_rs2_exponent;
  reg                 decode_add_payload_rs2_sign;
  reg                 decode_add_payload_rs2_special;
  reg        [4:0]    decode_add_payload_rd;
  reg        [2:0]    decode_add_payload_roundMode;
  reg                 decode_add_payload_needCommit;
  wire                decode_mulToAdd_valid;
  wire                decode_mulToAdd_ready;
  wire       [24:0]   decode_mulToAdd_payload_rs1_mantissa;
  wire       [8:0]    decode_mulToAdd_payload_rs1_exponent;
  wire                decode_mulToAdd_payload_rs1_sign;
  wire                decode_mulToAdd_payload_rs1_special;
  wire       [24:0]   decode_mulToAdd_payload_rs2_mantissa;
  wire       [8:0]    decode_mulToAdd_payload_rs2_exponent;
  wire                decode_mulToAdd_payload_rs2_sign;
  wire                decode_mulToAdd_payload_rs2_special;
  wire       [4:0]    decode_mulToAdd_payload_rd;
  wire       [2:0]    decode_mulToAdd_payload_roundMode;
  wire                decode_mulToAdd_payload_needCommit;
  wire                when_FpuCore_l399;
  wire                when_FpuCore_l404;
  wire                decode_load_s2mPipe_valid;
  reg                 decode_load_s2mPipe_ready;
  wire       [4:0]    decode_load_s2mPipe_payload_rd;
  wire                decode_load_s2mPipe_payload_i2f;
  wire       [1:0]    decode_load_s2mPipe_payload_arg;
  wire       [2:0]    decode_load_s2mPipe_payload_roundMode;
  reg                 decode_load_rValidN;
  reg        [4:0]    decode_load_rData_rd;
  reg                 decode_load_rData_i2f;
  reg        [1:0]    decode_load_rData_arg;
  reg        [2:0]    decode_load_rData_roundMode;
  wire       [2:0]    _zz_decode_load_s2mPipe_payload_roundMode;
  wire                decode_load_s2mPipe_m2sPipe_valid;
  reg                 decode_load_s2mPipe_m2sPipe_ready;
  wire       [4:0]    decode_load_s2mPipe_m2sPipe_payload_rd;
  wire                decode_load_s2mPipe_m2sPipe_payload_i2f;
  wire       [1:0]    decode_load_s2mPipe_m2sPipe_payload_arg;
  wire       [2:0]    decode_load_s2mPipe_m2sPipe_payload_roundMode;
  reg                 decode_load_s2mPipe_rValid;
  reg        [4:0]    decode_load_s2mPipe_rData_rd;
  reg                 decode_load_s2mPipe_rData_i2f;
  reg        [1:0]    decode_load_s2mPipe_rData_arg;
  reg        [2:0]    decode_load_s2mPipe_rData_roundMode;
  wire                when_Stream_l375_1;
  wire                load_s0_input_valid;
  wire                load_s0_input_ready;
  wire       [4:0]    load_s0_input_payload_rd;
  wire                load_s0_input_payload_i2f;
  wire       [1:0]    load_s0_input_payload_arg;
  wire       [2:0]    load_s0_input_payload_roundMode;
  reg                 decode_load_s2mPipe_m2sPipe_rValid;
  reg        [4:0]    decode_load_s2mPipe_m2sPipe_rData_rd;
  reg                 decode_load_s2mPipe_m2sPipe_rData_i2f;
  reg        [1:0]    decode_load_s2mPipe_m2sPipe_rData_arg;
  reg        [2:0]    decode_load_s2mPipe_m2sPipe_rData_roundMode;
  wire                when_Stream_l375_2;
  wire                when_Stream_l445;
  reg                 load_s0_filtred_0_valid;
  reg                 load_s0_filtred_0_ready;
  wire       [3:0]    load_s0_filtred_0_payload_opcode;
  wire       [4:0]    load_s0_filtred_0_payload_rd;
  wire                load_s0_filtred_0_payload_write;
  wire       [31:0]   load_s0_filtred_0_payload_value;
  wire                load_s0_hazard;
  wire                _zz_load_s0_input_ready;
  wire                load_s0_output_valid;
  reg                 load_s0_output_ready;
  wire       [4:0]    load_s0_output_payload_rd;
  wire       [31:0]   load_s0_output_payload_value;
  wire                load_s0_output_payload_i2f;
  wire       [1:0]    load_s0_output_payload_arg;
  wire       [2:0]    load_s0_output_payload_roundMode;
  wire                load_s1_input_valid;
  wire                load_s1_input_ready;
  wire       [4:0]    load_s1_input_payload_rd;
  wire       [31:0]   load_s1_input_payload_value;
  wire                load_s1_input_payload_i2f;
  wire       [1:0]    load_s1_input_payload_arg;
  wire       [2:0]    load_s1_input_payload_roundMode;
  reg                 load_s0_output_rValid;
  reg        [4:0]    load_s0_output_rData_rd;
  reg        [31:0]   load_s0_output_rData_value;
  reg                 load_s0_output_rData_i2f;
  reg        [1:0]    load_s0_output_rData_arg;
  reg        [2:0]    load_s0_output_rData_roundMode;
  wire                when_Stream_l375_3;
  reg                 load_s1_busy;
  wire       [22:0]   load_s1_f32_mantissa;
  wire       [7:0]    load_s1_f32_exponent;
  wire                load_s1_f32_sign;
  wire       [8:0]    load_s1_recodedExpOffset;
  wire       [22:0]   load_s1_passThroughFloat_mantissa;
  wire       [8:0]    load_s1_passThroughFloat_exponent;
  wire                load_s1_passThroughFloat_sign;
  wire                load_s1_passThroughFloat_special;
  wire                load_s1_manZero;
  wire                load_s1_expZero;
  wire                load_s1_expOne;
  wire                load_s1_isZero;
  wire                load_s1_isSubnormal;
  wire                load_s1_isInfinity;
  wire                load_s1_isNan;
  reg                 load_s1_fsm_done;
  reg                 load_s1_fsm_boot;
  reg                 load_s1_fsm_patched;
  reg        [31:0]   load_s1_fsm_ohInput;
  wire                when_FpuCore_l508;
  reg                 load_s1_fsm_i2fZero;
  reg        [4:0]    load_s1_fsm_shift_by;
  reg        [31:0]   load_s1_fsm_shift_input;
  wire                when_FpuCore_l525;
  reg        [31:0]   load_s1_fsm_shift_output;
  wire                when_FpuCore_l529;
  wire                when_FpuCore_l532;
  wire       [31:0]   _zz_load_s0_output_rData_value;
  wire                _zz_load_s0_output_rData_value_1;
  wire       [31:0]   _zz_load_s1_fsm_shift_by;
  wire       [31:0]   _zz_load_s1_fsm_shift_by_1;
  wire                _zz_load_s1_fsm_shift_by_2;
  wire                _zz_load_s1_fsm_shift_by_3;
  wire                _zz_load_s1_fsm_shift_by_4;
  wire                _zz_load_s1_fsm_shift_by_5;
  wire                _zz_load_s1_fsm_shift_by_6;
  wire                _zz_load_s1_fsm_shift_by_7;
  wire                _zz_load_s1_fsm_shift_by_8;
  wire                _zz_load_s1_fsm_shift_by_9;
  wire                _zz_load_s1_fsm_shift_by_10;
  wire                _zz_load_s1_fsm_shift_by_11;
  wire                _zz_load_s1_fsm_shift_by_12;
  wire                _zz_load_s1_fsm_shift_by_13;
  wire                _zz_load_s1_fsm_shift_by_14;
  wire                _zz_load_s1_fsm_shift_by_15;
  wire                _zz_load_s1_fsm_shift_by_16;
  wire                _zz_load_s1_fsm_shift_by_17;
  wire                _zz_load_s1_fsm_shift_by_18;
  wire                _zz_load_s1_fsm_shift_by_19;
  wire                _zz_load_s1_fsm_shift_by_20;
  wire                _zz_load_s1_fsm_shift_by_21;
  wire                _zz_load_s1_fsm_shift_by_22;
  wire                _zz_load_s1_fsm_shift_by_23;
  wire                _zz_load_s1_fsm_shift_by_24;
  wire                _zz_load_s1_fsm_shift_by_25;
  wire                _zz_load_s1_fsm_shift_by_26;
  wire                _zz_load_s1_fsm_shift_by_27;
  wire                _zz_load_s1_fsm_shift_by_28;
  wire                _zz_load_s1_fsm_shift_by_29;
  wire                _zz_load_s1_fsm_shift_by_30;
  wire                _zz_load_s1_fsm_shift_by_31;
  wire                _zz_load_s1_fsm_shift_by_32;
  reg        [8:0]    load_s1_fsm_expOffset;
  wire                load_s1_input_isStall;
  wire                when_FpuCore_l551;
  wire       [23:0]   load_s1_i2fHigh;
  wire       [7:0]    load_s1_i2fLow;
  wire                load_s1_scrap;
  wire       [22:0]   load_s1_recoded_mantissa;
  reg        [8:0]    load_s1_recoded_exponent;
  wire                load_s1_recoded_sign;
  reg                 load_s1_recoded_special;
  wire                _zz_load_s1_input_ready;
  wire                load_s1_output_valid;
  reg                 load_s1_output_ready;
  wire       [4:0]    load_s1_output_payload_rd;
  reg        [23:0]   load_s1_output_payload_value_mantissa;
  reg        [8:0]    load_s1_output_payload_value_exponent;
  reg                 load_s1_output_payload_value_sign;
  reg                 load_s1_output_payload_value_special;
  reg                 load_s1_output_payload_scrap;
  wire       [2:0]    load_s1_output_payload_roundMode;
  wire                load_s1_output_payload_NV;
  wire                load_s1_output_payload_DZ;
  wire                when_FpuCore_l594;
  wire                shortPip_input_valid;
  wire                shortPip_input_ready;
  wire       [3:0]    shortPip_input_payload_opcode;
  wire       [22:0]   shortPip_input_payload_rs1_mantissa;
  wire       [8:0]    shortPip_input_payload_rs1_exponent;
  wire                shortPip_input_payload_rs1_sign;
  wire                shortPip_input_payload_rs1_special;
  wire       [22:0]   shortPip_input_payload_rs2_mantissa;
  wire       [8:0]    shortPip_input_payload_rs2_exponent;
  wire                shortPip_input_payload_rs2_sign;
  wire                shortPip_input_payload_rs2_special;
  wire       [4:0]    shortPip_input_payload_rd;
  wire       [31:0]   shortPip_input_payload_value;
  wire       [1:0]    shortPip_input_payload_arg;
  wire       [2:0]    shortPip_input_payload_roundMode;
  reg                 decode_shortPip_rValid;
  reg        [3:0]    decode_shortPip_rData_opcode;
  reg        [22:0]   decode_shortPip_rData_rs1_mantissa;
  reg        [8:0]    decode_shortPip_rData_rs1_exponent;
  reg                 decode_shortPip_rData_rs1_sign;
  reg                 decode_shortPip_rData_rs1_special;
  reg        [22:0]   decode_shortPip_rData_rs2_mantissa;
  reg        [8:0]    decode_shortPip_rData_rs2_exponent;
  reg                 decode_shortPip_rData_rs2_sign;
  reg                 decode_shortPip_rData_rs2_special;
  reg        [4:0]    decode_shortPip_rData_rd;
  reg        [31:0]   decode_shortPip_rData_value;
  reg        [1:0]    decode_shortPip_rData_arg;
  reg        [2:0]    decode_shortPip_rData_roundMode;
  wire                when_Stream_l375_4;
  wire                shortPip_toFpuRf;
  wire                shortPip_rfOutput_valid;
  wire                shortPip_rfOutput_ready;
  wire       [4:0]    shortPip_rfOutput_payload_rd;
  reg        [23:0]   shortPip_rfOutput_payload_value_mantissa;
  reg        [8:0]    shortPip_rfOutput_payload_value_exponent;
  reg                 shortPip_rfOutput_payload_value_sign;
  reg                 shortPip_rfOutput_payload_value_special;
  wire                shortPip_rfOutput_payload_scrap;
  wire       [2:0]    shortPip_rfOutput_payload_roundMode;
  wire                shortPip_rfOutput_payload_NV;
  wire                shortPip_rfOutput_payload_DZ;
  wire                shortPip_input_fire;
  wire                when_FpuCore_l221;
  wire                shortPip_isCommited;
  wire                _zz_shortPip_rfOutput_ready;
  wire                shortPip_output_valid;
  reg                 shortPip_output_ready;
  wire       [4:0]    shortPip_output_payload_rd;
  wire       [23:0]   shortPip_output_payload_value_mantissa;
  wire       [8:0]    shortPip_output_payload_value_exponent;
  wire                shortPip_output_payload_value_sign;
  wire                shortPip_output_payload_value_special;
  wire                shortPip_output_payload_scrap;
  wire       [2:0]    shortPip_output_payload_roundMode;
  wire                shortPip_output_payload_NV;
  wire                shortPip_output_payload_DZ;
  reg        [31:0]   shortPip_result;
  reg                 shortPip_halt;
  reg        [31:0]   shortPip_recodedResult;
  wire       [7:0]    shortPip_f32_exp;
  wire       [22:0]   shortPip_f32_man;
  wire       [7:0]    shortPip_expSubnormalThreshold;
  wire                shortPip_expInSubnormalRange;
  wire                shortPip_isSubnormal;
  wire                shortPip_isNormal;
  wire       [8:0]    shortPip_fsm_f2iShift;
  wire                shortPip_fsm_isF2i;
  wire                shortPip_fsm_needRecoding;
  reg                 shortPip_fsm_done;
  reg                 shortPip_fsm_boot;
  wire                shortPip_fsm_isZero;
  reg        [5:0]    shortPip_fsm_shift_by;
  reg        [32:0]   shortPip_fsm_shift_input;
  reg                 shortPip_fsm_shift_scrap;
  wire                when_FpuCore_l646;
  wire                when_FpuCore_l646_1;
  wire                when_FpuCore_l646_2;
  wire                when_FpuCore_l646_3;
  wire                when_FpuCore_l646_4;
  wire                when_FpuCore_l646_5;
  wire                when_FpuCore_l652;
  reg        [32:0]   shortPip_fsm_shift_output;
  wire       [7:0]    shortPip_fsm_formatShiftOffset;
  wire                when_FpuCore_l658;
  wire       [8:0]    _zz_shortPip_fsm_shift_by;
  wire       [5:0]    _zz_shortPip_fsm_shift_by_1;
  wire                shortPip_input_isStall;
  wire                when_FpuCore_l672;
  reg                 shortPip_mantissaForced;
  reg                 shortPip_exponentForced;
  reg                 shortPip_mantissaForcedValue;
  reg                 shortPip_exponentForcedValue;
  reg                 shortPip_cononicalForced;
  wire       [1:0]    switch_FpuCore_l686;
  wire                when_FpuCore_l702;
  reg                 shortPip_rspNv;
  reg                 shortPip_rspNx;
  wire       [31:0]   shortPip_f2i_unsigned;
  wire                shortPip_f2i_resign;
  wire       [1:0]    shortPip_f2i_round;
  reg                 shortPip_f2i_increment;
  reg        [31:0]   shortPip_f2i_result;
  wire                shortPip_f2i_overflow;
  wire                shortPip_f2i_underflow;
  wire                shortPip_f2i_isZero;
  wire                when_FpuCore_l767;
  wire                shortPip_bothZero;
  reg                 shortPip_rs1Equal;
  reg                 shortPip_rs1AbsSmaller;
  wire                when_FpuCore_l780;
  wire                when_FpuCore_l781;
  wire                when_FpuCore_l782;
  wire                when_FpuCore_l783;
  wire                when_FpuCore_l784;
  wire       [1:0]    switch_Misc_l241;
  reg                 shortPip_rs1Smaller;
  wire                shortPip_minMaxSelectRs2;
  wire                shortPip_minMaxSelectNanQuiet;
  reg        [0:0]    shortPip_cmpResult;
  wire                when_FpuCore_l796;
  wire                shortPip_sgnjRs1Sign;
  wire                shortPip_sgnjRs2Sign;
  wire                shortPip_sgnjResult;
  reg        [31:0]   shortPip_fclassResult;
  wire                shortPip_decoded_isNan;
  wire                shortPip_decoded_isNormal;
  wire                shortPip_decoded_isSubnormal;
  wire                shortPip_decoded_isZero;
  wire                shortPip_decoded_isInfinity;
  wire                shortPip_decoded_isQuiet;
  wire                when_FpuCore_l850;
  wire                shortPip_signalQuiet;
  wire                shortPip_rs1Nan;
  wire                shortPip_rs2Nan;
  wire                shortPip_rs1NanNv;
  wire                shortPip_rs2NanNv;
  wire                shortPip_NV;
  wire                shortPip_rspStreams_0_valid;
  reg                 shortPip_rspStreams_0_ready;
  wire       [31:0]   shortPip_rspStreams_0_payload_value;
  wire                shortPip_rspStreams_0_payload_NV;
  wire                shortPip_rspStreams_0_payload_NX;
  wire                shortPip_rspStreams_0_m2sPipe_valid;
  wire                shortPip_rspStreams_0_m2sPipe_ready;
  wire       [31:0]   shortPip_rspStreams_0_m2sPipe_payload_value;
  wire                shortPip_rspStreams_0_m2sPipe_payload_NV;
  wire                shortPip_rspStreams_0_m2sPipe_payload_NX;
  reg                 shortPip_rspStreams_0_rValid;
  reg        [31:0]   shortPip_rspStreams_0_rData_value;
  reg                 shortPip_rspStreams_0_rData_NV;
  reg                 shortPip_rspStreams_0_rData_NX;
  wire                when_Stream_l375_5;
  wire                mul_preMul_input_valid;
  wire                mul_preMul_input_ready;
  wire       [22:0]   mul_preMul_input_payload_rs1_mantissa;
  wire       [8:0]    mul_preMul_input_payload_rs1_exponent;
  wire                mul_preMul_input_payload_rs1_sign;
  wire                mul_preMul_input_payload_rs1_special;
  wire       [22:0]   mul_preMul_input_payload_rs2_mantissa;
  wire       [8:0]    mul_preMul_input_payload_rs2_exponent;
  wire                mul_preMul_input_payload_rs2_sign;
  wire                mul_preMul_input_payload_rs2_special;
  wire       [22:0]   mul_preMul_input_payload_rs3_mantissa;
  wire       [8:0]    mul_preMul_input_payload_rs3_exponent;
  wire                mul_preMul_input_payload_rs3_sign;
  wire                mul_preMul_input_payload_rs3_special;
  wire       [4:0]    mul_preMul_input_payload_rd;
  wire                mul_preMul_input_payload_add;
  wire                mul_preMul_input_payload_divSqrt;
  wire                mul_preMul_input_payload_msb1;
  wire                mul_preMul_input_payload_msb2;
  wire       [2:0]    mul_preMul_input_payload_roundMode;
  reg                 decode_mul_rValid;
  reg        [22:0]   decode_mul_rData_rs1_mantissa;
  reg        [8:0]    decode_mul_rData_rs1_exponent;
  reg                 decode_mul_rData_rs1_sign;
  reg                 decode_mul_rData_rs1_special;
  reg        [22:0]   decode_mul_rData_rs2_mantissa;
  reg        [8:0]    decode_mul_rData_rs2_exponent;
  reg                 decode_mul_rData_rs2_sign;
  reg                 decode_mul_rData_rs2_special;
  reg        [22:0]   decode_mul_rData_rs3_mantissa;
  reg        [8:0]    decode_mul_rData_rs3_exponent;
  reg                 decode_mul_rData_rs3_sign;
  reg                 decode_mul_rData_rs3_special;
  reg        [4:0]    decode_mul_rData_rd;
  reg                 decode_mul_rData_add;
  reg                 decode_mul_rData_divSqrt;
  reg                 decode_mul_rData_msb1;
  reg                 decode_mul_rData_msb2;
  reg        [2:0]    decode_mul_rData_roundMode;
  wire                when_Stream_l375_6;
  wire                mul_preMul_output_valid;
  reg                 mul_preMul_output_ready;
  wire       [22:0]   mul_preMul_output_payload_rs1_mantissa;
  wire       [8:0]    mul_preMul_output_payload_rs1_exponent;
  wire                mul_preMul_output_payload_rs1_sign;
  wire                mul_preMul_output_payload_rs1_special;
  wire       [22:0]   mul_preMul_output_payload_rs2_mantissa;
  wire       [8:0]    mul_preMul_output_payload_rs2_exponent;
  wire                mul_preMul_output_payload_rs2_sign;
  wire                mul_preMul_output_payload_rs2_special;
  wire       [22:0]   mul_preMul_output_payload_rs3_mantissa;
  wire       [8:0]    mul_preMul_output_payload_rs3_exponent;
  wire                mul_preMul_output_payload_rs3_sign;
  wire                mul_preMul_output_payload_rs3_special;
  wire       [4:0]    mul_preMul_output_payload_rd;
  wire                mul_preMul_output_payload_add;
  wire                mul_preMul_output_payload_divSqrt;
  wire                mul_preMul_output_payload_msb1;
  wire                mul_preMul_output_payload_msb2;
  wire       [2:0]    mul_preMul_output_payload_roundMode;
  wire       [9:0]    mul_preMul_output_payload_exp;
  wire                mul_mul_input_valid;
  wire                mul_mul_input_ready;
  wire       [22:0]   mul_mul_input_payload_rs1_mantissa;
  wire       [8:0]    mul_mul_input_payload_rs1_exponent;
  wire                mul_mul_input_payload_rs1_sign;
  wire                mul_mul_input_payload_rs1_special;
  wire       [22:0]   mul_mul_input_payload_rs2_mantissa;
  wire       [8:0]    mul_mul_input_payload_rs2_exponent;
  wire                mul_mul_input_payload_rs2_sign;
  wire                mul_mul_input_payload_rs2_special;
  wire       [22:0]   mul_mul_input_payload_rs3_mantissa;
  wire       [8:0]    mul_mul_input_payload_rs3_exponent;
  wire                mul_mul_input_payload_rs3_sign;
  wire                mul_mul_input_payload_rs3_special;
  wire       [4:0]    mul_mul_input_payload_rd;
  wire                mul_mul_input_payload_add;
  wire                mul_mul_input_payload_divSqrt;
  wire                mul_mul_input_payload_msb1;
  wire                mul_mul_input_payload_msb2;
  wire       [2:0]    mul_mul_input_payload_roundMode;
  wire       [9:0]    mul_mul_input_payload_exp;
  reg                 mul_preMul_output_rValid;
  reg        [22:0]   mul_preMul_output_rData_rs1_mantissa;
  reg        [8:0]    mul_preMul_output_rData_rs1_exponent;
  reg                 mul_preMul_output_rData_rs1_sign;
  reg                 mul_preMul_output_rData_rs1_special;
  reg        [22:0]   mul_preMul_output_rData_rs2_mantissa;
  reg        [8:0]    mul_preMul_output_rData_rs2_exponent;
  reg                 mul_preMul_output_rData_rs2_sign;
  reg                 mul_preMul_output_rData_rs2_special;
  reg        [22:0]   mul_preMul_output_rData_rs3_mantissa;
  reg        [8:0]    mul_preMul_output_rData_rs3_exponent;
  reg                 mul_preMul_output_rData_rs3_sign;
  reg                 mul_preMul_output_rData_rs3_special;
  reg        [4:0]    mul_preMul_output_rData_rd;
  reg                 mul_preMul_output_rData_add;
  reg                 mul_preMul_output_rData_divSqrt;
  reg                 mul_preMul_output_rData_msb1;
  reg                 mul_preMul_output_rData_msb2;
  reg        [2:0]    mul_preMul_output_rData_roundMode;
  reg        [9:0]    mul_preMul_output_rData_exp;
  wire                when_Stream_l375_7;
  wire                mul_mul_output_valid;
  reg                 mul_mul_output_ready;
  wire       [22:0]   mul_mul_output_payload_rs1_mantissa;
  wire       [8:0]    mul_mul_output_payload_rs1_exponent;
  wire                mul_mul_output_payload_rs1_sign;
  wire                mul_mul_output_payload_rs1_special;
  wire       [22:0]   mul_mul_output_payload_rs2_mantissa;
  wire       [8:0]    mul_mul_output_payload_rs2_exponent;
  wire                mul_mul_output_payload_rs2_sign;
  wire                mul_mul_output_payload_rs2_special;
  wire       [22:0]   mul_mul_output_payload_rs3_mantissa;
  wire       [8:0]    mul_mul_output_payload_rs3_exponent;
  wire                mul_mul_output_payload_rs3_sign;
  wire                mul_mul_output_payload_rs3_special;
  wire       [4:0]    mul_mul_output_payload_rd;
  wire                mul_mul_output_payload_add;
  wire                mul_mul_output_payload_divSqrt;
  wire                mul_mul_output_payload_msb1;
  wire                mul_mul_output_payload_msb2;
  wire       [2:0]    mul_mul_output_payload_roundMode;
  wire       [9:0]    mul_mul_output_payload_exp;
  wire       [35:0]   mul_mul_output_payload_muls_0;
  wire       [23:0]   mul_mul_output_payload_muls_1;
  wire       [23:0]   mul_mul_output_payload_muls_2;
  wire       [11:0]   mul_mul_output_payload_muls_3;
  wire       [23:0]   mul_mul_mulA;
  wire       [23:0]   mul_mul_mulB;
  wire                mul_sum1_input_valid;
  wire                mul_sum1_input_ready;
  wire       [22:0]   mul_sum1_input_payload_rs1_mantissa;
  wire       [8:0]    mul_sum1_input_payload_rs1_exponent;
  wire                mul_sum1_input_payload_rs1_sign;
  wire                mul_sum1_input_payload_rs1_special;
  wire       [22:0]   mul_sum1_input_payload_rs2_mantissa;
  wire       [8:0]    mul_sum1_input_payload_rs2_exponent;
  wire                mul_sum1_input_payload_rs2_sign;
  wire                mul_sum1_input_payload_rs2_special;
  wire       [22:0]   mul_sum1_input_payload_rs3_mantissa;
  wire       [8:0]    mul_sum1_input_payload_rs3_exponent;
  wire                mul_sum1_input_payload_rs3_sign;
  wire                mul_sum1_input_payload_rs3_special;
  wire       [4:0]    mul_sum1_input_payload_rd;
  wire                mul_sum1_input_payload_add;
  wire                mul_sum1_input_payload_divSqrt;
  wire                mul_sum1_input_payload_msb1;
  wire                mul_sum1_input_payload_msb2;
  wire       [2:0]    mul_sum1_input_payload_roundMode;
  wire       [9:0]    mul_sum1_input_payload_exp;
  wire       [35:0]   mul_sum1_input_payload_muls_0;
  wire       [23:0]   mul_sum1_input_payload_muls_1;
  wire       [23:0]   mul_sum1_input_payload_muls_2;
  wire       [11:0]   mul_sum1_input_payload_muls_3;
  reg                 mul_mul_output_rValid;
  reg        [22:0]   mul_mul_output_rData_rs1_mantissa;
  reg        [8:0]    mul_mul_output_rData_rs1_exponent;
  reg                 mul_mul_output_rData_rs1_sign;
  reg                 mul_mul_output_rData_rs1_special;
  reg        [22:0]   mul_mul_output_rData_rs2_mantissa;
  reg        [8:0]    mul_mul_output_rData_rs2_exponent;
  reg                 mul_mul_output_rData_rs2_sign;
  reg                 mul_mul_output_rData_rs2_special;
  reg        [22:0]   mul_mul_output_rData_rs3_mantissa;
  reg        [8:0]    mul_mul_output_rData_rs3_exponent;
  reg                 mul_mul_output_rData_rs3_sign;
  reg                 mul_mul_output_rData_rs3_special;
  reg        [4:0]    mul_mul_output_rData_rd;
  reg                 mul_mul_output_rData_add;
  reg                 mul_mul_output_rData_divSqrt;
  reg                 mul_mul_output_rData_msb1;
  reg                 mul_mul_output_rData_msb2;
  reg        [2:0]    mul_mul_output_rData_roundMode;
  reg        [9:0]    mul_mul_output_rData_exp;
  reg        [35:0]   mul_mul_output_rData_muls_0;
  reg        [23:0]   mul_mul_output_rData_muls_1;
  reg        [23:0]   mul_mul_output_rData_muls_2;
  reg        [11:0]   mul_mul_output_rData_muls_3;
  wire                when_Stream_l375_8;
  wire       [47:0]   mul_sum1_sum;
  wire                mul_sum1_output_valid;
  reg                 mul_sum1_output_ready;
  wire       [22:0]   mul_sum1_output_payload_rs1_mantissa;
  wire       [8:0]    mul_sum1_output_payload_rs1_exponent;
  wire                mul_sum1_output_payload_rs1_sign;
  wire                mul_sum1_output_payload_rs1_special;
  wire       [22:0]   mul_sum1_output_payload_rs2_mantissa;
  wire       [8:0]    mul_sum1_output_payload_rs2_exponent;
  wire                mul_sum1_output_payload_rs2_sign;
  wire                mul_sum1_output_payload_rs2_special;
  wire       [22:0]   mul_sum1_output_payload_rs3_mantissa;
  wire       [8:0]    mul_sum1_output_payload_rs3_exponent;
  wire                mul_sum1_output_payload_rs3_sign;
  wire                mul_sum1_output_payload_rs3_special;
  wire       [4:0]    mul_sum1_output_payload_rd;
  wire                mul_sum1_output_payload_add;
  wire                mul_sum1_output_payload_divSqrt;
  wire                mul_sum1_output_payload_msb1;
  wire                mul_sum1_output_payload_msb2;
  wire       [2:0]    mul_sum1_output_payload_roundMode;
  wire       [9:0]    mul_sum1_output_payload_exp;
  wire       [23:0]   mul_sum1_output_payload_muls2_0;
  wire       [11:0]   mul_sum1_output_payload_muls2_1;
  wire       [47:0]   mul_sum1_output_payload_mulC2;
  wire                mul_sum2_input_valid;
  wire                mul_sum2_input_ready;
  wire       [22:0]   mul_sum2_input_payload_rs1_mantissa;
  wire       [8:0]    mul_sum2_input_payload_rs1_exponent;
  wire                mul_sum2_input_payload_rs1_sign;
  wire                mul_sum2_input_payload_rs1_special;
  wire       [22:0]   mul_sum2_input_payload_rs2_mantissa;
  wire       [8:0]    mul_sum2_input_payload_rs2_exponent;
  wire                mul_sum2_input_payload_rs2_sign;
  wire                mul_sum2_input_payload_rs2_special;
  wire       [22:0]   mul_sum2_input_payload_rs3_mantissa;
  wire       [8:0]    mul_sum2_input_payload_rs3_exponent;
  wire                mul_sum2_input_payload_rs3_sign;
  wire                mul_sum2_input_payload_rs3_special;
  wire       [4:0]    mul_sum2_input_payload_rd;
  wire                mul_sum2_input_payload_add;
  wire                mul_sum2_input_payload_divSqrt;
  wire                mul_sum2_input_payload_msb1;
  wire                mul_sum2_input_payload_msb2;
  wire       [2:0]    mul_sum2_input_payload_roundMode;
  wire       [9:0]    mul_sum2_input_payload_exp;
  wire       [23:0]   mul_sum2_input_payload_muls2_0;
  wire       [11:0]   mul_sum2_input_payload_muls2_1;
  wire       [47:0]   mul_sum2_input_payload_mulC2;
  reg                 mul_sum1_output_rValid;
  reg        [22:0]   mul_sum1_output_rData_rs1_mantissa;
  reg        [8:0]    mul_sum1_output_rData_rs1_exponent;
  reg                 mul_sum1_output_rData_rs1_sign;
  reg                 mul_sum1_output_rData_rs1_special;
  reg        [22:0]   mul_sum1_output_rData_rs2_mantissa;
  reg        [8:0]    mul_sum1_output_rData_rs2_exponent;
  reg                 mul_sum1_output_rData_rs2_sign;
  reg                 mul_sum1_output_rData_rs2_special;
  reg        [22:0]   mul_sum1_output_rData_rs3_mantissa;
  reg        [8:0]    mul_sum1_output_rData_rs3_exponent;
  reg                 mul_sum1_output_rData_rs3_sign;
  reg                 mul_sum1_output_rData_rs3_special;
  reg        [4:0]    mul_sum1_output_rData_rd;
  reg                 mul_sum1_output_rData_add;
  reg                 mul_sum1_output_rData_divSqrt;
  reg                 mul_sum1_output_rData_msb1;
  reg                 mul_sum1_output_rData_msb2;
  reg        [2:0]    mul_sum1_output_rData_roundMode;
  reg        [9:0]    mul_sum1_output_rData_exp;
  reg        [23:0]   mul_sum1_output_rData_muls2_0;
  reg        [11:0]   mul_sum1_output_rData_muls2_1;
  reg        [47:0]   mul_sum1_output_rData_mulC2;
  wire                when_Stream_l375_9;
  wire       [47:0]   mul_sum2_sum;
  wire                mul_sum2_input_fire;
  wire                when_FpuCore_l221_1;
  wire                mul_sum2_isCommited;
  wire                _zz_mul_sum2_input_ready;
  wire                mul_sum2_output_valid;
  reg                 mul_sum2_output_ready;
  wire       [22:0]   mul_sum2_output_payload_rs1_mantissa;
  wire       [8:0]    mul_sum2_output_payload_rs1_exponent;
  wire                mul_sum2_output_payload_rs1_sign;
  wire                mul_sum2_output_payload_rs1_special;
  wire       [22:0]   mul_sum2_output_payload_rs2_mantissa;
  wire       [8:0]    mul_sum2_output_payload_rs2_exponent;
  wire                mul_sum2_output_payload_rs2_sign;
  wire                mul_sum2_output_payload_rs2_special;
  wire       [22:0]   mul_sum2_output_payload_rs3_mantissa;
  wire       [8:0]    mul_sum2_output_payload_rs3_exponent;
  wire                mul_sum2_output_payload_rs3_sign;
  wire                mul_sum2_output_payload_rs3_special;
  wire       [4:0]    mul_sum2_output_payload_rd;
  wire                mul_sum2_output_payload_add;
  wire                mul_sum2_output_payload_divSqrt;
  wire                mul_sum2_output_payload_msb1;
  wire                mul_sum2_output_payload_msb2;
  wire       [2:0]    mul_sum2_output_payload_roundMode;
  wire       [9:0]    mul_sum2_output_payload_exp;
  wire       [47:0]   mul_sum2_output_payload_mulC;
  wire                mul_norm_input_valid;
  wire                mul_norm_input_ready;
  wire       [22:0]   mul_norm_input_payload_rs1_mantissa;
  wire       [8:0]    mul_norm_input_payload_rs1_exponent;
  wire                mul_norm_input_payload_rs1_sign;
  wire                mul_norm_input_payload_rs1_special;
  wire       [22:0]   mul_norm_input_payload_rs2_mantissa;
  wire       [8:0]    mul_norm_input_payload_rs2_exponent;
  wire                mul_norm_input_payload_rs2_sign;
  wire                mul_norm_input_payload_rs2_special;
  wire       [22:0]   mul_norm_input_payload_rs3_mantissa;
  wire       [8:0]    mul_norm_input_payload_rs3_exponent;
  wire                mul_norm_input_payload_rs3_sign;
  wire                mul_norm_input_payload_rs3_special;
  wire       [4:0]    mul_norm_input_payload_rd;
  wire                mul_norm_input_payload_add;
  wire                mul_norm_input_payload_divSqrt;
  wire                mul_norm_input_payload_msb1;
  wire                mul_norm_input_payload_msb2;
  wire       [2:0]    mul_norm_input_payload_roundMode;
  wire       [9:0]    mul_norm_input_payload_exp;
  wire       [47:0]   mul_norm_input_payload_mulC;
  reg                 mul_sum2_output_rValid;
  reg        [22:0]   mul_sum2_output_rData_rs1_mantissa;
  reg        [8:0]    mul_sum2_output_rData_rs1_exponent;
  reg                 mul_sum2_output_rData_rs1_sign;
  reg                 mul_sum2_output_rData_rs1_special;
  reg        [22:0]   mul_sum2_output_rData_rs2_mantissa;
  reg        [8:0]    mul_sum2_output_rData_rs2_exponent;
  reg                 mul_sum2_output_rData_rs2_sign;
  reg                 mul_sum2_output_rData_rs2_special;
  reg        [22:0]   mul_sum2_output_rData_rs3_mantissa;
  reg        [8:0]    mul_sum2_output_rData_rs3_exponent;
  reg                 mul_sum2_output_rData_rs3_sign;
  reg                 mul_sum2_output_rData_rs3_special;
  reg        [4:0]    mul_sum2_output_rData_rd;
  reg                 mul_sum2_output_rData_add;
  reg                 mul_sum2_output_rData_divSqrt;
  reg                 mul_sum2_output_rData_msb1;
  reg                 mul_sum2_output_rData_msb2;
  reg        [2:0]    mul_sum2_output_rData_roundMode;
  reg        [9:0]    mul_sum2_output_rData_exp;
  reg        [47:0]   mul_sum2_output_rData_mulC;
  wire                when_Stream_l375_10;
  wire       [25:0]   mul_norm_mulHigh;
  wire       [21:0]   mul_norm_mulLow;
  reg                 mul_norm_scrap;
  wire                mul_norm_needShift;
  wire       [9:0]    mul_norm_exp;
  wire       [23:0]   mul_norm_man;
  wire                when_FpuCore_l967;
  wire                mul_norm_forceZero;
  wire       [8:0]    mul_norm_underflowThreshold;
  wire       [6:0]    mul_norm_underflowExp;
  wire                mul_norm_forceUnderflow;
  wire                mul_norm_forceOverflow;
  wire                mul_norm_infinitynan;
  wire                mul_norm_forceNan;
  reg        [23:0]   mul_norm_output_mantissa;
  reg        [8:0]    mul_norm_output_exponent;
  wire                mul_norm_output_sign;
  reg                 mul_norm_output_special;
  reg                 mul_norm_NV;
  wire                when_FpuCore_l983;
  wire                when_FpuCore_l987;
  wire                mul_result_notMul_output_valid;
  wire       [23:0]   mul_result_notMul_output_payload;
  wire                mul_result_output_valid;
  wire                mul_result_output_ready;
  wire       [4:0]    mul_result_output_payload_rd;
  wire       [23:0]   mul_result_output_payload_value_mantissa;
  wire       [8:0]    mul_result_output_payload_value_exponent;
  wire                mul_result_output_payload_value_sign;
  wire                mul_result_output_payload_value_special;
  wire                mul_result_output_payload_scrap;
  wire       [2:0]    mul_result_output_payload_roundMode;
  wire                mul_result_output_payload_NV;
  wire                mul_result_output_payload_DZ;
  wire                mul_result_mulToAdd_valid;
  reg                 mul_result_mulToAdd_ready;
  reg        [24:0]   mul_result_mulToAdd_payload_rs1_mantissa;
  wire       [8:0]    mul_result_mulToAdd_payload_rs1_exponent;
  wire                mul_result_mulToAdd_payload_rs1_sign;
  wire                mul_result_mulToAdd_payload_rs1_special;
  wire       [24:0]   mul_result_mulToAdd_payload_rs2_mantissa;
  wire       [8:0]    mul_result_mulToAdd_payload_rs2_exponent;
  wire                mul_result_mulToAdd_payload_rs2_sign;
  wire                mul_result_mulToAdd_payload_rs2_special;
  wire       [4:0]    mul_result_mulToAdd_payload_rd;
  wire       [2:0]    mul_result_mulToAdd_payload_roundMode;
  wire                mul_result_mulToAdd_payload_needCommit;
  wire                mul_result_mulToAdd_m2sPipe_valid;
  wire                mul_result_mulToAdd_m2sPipe_ready;
  wire       [24:0]   mul_result_mulToAdd_m2sPipe_payload_rs1_mantissa;
  wire       [8:0]    mul_result_mulToAdd_m2sPipe_payload_rs1_exponent;
  wire                mul_result_mulToAdd_m2sPipe_payload_rs1_sign;
  wire                mul_result_mulToAdd_m2sPipe_payload_rs1_special;
  wire       [24:0]   mul_result_mulToAdd_m2sPipe_payload_rs2_mantissa;
  wire       [8:0]    mul_result_mulToAdd_m2sPipe_payload_rs2_exponent;
  wire                mul_result_mulToAdd_m2sPipe_payload_rs2_sign;
  wire                mul_result_mulToAdd_m2sPipe_payload_rs2_special;
  wire       [4:0]    mul_result_mulToAdd_m2sPipe_payload_rd;
  wire       [2:0]    mul_result_mulToAdd_m2sPipe_payload_roundMode;
  wire                mul_result_mulToAdd_m2sPipe_payload_needCommit;
  reg                 mul_result_mulToAdd_rValid;
  reg        [24:0]   mul_result_mulToAdd_rData_rs1_mantissa;
  reg        [8:0]    mul_result_mulToAdd_rData_rs1_exponent;
  reg                 mul_result_mulToAdd_rData_rs1_sign;
  reg                 mul_result_mulToAdd_rData_rs1_special;
  reg        [24:0]   mul_result_mulToAdd_rData_rs2_mantissa;
  reg        [8:0]    mul_result_mulToAdd_rData_rs2_exponent;
  reg                 mul_result_mulToAdd_rData_rs2_sign;
  reg                 mul_result_mulToAdd_rData_rs2_special;
  reg        [4:0]    mul_result_mulToAdd_rData_rd;
  reg        [2:0]    mul_result_mulToAdd_rData_roundMode;
  reg                 mul_result_mulToAdd_rData_needCommit;
  wire                when_Stream_l375_11;
  wire                div_input_valid;
  wire                div_input_ready;
  wire       [22:0]   div_input_payload_rs1_mantissa;
  wire       [8:0]    div_input_payload_rs1_exponent;
  wire                div_input_payload_rs1_sign;
  wire                div_input_payload_rs1_special;
  wire       [22:0]   div_input_payload_rs2_mantissa;
  wire       [8:0]    div_input_payload_rs2_exponent;
  wire                div_input_payload_rs2_sign;
  wire                div_input_payload_rs2_special;
  wire       [4:0]    div_input_payload_rd;
  wire       [2:0]    div_input_payload_roundMode;
  reg                 decode_div_rValid;
  wire                div_input_fire;
  reg        [22:0]   decode_div_rData_rs1_mantissa;
  reg        [8:0]    decode_div_rData_rs1_exponent;
  reg                 decode_div_rData_rs1_sign;
  reg                 decode_div_rData_rs1_special;
  reg        [22:0]   decode_div_rData_rs2_mantissa;
  reg        [8:0]    decode_div_rData_rs2_exponent;
  reg                 decode_div_rData_rs2_sign;
  reg                 decode_div_rData_rs2_special;
  reg        [4:0]    decode_div_rData_rd;
  reg        [2:0]    decode_div_rData_roundMode;
  reg                 div_haltIt;
  wire                when_FpuCore_l221_2;
  reg                 div_isCommited;
  wire                _zz_div_input_ready;
  wire                div_output_valid;
  wire                div_output_ready;
  wire       [4:0]    div_output_payload_rd;
  reg        [23:0]   div_output_payload_value_mantissa;
  reg        [8:0]    div_output_payload_value_exponent;
  wire                div_output_payload_value_sign;
  reg                 div_output_payload_value_special;
  wire                div_output_payload_scrap;
  wire       [2:0]    div_output_payload_roundMode;
  reg                 div_output_payload_NV;
  wire                div_output_payload_DZ;
  wire       [25:0]   div_dividerResult;
  wire                div_dividerScrap;
  reg                 div_cmdSent;
  wire                FpuPlugin_fpu_div_divider_io_input_fire;
  wire                when_FpuCore_l1056;
  wire                div_needShift;
  wire       [23:0]   div_mantissa;
  wire                div_scrap;
  wire       [10:0]   div_exponent;
  wire                when_FpuCore_l1072;
  wire       [10:0]   div_underflowThreshold;
  wire       [10:0]   div_underflowExp;
  wire                div_forceUnderflow;
  wire                div_forceOverflow;
  wire                div_infinitynan;
  wire                div_forceNan;
  wire                div_forceZero;
  wire                when_FpuCore_l1089;
  wire                when_FpuCore_l1093;
  wire                sqrt_input_valid;
  wire                sqrt_input_ready;
  wire       [22:0]   sqrt_input_payload_rs1_mantissa;
  wire       [8:0]    sqrt_input_payload_rs1_exponent;
  wire                sqrt_input_payload_rs1_sign;
  wire                sqrt_input_payload_rs1_special;
  wire       [4:0]    sqrt_input_payload_rd;
  wire       [2:0]    sqrt_input_payload_roundMode;
  reg                 decode_sqrt_rValid;
  wire                sqrt_input_fire;
  reg        [22:0]   decode_sqrt_rData_rs1_mantissa;
  reg        [8:0]    decode_sqrt_rData_rs1_exponent;
  reg                 decode_sqrt_rData_rs1_sign;
  reg                 decode_sqrt_rData_rs1_special;
  reg        [4:0]    decode_sqrt_rData_rd;
  reg        [2:0]    decode_sqrt_rData_roundMode;
  reg                 sqrt_haltIt;
  wire                when_FpuCore_l221_3;
  reg                 sqrt_isCommited;
  wire                _zz_sqrt_input_ready;
  wire                sqrt_output_valid;
  wire                sqrt_output_ready;
  wire       [4:0]    sqrt_output_payload_rd;
  reg        [23:0]   sqrt_output_payload_value_mantissa;
  reg        [8:0]    sqrt_output_payload_value_exponent;
  wire                sqrt_output_payload_value_sign;
  reg                 sqrt_output_payload_value_special;
  wire                sqrt_output_payload_scrap;
  wire       [2:0]    sqrt_output_payload_roundMode;
  reg                 sqrt_output_payload_NV;
  wire                sqrt_output_payload_DZ;
  wire                sqrt_needShift;
  reg                 sqrt_cmdSent;
  wire                FpuPlugin_fpu_sqrt_sqrt_io_input_fire;
  wire                when_FpuCore_l1118;
  wire                sqrt_scrap;
  reg        [8:0]    sqrt_exponent;
  wire                sqrt_negative;
  wire                when_FpuCore_l1137;
  wire                when_FpuCore_l1144;
  wire                when_FpuCore_l1148;
  wire                add_preShifter_input_valid;
  wire                add_preShifter_input_ready;
  wire       [24:0]   add_preShifter_input_payload_rs1_mantissa;
  wire       [8:0]    add_preShifter_input_payload_rs1_exponent;
  wire                add_preShifter_input_payload_rs1_sign;
  wire                add_preShifter_input_payload_rs1_special;
  wire       [24:0]   add_preShifter_input_payload_rs2_mantissa;
  wire       [8:0]    add_preShifter_input_payload_rs2_exponent;
  wire                add_preShifter_input_payload_rs2_sign;
  wire                add_preShifter_input_payload_rs2_special;
  wire       [4:0]    add_preShifter_input_payload_rd;
  wire       [2:0]    add_preShifter_input_payload_roundMode;
  wire                add_preShifter_input_payload_needCommit;
  wire                add_preShifter_output_valid;
  reg                 add_preShifter_output_ready;
  wire       [24:0]   add_preShifter_output_payload_rs1_mantissa;
  wire       [8:0]    add_preShifter_output_payload_rs1_exponent;
  wire                add_preShifter_output_payload_rs1_sign;
  wire                add_preShifter_output_payload_rs1_special;
  wire       [24:0]   add_preShifter_output_payload_rs2_mantissa;
  wire       [8:0]    add_preShifter_output_payload_rs2_exponent;
  wire                add_preShifter_output_payload_rs2_sign;
  wire                add_preShifter_output_payload_rs2_special;
  wire       [4:0]    add_preShifter_output_payload_rd;
  wire       [2:0]    add_preShifter_output_payload_roundMode;
  wire                add_preShifter_output_payload_needCommit;
  wire                add_preShifter_output_payload_absRs1Bigger;
  wire                add_preShifter_output_payload_rs1ExponentBigger;
  wire       [9:0]    add_preShifter_exp21;
  wire                add_preShifter_rs1ExponentBigger;
  wire                add_preShifter_rs1ExponentEqual;
  wire                add_preShifter_rs1MantissaBigger;
  wire                add_preShifter_absRs1Bigger;
  wire                add_shifter_input_valid;
  wire                add_shifter_input_ready;
  wire       [24:0]   add_shifter_input_payload_rs1_mantissa;
  wire       [8:0]    add_shifter_input_payload_rs1_exponent;
  wire                add_shifter_input_payload_rs1_sign;
  wire                add_shifter_input_payload_rs1_special;
  wire       [24:0]   add_shifter_input_payload_rs2_mantissa;
  wire       [8:0]    add_shifter_input_payload_rs2_exponent;
  wire                add_shifter_input_payload_rs2_sign;
  wire                add_shifter_input_payload_rs2_special;
  wire       [4:0]    add_shifter_input_payload_rd;
  wire       [2:0]    add_shifter_input_payload_roundMode;
  wire                add_shifter_input_payload_needCommit;
  wire                add_shifter_input_payload_absRs1Bigger;
  wire                add_shifter_input_payload_rs1ExponentBigger;
  reg                 add_preShifter_output_rValid;
  reg        [24:0]   add_preShifter_output_rData_rs1_mantissa;
  reg        [8:0]    add_preShifter_output_rData_rs1_exponent;
  reg                 add_preShifter_output_rData_rs1_sign;
  reg                 add_preShifter_output_rData_rs1_special;
  reg        [24:0]   add_preShifter_output_rData_rs2_mantissa;
  reg        [8:0]    add_preShifter_output_rData_rs2_exponent;
  reg                 add_preShifter_output_rData_rs2_sign;
  reg                 add_preShifter_output_rData_rs2_special;
  reg        [4:0]    add_preShifter_output_rData_rd;
  reg        [2:0]    add_preShifter_output_rData_roundMode;
  reg                 add_preShifter_output_rData_needCommit;
  reg                 add_preShifter_output_rData_absRs1Bigger;
  reg                 add_preShifter_output_rData_rs1ExponentBigger;
  wire                when_Stream_l375_12;
  wire                add_shifter_output_valid;
  reg                 add_shifter_output_ready;
  wire       [24:0]   add_shifter_output_payload_rs1_mantissa;
  wire       [8:0]    add_shifter_output_payload_rs1_exponent;
  wire                add_shifter_output_payload_rs1_sign;
  wire                add_shifter_output_payload_rs1_special;
  wire       [24:0]   add_shifter_output_payload_rs2_mantissa;
  wire       [8:0]    add_shifter_output_payload_rs2_exponent;
  wire                add_shifter_output_payload_rs2_sign;
  wire                add_shifter_output_payload_rs2_special;
  wire       [4:0]    add_shifter_output_payload_rd;
  wire       [2:0]    add_shifter_output_payload_roundMode;
  wire                add_shifter_output_payload_needCommit;
  wire                add_shifter_output_payload_xSign;
  wire                add_shifter_output_payload_ySign;
  wire       [25:0]   add_shifter_output_payload_xMantissa;
  wire       [25:0]   add_shifter_output_payload_yMantissa;
  wire       [8:0]    add_shifter_output_payload_xyExponent;
  wire                add_shifter_output_payload_xySign;
  wire                add_shifter_output_payload_roundingScrap;
  wire       [9:0]    add_shifter_exp21;
  wire       [9:0]    _zz_add_shifter_shiftBy;
  wire       [9:0]    add_shifter_shiftBy;
  wire                add_shifter_shiftOverflow;
  wire                add_shifter_passThrough;
  wire                add_shifter_xySign;
  wire       [25:0]   add_shifter_xMantissa;
  wire       [25:0]   add_shifter_yMantissaUnshifted;
  wire       [25:0]   add_shifter_yMantissa;
  reg                 add_shifter_roundingScrap;
  wire                when_FpuCore_l1419;
  wire                when_FpuCore_l1419_1;
  wire                when_FpuCore_l1419_2;
  wire                when_FpuCore_l1419_3;
  wire                when_FpuCore_l1419_4;
  wire                when_FpuCore_l1424;
  wire                add_math_input_valid;
  wire                add_math_input_ready;
  wire       [24:0]   add_math_input_payload_rs1_mantissa;
  wire       [8:0]    add_math_input_payload_rs1_exponent;
  wire                add_math_input_payload_rs1_sign;
  wire                add_math_input_payload_rs1_special;
  wire       [24:0]   add_math_input_payload_rs2_mantissa;
  wire       [8:0]    add_math_input_payload_rs2_exponent;
  wire                add_math_input_payload_rs2_sign;
  wire                add_math_input_payload_rs2_special;
  wire       [4:0]    add_math_input_payload_rd;
  wire       [2:0]    add_math_input_payload_roundMode;
  wire                add_math_input_payload_needCommit;
  wire                add_math_input_payload_xSign;
  wire                add_math_input_payload_ySign;
  wire       [25:0]   add_math_input_payload_xMantissa;
  wire       [25:0]   add_math_input_payload_yMantissa;
  wire       [8:0]    add_math_input_payload_xyExponent;
  wire                add_math_input_payload_xySign;
  wire                add_math_input_payload_roundingScrap;
  reg                 add_shifter_output_rValid;
  reg        [24:0]   add_shifter_output_rData_rs1_mantissa;
  reg        [8:0]    add_shifter_output_rData_rs1_exponent;
  reg                 add_shifter_output_rData_rs1_sign;
  reg                 add_shifter_output_rData_rs1_special;
  reg        [24:0]   add_shifter_output_rData_rs2_mantissa;
  reg        [8:0]    add_shifter_output_rData_rs2_exponent;
  reg                 add_shifter_output_rData_rs2_sign;
  reg                 add_shifter_output_rData_rs2_special;
  reg        [4:0]    add_shifter_output_rData_rd;
  reg        [2:0]    add_shifter_output_rData_roundMode;
  reg                 add_shifter_output_rData_needCommit;
  reg                 add_shifter_output_rData_xSign;
  reg                 add_shifter_output_rData_ySign;
  reg        [25:0]   add_shifter_output_rData_xMantissa;
  reg        [25:0]   add_shifter_output_rData_yMantissa;
  reg        [8:0]    add_shifter_output_rData_xyExponent;
  reg                 add_shifter_output_rData_xySign;
  reg                 add_shifter_output_rData_roundingScrap;
  wire                when_Stream_l375_13;
  wire                add_math_output_valid;
  reg                 add_math_output_ready;
  wire       [24:0]   add_math_output_payload_rs1_mantissa;
  wire       [8:0]    add_math_output_payload_rs1_exponent;
  wire                add_math_output_payload_rs1_sign;
  wire                add_math_output_payload_rs1_special;
  wire       [24:0]   add_math_output_payload_rs2_mantissa;
  wire       [8:0]    add_math_output_payload_rs2_exponent;
  wire                add_math_output_payload_rs2_sign;
  wire                add_math_output_payload_rs2_special;
  wire       [4:0]    add_math_output_payload_rd;
  wire       [2:0]    add_math_output_payload_roundMode;
  wire                add_math_output_payload_needCommit;
  wire                add_math_output_payload_xSign;
  wire                add_math_output_payload_ySign;
  wire       [25:0]   add_math_output_payload_xMantissa;
  wire       [25:0]   add_math_output_payload_yMantissa;
  wire       [8:0]    add_math_output_payload_xyExponent;
  wire                add_math_output_payload_xySign;
  wire                add_math_output_payload_roundingScrap;
  wire       [26:0]   add_math_output_payload_xyMantissa;
  wire       [26:0]   add_math_xSigned;
  wire       [26:0]   add_math_ySigned;
  wire                add_oh_input_valid;
  wire                add_oh_input_ready;
  wire       [24:0]   add_oh_input_payload_rs1_mantissa;
  wire       [8:0]    add_oh_input_payload_rs1_exponent;
  wire                add_oh_input_payload_rs1_sign;
  wire                add_oh_input_payload_rs1_special;
  wire       [24:0]   add_oh_input_payload_rs2_mantissa;
  wire       [8:0]    add_oh_input_payload_rs2_exponent;
  wire                add_oh_input_payload_rs2_sign;
  wire                add_oh_input_payload_rs2_special;
  wire       [4:0]    add_oh_input_payload_rd;
  wire       [2:0]    add_oh_input_payload_roundMode;
  wire                add_oh_input_payload_needCommit;
  wire                add_oh_input_payload_xSign;
  wire                add_oh_input_payload_ySign;
  wire       [25:0]   add_oh_input_payload_xMantissa;
  wire       [25:0]   add_oh_input_payload_yMantissa;
  wire       [8:0]    add_oh_input_payload_xyExponent;
  wire                add_oh_input_payload_xySign;
  wire                add_oh_input_payload_roundingScrap;
  wire       [26:0]   add_oh_input_payload_xyMantissa;
  reg                 add_math_output_rValid;
  reg        [24:0]   add_math_output_rData_rs1_mantissa;
  reg        [8:0]    add_math_output_rData_rs1_exponent;
  reg                 add_math_output_rData_rs1_sign;
  reg                 add_math_output_rData_rs1_special;
  reg        [24:0]   add_math_output_rData_rs2_mantissa;
  reg        [8:0]    add_math_output_rData_rs2_exponent;
  reg                 add_math_output_rData_rs2_sign;
  reg                 add_math_output_rData_rs2_special;
  reg        [4:0]    add_math_output_rData_rd;
  reg        [2:0]    add_math_output_rData_roundMode;
  reg                 add_math_output_rData_needCommit;
  reg                 add_math_output_rData_xSign;
  reg                 add_math_output_rData_ySign;
  reg        [25:0]   add_math_output_rData_xMantissa;
  reg        [25:0]   add_math_output_rData_yMantissa;
  reg        [8:0]    add_math_output_rData_xyExponent;
  reg                 add_math_output_rData_xySign;
  reg                 add_math_output_rData_roundingScrap;
  reg        [26:0]   add_math_output_rData_xyMantissa;
  wire                when_Stream_l375_14;
  wire                add_oh_input_fire;
  wire                when_FpuCore_l221_4;
  wire                add_oh_isCommited;
  wire                _zz_add_oh_input_ready;
  wire                add_oh_output_valid;
  reg                 add_oh_output_ready;
  wire       [24:0]   add_oh_output_payload_rs1_mantissa;
  wire       [8:0]    add_oh_output_payload_rs1_exponent;
  wire                add_oh_output_payload_rs1_sign;
  wire                add_oh_output_payload_rs1_special;
  wire       [24:0]   add_oh_output_payload_rs2_mantissa;
  wire       [8:0]    add_oh_output_payload_rs2_exponent;
  wire                add_oh_output_payload_rs2_sign;
  wire                add_oh_output_payload_rs2_special;
  wire       [4:0]    add_oh_output_payload_rd;
  wire       [2:0]    add_oh_output_payload_roundMode;
  wire                add_oh_output_payload_needCommit;
  wire                add_oh_output_payload_xSign;
  wire                add_oh_output_payload_ySign;
  wire       [25:0]   add_oh_output_payload_xMantissa;
  wire       [25:0]   add_oh_output_payload_yMantissa;
  wire       [8:0]    add_oh_output_payload_xyExponent;
  wire                add_oh_output_payload_xySign;
  wire                add_oh_output_payload_roundingScrap;
  wire       [26:0]   add_oh_output_payload_xyMantissa;
  wire       [4:0]    add_oh_output_payload_shift;
  wire       [26:0]   _zz_add_oh_shift;
  wire       [26:0]   _zz_add_oh_shift_1;
  wire                _zz_add_oh_shift_2;
  wire                _zz_add_oh_shift_3;
  wire                _zz_add_oh_shift_4;
  wire                _zz_add_oh_shift_5;
  wire                _zz_add_oh_shift_6;
  wire                _zz_add_oh_shift_7;
  wire                _zz_add_oh_shift_8;
  wire                _zz_add_oh_shift_9;
  wire                _zz_add_oh_shift_10;
  wire                _zz_add_oh_shift_11;
  wire                _zz_add_oh_shift_12;
  wire                _zz_add_oh_shift_13;
  wire                _zz_add_oh_shift_14;
  wire                _zz_add_oh_shift_15;
  wire                _zz_add_oh_shift_16;
  wire                _zz_add_oh_shift_17;
  wire                _zz_add_oh_shift_18;
  wire                _zz_add_oh_shift_19;
  wire                _zz_add_oh_shift_20;
  wire                _zz_add_oh_shift_21;
  wire                _zz_add_oh_shift_22;
  wire                _zz_add_oh_shift_23;
  wire                _zz_add_oh_shift_24;
  wire                _zz_add_oh_shift_25;
  wire                _zz_add_oh_shift_26;
  wire                _zz_add_oh_shift_27;
  wire       [4:0]    add_oh_shift;
  wire                add_norm_input_valid;
  wire                add_norm_input_ready;
  wire       [24:0]   add_norm_input_payload_rs1_mantissa;
  wire       [8:0]    add_norm_input_payload_rs1_exponent;
  wire                add_norm_input_payload_rs1_sign;
  wire                add_norm_input_payload_rs1_special;
  wire       [24:0]   add_norm_input_payload_rs2_mantissa;
  wire       [8:0]    add_norm_input_payload_rs2_exponent;
  wire                add_norm_input_payload_rs2_sign;
  wire                add_norm_input_payload_rs2_special;
  wire       [4:0]    add_norm_input_payload_rd;
  wire       [2:0]    add_norm_input_payload_roundMode;
  wire                add_norm_input_payload_needCommit;
  wire                add_norm_input_payload_xSign;
  wire                add_norm_input_payload_ySign;
  wire       [25:0]   add_norm_input_payload_xMantissa;
  wire       [25:0]   add_norm_input_payload_yMantissa;
  wire       [8:0]    add_norm_input_payload_xyExponent;
  wire                add_norm_input_payload_xySign;
  wire                add_norm_input_payload_roundingScrap;
  wire       [26:0]   add_norm_input_payload_xyMantissa;
  wire       [4:0]    add_norm_input_payload_shift;
  reg                 add_oh_output_rValid;
  reg        [24:0]   add_oh_output_rData_rs1_mantissa;
  reg        [8:0]    add_oh_output_rData_rs1_exponent;
  reg                 add_oh_output_rData_rs1_sign;
  reg                 add_oh_output_rData_rs1_special;
  reg        [24:0]   add_oh_output_rData_rs2_mantissa;
  reg        [8:0]    add_oh_output_rData_rs2_exponent;
  reg                 add_oh_output_rData_rs2_sign;
  reg                 add_oh_output_rData_rs2_special;
  reg        [4:0]    add_oh_output_rData_rd;
  reg        [2:0]    add_oh_output_rData_roundMode;
  reg                 add_oh_output_rData_needCommit;
  reg                 add_oh_output_rData_xSign;
  reg                 add_oh_output_rData_ySign;
  reg        [25:0]   add_oh_output_rData_xMantissa;
  reg        [25:0]   add_oh_output_rData_yMantissa;
  reg        [8:0]    add_oh_output_rData_xyExponent;
  reg                 add_oh_output_rData_xySign;
  reg                 add_oh_output_rData_roundingScrap;
  reg        [26:0]   add_oh_output_rData_xyMantissa;
  reg        [4:0]    add_oh_output_rData_shift;
  wire                when_Stream_l375_15;
  wire                add_norm_output_valid;
  wire                add_norm_output_ready;
  wire       [24:0]   add_norm_output_payload_rs1_mantissa;
  wire       [8:0]    add_norm_output_payload_rs1_exponent;
  wire                add_norm_output_payload_rs1_sign;
  wire                add_norm_output_payload_rs1_special;
  wire       [24:0]   add_norm_output_payload_rs2_mantissa;
  wire       [8:0]    add_norm_output_payload_rs2_exponent;
  wire                add_norm_output_payload_rs2_sign;
  wire                add_norm_output_payload_rs2_special;
  wire       [4:0]    add_norm_output_payload_rd;
  wire       [2:0]    add_norm_output_payload_roundMode;
  wire                add_norm_output_payload_needCommit;
  wire       [26:0]   add_norm_output_payload_mantissa;
  wire       [9:0]    add_norm_output_payload_exponent;
  wire                add_norm_output_payload_infinityNan;
  wire                add_norm_output_payload_forceNan;
  wire                add_norm_output_payload_forceZero;
  wire                add_norm_output_payload_forceInfinity;
  wire                add_norm_output_payload_xySign;
  wire                add_norm_output_payload_roundingScrap;
  wire                add_norm_output_payload_xyMantissaZero;
  wire                add_result_input_valid;
  wire                add_result_input_ready;
  wire       [24:0]   add_result_input_payload_rs1_mantissa;
  wire       [8:0]    add_result_input_payload_rs1_exponent;
  wire                add_result_input_payload_rs1_sign;
  wire                add_result_input_payload_rs1_special;
  wire       [24:0]   add_result_input_payload_rs2_mantissa;
  wire       [8:0]    add_result_input_payload_rs2_exponent;
  wire                add_result_input_payload_rs2_sign;
  wire                add_result_input_payload_rs2_special;
  wire       [4:0]    add_result_input_payload_rd;
  wire       [2:0]    add_result_input_payload_roundMode;
  wire                add_result_input_payload_needCommit;
  wire       [26:0]   add_result_input_payload_mantissa;
  wire       [9:0]    add_result_input_payload_exponent;
  wire                add_result_input_payload_infinityNan;
  wire                add_result_input_payload_forceNan;
  wire                add_result_input_payload_forceZero;
  wire                add_result_input_payload_forceInfinity;
  wire                add_result_input_payload_xySign;
  wire                add_result_input_payload_roundingScrap;
  wire                add_result_input_payload_xyMantissaZero;
  wire                add_result_output_valid;
  wire                add_result_output_ready;
  wire       [4:0]    add_result_output_payload_rd;
  reg        [23:0]   add_result_output_payload_value_mantissa;
  reg        [8:0]    add_result_output_payload_value_exponent;
  reg                 add_result_output_payload_value_sign;
  reg                 add_result_output_payload_value_special;
  wire                add_result_output_payload_scrap;
  wire       [2:0]    add_result_output_payload_roundMode;
  wire                add_result_output_payload_NV;
  wire                add_result_output_payload_DZ;
  wire                when_FpuCore_l1513;
  wire                when_FpuCore_l1516;
  wire                load_s1_output_m2sPipe_valid;
  wire                load_s1_output_m2sPipe_ready;
  wire       [4:0]    load_s1_output_m2sPipe_payload_rd;
  wire       [23:0]   load_s1_output_m2sPipe_payload_value_mantissa;
  wire       [8:0]    load_s1_output_m2sPipe_payload_value_exponent;
  wire                load_s1_output_m2sPipe_payload_value_sign;
  wire                load_s1_output_m2sPipe_payload_value_special;
  wire                load_s1_output_m2sPipe_payload_scrap;
  wire       [2:0]    load_s1_output_m2sPipe_payload_roundMode;
  wire                load_s1_output_m2sPipe_payload_NV;
  wire                load_s1_output_m2sPipe_payload_DZ;
  reg                 load_s1_output_rValid;
  reg        [4:0]    load_s1_output_rData_rd;
  reg        [23:0]   load_s1_output_rData_value_mantissa;
  reg        [8:0]    load_s1_output_rData_value_exponent;
  reg                 load_s1_output_rData_value_sign;
  reg                 load_s1_output_rData_value_special;
  reg                 load_s1_output_rData_scrap;
  reg        [2:0]    load_s1_output_rData_roundMode;
  reg                 load_s1_output_rData_NV;
  reg                 load_s1_output_rData_DZ;
  wire                when_Stream_l375_16;
  wire                shortPip_output_m2sPipe_valid;
  wire                shortPip_output_m2sPipe_ready;
  wire       [4:0]    shortPip_output_m2sPipe_payload_rd;
  wire       [23:0]   shortPip_output_m2sPipe_payload_value_mantissa;
  wire       [8:0]    shortPip_output_m2sPipe_payload_value_exponent;
  wire                shortPip_output_m2sPipe_payload_value_sign;
  wire                shortPip_output_m2sPipe_payload_value_special;
  wire                shortPip_output_m2sPipe_payload_scrap;
  wire       [2:0]    shortPip_output_m2sPipe_payload_roundMode;
  wire                shortPip_output_m2sPipe_payload_NV;
  wire                shortPip_output_m2sPipe_payload_DZ;
  reg                 shortPip_output_rValid;
  reg        [4:0]    shortPip_output_rData_rd;
  reg        [23:0]   shortPip_output_rData_value_mantissa;
  reg        [8:0]    shortPip_output_rData_value_exponent;
  reg                 shortPip_output_rData_value_sign;
  reg                 shortPip_output_rData_value_special;
  reg                 shortPip_output_rData_scrap;
  reg        [2:0]    shortPip_output_rData_roundMode;
  reg                 shortPip_output_rData_NV;
  reg                 shortPip_output_rData_DZ;
  wire                when_Stream_l375_17;
  wire                FpuPlugin_fpu_streamArbiter_2_io_output_combStage_valid;
  wire                FpuPlugin_fpu_streamArbiter_2_io_output_combStage_ready;
  wire       [4:0]    FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_rd;
  wire       [23:0]   FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_mantissa;
  wire       [8:0]    FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_exponent;
  wire                FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_sign;
  wire                FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_special;
  wire                FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_scrap;
  wire       [2:0]    FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode;
  wire                FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_NV;
  wire                FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_DZ;
  wire                merge_arbitrated_valid;
  wire       [4:0]    merge_arbitrated_payload_rd;
  wire       [23:0]   merge_arbitrated_payload_value_mantissa;
  wire       [8:0]    merge_arbitrated_payload_value_exponent;
  wire                merge_arbitrated_payload_value_sign;
  wire                merge_arbitrated_payload_value_special;
  wire                merge_arbitrated_payload_scrap;
  wire       [2:0]    merge_arbitrated_payload_roundMode;
  wire                merge_arbitrated_payload_NV;
  wire                merge_arbitrated_payload_DZ;
  reg                 roundFront_input_valid;
  reg        [4:0]    roundFront_input_payload_rd;
  reg        [23:0]   roundFront_input_payload_value_mantissa;
  reg        [8:0]    roundFront_input_payload_value_exponent;
  reg                 roundFront_input_payload_value_sign;
  reg                 roundFront_input_payload_value_special;
  reg                 roundFront_input_payload_scrap;
  reg        [2:0]    roundFront_input_payload_roundMode;
  reg                 roundFront_input_payload_NV;
  reg                 roundFront_input_payload_DZ;
  wire                roundFront_output_valid;
  wire       [4:0]    roundFront_output_payload_rd;
  wire       [23:0]   roundFront_output_payload_value_mantissa;
  wire       [8:0]    roundFront_output_payload_value_exponent;
  wire                roundFront_output_payload_value_sign;
  wire                roundFront_output_payload_value_special;
  wire                roundFront_output_payload_scrap;
  wire       [2:0]    roundFront_output_payload_roundMode;
  wire                roundFront_output_payload_NV;
  wire                roundFront_output_payload_DZ;
  wire                roundFront_output_payload_mantissaIncrement;
  wire       [1:0]    roundFront_output_payload_roundAdjusted;
  wire       [24:0]   roundFront_output_payload_exactMask;
  wire       [24:0]   roundFront_manAggregate;
  wire       [7:0]    roundFront_expBase;
  wire       [9:0]    roundFront_expDif;
  wire                roundFront_expSubnormal;
  wire       [9:0]    roundFront_discardCount;
  wire       [4:0]    roundFront_discardCountTrunk;
  reg        [24:0]   roundFront_exactMask;
  reg        [1:0]    roundFront_roundAdjusted;
  reg                 roundFront_rneBit;
  wire                when_FpuCore_l1559;
  reg                 _zz_roundFront_mantissaIncrement;
  wire                roundFront_mantissaIncrement;
  reg                 roundBack_input_valid;
  reg        [4:0]    roundBack_input_payload_rd;
  reg        [23:0]   roundBack_input_payload_value_mantissa;
  reg        [8:0]    roundBack_input_payload_value_exponent;
  reg                 roundBack_input_payload_value_sign;
  reg                 roundBack_input_payload_value_special;
  reg                 roundBack_input_payload_scrap;
  reg        [2:0]    roundBack_input_payload_roundMode;
  reg                 roundBack_input_payload_NV;
  reg                 roundBack_input_payload_DZ;
  reg                 roundBack_input_payload_mantissaIncrement;
  reg        [1:0]    roundBack_input_payload_roundAdjusted;
  reg        [24:0]   roundBack_input_payload_exactMask;
  wire                roundBack_output_valid;
  wire       [4:0]    roundBack_output_payload_rd;
  wire       [22:0]   roundBack_output_payload_value_mantissa;
  wire       [8:0]    roundBack_output_payload_value_exponent;
  wire                roundBack_output_payload_value_sign;
  wire                roundBack_output_payload_value_special;
  wire                roundBack_output_payload_NV;
  wire                roundBack_output_payload_NX;
  wire                roundBack_output_payload_OF;
  wire                roundBack_output_payload_UF;
  wire                roundBack_output_payload_DZ;
  wire                roundBack_output_payload_write;
  wire       [22:0]   roundBack_math_mantissa;
  wire       [8:0]    roundBack_math_exponent;
  wire                roundBack_math_sign;
  wire                roundBack_math_special;
  wire       [22:0]   roundBack_adderMantissa;
  (* keep , syn_keep *) wire       [22:0]   roundBack_adderRightOp /* synthesis syn_keep = 1 */ ;
  (* keep , syn_keep *) wire       [31:0]   _zz_roundBack_adder /* synthesis syn_keep = 1 */ ;
  (* keep , syn_keep *) wire       [0:0]    _zz_roundBack_adder_1 /* synthesis syn_keep = 1 */ ;
  (* keep , syn_keep *) wire       [31:0]   roundBack_adder /* synthesis syn_keep = 1 */ ;
  wire       [31:0]   roundBack_masked;
  reg        [22:0]   roundBack_patched_mantissa;
  reg        [8:0]    roundBack_patched_exponent;
  wire                roundBack_patched_sign;
  reg                 roundBack_patched_special;
  reg                 roundBack_nx;
  reg                 roundBack_of;
  reg                 roundBack_uf;
  wire       [7:0]    roundBack_ufSubnormalThreshold;
  wire       [6:0]    roundBack_ufThreshold;
  wire       [8:0]    roundBack_ofThreshold;
  reg        [2:0]    roundBack_threshold;
  wire       [2:0]    roundBack_borringRound;
  wire                roundBack_borringCase;
  wire                when_FpuCore_l1616;
  wire                when_FpuCore_l1619;
  reg                 when_FpuCore_l1629;
  wire                when_FpuCore_l1638;
  reg                 when_FpuCore_l1648;
  wire                when_FpuCore_l1657;
  wire                roundBack_writes_0;
  wire                roundBack_write;
  reg                 writeback_input_valid;
  reg        [4:0]    writeback_input_payload_rd;
  reg        [22:0]   writeback_input_payload_value_mantissa;
  reg        [8:0]    writeback_input_payload_value_exponent;
  reg                 writeback_input_payload_value_sign;
  reg                 writeback_input_payload_value_special;
  reg                 writeback_input_payload_NV;
  reg                 writeback_input_payload_NX;
  reg                 writeback_input_payload_OF;
  reg                 writeback_input_payload_UF;
  reg                 writeback_input_payload_DZ;
  reg                 writeback_input_payload_write;
  wire                when_FpuCore_l1689;
  wire                writeback_port_valid;
  wire       [4:0]    writeback_port_payload_address;
  wire       [22:0]   writeback_port_payload_data_value_mantissa;
  wire       [8:0]    writeback_port_payload_data_value_exponent;
  wire                writeback_port_payload_data_value_sign;
  wire                writeback_port_payload_data_value_special;
  `ifndef SYNTHESIS
  reg [63:0] io_port_0_cmd_payload_opcode_string;
  reg [47:0] io_port_0_cmd_payload_format_string;
  reg [23:0] io_port_0_cmd_payload_roundMode_string;
  reg [63:0] io_port_0_commit_payload_opcode_string;
  reg [63:0] commitFork_load_0_payload_opcode_string;
  reg [63:0] commitFork_commit_0_payload_opcode_string;
  reg [63:0] FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string;
  reg [63:0] FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string;
  reg [63:0] _zz_payload_opcode_string;
  reg [63:0] _zz_commitLogic_0_input_payload_opcode_string;
  reg [63:0] commitLogic_0_input_payload_opcode_string;
  reg [63:0] scheduler_0_input_payload_opcode_string;
  reg [47:0] scheduler_0_input_payload_format_string;
  reg [23:0] scheduler_0_input_payload_roundMode_string;
  reg [63:0] io_port_0_cmd_rData_opcode_string;
  reg [47:0] io_port_0_cmd_rData_format_string;
  reg [23:0] io_port_0_cmd_rData_roundMode_string;
  reg [63:0] _zz_scheduler_0_input_payload_opcode_string;
  reg [47:0] _zz_scheduler_0_input_payload_format_string;
  reg [23:0] _zz_scheduler_0_input_payload_roundMode_string;
  reg [63:0] scheduler_0_output_payload_opcode_string;
  reg [47:0] scheduler_0_output_payload_format_string;
  reg [23:0] scheduler_0_output_payload_roundMode_string;
  reg [63:0] _zz_io_inputs_0_payload_opcode_string;
  reg [47:0] _zz_io_inputs_0_payload_format_string;
  reg [23:0] _zz_io_inputs_0_payload_roundMode_string;
  reg [63:0] cmdArbiter_output_payload_opcode_string;
  reg [23:0] cmdArbiter_output_payload_roundMode_string;
  reg [63:0] read_s0_payload_opcode_string;
  reg [23:0] read_s0_payload_roundMode_string;
  reg [63:0] read_s1_payload_opcode_string;
  reg [23:0] read_s1_payload_roundMode_string;
  reg [63:0] read_s0_rData_opcode_string;
  reg [23:0] read_s0_rData_roundMode_string;
  reg [63:0] read_output_payload_opcode_string;
  reg [23:0] read_output_payload_roundMode_string;
  reg [63:0] decode_input_payload_opcode_string;
  reg [23:0] decode_input_payload_roundMode_string;
  reg [23:0] decode_load_payload_roundMode_string;
  reg [63:0] decode_shortPip_payload_opcode_string;
  reg [23:0] decode_shortPip_payload_roundMode_string;
  reg [23:0] decode_divSqrt_payload_roundMode_string;
  reg [23:0] decode_div_payload_roundMode_string;
  reg [23:0] decode_sqrt_payload_roundMode_string;
  reg [23:0] decode_mul_payload_roundMode_string;
  reg [23:0] decode_divSqrtToMul_payload_roundMode_string;
  reg [23:0] decode_add_payload_roundMode_string;
  reg [23:0] decode_mulToAdd_payload_roundMode_string;
  reg [23:0] decode_load_s2mPipe_payload_roundMode_string;
  reg [23:0] decode_load_rData_roundMode_string;
  reg [23:0] _zz_decode_load_s2mPipe_payload_roundMode_string;
  reg [23:0] decode_load_s2mPipe_m2sPipe_payload_roundMode_string;
  reg [23:0] decode_load_s2mPipe_rData_roundMode_string;
  reg [23:0] load_s0_input_payload_roundMode_string;
  reg [23:0] decode_load_s2mPipe_m2sPipe_rData_roundMode_string;
  reg [63:0] load_s0_filtred_0_payload_opcode_string;
  reg [23:0] load_s0_output_payload_roundMode_string;
  reg [23:0] load_s1_input_payload_roundMode_string;
  reg [23:0] load_s0_output_rData_roundMode_string;
  reg [23:0] load_s1_output_payload_roundMode_string;
  reg [63:0] shortPip_input_payload_opcode_string;
  reg [23:0] shortPip_input_payload_roundMode_string;
  reg [63:0] decode_shortPip_rData_opcode_string;
  reg [23:0] decode_shortPip_rData_roundMode_string;
  reg [23:0] shortPip_rfOutput_payload_roundMode_string;
  reg [23:0] shortPip_output_payload_roundMode_string;
  reg [23:0] mul_preMul_input_payload_roundMode_string;
  reg [23:0] decode_mul_rData_roundMode_string;
  reg [23:0] mul_preMul_output_payload_roundMode_string;
  reg [23:0] mul_mul_input_payload_roundMode_string;
  reg [23:0] mul_preMul_output_rData_roundMode_string;
  reg [23:0] mul_mul_output_payload_roundMode_string;
  reg [23:0] mul_sum1_input_payload_roundMode_string;
  reg [23:0] mul_mul_output_rData_roundMode_string;
  reg [23:0] mul_sum1_output_payload_roundMode_string;
  reg [23:0] mul_sum2_input_payload_roundMode_string;
  reg [23:0] mul_sum1_output_rData_roundMode_string;
  reg [23:0] mul_sum2_output_payload_roundMode_string;
  reg [23:0] mul_norm_input_payload_roundMode_string;
  reg [23:0] mul_sum2_output_rData_roundMode_string;
  reg [23:0] mul_result_output_payload_roundMode_string;
  reg [23:0] mul_result_mulToAdd_payload_roundMode_string;
  reg [23:0] mul_result_mulToAdd_m2sPipe_payload_roundMode_string;
  reg [23:0] mul_result_mulToAdd_rData_roundMode_string;
  reg [23:0] div_input_payload_roundMode_string;
  reg [23:0] decode_div_rData_roundMode_string;
  reg [23:0] div_output_payload_roundMode_string;
  reg [23:0] sqrt_input_payload_roundMode_string;
  reg [23:0] decode_sqrt_rData_roundMode_string;
  reg [23:0] sqrt_output_payload_roundMode_string;
  reg [23:0] add_preShifter_input_payload_roundMode_string;
  reg [23:0] add_preShifter_output_payload_roundMode_string;
  reg [23:0] add_shifter_input_payload_roundMode_string;
  reg [23:0] add_preShifter_output_rData_roundMode_string;
  reg [23:0] add_shifter_output_payload_roundMode_string;
  reg [23:0] add_math_input_payload_roundMode_string;
  reg [23:0] add_shifter_output_rData_roundMode_string;
  reg [23:0] add_math_output_payload_roundMode_string;
  reg [23:0] add_oh_input_payload_roundMode_string;
  reg [23:0] add_math_output_rData_roundMode_string;
  reg [23:0] add_oh_output_payload_roundMode_string;
  reg [23:0] add_norm_input_payload_roundMode_string;
  reg [23:0] add_oh_output_rData_roundMode_string;
  reg [23:0] add_norm_output_payload_roundMode_string;
  reg [23:0] add_result_input_payload_roundMode_string;
  reg [23:0] add_result_output_payload_roundMode_string;
  reg [23:0] load_s1_output_m2sPipe_payload_roundMode_string;
  reg [23:0] load_s1_output_rData_roundMode_string;
  reg [23:0] shortPip_output_m2sPipe_payload_roundMode_string;
  reg [23:0] shortPip_output_rData_roundMode_string;
  reg [23:0] FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode_string;
  reg [23:0] merge_arbitrated_payload_roundMode_string;
  reg [23:0] roundFront_input_payload_roundMode_string;
  reg [23:0] roundFront_output_payload_roundMode_string;
  reg [23:0] roundBack_input_payload_roundMode_string;
  `endif

  reg [33:0] rf_ram [0:31];
  (* ram_style = "distributed" *) reg [0:0] rf_scoreboards_0_target [0:31];
  (* ram_style = "distributed" *) reg [0:0] rf_scoreboards_0_hit [0:31];
  (* ram_style = "distributed" *) reg [0:0] rf_scoreboards_0_writes [0:31];

  assign _zz_commitLogic_0_pending_counter = (commitLogic_0_pending_counter + _zz_commitLogic_0_pending_counter_1);
  assign _zz_commitLogic_0_pending_counter_2 = commitLogic_0_pending_inc;
  assign _zz_commitLogic_0_pending_counter_1 = {3'd0, _zz_commitLogic_0_pending_counter_2};
  assign _zz_commitLogic_0_pending_counter_4 = commitLogic_0_pending_dec;
  assign _zz_commitLogic_0_pending_counter_3 = {3'd0, _zz_commitLogic_0_pending_counter_4};
  assign _zz_commitLogic_0_add_counter = (commitLogic_0_add_counter + _zz_commitLogic_0_add_counter_1);
  assign _zz_commitLogic_0_add_counter_2 = commitLogic_0_add_inc;
  assign _zz_commitLogic_0_add_counter_1 = {3'd0, _zz_commitLogic_0_add_counter_2};
  assign _zz_commitLogic_0_add_counter_4 = commitLogic_0_add_dec;
  assign _zz_commitLogic_0_add_counter_3 = {3'd0, _zz_commitLogic_0_add_counter_4};
  assign _zz_commitLogic_0_mul_counter = (commitLogic_0_mul_counter + _zz_commitLogic_0_mul_counter_1);
  assign _zz_commitLogic_0_mul_counter_2 = commitLogic_0_mul_inc;
  assign _zz_commitLogic_0_mul_counter_1 = {3'd0, _zz_commitLogic_0_mul_counter_2};
  assign _zz_commitLogic_0_mul_counter_4 = commitLogic_0_mul_dec;
  assign _zz_commitLogic_0_mul_counter_3 = {3'd0, _zz_commitLogic_0_mul_counter_4};
  assign _zz_commitLogic_0_div_counter = (commitLogic_0_div_counter + _zz_commitLogic_0_div_counter_1);
  assign _zz_commitLogic_0_div_counter_2 = commitLogic_0_div_inc;
  assign _zz_commitLogic_0_div_counter_1 = {3'd0, _zz_commitLogic_0_div_counter_2};
  assign _zz_commitLogic_0_div_counter_4 = commitLogic_0_div_dec;
  assign _zz_commitLogic_0_div_counter_3 = {3'd0, _zz_commitLogic_0_div_counter_4};
  assign _zz_commitLogic_0_sqrt_counter = (commitLogic_0_sqrt_counter + _zz_commitLogic_0_sqrt_counter_1);
  assign _zz_commitLogic_0_sqrt_counter_2 = commitLogic_0_sqrt_inc;
  assign _zz_commitLogic_0_sqrt_counter_1 = {3'd0, _zz_commitLogic_0_sqrt_counter_2};
  assign _zz_commitLogic_0_sqrt_counter_4 = commitLogic_0_sqrt_dec;
  assign _zz_commitLogic_0_sqrt_counter_3 = {3'd0, _zz_commitLogic_0_sqrt_counter_4};
  assign _zz_commitLogic_0_short_counter = (commitLogic_0_short_counter + _zz_commitLogic_0_short_counter_1);
  assign _zz_commitLogic_0_short_counter_2 = commitLogic_0_short_inc;
  assign _zz_commitLogic_0_short_counter_1 = {3'd0, _zz_commitLogic_0_short_counter_2};
  assign _zz_commitLogic_0_short_counter_4 = commitLogic_0_short_dec;
  assign _zz_commitLogic_0_short_counter_3 = {3'd0, _zz_commitLogic_0_short_counter_4};
  assign _zz_when = 1'b1;
  assign _zz_load_s1_fsm_shift_input_1 = (load_s1_fsm_shift_input <<< 1'b1);
  assign _zz_load_s1_fsm_shift_input_2 = (load_s1_fsm_shift_input_1 <<< 2'b10);
  assign _zz_load_s1_fsm_shift_input_3 = (load_s1_fsm_shift_input_2 <<< 3'b100);
  assign _zz_load_s1_fsm_shift_input_4 = (load_s1_fsm_shift_input_3 <<< 4'b1000);
  assign _zz_load_s1_fsm_shift_input_5 = (load_s1_fsm_shift_input_4 <<< 5'h10);
  assign _zz_load_s0_output_rData_value_3 = _zz_load_s0_output_rData_value_4;
  assign _zz_load_s0_output_rData_value_2 = _zz_load_s0_output_rData_value_3[31:0];
  assign _zz_load_s0_output_rData_value_4 = ({_zz_load_s0_output_rData_value_1,(_zz_load_s0_output_rData_value_1 ? (~ _zz_load_s0_output_rData_value) : _zz_load_s0_output_rData_value)} + _zz_load_s0_output_rData_value_5);
  assign _zz_load_s0_output_rData_value_6 = _zz_load_s0_output_rData_value_1;
  assign _zz_load_s0_output_rData_value_5 = {32'd0, _zz_load_s0_output_rData_value_6};
  assign _zz__zz_load_s1_fsm_shift_by_1_1 = (_zz_load_s1_fsm_shift_by - 32'h00000001);
  assign _zz_load_s1_recoded_exponent = (_zz_load_s1_recoded_exponent_1 + _zz_load_s1_recoded_exponent_2);
  assign _zz_load_s1_recoded_exponent_1 = ({1'b0,load_s1_passThroughFloat_exponent} - {1'b0,load_s1_fsm_expOffset});
  assign _zz_load_s1_recoded_exponent_2 = {1'd0, load_s1_recodedExpOffset};
  assign _zz_load_s1_output_payload_value_exponent = {4'd0, load_s1_fsm_shift_by};
  assign _zz_shortPip_f32_exp = (shortPip_input_payload_rs1_exponent - 9'h080);
  assign _zz_shortPip_expInSubnormalRange = {1'd0, shortPip_expSubnormalThreshold};
  assign _zz_shortPip_fsm_shift_input_1 = (shortPip_fsm_shift_input >>> 6'h20);
  assign _zz_shortPip_fsm_shift_input_2 = (shortPip_fsm_shift_input_1 >>> 5'h10);
  assign _zz_shortPip_fsm_shift_input_3 = (shortPip_fsm_shift_input_2 >>> 4'b1000);
  assign _zz_shortPip_fsm_shift_input_4 = (shortPip_fsm_shift_input_3 >>> 3'b100);
  assign _zz_shortPip_fsm_shift_input_5 = (shortPip_fsm_shift_input_4 >>> 2'b10);
  assign _zz_shortPip_fsm_shift_input_6 = (shortPip_fsm_shift_input_5 >>> 1'b1);
  assign _zz_shortPip_fsm_shift_by_2 = (((_zz_shortPip_fsm_shift_by < _zz_shortPip_fsm_shift_by_3) ? _zz_shortPip_fsm_shift_by : _zz_shortPip_fsm_shift_by_4) + 9'h0);
  assign _zz_shortPip_fsm_shift_by_3 = {3'd0, _zz_shortPip_fsm_shift_by_1};
  assign _zz_shortPip_fsm_shift_by_4 = {3'd0, _zz_shortPip_fsm_shift_by_1};
  assign _zz_shortPip_fsm_shift_by_5 = (_zz_shortPip_fsm_shift_by_6 - shortPip_input_payload_rs1_exponent);
  assign _zz_shortPip_fsm_shift_by_6 = {1'd0, shortPip_fsm_formatShiftOffset};
  assign _zz_shortPip_f2i_result_1 = (shortPip_f2i_resign ^ shortPip_f2i_increment);
  assign _zz_shortPip_f2i_result = {31'd0, _zz_shortPip_f2i_result_1};
  assign _zz_mul_sum1_sum = {12'd0, mul_sum1_input_payload_muls_0};
  assign _zz_mul_sum1_sum_2 = ({18'd0,mul_sum1_input_payload_muls_1} <<< 5'd18);
  assign _zz_mul_sum1_sum_1 = {6'd0, _zz_mul_sum1_sum_2};
  assign _zz_mul_sum2_sum = (_zz_mul_sum2_sum_1 + _zz_mul_sum2_sum_3);
  assign _zz_mul_sum2_sum_2 = ({18'd0,mul_sum2_input_payload_muls2_0} <<< 5'd18);
  assign _zz_mul_sum2_sum_1 = {6'd0, _zz_mul_sum2_sum_2};
  assign _zz_mul_sum2_sum_3 = ({36'd0,mul_sum2_input_payload_muls2_1} <<< 6'd36);
  assign _zz_mul_norm_exp_1 = mul_norm_needShift;
  assign _zz_mul_norm_exp = {9'd0, _zz_mul_norm_exp_1};
  assign _zz_mul_norm_forceUnderflow = {1'd0, mul_norm_underflowThreshold};
  assign _zz_mul_norm_output_exponent = (mul_norm_exp - 10'h0ff);
  assign _zz_div_exponent = (_zz_div_exponent_1 - _zz_div_exponent_3);
  assign _zz_div_exponent_1 = (_zz_div_exponent_2 + 11'h4ff);
  assign _zz_div_exponent_2 = {2'd0, div_input_payload_rs1_exponent};
  assign _zz_div_exponent_3 = {2'd0, div_input_payload_rs2_exponent};
  assign _zz_div_exponent_5 = div_needShift;
  assign _zz_div_exponent_4 = {10'd0, _zz_div_exponent_5};
  assign _zz_sqrt_exponent = (_zz_sqrt_exponent_1 + {1'b0,_zz_sqrt_exponent_3});
  assign _zz_sqrt_exponent_2 = {1'b0,7'h7f};
  assign _zz_sqrt_exponent_1 = {1'd0, _zz_sqrt_exponent_2};
  assign _zz_sqrt_exponent_3 = (sqrt_input_payload_rs1_exponent >>> 1'd1);
  assign _zz_sqrt_exponent_5 = sqrt_input_payload_rs1_exponent[0];
  assign _zz_sqrt_exponent_4 = {8'd0, _zz_sqrt_exponent_5};
  assign _zz_add_shifter_shiftBy_1 = (_zz_add_shifter_shiftBy[9] ? _zz_add_shifter_shiftBy_2 : _zz_add_shifter_shiftBy);
  assign _zz_add_shifter_shiftBy_2 = (~ _zz_add_shifter_shiftBy);
  assign _zz_add_shifter_shiftBy_4 = _zz_add_shifter_shiftBy[9];
  assign _zz_add_shifter_shiftBy_3 = {9'd0, _zz_add_shifter_shiftBy_4};
  assign _zz_add_shifter_yMantissa_1 = (add_shifter_yMantissa >>> 5'h10);
  assign _zz_add_shifter_yMantissa_2 = (add_shifter_yMantissa_1 >>> 4'b1000);
  assign _zz_add_shifter_yMantissa_3 = (add_shifter_yMantissa_2 >>> 3'b100);
  assign _zz_add_shifter_yMantissa_4 = (add_shifter_yMantissa_3 >>> 2'b10);
  assign _zz_add_shifter_yMantissa_5 = (add_shifter_yMantissa_4 >>> 1'b1);
  assign _zz_add_math_xSigned = ({add_math_input_payload_xSign,(add_math_input_payload_xSign ? (~ add_math_input_payload_xMantissa) : add_math_input_payload_xMantissa)} + _zz_add_math_xSigned_1);
  assign _zz_add_math_xSigned_2 = add_math_input_payload_xSign;
  assign _zz_add_math_xSigned_1 = {26'd0, _zz_add_math_xSigned_2};
  assign _zz_add_math_ySigned = ({add_math_input_payload_ySign,(add_math_input_payload_ySign ? (~ add_math_input_payload_yMantissa) : add_math_input_payload_yMantissa)} + _zz_add_math_ySigned_1);
  assign _zz_add_math_ySigned_2 = (add_math_input_payload_ySign && (! add_math_input_payload_roundingScrap));
  assign _zz_add_math_ySigned_1 = {26'd0, _zz_add_math_ySigned_2};
  assign _zz_add_math_output_payload_xyMantissa = _zz_add_math_output_payload_xyMantissa_1;
  assign _zz_add_math_output_payload_xyMantissa_1 = ($signed(_zz_add_math_output_payload_xyMantissa_2) + $signed(_zz_add_math_output_payload_xyMantissa_3));
  assign _zz_add_math_output_payload_xyMantissa_2 = {add_math_xSigned[26],add_math_xSigned};
  assign _zz_add_math_output_payload_xyMantissa_3 = {add_math_ySigned[26],add_math_ySigned};
  assign _zz__zz_add_oh_shift_1_1 = (_zz_add_oh_shift - 27'h0000001);
  assign _zz_add_norm_output_payload_exponent = ({1'b0,add_norm_input_payload_xyExponent} - _zz_add_norm_output_payload_exponent_1);
  assign _zz_add_norm_output_payload_exponent_2 = {1'b0,add_norm_input_payload_shift};
  assign _zz_add_norm_output_payload_exponent_1 = {4'd0, _zz_add_norm_output_payload_exponent_2};
  assign _zz_add_result_output_payload_value_mantissa = (add_result_input_payload_mantissa >>> 2'd2);
  assign _zz_roundFront_expDif_1 = {1'b0,roundFront_expBase};
  assign _zz_roundFront_expDif = {1'd0, _zz_roundFront_expDif_1};
  assign _zz_roundFront_roundAdjusted = {1'b1,_zz_roundFront_roundAdjusted_1};
  assign _zz_roundFront_roundAdjusted_1 = (roundFront_manAggregate >>> 1'd1);
  assign _zz_roundFront_rneBit = {2'b01,_zz_roundFront_rneBit_1};
  assign _zz_roundFront_rneBit_1 = (roundFront_manAggregate >>> 2'd2);
  assign _zz_roundBack_adderMantissa = (roundBack_input_payload_exactMask[23 : 0] >>> 1'd1);
  assign _zz_roundBack_adderRightOp = (roundBack_input_payload_mantissaIncrement ? _zz_roundBack_adderRightOp_1 : 24'h0);
  assign _zz_roundBack_adderRightOp_1 = (roundBack_input_payload_exactMask >>> 1'd1);
  assign _zz_roundBack_adder_2 = (_zz_roundBack_adder + _zz_roundBack_adder_3);
  assign _zz_roundBack_adder_3 = {9'd0, roundBack_adderRightOp};
  assign _zz_roundBack_adder_4 = {31'd0, _zz_roundBack_adder_1};
  assign _zz_roundBack_masked_1 = _zz_roundBack_masked_2[22:0];
  assign _zz_roundBack_masked = {9'd0, _zz_roundBack_masked_1};
  assign _zz_roundBack_masked_2 = (roundBack_input_payload_exactMask >>> 1'd1);
  assign _zz_roundBack_borringCase = {1'd0, roundBack_ufSubnormalThreshold};
  assign _zz_when_FpuCore_l1616 = {1'd0, roundBack_ufSubnormalThreshold};
  assign _zz_when_FpuCore_l1638 = {2'd0, roundBack_ufThreshold};
  assign _zz_rf_ram_port = {writeback_port_payload_data_value_special,{writeback_port_payload_data_value_sign,{writeback_port_payload_data_value_exponent,writeback_port_payload_data_value_mantissa}}};
  assign _zz_rf_scoreboards_0_target_port = rf_scoreboards_0_targetWrite_payload_data;
  assign _zz_rf_scoreboards_0_hit_port = rf_scoreboards_0_hitWrite_payload_data;
  assign _zz_rf_scoreboards_0_writes_port = commitLogic_0_input_payload_write;
  assign _zz_decode_shortPipHit = FpuOpcode_MIN_MAX;
  assign _zz_decode_shortPipHit_1 = (decode_input_payload_opcode == FpuOpcode_CMP);
  assign _zz_decode_shortPipHit_2 = (decode_input_payload_opcode == FpuOpcode_F2I);
  assign _zz_decode_shortPipHit_3 = (decode_input_payload_opcode == FpuOpcode_STORE);
  assign _zz__zz_load_s1_fsm_shift_by = load_s1_fsm_ohInput[7];
  assign _zz__zz_load_s1_fsm_shift_by_1 = load_s1_fsm_ohInput[8];
  assign _zz__zz_load_s1_fsm_shift_by_2 = {load_s1_fsm_ohInput[9],{load_s1_fsm_ohInput[10],{load_s1_fsm_ohInput[11],{load_s1_fsm_ohInput[12],{load_s1_fsm_ohInput[13],{load_s1_fsm_ohInput[14],{load_s1_fsm_ohInput[15],{load_s1_fsm_ohInput[16],{load_s1_fsm_ohInput[17],{_zz__zz_load_s1_fsm_shift_by_3,{_zz__zz_load_s1_fsm_shift_by_4,_zz__zz_load_s1_fsm_shift_by_5}}}}}}}}}}};
  assign _zz__zz_load_s1_fsm_shift_by_3 = load_s1_fsm_ohInput[18];
  assign _zz__zz_load_s1_fsm_shift_by_4 = load_s1_fsm_ohInput[19];
  assign _zz__zz_load_s1_fsm_shift_by_5 = {load_s1_fsm_ohInput[20],{load_s1_fsm_ohInput[21],{load_s1_fsm_ohInput[22],{load_s1_fsm_ohInput[23],{load_s1_fsm_ohInput[24],{load_s1_fsm_ohInput[25],{load_s1_fsm_ohInput[26],{load_s1_fsm_ohInput[27],{load_s1_fsm_ohInput[28],{_zz__zz_load_s1_fsm_shift_by_6,{_zz__zz_load_s1_fsm_shift_by_7,_zz__zz_load_s1_fsm_shift_by_8}}}}}}}}}}};
  assign _zz__zz_load_s1_fsm_shift_by_6 = load_s1_fsm_ohInput[29];
  assign _zz__zz_load_s1_fsm_shift_by_7 = load_s1_fsm_ohInput[30];
  assign _zz__zz_load_s1_fsm_shift_by_8 = load_s1_fsm_ohInput[31];
  assign _zz_shortPip_f2i_underflow = shortPip_f2i_unsigned[30 : 0];
  assign _zz_shortPip_f2i_underflow_1 = 31'h0;
  assign _zz__zz_add_oh_shift = add_oh_output_payload_xyMantissa[7];
  assign _zz__zz_add_oh_shift_1 = add_oh_output_payload_xyMantissa[8];
  assign _zz__zz_add_oh_shift_2 = {add_oh_output_payload_xyMantissa[9],{add_oh_output_payload_xyMantissa[10],{add_oh_output_payload_xyMantissa[11],{add_oh_output_payload_xyMantissa[12],{add_oh_output_payload_xyMantissa[13],{add_oh_output_payload_xyMantissa[14],{add_oh_output_payload_xyMantissa[15],{add_oh_output_payload_xyMantissa[16],{add_oh_output_payload_xyMantissa[17],{_zz__zz_add_oh_shift_3,{_zz__zz_add_oh_shift_4,_zz__zz_add_oh_shift_5}}}}}}}}}}};
  assign _zz__zz_add_oh_shift_3 = add_oh_output_payload_xyMantissa[18];
  assign _zz__zz_add_oh_shift_4 = add_oh_output_payload_xyMantissa[19];
  assign _zz__zz_add_oh_shift_5 = {add_oh_output_payload_xyMantissa[20],{add_oh_output_payload_xyMantissa[21],{add_oh_output_payload_xyMantissa[22],{add_oh_output_payload_xyMantissa[23],{add_oh_output_payload_xyMantissa[24],{add_oh_output_payload_xyMantissa[25],add_oh_output_payload_xyMantissa[26]}}}}}};
  assign _zz_roundFront_exactMask = 5'h13;
  assign _zz_roundFront_exactMask_1 = (5'h12 < roundFront_discardCountTrunk);
  assign _zz_roundFront_exactMask_2 = (5'h11 < roundFront_discardCountTrunk);
  assign _zz_roundFront_exactMask_3 = {(5'h10 < roundFront_discardCountTrunk),{(5'h0f < roundFront_discardCountTrunk),{(5'h0e < roundFront_discardCountTrunk),{(5'h0d < roundFront_discardCountTrunk),{(5'h0c < roundFront_discardCountTrunk),{(_zz_roundFront_exactMask_4 < roundFront_discardCountTrunk),{_zz_roundFront_exactMask_5,{_zz_roundFront_exactMask_6,_zz_roundFront_exactMask_7}}}}}}}};
  assign _zz_roundFront_exactMask_4 = 5'h0b;
  assign _zz_roundFront_exactMask_5 = (5'h0a < roundFront_discardCountTrunk);
  assign _zz_roundFront_exactMask_6 = (5'h09 < roundFront_discardCountTrunk);
  assign _zz_roundFront_exactMask_7 = {(5'h08 < roundFront_discardCountTrunk),{(5'h07 < roundFront_discardCountTrunk),{(5'h06 < roundFront_discardCountTrunk),{(5'h05 < roundFront_discardCountTrunk),{(5'h04 < roundFront_discardCountTrunk),{(_zz_roundFront_exactMask_8 < roundFront_discardCountTrunk),{_zz_roundFront_exactMask_9,{_zz_roundFront_exactMask_10,_zz_roundFront_exactMask_11}}}}}}}};
  assign _zz_roundFront_exactMask_8 = 5'h03;
  assign _zz_roundFront_exactMask_9 = (5'h02 < roundFront_discardCountTrunk);
  assign _zz_roundFront_exactMask_10 = (5'h01 < roundFront_discardCountTrunk);
  assign _zz_roundFront_exactMask_11 = {(5'h0 < roundFront_discardCountTrunk),1'b1};
  always @(posedge clk) begin
    if(_zz_read_rs_0_value_mantissa_1) begin
      rf_ram_spinal_port0 <= rf_ram[_zz_read_rs_0_value_mantissa];
    end
  end

  always @(posedge clk) begin
    if(_zz_read_rs_1_value_mantissa_1) begin
      rf_ram_spinal_port1 <= rf_ram[_zz_read_rs_1_value_mantissa];
    end
  end

  always @(posedge clk) begin
    if(_zz_read_rs_2_value_mantissa_1) begin
      rf_ram_spinal_port2 <= rf_ram[_zz_read_rs_2_value_mantissa];
    end
  end

  always @(posedge clk) begin
    if(_zz_1) begin
      rf_ram[writeback_port_payload_address] <= _zz_rf_ram_port;
    end
  end

  always @(posedge clk) begin
    if(_zz_4) begin
      rf_scoreboards_0_target[rf_scoreboards_0_targetWrite_payload_address] <= _zz_rf_scoreboards_0_target_port;
    end
  end

  assign rf_scoreboards_0_target_spinal_port1 = rf_scoreboards_0_target[scheduler_0_input_payload_rs1];
  assign rf_scoreboards_0_target_spinal_port2 = rf_scoreboards_0_target[scheduler_0_input_payload_rs2];
  assign rf_scoreboards_0_target_spinal_port3 = rf_scoreboards_0_target[scheduler_0_input_payload_rs3];
  assign rf_scoreboards_0_target_spinal_port4 = rf_scoreboards_0_target[scheduler_0_input_payload_rd];
  always @(posedge clk) begin
    if(_zz_3) begin
      rf_scoreboards_0_hit[rf_scoreboards_0_hitWrite_payload_address] <= _zz_rf_scoreboards_0_hit_port;
    end
  end

  assign rf_scoreboards_0_hit_spinal_port1 = rf_scoreboards_0_hit[scheduler_0_input_payload_rs1];
  assign rf_scoreboards_0_hit_spinal_port2 = rf_scoreboards_0_hit[scheduler_0_input_payload_rs2];
  assign rf_scoreboards_0_hit_spinal_port3 = rf_scoreboards_0_hit[scheduler_0_input_payload_rs3];
  assign rf_scoreboards_0_hit_spinal_port4 = rf_scoreboards_0_hit[scheduler_0_input_payload_rd];
  assign rf_scoreboards_0_hit_spinal_port5 = rf_scoreboards_0_hit[writeback_input_payload_rd];
  always @(posedge clk) begin
    if(_zz_2) begin
      rf_scoreboards_0_writes[commitLogic_0_input_payload_rd] <= _zz_rf_scoreboards_0_writes_port;
    end
  end

  assign rf_scoreboards_0_writes_spinal_port1 = rf_scoreboards_0_writes[roundBack_input_payload_rd];
  StreamFork streamFork_1 (
    .io_input_valid              (io_port_0_commit_valid                         ), //i
    .io_input_ready              (streamFork_1_io_input_ready                    ), //o
    .io_input_payload_opcode     (io_port_0_commit_payload_opcode[3:0]           ), //i
    .io_input_payload_rd         (io_port_0_commit_payload_rd[4:0]               ), //i
    .io_input_payload_write      (io_port_0_commit_payload_write                 ), //i
    .io_input_payload_value      (io_port_0_commit_payload_value[31:0]           ), //i
    .io_outputs_0_valid          (streamFork_1_io_outputs_0_valid                ), //o
    .io_outputs_0_ready          (commitFork_load_0_ready                        ), //i
    .io_outputs_0_payload_opcode (streamFork_1_io_outputs_0_payload_opcode[3:0]  ), //o
    .io_outputs_0_payload_rd     (streamFork_1_io_outputs_0_payload_rd[4:0]      ), //o
    .io_outputs_0_payload_write  (streamFork_1_io_outputs_0_payload_write        ), //o
    .io_outputs_0_payload_value  (streamFork_1_io_outputs_0_payload_value[31:0]  ), //o
    .io_outputs_1_valid          (streamFork_1_io_outputs_1_valid                ), //o
    .io_outputs_1_ready          (FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN), //i
    .io_outputs_1_payload_opcode (streamFork_1_io_outputs_1_payload_opcode[3:0]  ), //o
    .io_outputs_1_payload_rd     (streamFork_1_io_outputs_1_payload_rd[4:0]      ), //o
    .io_outputs_1_payload_write  (streamFork_1_io_outputs_1_payload_write        ), //o
    .io_outputs_1_payload_value  (streamFork_1_io_outputs_1_payload_value[31:0]  )  //o
  );
  StreamArbiter cmdArbiter_arbiter (
    .io_inputs_0_valid             (scheduler_0_output_valid                           ), //i
    .io_inputs_0_ready             (cmdArbiter_arbiter_io_inputs_0_ready               ), //o
    .io_inputs_0_payload_opcode    (_zz_io_inputs_0_payload_opcode[3:0]                ), //i
    .io_inputs_0_payload_arg       (scheduler_0_output_payload_arg[1:0]                ), //i
    .io_inputs_0_payload_rs1       (scheduler_0_output_payload_rs1[4:0]                ), //i
    .io_inputs_0_payload_rs2       (scheduler_0_output_payload_rs2[4:0]                ), //i
    .io_inputs_0_payload_rs3       (scheduler_0_output_payload_rs3[4:0]                ), //i
    .io_inputs_0_payload_rd        (scheduler_0_output_payload_rd[4:0]                 ), //i
    .io_inputs_0_payload_format    (_zz_io_inputs_0_payload_format                     ), //i
    .io_inputs_0_payload_roundMode (_zz_io_inputs_0_payload_roundMode[2:0]             ), //i
    .io_output_valid               (cmdArbiter_arbiter_io_output_valid                 ), //o
    .io_output_ready               (cmdArbiter_output_ready                            ), //i
    .io_output_payload_opcode      (cmdArbiter_arbiter_io_output_payload_opcode[3:0]   ), //o
    .io_output_payload_arg         (cmdArbiter_arbiter_io_output_payload_arg[1:0]      ), //o
    .io_output_payload_rs1         (cmdArbiter_arbiter_io_output_payload_rs1[4:0]      ), //o
    .io_output_payload_rs2         (cmdArbiter_arbiter_io_output_payload_rs2[4:0]      ), //o
    .io_output_payload_rs3         (cmdArbiter_arbiter_io_output_payload_rs3[4:0]      ), //o
    .io_output_payload_rd          (cmdArbiter_arbiter_io_output_payload_rd[4:0]       ), //o
    .io_output_payload_format      (cmdArbiter_arbiter_io_output_payload_format        ), //o
    .io_output_payload_roundMode   (cmdArbiter_arbiter_io_output_payload_roundMode[2:0]), //o
    .io_chosenOH                   (cmdArbiter_arbiter_io_chosenOH                     ), //o
    .clk                           (clk                                                ), //i
    .reset                         (reset                                              )  //i
  );
  FpuDiv div_divider (
    .io_input_valid           (div_divider_io_input_valid                ), //i
    .io_input_ready           (div_divider_io_input_ready                ), //o
    .io_input_payload_a       (div_divider_io_input_payload_a[23:0]      ), //i
    .io_input_payload_b       (div_divider_io_input_payload_b[23:0]      ), //i
    .io_output_valid          (div_divider_io_output_valid               ), //o
    .io_output_ready          (div_input_ready                           ), //i
    .io_output_payload_result (div_divider_io_output_payload_result[26:0]), //o
    .io_output_payload_remain (div_divider_io_output_payload_remain[24:0]), //o
    .clk                      (clk                                       ), //i
    .reset                    (reset                                     )  //i
  );
  FpuSqrt sqrt_sqrt (
    .io_input_valid           (sqrt_sqrt_io_input_valid                ), //i
    .io_input_ready           (sqrt_sqrt_io_input_ready                ), //o
    .io_input_payload_a       (sqrt_sqrt_io_input_payload_a[24:0]      ), //i
    .io_output_valid          (sqrt_sqrt_io_output_valid               ), //o
    .io_output_ready          (sqrt_input_ready                        ), //i
    .io_output_payload_result (sqrt_sqrt_io_output_payload_result[23:0]), //o
    .io_output_payload_remain (sqrt_sqrt_io_output_payload_remain[27:0]), //o
    .clk                      (clk                                     ), //i
    .reset                    (reset                                   )  //i
  );
  StreamArbiter_1 streamArbiter_2 (
    .io_inputs_0_valid                  (load_s1_output_m2sPipe_valid                           ), //i
    .io_inputs_0_ready                  (streamArbiter_2_io_inputs_0_ready                      ), //o
    .io_inputs_0_payload_rd             (load_s1_output_m2sPipe_payload_rd[4:0]                 ), //i
    .io_inputs_0_payload_value_mantissa (load_s1_output_m2sPipe_payload_value_mantissa[23:0]    ), //i
    .io_inputs_0_payload_value_exponent (load_s1_output_m2sPipe_payload_value_exponent[8:0]     ), //i
    .io_inputs_0_payload_value_sign     (load_s1_output_m2sPipe_payload_value_sign              ), //i
    .io_inputs_0_payload_value_special  (load_s1_output_m2sPipe_payload_value_special           ), //i
    .io_inputs_0_payload_scrap          (load_s1_output_m2sPipe_payload_scrap                   ), //i
    .io_inputs_0_payload_roundMode      (load_s1_output_m2sPipe_payload_roundMode[2:0]          ), //i
    .io_inputs_0_payload_NV             (load_s1_output_m2sPipe_payload_NV                      ), //i
    .io_inputs_0_payload_DZ             (load_s1_output_m2sPipe_payload_DZ                      ), //i
    .io_inputs_1_valid                  (sqrt_output_valid                                      ), //i
    .io_inputs_1_ready                  (streamArbiter_2_io_inputs_1_ready                      ), //o
    .io_inputs_1_payload_rd             (sqrt_output_payload_rd[4:0]                            ), //i
    .io_inputs_1_payload_value_mantissa (sqrt_output_payload_value_mantissa[23:0]               ), //i
    .io_inputs_1_payload_value_exponent (sqrt_output_payload_value_exponent[8:0]                ), //i
    .io_inputs_1_payload_value_sign     (sqrt_output_payload_value_sign                         ), //i
    .io_inputs_1_payload_value_special  (sqrt_output_payload_value_special                      ), //i
    .io_inputs_1_payload_scrap          (sqrt_output_payload_scrap                              ), //i
    .io_inputs_1_payload_roundMode      (sqrt_output_payload_roundMode[2:0]                     ), //i
    .io_inputs_1_payload_NV             (sqrt_output_payload_NV                                 ), //i
    .io_inputs_1_payload_DZ             (sqrt_output_payload_DZ                                 ), //i
    .io_inputs_2_valid                  (div_output_valid                                       ), //i
    .io_inputs_2_ready                  (streamArbiter_2_io_inputs_2_ready                      ), //o
    .io_inputs_2_payload_rd             (div_output_payload_rd[4:0]                             ), //i
    .io_inputs_2_payload_value_mantissa (div_output_payload_value_mantissa[23:0]                ), //i
    .io_inputs_2_payload_value_exponent (div_output_payload_value_exponent[8:0]                 ), //i
    .io_inputs_2_payload_value_sign     (div_output_payload_value_sign                          ), //i
    .io_inputs_2_payload_value_special  (div_output_payload_value_special                       ), //i
    .io_inputs_2_payload_scrap          (div_output_payload_scrap                               ), //i
    .io_inputs_2_payload_roundMode      (div_output_payload_roundMode[2:0]                      ), //i
    .io_inputs_2_payload_NV             (div_output_payload_NV                                  ), //i
    .io_inputs_2_payload_DZ             (div_output_payload_DZ                                  ), //i
    .io_inputs_3_valid                  (add_result_output_valid                                ), //i
    .io_inputs_3_ready                  (streamArbiter_2_io_inputs_3_ready                      ), //o
    .io_inputs_3_payload_rd             (add_result_output_payload_rd[4:0]                      ), //i
    .io_inputs_3_payload_value_mantissa (add_result_output_payload_value_mantissa[23:0]         ), //i
    .io_inputs_3_payload_value_exponent (add_result_output_payload_value_exponent[8:0]          ), //i
    .io_inputs_3_payload_value_sign     (add_result_output_payload_value_sign                   ), //i
    .io_inputs_3_payload_value_special  (add_result_output_payload_value_special                ), //i
    .io_inputs_3_payload_scrap          (add_result_output_payload_scrap                        ), //i
    .io_inputs_3_payload_roundMode      (add_result_output_payload_roundMode[2:0]               ), //i
    .io_inputs_3_payload_NV             (add_result_output_payload_NV                           ), //i
    .io_inputs_3_payload_DZ             (add_result_output_payload_DZ                           ), //i
    .io_inputs_4_valid                  (mul_result_output_valid                                ), //i
    .io_inputs_4_ready                  (streamArbiter_2_io_inputs_4_ready                      ), //o
    .io_inputs_4_payload_rd             (mul_result_output_payload_rd[4:0]                      ), //i
    .io_inputs_4_payload_value_mantissa (mul_result_output_payload_value_mantissa[23:0]         ), //i
    .io_inputs_4_payload_value_exponent (mul_result_output_payload_value_exponent[8:0]          ), //i
    .io_inputs_4_payload_value_sign     (mul_result_output_payload_value_sign                   ), //i
    .io_inputs_4_payload_value_special  (mul_result_output_payload_value_special                ), //i
    .io_inputs_4_payload_scrap          (mul_result_output_payload_scrap                        ), //i
    .io_inputs_4_payload_roundMode      (mul_result_output_payload_roundMode[2:0]               ), //i
    .io_inputs_4_payload_NV             (mul_result_output_payload_NV                           ), //i
    .io_inputs_4_payload_DZ             (mul_result_output_payload_DZ                           ), //i
    .io_inputs_5_valid                  (shortPip_output_m2sPipe_valid                          ), //i
    .io_inputs_5_ready                  (streamArbiter_2_io_inputs_5_ready                      ), //o
    .io_inputs_5_payload_rd             (shortPip_output_m2sPipe_payload_rd[4:0]                ), //i
    .io_inputs_5_payload_value_mantissa (shortPip_output_m2sPipe_payload_value_mantissa[23:0]   ), //i
    .io_inputs_5_payload_value_exponent (shortPip_output_m2sPipe_payload_value_exponent[8:0]    ), //i
    .io_inputs_5_payload_value_sign     (shortPip_output_m2sPipe_payload_value_sign             ), //i
    .io_inputs_5_payload_value_special  (shortPip_output_m2sPipe_payload_value_special          ), //i
    .io_inputs_5_payload_scrap          (shortPip_output_m2sPipe_payload_scrap                  ), //i
    .io_inputs_5_payload_roundMode      (shortPip_output_m2sPipe_payload_roundMode[2:0]         ), //i
    .io_inputs_5_payload_NV             (shortPip_output_m2sPipe_payload_NV                     ), //i
    .io_inputs_5_payload_DZ             (shortPip_output_m2sPipe_payload_DZ                     ), //i
    .io_output_valid                    (streamArbiter_2_io_output_valid                        ), //o
    .io_output_ready                    (FpuPlugin_fpu_streamArbiter_2_io_output_combStage_ready), //i
    .io_output_payload_rd               (streamArbiter_2_io_output_payload_rd[4:0]              ), //o
    .io_output_payload_value_mantissa   (streamArbiter_2_io_output_payload_value_mantissa[23:0] ), //o
    .io_output_payload_value_exponent   (streamArbiter_2_io_output_payload_value_exponent[8:0]  ), //o
    .io_output_payload_value_sign       (streamArbiter_2_io_output_payload_value_sign           ), //o
    .io_output_payload_value_special    (streamArbiter_2_io_output_payload_value_special        ), //o
    .io_output_payload_scrap            (streamArbiter_2_io_output_payload_scrap                ), //o
    .io_output_payload_roundMode        (streamArbiter_2_io_output_payload_roundMode[2:0]       ), //o
    .io_output_payload_NV               (streamArbiter_2_io_output_payload_NV                   ), //o
    .io_output_payload_DZ               (streamArbiter_2_io_output_payload_DZ                   ), //o
    .io_chosen                          (streamArbiter_2_io_chosen[2:0]                         ), //o
    .io_chosenOH                        (streamArbiter_2_io_chosenOH[5:0]                       ), //o
    .clk                                (clk                                                    ), //i
    .reset                              (reset                                                  )  //i
  );
  `ifndef SYNTHESIS
  always @(*) begin
    case(io_port_0_cmd_payload_opcode)
      FpuOpcode_LOAD : io_port_0_cmd_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : io_port_0_cmd_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : io_port_0_cmd_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : io_port_0_cmd_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : io_port_0_cmd_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : io_port_0_cmd_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : io_port_0_cmd_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : io_port_0_cmd_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : io_port_0_cmd_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : io_port_0_cmd_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : io_port_0_cmd_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : io_port_0_cmd_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : io_port_0_cmd_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : io_port_0_cmd_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : io_port_0_cmd_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : io_port_0_cmd_payload_opcode_string = "FCVT_X_X";
      default : io_port_0_cmd_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(io_port_0_cmd_payload_format)
      FpuFormat_FLOAT : io_port_0_cmd_payload_format_string = "FLOAT ";
      FpuFormat_DOUBLE : io_port_0_cmd_payload_format_string = "DOUBLE";
      default : io_port_0_cmd_payload_format_string = "??????";
    endcase
  end
  always @(*) begin
    case(io_port_0_cmd_payload_roundMode)
      FpuRoundMode_RNE : io_port_0_cmd_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_port_0_cmd_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_port_0_cmd_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_port_0_cmd_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_port_0_cmd_payload_roundMode_string = "RMM";
      default : io_port_0_cmd_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(io_port_0_commit_payload_opcode)
      FpuOpcode_LOAD : io_port_0_commit_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : io_port_0_commit_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : io_port_0_commit_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : io_port_0_commit_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : io_port_0_commit_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : io_port_0_commit_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : io_port_0_commit_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : io_port_0_commit_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : io_port_0_commit_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : io_port_0_commit_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : io_port_0_commit_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : io_port_0_commit_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : io_port_0_commit_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : io_port_0_commit_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : io_port_0_commit_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : io_port_0_commit_payload_opcode_string = "FCVT_X_X";
      default : io_port_0_commit_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(commitFork_load_0_payload_opcode)
      FpuOpcode_LOAD : commitFork_load_0_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : commitFork_load_0_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : commitFork_load_0_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : commitFork_load_0_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : commitFork_load_0_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : commitFork_load_0_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : commitFork_load_0_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : commitFork_load_0_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : commitFork_load_0_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : commitFork_load_0_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : commitFork_load_0_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : commitFork_load_0_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : commitFork_load_0_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : commitFork_load_0_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : commitFork_load_0_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : commitFork_load_0_payload_opcode_string = "FCVT_X_X";
      default : commitFork_load_0_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(commitFork_commit_0_payload_opcode)
      FpuOpcode_LOAD : commitFork_commit_0_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : commitFork_commit_0_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : commitFork_commit_0_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : commitFork_commit_0_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : commitFork_commit_0_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : commitFork_commit_0_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : commitFork_commit_0_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : commitFork_commit_0_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : commitFork_commit_0_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : commitFork_commit_0_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : commitFork_commit_0_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : commitFork_commit_0_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : commitFork_commit_0_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : commitFork_commit_0_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : commitFork_commit_0_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : commitFork_commit_0_payload_opcode_string = "FCVT_X_X";
      default : commitFork_commit_0_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode)
      FpuOpcode_LOAD : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "FCVT_X_X";
      default : FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode)
      FpuOpcode_LOAD : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "LOAD    ";
      FpuOpcode_STORE : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "STORE   ";
      FpuOpcode_MUL : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "MUL     ";
      FpuOpcode_ADD : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "ADD     ";
      FpuOpcode_FMA : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "FMA     ";
      FpuOpcode_I2F : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "I2F     ";
      FpuOpcode_F2I : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "F2I     ";
      FpuOpcode_CMP : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "CMP     ";
      FpuOpcode_DIV : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "DIV     ";
      FpuOpcode_SQRT : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "FCVT_X_X";
      default : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_payload_opcode)
      FpuOpcode_LOAD : _zz_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : _zz_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : _zz_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : _zz_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : _zz_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : _zz_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : _zz_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : _zz_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : _zz_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : _zz_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_payload_opcode_string = "FCVT_X_X";
      default : _zz_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_commitLogic_0_input_payload_opcode)
      FpuOpcode_LOAD : _zz_commitLogic_0_input_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : _zz_commitLogic_0_input_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : _zz_commitLogic_0_input_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : _zz_commitLogic_0_input_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : _zz_commitLogic_0_input_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : _zz_commitLogic_0_input_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : _zz_commitLogic_0_input_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : _zz_commitLogic_0_input_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : _zz_commitLogic_0_input_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : _zz_commitLogic_0_input_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_commitLogic_0_input_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_commitLogic_0_input_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_commitLogic_0_input_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_commitLogic_0_input_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_commitLogic_0_input_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_commitLogic_0_input_payload_opcode_string = "FCVT_X_X";
      default : _zz_commitLogic_0_input_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(commitLogic_0_input_payload_opcode)
      FpuOpcode_LOAD : commitLogic_0_input_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : commitLogic_0_input_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : commitLogic_0_input_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : commitLogic_0_input_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : commitLogic_0_input_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : commitLogic_0_input_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : commitLogic_0_input_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : commitLogic_0_input_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : commitLogic_0_input_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : commitLogic_0_input_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : commitLogic_0_input_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : commitLogic_0_input_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : commitLogic_0_input_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : commitLogic_0_input_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : commitLogic_0_input_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : commitLogic_0_input_payload_opcode_string = "FCVT_X_X";
      default : commitLogic_0_input_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(scheduler_0_input_payload_opcode)
      FpuOpcode_LOAD : scheduler_0_input_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : scheduler_0_input_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : scheduler_0_input_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : scheduler_0_input_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : scheduler_0_input_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : scheduler_0_input_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : scheduler_0_input_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : scheduler_0_input_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : scheduler_0_input_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : scheduler_0_input_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : scheduler_0_input_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : scheduler_0_input_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : scheduler_0_input_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : scheduler_0_input_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : scheduler_0_input_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : scheduler_0_input_payload_opcode_string = "FCVT_X_X";
      default : scheduler_0_input_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(scheduler_0_input_payload_format)
      FpuFormat_FLOAT : scheduler_0_input_payload_format_string = "FLOAT ";
      FpuFormat_DOUBLE : scheduler_0_input_payload_format_string = "DOUBLE";
      default : scheduler_0_input_payload_format_string = "??????";
    endcase
  end
  always @(*) begin
    case(scheduler_0_input_payload_roundMode)
      FpuRoundMode_RNE : scheduler_0_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : scheduler_0_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : scheduler_0_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : scheduler_0_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : scheduler_0_input_payload_roundMode_string = "RMM";
      default : scheduler_0_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(io_port_0_cmd_rData_opcode)
      FpuOpcode_LOAD : io_port_0_cmd_rData_opcode_string = "LOAD    ";
      FpuOpcode_STORE : io_port_0_cmd_rData_opcode_string = "STORE   ";
      FpuOpcode_MUL : io_port_0_cmd_rData_opcode_string = "MUL     ";
      FpuOpcode_ADD : io_port_0_cmd_rData_opcode_string = "ADD     ";
      FpuOpcode_FMA : io_port_0_cmd_rData_opcode_string = "FMA     ";
      FpuOpcode_I2F : io_port_0_cmd_rData_opcode_string = "I2F     ";
      FpuOpcode_F2I : io_port_0_cmd_rData_opcode_string = "F2I     ";
      FpuOpcode_CMP : io_port_0_cmd_rData_opcode_string = "CMP     ";
      FpuOpcode_DIV : io_port_0_cmd_rData_opcode_string = "DIV     ";
      FpuOpcode_SQRT : io_port_0_cmd_rData_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : io_port_0_cmd_rData_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : io_port_0_cmd_rData_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : io_port_0_cmd_rData_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : io_port_0_cmd_rData_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : io_port_0_cmd_rData_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : io_port_0_cmd_rData_opcode_string = "FCVT_X_X";
      default : io_port_0_cmd_rData_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(io_port_0_cmd_rData_format)
      FpuFormat_FLOAT : io_port_0_cmd_rData_format_string = "FLOAT ";
      FpuFormat_DOUBLE : io_port_0_cmd_rData_format_string = "DOUBLE";
      default : io_port_0_cmd_rData_format_string = "??????";
    endcase
  end
  always @(*) begin
    case(io_port_0_cmd_rData_roundMode)
      FpuRoundMode_RNE : io_port_0_cmd_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_port_0_cmd_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_port_0_cmd_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_port_0_cmd_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_port_0_cmd_rData_roundMode_string = "RMM";
      default : io_port_0_cmd_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(_zz_scheduler_0_input_payload_opcode)
      FpuOpcode_LOAD : _zz_scheduler_0_input_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : _zz_scheduler_0_input_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : _zz_scheduler_0_input_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : _zz_scheduler_0_input_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : _zz_scheduler_0_input_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : _zz_scheduler_0_input_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : _zz_scheduler_0_input_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : _zz_scheduler_0_input_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : _zz_scheduler_0_input_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : _zz_scheduler_0_input_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_scheduler_0_input_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_scheduler_0_input_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_scheduler_0_input_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_scheduler_0_input_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_scheduler_0_input_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_scheduler_0_input_payload_opcode_string = "FCVT_X_X";
      default : _zz_scheduler_0_input_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_scheduler_0_input_payload_format)
      FpuFormat_FLOAT : _zz_scheduler_0_input_payload_format_string = "FLOAT ";
      FpuFormat_DOUBLE : _zz_scheduler_0_input_payload_format_string = "DOUBLE";
      default : _zz_scheduler_0_input_payload_format_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_scheduler_0_input_payload_roundMode)
      FpuRoundMode_RNE : _zz_scheduler_0_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : _zz_scheduler_0_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : _zz_scheduler_0_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : _zz_scheduler_0_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : _zz_scheduler_0_input_payload_roundMode_string = "RMM";
      default : _zz_scheduler_0_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(scheduler_0_output_payload_opcode)
      FpuOpcode_LOAD : scheduler_0_output_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : scheduler_0_output_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : scheduler_0_output_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : scheduler_0_output_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : scheduler_0_output_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : scheduler_0_output_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : scheduler_0_output_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : scheduler_0_output_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : scheduler_0_output_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : scheduler_0_output_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : scheduler_0_output_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : scheduler_0_output_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : scheduler_0_output_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : scheduler_0_output_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : scheduler_0_output_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : scheduler_0_output_payload_opcode_string = "FCVT_X_X";
      default : scheduler_0_output_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(scheduler_0_output_payload_format)
      FpuFormat_FLOAT : scheduler_0_output_payload_format_string = "FLOAT ";
      FpuFormat_DOUBLE : scheduler_0_output_payload_format_string = "DOUBLE";
      default : scheduler_0_output_payload_format_string = "??????";
    endcase
  end
  always @(*) begin
    case(scheduler_0_output_payload_roundMode)
      FpuRoundMode_RNE : scheduler_0_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : scheduler_0_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : scheduler_0_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : scheduler_0_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : scheduler_0_output_payload_roundMode_string = "RMM";
      default : scheduler_0_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(_zz_io_inputs_0_payload_opcode)
      FpuOpcode_LOAD : _zz_io_inputs_0_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : _zz_io_inputs_0_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : _zz_io_inputs_0_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : _zz_io_inputs_0_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : _zz_io_inputs_0_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : _zz_io_inputs_0_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : _zz_io_inputs_0_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : _zz_io_inputs_0_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : _zz_io_inputs_0_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : _zz_io_inputs_0_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_io_inputs_0_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_io_inputs_0_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_io_inputs_0_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_io_inputs_0_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_io_inputs_0_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_io_inputs_0_payload_opcode_string = "FCVT_X_X";
      default : _zz_io_inputs_0_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_io_inputs_0_payload_format)
      FpuFormat_FLOAT : _zz_io_inputs_0_payload_format_string = "FLOAT ";
      FpuFormat_DOUBLE : _zz_io_inputs_0_payload_format_string = "DOUBLE";
      default : _zz_io_inputs_0_payload_format_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_io_inputs_0_payload_roundMode)
      FpuRoundMode_RNE : _zz_io_inputs_0_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : _zz_io_inputs_0_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : _zz_io_inputs_0_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : _zz_io_inputs_0_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : _zz_io_inputs_0_payload_roundMode_string = "RMM";
      default : _zz_io_inputs_0_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(cmdArbiter_output_payload_opcode)
      FpuOpcode_LOAD : cmdArbiter_output_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : cmdArbiter_output_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : cmdArbiter_output_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : cmdArbiter_output_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : cmdArbiter_output_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : cmdArbiter_output_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : cmdArbiter_output_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : cmdArbiter_output_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : cmdArbiter_output_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : cmdArbiter_output_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : cmdArbiter_output_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : cmdArbiter_output_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : cmdArbiter_output_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : cmdArbiter_output_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : cmdArbiter_output_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : cmdArbiter_output_payload_opcode_string = "FCVT_X_X";
      default : cmdArbiter_output_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(cmdArbiter_output_payload_roundMode)
      FpuRoundMode_RNE : cmdArbiter_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : cmdArbiter_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : cmdArbiter_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : cmdArbiter_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : cmdArbiter_output_payload_roundMode_string = "RMM";
      default : cmdArbiter_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(read_s0_payload_opcode)
      FpuOpcode_LOAD : read_s0_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : read_s0_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : read_s0_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : read_s0_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : read_s0_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : read_s0_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : read_s0_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : read_s0_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : read_s0_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : read_s0_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : read_s0_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : read_s0_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : read_s0_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : read_s0_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : read_s0_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : read_s0_payload_opcode_string = "FCVT_X_X";
      default : read_s0_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(read_s0_payload_roundMode)
      FpuRoundMode_RNE : read_s0_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : read_s0_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : read_s0_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : read_s0_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : read_s0_payload_roundMode_string = "RMM";
      default : read_s0_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(read_s1_payload_opcode)
      FpuOpcode_LOAD : read_s1_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : read_s1_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : read_s1_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : read_s1_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : read_s1_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : read_s1_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : read_s1_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : read_s1_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : read_s1_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : read_s1_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : read_s1_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : read_s1_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : read_s1_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : read_s1_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : read_s1_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : read_s1_payload_opcode_string = "FCVT_X_X";
      default : read_s1_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(read_s1_payload_roundMode)
      FpuRoundMode_RNE : read_s1_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : read_s1_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : read_s1_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : read_s1_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : read_s1_payload_roundMode_string = "RMM";
      default : read_s1_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(read_s0_rData_opcode)
      FpuOpcode_LOAD : read_s0_rData_opcode_string = "LOAD    ";
      FpuOpcode_STORE : read_s0_rData_opcode_string = "STORE   ";
      FpuOpcode_MUL : read_s0_rData_opcode_string = "MUL     ";
      FpuOpcode_ADD : read_s0_rData_opcode_string = "ADD     ";
      FpuOpcode_FMA : read_s0_rData_opcode_string = "FMA     ";
      FpuOpcode_I2F : read_s0_rData_opcode_string = "I2F     ";
      FpuOpcode_F2I : read_s0_rData_opcode_string = "F2I     ";
      FpuOpcode_CMP : read_s0_rData_opcode_string = "CMP     ";
      FpuOpcode_DIV : read_s0_rData_opcode_string = "DIV     ";
      FpuOpcode_SQRT : read_s0_rData_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : read_s0_rData_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : read_s0_rData_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : read_s0_rData_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : read_s0_rData_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : read_s0_rData_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : read_s0_rData_opcode_string = "FCVT_X_X";
      default : read_s0_rData_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(read_s0_rData_roundMode)
      FpuRoundMode_RNE : read_s0_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : read_s0_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : read_s0_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : read_s0_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : read_s0_rData_roundMode_string = "RMM";
      default : read_s0_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(read_output_payload_opcode)
      FpuOpcode_LOAD : read_output_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : read_output_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : read_output_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : read_output_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : read_output_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : read_output_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : read_output_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : read_output_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : read_output_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : read_output_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : read_output_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : read_output_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : read_output_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : read_output_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : read_output_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : read_output_payload_opcode_string = "FCVT_X_X";
      default : read_output_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(read_output_payload_roundMode)
      FpuRoundMode_RNE : read_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : read_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : read_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : read_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : read_output_payload_roundMode_string = "RMM";
      default : read_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_input_payload_opcode)
      FpuOpcode_LOAD : decode_input_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : decode_input_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : decode_input_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : decode_input_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : decode_input_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : decode_input_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : decode_input_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : decode_input_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : decode_input_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : decode_input_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : decode_input_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : decode_input_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : decode_input_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : decode_input_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : decode_input_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : decode_input_payload_opcode_string = "FCVT_X_X";
      default : decode_input_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(decode_input_payload_roundMode)
      FpuRoundMode_RNE : decode_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_input_payload_roundMode_string = "RMM";
      default : decode_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_load_payload_roundMode)
      FpuRoundMode_RNE : decode_load_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_load_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_load_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_load_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_load_payload_roundMode_string = "RMM";
      default : decode_load_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_shortPip_payload_opcode)
      FpuOpcode_LOAD : decode_shortPip_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : decode_shortPip_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : decode_shortPip_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : decode_shortPip_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : decode_shortPip_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : decode_shortPip_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : decode_shortPip_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : decode_shortPip_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : decode_shortPip_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : decode_shortPip_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : decode_shortPip_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : decode_shortPip_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : decode_shortPip_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : decode_shortPip_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : decode_shortPip_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : decode_shortPip_payload_opcode_string = "FCVT_X_X";
      default : decode_shortPip_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(decode_shortPip_payload_roundMode)
      FpuRoundMode_RNE : decode_shortPip_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_shortPip_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_shortPip_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_shortPip_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_shortPip_payload_roundMode_string = "RMM";
      default : decode_shortPip_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_divSqrt_payload_roundMode)
      FpuRoundMode_RNE : decode_divSqrt_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_divSqrt_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_divSqrt_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_divSqrt_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_divSqrt_payload_roundMode_string = "RMM";
      default : decode_divSqrt_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_div_payload_roundMode)
      FpuRoundMode_RNE : decode_div_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_div_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_div_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_div_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_div_payload_roundMode_string = "RMM";
      default : decode_div_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_sqrt_payload_roundMode)
      FpuRoundMode_RNE : decode_sqrt_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_sqrt_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_sqrt_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_sqrt_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_sqrt_payload_roundMode_string = "RMM";
      default : decode_sqrt_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_mul_payload_roundMode)
      FpuRoundMode_RNE : decode_mul_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_mul_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_mul_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_mul_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_mul_payload_roundMode_string = "RMM";
      default : decode_mul_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_divSqrtToMul_payload_roundMode)
      FpuRoundMode_RNE : decode_divSqrtToMul_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_divSqrtToMul_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_divSqrtToMul_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_divSqrtToMul_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_divSqrtToMul_payload_roundMode_string = "RMM";
      default : decode_divSqrtToMul_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_add_payload_roundMode)
      FpuRoundMode_RNE : decode_add_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_add_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_add_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_add_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_add_payload_roundMode_string = "RMM";
      default : decode_add_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_mulToAdd_payload_roundMode)
      FpuRoundMode_RNE : decode_mulToAdd_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_mulToAdd_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_mulToAdd_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_mulToAdd_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_mulToAdd_payload_roundMode_string = "RMM";
      default : decode_mulToAdd_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_load_s2mPipe_payload_roundMode)
      FpuRoundMode_RNE : decode_load_s2mPipe_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_load_s2mPipe_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_load_s2mPipe_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_load_s2mPipe_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_load_s2mPipe_payload_roundMode_string = "RMM";
      default : decode_load_s2mPipe_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_load_rData_roundMode)
      FpuRoundMode_RNE : decode_load_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_load_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_load_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_load_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_load_rData_roundMode_string = "RMM";
      default : decode_load_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(_zz_decode_load_s2mPipe_payload_roundMode)
      FpuRoundMode_RNE : _zz_decode_load_s2mPipe_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : _zz_decode_load_s2mPipe_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : _zz_decode_load_s2mPipe_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : _zz_decode_load_s2mPipe_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : _zz_decode_load_s2mPipe_payload_roundMode_string = "RMM";
      default : _zz_decode_load_s2mPipe_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_load_s2mPipe_m2sPipe_payload_roundMode)
      FpuRoundMode_RNE : decode_load_s2mPipe_m2sPipe_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_load_s2mPipe_m2sPipe_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_load_s2mPipe_m2sPipe_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_load_s2mPipe_m2sPipe_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_load_s2mPipe_m2sPipe_payload_roundMode_string = "RMM";
      default : decode_load_s2mPipe_m2sPipe_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_load_s2mPipe_rData_roundMode)
      FpuRoundMode_RNE : decode_load_s2mPipe_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_load_s2mPipe_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_load_s2mPipe_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_load_s2mPipe_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_load_s2mPipe_rData_roundMode_string = "RMM";
      default : decode_load_s2mPipe_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(load_s0_input_payload_roundMode)
      FpuRoundMode_RNE : load_s0_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : load_s0_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : load_s0_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : load_s0_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : load_s0_input_payload_roundMode_string = "RMM";
      default : load_s0_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_load_s2mPipe_m2sPipe_rData_roundMode)
      FpuRoundMode_RNE : decode_load_s2mPipe_m2sPipe_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_load_s2mPipe_m2sPipe_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_load_s2mPipe_m2sPipe_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_load_s2mPipe_m2sPipe_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_load_s2mPipe_m2sPipe_rData_roundMode_string = "RMM";
      default : decode_load_s2mPipe_m2sPipe_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(load_s0_filtred_0_payload_opcode)
      FpuOpcode_LOAD : load_s0_filtred_0_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : load_s0_filtred_0_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : load_s0_filtred_0_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : load_s0_filtred_0_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : load_s0_filtred_0_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : load_s0_filtred_0_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : load_s0_filtred_0_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : load_s0_filtred_0_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : load_s0_filtred_0_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : load_s0_filtred_0_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : load_s0_filtred_0_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : load_s0_filtred_0_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : load_s0_filtred_0_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : load_s0_filtred_0_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : load_s0_filtred_0_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : load_s0_filtred_0_payload_opcode_string = "FCVT_X_X";
      default : load_s0_filtred_0_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(load_s0_output_payload_roundMode)
      FpuRoundMode_RNE : load_s0_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : load_s0_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : load_s0_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : load_s0_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : load_s0_output_payload_roundMode_string = "RMM";
      default : load_s0_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(load_s1_input_payload_roundMode)
      FpuRoundMode_RNE : load_s1_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : load_s1_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : load_s1_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : load_s1_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : load_s1_input_payload_roundMode_string = "RMM";
      default : load_s1_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(load_s0_output_rData_roundMode)
      FpuRoundMode_RNE : load_s0_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : load_s0_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : load_s0_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : load_s0_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : load_s0_output_rData_roundMode_string = "RMM";
      default : load_s0_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(load_s1_output_payload_roundMode)
      FpuRoundMode_RNE : load_s1_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : load_s1_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : load_s1_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : load_s1_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : load_s1_output_payload_roundMode_string = "RMM";
      default : load_s1_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(shortPip_input_payload_opcode)
      FpuOpcode_LOAD : shortPip_input_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : shortPip_input_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : shortPip_input_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : shortPip_input_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : shortPip_input_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : shortPip_input_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : shortPip_input_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : shortPip_input_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : shortPip_input_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : shortPip_input_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : shortPip_input_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : shortPip_input_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : shortPip_input_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : shortPip_input_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : shortPip_input_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : shortPip_input_payload_opcode_string = "FCVT_X_X";
      default : shortPip_input_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(shortPip_input_payload_roundMode)
      FpuRoundMode_RNE : shortPip_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : shortPip_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : shortPip_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : shortPip_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : shortPip_input_payload_roundMode_string = "RMM";
      default : shortPip_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_shortPip_rData_opcode)
      FpuOpcode_LOAD : decode_shortPip_rData_opcode_string = "LOAD    ";
      FpuOpcode_STORE : decode_shortPip_rData_opcode_string = "STORE   ";
      FpuOpcode_MUL : decode_shortPip_rData_opcode_string = "MUL     ";
      FpuOpcode_ADD : decode_shortPip_rData_opcode_string = "ADD     ";
      FpuOpcode_FMA : decode_shortPip_rData_opcode_string = "FMA     ";
      FpuOpcode_I2F : decode_shortPip_rData_opcode_string = "I2F     ";
      FpuOpcode_F2I : decode_shortPip_rData_opcode_string = "F2I     ";
      FpuOpcode_CMP : decode_shortPip_rData_opcode_string = "CMP     ";
      FpuOpcode_DIV : decode_shortPip_rData_opcode_string = "DIV     ";
      FpuOpcode_SQRT : decode_shortPip_rData_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : decode_shortPip_rData_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : decode_shortPip_rData_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : decode_shortPip_rData_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : decode_shortPip_rData_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : decode_shortPip_rData_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : decode_shortPip_rData_opcode_string = "FCVT_X_X";
      default : decode_shortPip_rData_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(decode_shortPip_rData_roundMode)
      FpuRoundMode_RNE : decode_shortPip_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_shortPip_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_shortPip_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_shortPip_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_shortPip_rData_roundMode_string = "RMM";
      default : decode_shortPip_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(shortPip_rfOutput_payload_roundMode)
      FpuRoundMode_RNE : shortPip_rfOutput_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : shortPip_rfOutput_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : shortPip_rfOutput_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : shortPip_rfOutput_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : shortPip_rfOutput_payload_roundMode_string = "RMM";
      default : shortPip_rfOutput_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(shortPip_output_payload_roundMode)
      FpuRoundMode_RNE : shortPip_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : shortPip_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : shortPip_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : shortPip_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : shortPip_output_payload_roundMode_string = "RMM";
      default : shortPip_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_preMul_input_payload_roundMode)
      FpuRoundMode_RNE : mul_preMul_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_preMul_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_preMul_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_preMul_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_preMul_input_payload_roundMode_string = "RMM";
      default : mul_preMul_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_mul_rData_roundMode)
      FpuRoundMode_RNE : decode_mul_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_mul_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_mul_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_mul_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_mul_rData_roundMode_string = "RMM";
      default : decode_mul_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_preMul_output_payload_roundMode)
      FpuRoundMode_RNE : mul_preMul_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_preMul_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_preMul_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_preMul_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_preMul_output_payload_roundMode_string = "RMM";
      default : mul_preMul_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_mul_input_payload_roundMode)
      FpuRoundMode_RNE : mul_mul_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_mul_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_mul_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_mul_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_mul_input_payload_roundMode_string = "RMM";
      default : mul_mul_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_preMul_output_rData_roundMode)
      FpuRoundMode_RNE : mul_preMul_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_preMul_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_preMul_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_preMul_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_preMul_output_rData_roundMode_string = "RMM";
      default : mul_preMul_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_mul_output_payload_roundMode)
      FpuRoundMode_RNE : mul_mul_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_mul_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_mul_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_mul_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_mul_output_payload_roundMode_string = "RMM";
      default : mul_mul_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_sum1_input_payload_roundMode)
      FpuRoundMode_RNE : mul_sum1_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_sum1_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_sum1_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_sum1_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_sum1_input_payload_roundMode_string = "RMM";
      default : mul_sum1_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_mul_output_rData_roundMode)
      FpuRoundMode_RNE : mul_mul_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_mul_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_mul_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_mul_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_mul_output_rData_roundMode_string = "RMM";
      default : mul_mul_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_sum1_output_payload_roundMode)
      FpuRoundMode_RNE : mul_sum1_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_sum1_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_sum1_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_sum1_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_sum1_output_payload_roundMode_string = "RMM";
      default : mul_sum1_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_sum2_input_payload_roundMode)
      FpuRoundMode_RNE : mul_sum2_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_sum2_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_sum2_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_sum2_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_sum2_input_payload_roundMode_string = "RMM";
      default : mul_sum2_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_sum1_output_rData_roundMode)
      FpuRoundMode_RNE : mul_sum1_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_sum1_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_sum1_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_sum1_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_sum1_output_rData_roundMode_string = "RMM";
      default : mul_sum1_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_sum2_output_payload_roundMode)
      FpuRoundMode_RNE : mul_sum2_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_sum2_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_sum2_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_sum2_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_sum2_output_payload_roundMode_string = "RMM";
      default : mul_sum2_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_norm_input_payload_roundMode)
      FpuRoundMode_RNE : mul_norm_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_norm_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_norm_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_norm_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_norm_input_payload_roundMode_string = "RMM";
      default : mul_norm_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_sum2_output_rData_roundMode)
      FpuRoundMode_RNE : mul_sum2_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_sum2_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_sum2_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_sum2_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_sum2_output_rData_roundMode_string = "RMM";
      default : mul_sum2_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_result_output_payload_roundMode)
      FpuRoundMode_RNE : mul_result_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_result_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_result_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_result_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_result_output_payload_roundMode_string = "RMM";
      default : mul_result_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_result_mulToAdd_payload_roundMode)
      FpuRoundMode_RNE : mul_result_mulToAdd_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_result_mulToAdd_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_result_mulToAdd_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_result_mulToAdd_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_result_mulToAdd_payload_roundMode_string = "RMM";
      default : mul_result_mulToAdd_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_result_mulToAdd_m2sPipe_payload_roundMode)
      FpuRoundMode_RNE : mul_result_mulToAdd_m2sPipe_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_result_mulToAdd_m2sPipe_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_result_mulToAdd_m2sPipe_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_result_mulToAdd_m2sPipe_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_result_mulToAdd_m2sPipe_payload_roundMode_string = "RMM";
      default : mul_result_mulToAdd_m2sPipe_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(mul_result_mulToAdd_rData_roundMode)
      FpuRoundMode_RNE : mul_result_mulToAdd_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : mul_result_mulToAdd_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : mul_result_mulToAdd_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : mul_result_mulToAdd_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : mul_result_mulToAdd_rData_roundMode_string = "RMM";
      default : mul_result_mulToAdd_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(div_input_payload_roundMode)
      FpuRoundMode_RNE : div_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : div_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : div_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : div_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : div_input_payload_roundMode_string = "RMM";
      default : div_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_div_rData_roundMode)
      FpuRoundMode_RNE : decode_div_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_div_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_div_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_div_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_div_rData_roundMode_string = "RMM";
      default : decode_div_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(div_output_payload_roundMode)
      FpuRoundMode_RNE : div_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : div_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : div_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : div_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : div_output_payload_roundMode_string = "RMM";
      default : div_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(sqrt_input_payload_roundMode)
      FpuRoundMode_RNE : sqrt_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : sqrt_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : sqrt_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : sqrt_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : sqrt_input_payload_roundMode_string = "RMM";
      default : sqrt_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(decode_sqrt_rData_roundMode)
      FpuRoundMode_RNE : decode_sqrt_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : decode_sqrt_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : decode_sqrt_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : decode_sqrt_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : decode_sqrt_rData_roundMode_string = "RMM";
      default : decode_sqrt_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(sqrt_output_payload_roundMode)
      FpuRoundMode_RNE : sqrt_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : sqrt_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : sqrt_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : sqrt_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : sqrt_output_payload_roundMode_string = "RMM";
      default : sqrt_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_preShifter_input_payload_roundMode)
      FpuRoundMode_RNE : add_preShifter_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_preShifter_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_preShifter_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_preShifter_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_preShifter_input_payload_roundMode_string = "RMM";
      default : add_preShifter_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_preShifter_output_payload_roundMode)
      FpuRoundMode_RNE : add_preShifter_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_preShifter_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_preShifter_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_preShifter_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_preShifter_output_payload_roundMode_string = "RMM";
      default : add_preShifter_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_shifter_input_payload_roundMode)
      FpuRoundMode_RNE : add_shifter_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_shifter_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_shifter_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_shifter_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_shifter_input_payload_roundMode_string = "RMM";
      default : add_shifter_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_preShifter_output_rData_roundMode)
      FpuRoundMode_RNE : add_preShifter_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_preShifter_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_preShifter_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_preShifter_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_preShifter_output_rData_roundMode_string = "RMM";
      default : add_preShifter_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_shifter_output_payload_roundMode)
      FpuRoundMode_RNE : add_shifter_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_shifter_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_shifter_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_shifter_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_shifter_output_payload_roundMode_string = "RMM";
      default : add_shifter_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_math_input_payload_roundMode)
      FpuRoundMode_RNE : add_math_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_math_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_math_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_math_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_math_input_payload_roundMode_string = "RMM";
      default : add_math_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_shifter_output_rData_roundMode)
      FpuRoundMode_RNE : add_shifter_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_shifter_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_shifter_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_shifter_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_shifter_output_rData_roundMode_string = "RMM";
      default : add_shifter_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_math_output_payload_roundMode)
      FpuRoundMode_RNE : add_math_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_math_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_math_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_math_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_math_output_payload_roundMode_string = "RMM";
      default : add_math_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_oh_input_payload_roundMode)
      FpuRoundMode_RNE : add_oh_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_oh_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_oh_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_oh_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_oh_input_payload_roundMode_string = "RMM";
      default : add_oh_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_math_output_rData_roundMode)
      FpuRoundMode_RNE : add_math_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_math_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_math_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_math_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_math_output_rData_roundMode_string = "RMM";
      default : add_math_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_oh_output_payload_roundMode)
      FpuRoundMode_RNE : add_oh_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_oh_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_oh_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_oh_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_oh_output_payload_roundMode_string = "RMM";
      default : add_oh_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_norm_input_payload_roundMode)
      FpuRoundMode_RNE : add_norm_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_norm_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_norm_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_norm_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_norm_input_payload_roundMode_string = "RMM";
      default : add_norm_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_oh_output_rData_roundMode)
      FpuRoundMode_RNE : add_oh_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_oh_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_oh_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_oh_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_oh_output_rData_roundMode_string = "RMM";
      default : add_oh_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_norm_output_payload_roundMode)
      FpuRoundMode_RNE : add_norm_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_norm_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_norm_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_norm_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_norm_output_payload_roundMode_string = "RMM";
      default : add_norm_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_result_input_payload_roundMode)
      FpuRoundMode_RNE : add_result_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_result_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_result_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_result_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_result_input_payload_roundMode_string = "RMM";
      default : add_result_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(add_result_output_payload_roundMode)
      FpuRoundMode_RNE : add_result_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : add_result_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : add_result_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : add_result_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : add_result_output_payload_roundMode_string = "RMM";
      default : add_result_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(load_s1_output_m2sPipe_payload_roundMode)
      FpuRoundMode_RNE : load_s1_output_m2sPipe_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : load_s1_output_m2sPipe_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : load_s1_output_m2sPipe_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : load_s1_output_m2sPipe_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : load_s1_output_m2sPipe_payload_roundMode_string = "RMM";
      default : load_s1_output_m2sPipe_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(load_s1_output_rData_roundMode)
      FpuRoundMode_RNE : load_s1_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : load_s1_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : load_s1_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : load_s1_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : load_s1_output_rData_roundMode_string = "RMM";
      default : load_s1_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(shortPip_output_m2sPipe_payload_roundMode)
      FpuRoundMode_RNE : shortPip_output_m2sPipe_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : shortPip_output_m2sPipe_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : shortPip_output_m2sPipe_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : shortPip_output_m2sPipe_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : shortPip_output_m2sPipe_payload_roundMode_string = "RMM";
      default : shortPip_output_m2sPipe_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(shortPip_output_rData_roundMode)
      FpuRoundMode_RNE : shortPip_output_rData_roundMode_string = "RNE";
      FpuRoundMode_RTZ : shortPip_output_rData_roundMode_string = "RTZ";
      FpuRoundMode_RDN : shortPip_output_rData_roundMode_string = "RDN";
      FpuRoundMode_RUP : shortPip_output_rData_roundMode_string = "RUP";
      FpuRoundMode_RMM : shortPip_output_rData_roundMode_string = "RMM";
      default : shortPip_output_rData_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode)
      FpuRoundMode_RNE : FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode_string = "RMM";
      default : FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(merge_arbitrated_payload_roundMode)
      FpuRoundMode_RNE : merge_arbitrated_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : merge_arbitrated_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : merge_arbitrated_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : merge_arbitrated_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : merge_arbitrated_payload_roundMode_string = "RMM";
      default : merge_arbitrated_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(roundFront_input_payload_roundMode)
      FpuRoundMode_RNE : roundFront_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : roundFront_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : roundFront_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : roundFront_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : roundFront_input_payload_roundMode_string = "RMM";
      default : roundFront_input_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(roundFront_output_payload_roundMode)
      FpuRoundMode_RNE : roundFront_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : roundFront_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : roundFront_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : roundFront_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : roundFront_output_payload_roundMode_string = "RMM";
      default : roundFront_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(roundBack_input_payload_roundMode)
      FpuRoundMode_RNE : roundBack_input_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : roundBack_input_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : roundBack_input_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : roundBack_input_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : roundBack_input_payload_roundMode_string = "RMM";
      default : roundBack_input_payload_roundMode_string = "???";
    endcase
  end
  `endif

  always @(*) begin
    _zz_1 = 1'b0;
    if(writeback_port_valid) begin
      _zz_1 = 1'b1;
    end
  end

  always @(*) begin
    add_shifter_yMantissa_5 = add_shifter_yMantissa_4;
    add_shifter_yMantissa_5 = (add_shifter_shiftBy[0] ? _zz_add_shifter_yMantissa_5 : add_shifter_yMantissa_4);
    if(add_shifter_passThrough) begin
      add_shifter_yMantissa_5 = 26'h0;
    end
  end

  always @(*) begin
    add_shifter_yMantissa_4 = add_shifter_yMantissa_3;
    add_shifter_yMantissa_4 = (add_shifter_shiftBy[1] ? _zz_add_shifter_yMantissa_4 : add_shifter_yMantissa_3);
  end

  always @(*) begin
    add_shifter_yMantissa_3 = add_shifter_yMantissa_2;
    add_shifter_yMantissa_3 = (add_shifter_shiftBy[2] ? _zz_add_shifter_yMantissa_3 : add_shifter_yMantissa_2);
  end

  always @(*) begin
    add_shifter_yMantissa_2 = add_shifter_yMantissa_1;
    add_shifter_yMantissa_2 = (add_shifter_shiftBy[3] ? _zz_add_shifter_yMantissa_2 : add_shifter_yMantissa_1);
  end

  always @(*) begin
    add_shifter_yMantissa_1 = add_shifter_yMantissa;
    add_shifter_yMantissa_1 = (add_shifter_shiftBy[4] ? _zz_add_shifter_yMantissa_1 : add_shifter_yMantissa);
  end

  always @(*) begin
    shortPip_fsm_shift_input_6 = shortPip_fsm_shift_input_5;
    shortPip_fsm_shift_input_6 = (shortPip_fsm_shift_by[0] ? _zz_shortPip_fsm_shift_input_6 : shortPip_fsm_shift_input_5);
  end

  always @(*) begin
    shortPip_fsm_shift_input_5 = shortPip_fsm_shift_input_4;
    shortPip_fsm_shift_input_5 = (shortPip_fsm_shift_by[1] ? _zz_shortPip_fsm_shift_input_5 : shortPip_fsm_shift_input_4);
  end

  always @(*) begin
    shortPip_fsm_shift_input_4 = shortPip_fsm_shift_input_3;
    shortPip_fsm_shift_input_4 = (shortPip_fsm_shift_by[2] ? _zz_shortPip_fsm_shift_input_4 : shortPip_fsm_shift_input_3);
  end

  always @(*) begin
    shortPip_fsm_shift_input_3 = shortPip_fsm_shift_input_2;
    shortPip_fsm_shift_input_3 = (shortPip_fsm_shift_by[3] ? _zz_shortPip_fsm_shift_input_3 : shortPip_fsm_shift_input_2);
  end

  always @(*) begin
    shortPip_fsm_shift_input_2 = shortPip_fsm_shift_input_1;
    shortPip_fsm_shift_input_2 = (shortPip_fsm_shift_by[4] ? _zz_shortPip_fsm_shift_input_2 : shortPip_fsm_shift_input_1);
  end

  always @(*) begin
    shortPip_fsm_shift_input_1 = shortPip_fsm_shift_input;
    shortPip_fsm_shift_input_1 = (shortPip_fsm_shift_by[5] ? _zz_shortPip_fsm_shift_input_1 : shortPip_fsm_shift_input);
  end

  always @(*) begin
    load_s1_fsm_shift_input_5 = load_s1_fsm_shift_input_4;
    load_s1_fsm_shift_input_5 = (load_s1_fsm_shift_by[4] ? _zz_load_s1_fsm_shift_input_5 : load_s1_fsm_shift_input_4);
  end

  always @(*) begin
    load_s1_fsm_shift_input_4 = load_s1_fsm_shift_input_3;
    load_s1_fsm_shift_input_4 = (load_s1_fsm_shift_by[3] ? _zz_load_s1_fsm_shift_input_4 : load_s1_fsm_shift_input_3);
  end

  always @(*) begin
    load_s1_fsm_shift_input_3 = load_s1_fsm_shift_input_2;
    load_s1_fsm_shift_input_3 = (load_s1_fsm_shift_by[2] ? _zz_load_s1_fsm_shift_input_3 : load_s1_fsm_shift_input_2);
  end

  always @(*) begin
    load_s1_fsm_shift_input_2 = load_s1_fsm_shift_input_1;
    load_s1_fsm_shift_input_2 = (load_s1_fsm_shift_by[1] ? _zz_load_s1_fsm_shift_input_2 : load_s1_fsm_shift_input_1);
  end

  always @(*) begin
    load_s1_fsm_shift_input_1 = load_s1_fsm_shift_input;
    load_s1_fsm_shift_input_1 = (load_s1_fsm_shift_by[0] ? _zz_load_s1_fsm_shift_input_1 : load_s1_fsm_shift_input);
  end

  always @(*) begin
    _zz_2 = 1'b0;
    if(commitLogic_0_input_valid) begin
      _zz_2 = 1'b1;
    end
  end

  always @(*) begin
    _zz_3 = 1'b0;
    if(rf_scoreboards_0_hitWrite_valid) begin
      _zz_3 = 1'b1;
    end
  end

  always @(*) begin
    _zz_4 = 1'b0;
    if(rf_scoreboards_0_targetWrite_valid) begin
      _zz_4 = 1'b1;
    end
  end

  assign rf_init_done = rf_init_counter[5];
  assign when_FpuCore_l163 = (! rf_init_done);
  always @(*) begin
    rf_scoreboards_0_targetWrite_valid = (! rf_init_done);
    if(when_FpuCore_l265) begin
      rf_scoreboards_0_targetWrite_valid = 1'b1;
    end
  end

  always @(*) begin
    rf_scoreboards_0_targetWrite_payload_address = rf_init_counter[4:0];
    if(when_FpuCore_l261) begin
      rf_scoreboards_0_targetWrite_payload_address = scheduler_0_input_payload_rd;
    end
  end

  always @(*) begin
    rf_scoreboards_0_targetWrite_payload_data = 1'b0;
    if(when_FpuCore_l261) begin
      rf_scoreboards_0_targetWrite_payload_data = (! scheduler_0_rfTargets_3);
    end
  end

  always @(*) begin
    rf_scoreboards_0_hitWrite_valid = (! rf_init_done);
    if(writeback_input_valid) begin
      if(when_FpuCore_l1689) begin
        rf_scoreboards_0_hitWrite_valid = 1'b1;
      end
    end
  end

  always @(*) begin
    rf_scoreboards_0_hitWrite_payload_address = rf_init_counter[4:0];
    if(writeback_input_valid) begin
      rf_scoreboards_0_hitWrite_payload_address = writeback_input_payload_rd;
    end
  end

  always @(*) begin
    rf_scoreboards_0_hitWrite_payload_data = 1'b0;
    if(writeback_input_valid) begin
      rf_scoreboards_0_hitWrite_payload_data = (! rf_scoreboards_0_hit_spinal_port5[0]);
    end
  end

  assign io_port_0_commit_ready = streamFork_1_io_input_ready;
  assign commitFork_load_0_valid = streamFork_1_io_outputs_0_valid;
  assign commitFork_load_0_payload_opcode = streamFork_1_io_outputs_0_payload_opcode;
  assign commitFork_load_0_payload_rd = streamFork_1_io_outputs_0_payload_rd;
  assign commitFork_load_0_payload_write = streamFork_1_io_outputs_0_payload_write;
  assign commitFork_load_0_payload_value = streamFork_1_io_outputs_0_payload_value;
  assign FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_valid = (streamFork_1_io_outputs_1_valid || (! FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN));
  assign _zz_payload_opcode = (FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN ? streamFork_1_io_outputs_1_payload_opcode : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode);
  assign FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode = _zz_payload_opcode;
  assign FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_rd = (FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN ? streamFork_1_io_outputs_1_payload_rd : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_rd);
  assign FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_write = (FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN ? streamFork_1_io_outputs_1_payload_write : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_write);
  assign FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_value = (FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN ? streamFork_1_io_outputs_1_payload_value : FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_value);
  assign commitFork_commit_0_valid = FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_valid;
  assign FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_ready = commitFork_commit_0_ready;
  assign commitFork_commit_0_payload_opcode = FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_opcode;
  assign commitFork_commit_0_payload_rd = FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_rd;
  assign commitFork_commit_0_payload_write = FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_write;
  assign commitFork_commit_0_payload_value = FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_payload_value;
  assign commitLogic_0_pending_full = (&commitLogic_0_pending_counter);
  assign commitLogic_0_pending_notEmpty = (|commitLogic_0_pending_counter);
  always @(*) begin
    commitLogic_0_pending_inc = 1'b0;
    if(when_FpuCore_l265) begin
      commitLogic_0_pending_inc = 1'b1;
    end
  end

  always @(*) begin
    commitLogic_0_pending_dec = 1'b0;
    if(commitLogic_0_input_valid) begin
      commitLogic_0_pending_dec = 1'b1;
    end
  end

  assign commitLogic_0_add_full = (&commitLogic_0_add_counter);
  assign commitLogic_0_add_notEmpty = (|commitLogic_0_add_counter);
  always @(*) begin
    commitLogic_0_add_inc = 1'b0;
    if(commitLogic_0_input_valid) begin
      if(when_FpuCore_l208) begin
        commitLogic_0_add_inc = 1'b1;
      end
    end
  end

  always @(*) begin
    commitLogic_0_add_dec = 1'b0;
    if(when_FpuCore_l221_4) begin
      commitLogic_0_add_dec = 1'b1;
    end
  end

  assign commitLogic_0_mul_full = (&commitLogic_0_mul_counter);
  assign commitLogic_0_mul_notEmpty = (|commitLogic_0_mul_counter);
  always @(*) begin
    commitLogic_0_mul_inc = 1'b0;
    if(commitLogic_0_input_valid) begin
      if(when_FpuCore_l209) begin
        commitLogic_0_mul_inc = 1'b1;
      end
    end
  end

  always @(*) begin
    commitLogic_0_mul_dec = 1'b0;
    if(when_FpuCore_l221_1) begin
      commitLogic_0_mul_dec = 1'b1;
    end
  end

  assign commitLogic_0_div_full = (&commitLogic_0_div_counter);
  assign commitLogic_0_div_notEmpty = (|commitLogic_0_div_counter);
  always @(*) begin
    commitLogic_0_div_inc = 1'b0;
    if(commitLogic_0_input_valid) begin
      if(when_FpuCore_l210) begin
        commitLogic_0_div_inc = 1'b1;
      end
    end
  end

  always @(*) begin
    commitLogic_0_div_dec = 1'b0;
    if(when_FpuCore_l221_2) begin
      commitLogic_0_div_dec = 1'b1;
    end
  end

  assign commitLogic_0_sqrt_full = (&commitLogic_0_sqrt_counter);
  assign commitLogic_0_sqrt_notEmpty = (|commitLogic_0_sqrt_counter);
  always @(*) begin
    commitLogic_0_sqrt_inc = 1'b0;
    if(commitLogic_0_input_valid) begin
      if(when_FpuCore_l211) begin
        commitLogic_0_sqrt_inc = 1'b1;
      end
    end
  end

  always @(*) begin
    commitLogic_0_sqrt_dec = 1'b0;
    if(when_FpuCore_l221_3) begin
      commitLogic_0_sqrt_dec = 1'b1;
    end
  end

  assign commitLogic_0_short_full = (&commitLogic_0_short_counter);
  assign commitLogic_0_short_notEmpty = (|commitLogic_0_short_counter);
  always @(*) begin
    commitLogic_0_short_inc = 1'b0;
    if(commitLogic_0_input_valid) begin
      if(when_FpuCore_l212) begin
        commitLogic_0_short_inc = 1'b1;
      end
    end
  end

  always @(*) begin
    commitLogic_0_short_dec = 1'b0;
    if(when_FpuCore_l221) begin
      commitLogic_0_short_dec = 1'b1;
    end
  end

  assign _zz_commitFork_commit_0_ready = (! ((|{commitLogic_0_short_full,{commitLogic_0_sqrt_full,{commitLogic_0_div_full,{commitLogic_0_mul_full,commitLogic_0_add_full}}}}) || (! commitLogic_0_pending_notEmpty)));
  assign commitFork_commit_0_ready = (1'b1 && _zz_commitFork_commit_0_ready);
  assign _zz_commitLogic_0_input_payload_opcode = commitFork_commit_0_payload_opcode;
  assign commitLogic_0_input_valid = (commitFork_commit_0_valid && _zz_commitFork_commit_0_ready);
  assign commitLogic_0_input_payload_opcode = _zz_commitLogic_0_input_payload_opcode;
  assign commitLogic_0_input_payload_rd = commitFork_commit_0_payload_rd;
  assign commitLogic_0_input_payload_write = commitFork_commit_0_payload_write;
  assign commitLogic_0_input_payload_value = commitFork_commit_0_payload_value;
  assign when_FpuCore_l208 = (|(commitLogic_0_input_payload_opcode == FpuOpcode_ADD));
  assign when_FpuCore_l209 = (|{(commitLogic_0_input_payload_opcode == FpuOpcode_FMA),(commitLogic_0_input_payload_opcode == FpuOpcode_MUL)});
  assign when_FpuCore_l210 = (|(commitLogic_0_input_payload_opcode == FpuOpcode_DIV));
  assign when_FpuCore_l211 = (|(commitLogic_0_input_payload_opcode == FpuOpcode_SQRT));
  assign when_FpuCore_l212 = (|{(commitLogic_0_input_payload_opcode == FpuOpcode_FCVT_X_X),{(commitLogic_0_input_payload_opcode == FpuOpcode_MIN_MAX),(commitLogic_0_input_payload_opcode == FpuOpcode_SGNJ)}});
  assign io_port_0_cmd_ready = io_port_0_cmd_rValidN;
  assign scheduler_0_input_valid = (io_port_0_cmd_valid || (! io_port_0_cmd_rValidN));
  assign _zz_scheduler_0_input_payload_opcode = (io_port_0_cmd_rValidN ? io_port_0_cmd_payload_opcode : io_port_0_cmd_rData_opcode);
  assign _zz_scheduler_0_input_payload_format = (io_port_0_cmd_rValidN ? io_port_0_cmd_payload_format : io_port_0_cmd_rData_format);
  assign _zz_scheduler_0_input_payload_roundMode = (io_port_0_cmd_rValidN ? io_port_0_cmd_payload_roundMode : io_port_0_cmd_rData_roundMode);
  assign scheduler_0_input_payload_opcode = _zz_scheduler_0_input_payload_opcode;
  assign scheduler_0_input_payload_arg = (io_port_0_cmd_rValidN ? io_port_0_cmd_payload_arg : io_port_0_cmd_rData_arg);
  assign scheduler_0_input_payload_rs1 = (io_port_0_cmd_rValidN ? io_port_0_cmd_payload_rs1 : io_port_0_cmd_rData_rs1);
  assign scheduler_0_input_payload_rs2 = (io_port_0_cmd_rValidN ? io_port_0_cmd_payload_rs2 : io_port_0_cmd_rData_rs2);
  assign scheduler_0_input_payload_rs3 = (io_port_0_cmd_rValidN ? io_port_0_cmd_payload_rs3 : io_port_0_cmd_rData_rs3);
  assign scheduler_0_input_payload_rd = (io_port_0_cmd_rValidN ? io_port_0_cmd_payload_rd : io_port_0_cmd_rData_rd);
  assign scheduler_0_input_payload_format = _zz_scheduler_0_input_payload_format;
  assign scheduler_0_input_payload_roundMode = _zz_scheduler_0_input_payload_roundMode;
  always @(*) begin
    scheduler_0_useRs1 = 1'b0;
    case(scheduler_0_input_payload_opcode)
      FpuOpcode_LOAD : begin
      end
      FpuOpcode_STORE : begin
      end
      FpuOpcode_ADD : begin
        scheduler_0_useRs1 = 1'b1;
      end
      FpuOpcode_MUL : begin
        scheduler_0_useRs1 = 1'b1;
      end
      FpuOpcode_DIV : begin
        scheduler_0_useRs1 = 1'b1;
      end
      FpuOpcode_SQRT : begin
        scheduler_0_useRs1 = 1'b1;
      end
      FpuOpcode_FMA : begin
        scheduler_0_useRs1 = 1'b1;
      end
      FpuOpcode_I2F : begin
      end
      FpuOpcode_F2I : begin
        scheduler_0_useRs1 = 1'b1;
      end
      FpuOpcode_MIN_MAX : begin
        scheduler_0_useRs1 = 1'b1;
      end
      FpuOpcode_CMP : begin
        scheduler_0_useRs1 = 1'b1;
      end
      FpuOpcode_SGNJ : begin
        scheduler_0_useRs1 = 1'b1;
      end
      FpuOpcode_FMV_X_W : begin
        scheduler_0_useRs1 = 1'b1;
      end
      FpuOpcode_FMV_W_X : begin
      end
      FpuOpcode_FCLASS : begin
        scheduler_0_useRs1 = 1'b1;
      end
      default : begin
        scheduler_0_useRs1 = 1'b1;
      end
    endcase
  end

  always @(*) begin
    scheduler_0_useRs2 = 1'b0;
    case(scheduler_0_input_payload_opcode)
      FpuOpcode_LOAD : begin
      end
      FpuOpcode_STORE : begin
        scheduler_0_useRs2 = 1'b1;
      end
      FpuOpcode_ADD : begin
        scheduler_0_useRs2 = 1'b1;
      end
      FpuOpcode_MUL : begin
        scheduler_0_useRs2 = 1'b1;
      end
      FpuOpcode_DIV : begin
        scheduler_0_useRs2 = 1'b1;
      end
      FpuOpcode_SQRT : begin
      end
      FpuOpcode_FMA : begin
        scheduler_0_useRs2 = 1'b1;
      end
      FpuOpcode_I2F : begin
      end
      FpuOpcode_F2I : begin
      end
      FpuOpcode_MIN_MAX : begin
        scheduler_0_useRs2 = 1'b1;
      end
      FpuOpcode_CMP : begin
        scheduler_0_useRs2 = 1'b1;
      end
      FpuOpcode_SGNJ : begin
        scheduler_0_useRs2 = 1'b1;
      end
      FpuOpcode_FMV_X_W : begin
      end
      FpuOpcode_FMV_W_X : begin
      end
      FpuOpcode_FCLASS : begin
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    scheduler_0_useRs3 = 1'b0;
    case(scheduler_0_input_payload_opcode)
      FpuOpcode_LOAD : begin
      end
      FpuOpcode_STORE : begin
      end
      FpuOpcode_ADD : begin
      end
      FpuOpcode_MUL : begin
      end
      FpuOpcode_DIV : begin
      end
      FpuOpcode_SQRT : begin
      end
      FpuOpcode_FMA : begin
        scheduler_0_useRs3 = 1'b1;
      end
      FpuOpcode_I2F : begin
      end
      FpuOpcode_F2I : begin
      end
      FpuOpcode_MIN_MAX : begin
      end
      FpuOpcode_CMP : begin
      end
      FpuOpcode_SGNJ : begin
      end
      FpuOpcode_FMV_X_W : begin
      end
      FpuOpcode_FMV_W_X : begin
      end
      FpuOpcode_FCLASS : begin
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    scheduler_0_useRd = 1'b0;
    case(scheduler_0_input_payload_opcode)
      FpuOpcode_LOAD : begin
        scheduler_0_useRd = 1'b1;
      end
      FpuOpcode_STORE : begin
      end
      FpuOpcode_ADD : begin
        scheduler_0_useRd = 1'b1;
      end
      FpuOpcode_MUL : begin
        scheduler_0_useRd = 1'b1;
      end
      FpuOpcode_DIV : begin
        scheduler_0_useRd = 1'b1;
      end
      FpuOpcode_SQRT : begin
        scheduler_0_useRd = 1'b1;
      end
      FpuOpcode_FMA : begin
        scheduler_0_useRd = 1'b1;
      end
      FpuOpcode_I2F : begin
        scheduler_0_useRd = 1'b1;
      end
      FpuOpcode_F2I : begin
      end
      FpuOpcode_MIN_MAX : begin
        scheduler_0_useRd = 1'b1;
      end
      FpuOpcode_CMP : begin
      end
      FpuOpcode_SGNJ : begin
        scheduler_0_useRd = 1'b1;
      end
      FpuOpcode_FMV_X_W : begin
      end
      FpuOpcode_FMV_W_X : begin
        scheduler_0_useRd = 1'b1;
      end
      FpuOpcode_FCLASS : begin
      end
      default : begin
        scheduler_0_useRd = 1'b1;
      end
    endcase
  end

  assign scheduler_0_rfHits_0 = rf_scoreboards_0_hit_spinal_port1[0];
  assign scheduler_0_rfHits_1 = rf_scoreboards_0_hit_spinal_port2[0];
  assign scheduler_0_rfHits_2 = rf_scoreboards_0_hit_spinal_port3[0];
  assign scheduler_0_rfHits_3 = rf_scoreboards_0_hit_spinal_port4[0];
  assign scheduler_0_rfTargets_0 = rf_scoreboards_0_target_spinal_port1[0];
  assign scheduler_0_rfTargets_1 = rf_scoreboards_0_target_spinal_port2[0];
  assign scheduler_0_rfTargets_2 = rf_scoreboards_0_target_spinal_port3[0];
  assign scheduler_0_rfTargets_3 = rf_scoreboards_0_target_spinal_port4[0];
  assign scheduler_0_rfBusy_0 = (scheduler_0_rfHits_0 ^ scheduler_0_rfTargets_0);
  assign scheduler_0_rfBusy_1 = (scheduler_0_rfHits_1 ^ scheduler_0_rfTargets_1);
  assign scheduler_0_rfBusy_2 = (scheduler_0_rfHits_2 ^ scheduler_0_rfTargets_2);
  assign scheduler_0_rfBusy_3 = (scheduler_0_rfHits_3 ^ scheduler_0_rfTargets_3);
  assign scheduler_0_hits_0 = (scheduler_0_useRs1 && scheduler_0_rfBusy_0);
  assign scheduler_0_hits_1 = (scheduler_0_useRs2 && scheduler_0_rfBusy_1);
  assign scheduler_0_hits_2 = (scheduler_0_useRs3 && scheduler_0_rfBusy_2);
  assign scheduler_0_hits_3 = (scheduler_0_useRd && scheduler_0_rfBusy_3);
  assign scheduler_0_hazard = (((|{scheduler_0_hits_3,{scheduler_0_hits_2,{scheduler_0_hits_1,scheduler_0_hits_0}}}) || (! rf_init_done)) || commitLogic_0_pending_full);
  assign _zz_scheduler_0_input_ready = (! scheduler_0_hazard);
  assign scheduler_0_output_valid = (scheduler_0_input_valid && _zz_scheduler_0_input_ready);
  assign scheduler_0_input_ready = (scheduler_0_output_ready && _zz_scheduler_0_input_ready);
  assign scheduler_0_output_payload_opcode = scheduler_0_input_payload_opcode;
  assign scheduler_0_output_payload_arg = scheduler_0_input_payload_arg;
  always @(*) begin
    scheduler_0_output_payload_rs1 = scheduler_0_input_payload_rs1;
    if(when_FpuCore_l258) begin
      scheduler_0_output_payload_rs1 = scheduler_0_input_payload_rs2;
    end
  end

  assign scheduler_0_output_payload_rs2 = scheduler_0_input_payload_rs2;
  assign scheduler_0_output_payload_rs3 = scheduler_0_input_payload_rs3;
  assign scheduler_0_output_payload_rd = scheduler_0_input_payload_rd;
  assign scheduler_0_output_payload_format = scheduler_0_input_payload_format;
  assign scheduler_0_output_payload_roundMode = scheduler_0_input_payload_roundMode;
  assign when_FpuCore_l258 = (scheduler_0_input_payload_opcode == FpuOpcode_STORE);
  assign when_FpuCore_l261 = (scheduler_0_input_valid && rf_init_done);
  assign scheduler_0_output_fire = (scheduler_0_output_valid && scheduler_0_output_ready);
  assign when_FpuCore_l265 = (scheduler_0_output_fire && scheduler_0_useRd);
  assign scheduler_0_output_ready = cmdArbiter_arbiter_io_inputs_0_ready;
  assign _zz_io_inputs_0_payload_opcode = scheduler_0_output_payload_opcode;
  assign _zz_io_inputs_0_payload_format = scheduler_0_output_payload_format;
  assign _zz_io_inputs_0_payload_roundMode = scheduler_0_output_payload_roundMode;
  assign cmdArbiter_output_valid = cmdArbiter_arbiter_io_output_valid;
  assign cmdArbiter_output_payload_opcode = cmdArbiter_arbiter_io_output_payload_opcode;
  assign cmdArbiter_output_payload_rs1 = cmdArbiter_arbiter_io_output_payload_rs1;
  assign cmdArbiter_output_payload_rs2 = cmdArbiter_arbiter_io_output_payload_rs2;
  assign cmdArbiter_output_payload_rs3 = cmdArbiter_arbiter_io_output_payload_rs3;
  assign cmdArbiter_output_payload_rd = cmdArbiter_arbiter_io_output_payload_rd;
  assign cmdArbiter_output_payload_arg = cmdArbiter_arbiter_io_output_payload_arg;
  assign cmdArbiter_output_payload_roundMode = cmdArbiter_arbiter_io_output_payload_roundMode;
  assign read_s0_valid = cmdArbiter_output_valid;
  assign cmdArbiter_output_ready = read_s0_ready;
  assign read_s0_payload_opcode = cmdArbiter_output_payload_opcode;
  assign read_s0_payload_rs1 = cmdArbiter_output_payload_rs1;
  assign read_s0_payload_rs2 = cmdArbiter_output_payload_rs2;
  assign read_s0_payload_rs3 = cmdArbiter_output_payload_rs3;
  assign read_s0_payload_rd = cmdArbiter_output_payload_rd;
  assign read_s0_payload_arg = cmdArbiter_output_payload_arg;
  assign read_s0_payload_roundMode = cmdArbiter_output_payload_roundMode;
  always @(*) begin
    read_s0_ready = read_s1_ready;
    if(when_Stream_l375) begin
      read_s0_ready = 1'b1;
    end
  end

  assign when_Stream_l375 = (! read_s1_valid);
  assign read_s1_valid = read_s0_rValid;
  assign read_s1_payload_opcode = read_s0_rData_opcode;
  assign read_s1_payload_rs1 = read_s0_rData_rs1;
  assign read_s1_payload_rs2 = read_s0_rData_rs2;
  assign read_s1_payload_rs3 = read_s0_rData_rs3;
  assign read_s1_payload_rd = read_s0_rData_rd;
  assign read_s1_payload_arg = read_s0_rData_arg;
  assign read_s1_payload_roundMode = read_s0_rData_roundMode;
  assign read_output_valid = read_s1_valid;
  assign read_s1_ready = read_output_ready;
  assign _zz_read_rs_0_value_mantissa = read_s0_payload_rs1;
  assign read_output_isStall = (read_output_valid && (! read_output_ready));
  assign _zz_read_rs_0_value_mantissa_1 = (! read_output_isStall);
  assign _zz_read_rs_0_value_mantissa_2 = rf_ram_spinal_port0[33 : 0];
  assign read_rs_0_value_mantissa = _zz_read_rs_0_value_mantissa_2[22 : 0];
  assign read_rs_0_value_exponent = _zz_read_rs_0_value_mantissa_2[31 : 23];
  assign read_rs_0_value_sign = _zz_read_rs_0_value_mantissa_2[32];
  assign read_rs_0_value_special = _zz_read_rs_0_value_mantissa_2[33];
  assign _zz_read_rs_1_value_mantissa = read_s0_payload_rs2;
  assign _zz_read_rs_1_value_mantissa_1 = (! read_output_isStall);
  assign _zz_read_rs_1_value_mantissa_2 = rf_ram_spinal_port1[33 : 0];
  assign read_rs_1_value_mantissa = _zz_read_rs_1_value_mantissa_2[22 : 0];
  assign read_rs_1_value_exponent = _zz_read_rs_1_value_mantissa_2[31 : 23];
  assign read_rs_1_value_sign = _zz_read_rs_1_value_mantissa_2[32];
  assign read_rs_1_value_special = _zz_read_rs_1_value_mantissa_2[33];
  assign _zz_read_rs_2_value_mantissa = read_s0_payload_rs3;
  assign _zz_read_rs_2_value_mantissa_1 = (! read_output_isStall);
  assign _zz_read_rs_2_value_mantissa_2 = rf_ram_spinal_port2[33 : 0];
  assign read_rs_2_value_mantissa = _zz_read_rs_2_value_mantissa_2[22 : 0];
  assign read_rs_2_value_exponent = _zz_read_rs_2_value_mantissa_2[31 : 23];
  assign read_rs_2_value_sign = _zz_read_rs_2_value_mantissa_2[32];
  assign read_rs_2_value_special = _zz_read_rs_2_value_mantissa_2[33];
  assign read_output_payload_opcode = read_s1_payload_opcode;
  assign read_output_payload_arg = read_s1_payload_arg;
  assign read_output_payload_roundMode = read_s1_payload_roundMode;
  assign read_output_payload_rd = read_s1_payload_rd;
  assign read_output_payload_rs1_mantissa = read_rs_0_value_mantissa;
  assign read_output_payload_rs1_exponent = read_rs_0_value_exponent;
  assign read_output_payload_rs1_sign = read_rs_0_value_sign;
  assign read_output_payload_rs1_special = read_rs_0_value_special;
  assign read_output_payload_rs2_mantissa = read_rs_1_value_mantissa;
  assign read_output_payload_rs2_exponent = read_rs_1_value_exponent;
  assign read_output_payload_rs2_sign = read_rs_1_value_sign;
  assign read_output_payload_rs2_special = read_rs_1_value_special;
  assign read_output_payload_rs3_mantissa = read_rs_2_value_mantissa;
  assign read_output_payload_rs3_exponent = read_rs_2_value_exponent;
  assign read_output_payload_rs3_sign = read_rs_2_value_sign;
  assign read_output_payload_rs3_special = read_rs_2_value_special;
  assign decode_input_valid = read_output_valid;
  assign read_output_ready = decode_input_ready;
  assign decode_input_payload_opcode = read_output_payload_opcode;
  assign decode_input_payload_rs1_mantissa = read_output_payload_rs1_mantissa;
  assign decode_input_payload_rs1_exponent = read_output_payload_rs1_exponent;
  assign decode_input_payload_rs1_sign = read_output_payload_rs1_sign;
  assign decode_input_payload_rs1_special = read_output_payload_rs1_special;
  assign decode_input_payload_rs2_mantissa = read_output_payload_rs2_mantissa;
  assign decode_input_payload_rs2_exponent = read_output_payload_rs2_exponent;
  assign decode_input_payload_rs2_sign = read_output_payload_rs2_sign;
  assign decode_input_payload_rs2_special = read_output_payload_rs2_special;
  assign decode_input_payload_rs3_mantissa = read_output_payload_rs3_mantissa;
  assign decode_input_payload_rs3_exponent = read_output_payload_rs3_exponent;
  assign decode_input_payload_rs3_sign = read_output_payload_rs3_sign;
  assign decode_input_payload_rs3_special = read_output_payload_rs3_special;
  assign decode_input_payload_rd = read_output_payload_rd;
  assign decode_input_payload_arg = read_output_payload_arg;
  assign decode_input_payload_roundMode = read_output_payload_roundMode;
  always @(*) begin
    decode_input_ready = 1'b0;
    if(when_FpuCore_l329) begin
      decode_input_ready = 1'b1;
    end
    if(when_FpuCore_l335) begin
      decode_input_ready = 1'b1;
    end
    if(when_FpuCore_l351) begin
      decode_input_ready = 1'b1;
    end
    if(when_FpuCore_l359) begin
      decode_input_ready = 1'b1;
    end
    if(when_FpuCore_l375) begin
      decode_input_ready = 1'b1;
    end
    if(when_FpuCore_l399) begin
      decode_input_ready = 1'b1;
    end
  end

  assign decode_loadHit = (|{(decode_input_payload_opcode == FpuOpcode_I2F),{(decode_input_payload_opcode == FpuOpcode_FMV_W_X),(decode_input_payload_opcode == FpuOpcode_LOAD)}});
  assign decode_load_valid = (decode_input_valid && decode_loadHit);
  assign when_FpuCore_l329 = (decode_loadHit && decode_load_ready);
  assign decode_load_payload_rd = decode_input_payload_rd;
  assign decode_load_payload_arg = decode_input_payload_arg;
  assign decode_load_payload_roundMode = decode_input_payload_roundMode;
  assign decode_load_payload_i2f = (decode_input_payload_opcode == FpuOpcode_I2F);
  assign decode_shortPipHit = (|{(decode_input_payload_opcode == FpuOpcode_FCVT_X_X),{(decode_input_payload_opcode == FpuOpcode_FCLASS),{(decode_input_payload_opcode == FpuOpcode_FMV_X_W),{(decode_input_payload_opcode == FpuOpcode_SGNJ),{(decode_input_payload_opcode == _zz_decode_shortPipHit),{_zz_decode_shortPipHit_1,{_zz_decode_shortPipHit_2,_zz_decode_shortPipHit_3}}}}}}});
  assign when_FpuCore_l335 = (decode_shortPipHit && decode_shortPip_ready);
  assign decode_shortPip_valid = (decode_input_valid && decode_shortPipHit);
  assign decode_shortPip_payload_opcode = decode_input_payload_opcode;
  assign decode_shortPip_payload_rs1_mantissa = decode_input_payload_rs1_mantissa;
  assign decode_shortPip_payload_rs1_exponent = decode_input_payload_rs1_exponent;
  assign decode_shortPip_payload_rs1_sign = decode_input_payload_rs1_sign;
  assign decode_shortPip_payload_rs1_special = decode_input_payload_rs1_special;
  assign decode_shortPip_payload_rs2_mantissa = decode_input_payload_rs2_mantissa;
  assign decode_shortPip_payload_rs2_exponent = decode_input_payload_rs2_exponent;
  assign decode_shortPip_payload_rs2_sign = decode_input_payload_rs2_sign;
  assign decode_shortPip_payload_rs2_special = decode_input_payload_rs2_special;
  assign decode_shortPip_payload_rd = decode_input_payload_rd;
  assign decode_shortPip_payload_arg = decode_input_payload_arg;
  assign decode_shortPip_payload_roundMode = decode_input_payload_roundMode;
  assign decode_divSqrtHit = ((decode_input_payload_opcode == FpuOpcode_DIV) || (decode_input_payload_opcode == FpuOpcode_SQRT));
  assign decode_divHit = (decode_input_payload_opcode == FpuOpcode_DIV);
  assign when_FpuCore_l351 = (decode_divHit && decode_div_ready);
  assign decode_div_valid = (decode_input_valid && decode_divHit);
  assign decode_div_payload_rs1_mantissa = decode_input_payload_rs1_mantissa;
  assign decode_div_payload_rs1_exponent = decode_input_payload_rs1_exponent;
  assign decode_div_payload_rs1_sign = decode_input_payload_rs1_sign;
  assign decode_div_payload_rs1_special = decode_input_payload_rs1_special;
  assign decode_div_payload_rs2_mantissa = decode_input_payload_rs2_mantissa;
  assign decode_div_payload_rs2_exponent = decode_input_payload_rs2_exponent;
  assign decode_div_payload_rs2_sign = decode_input_payload_rs2_sign;
  assign decode_div_payload_rs2_special = decode_input_payload_rs2_special;
  assign decode_div_payload_rd = decode_input_payload_rd;
  assign decode_div_payload_roundMode = decode_input_payload_roundMode;
  assign decode_sqrtHit = (decode_input_payload_opcode == FpuOpcode_SQRT);
  assign when_FpuCore_l359 = (decode_sqrtHit && decode_sqrt_ready);
  assign decode_sqrt_valid = (decode_input_valid && decode_sqrtHit);
  assign decode_sqrt_payload_rs1_mantissa = decode_input_payload_rs1_mantissa;
  assign decode_sqrt_payload_rs1_exponent = decode_input_payload_rs1_exponent;
  assign decode_sqrt_payload_rs1_sign = decode_input_payload_rs1_sign;
  assign decode_sqrt_payload_rs1_special = decode_input_payload_rs1_special;
  assign decode_sqrt_payload_rd = decode_input_payload_rd;
  assign decode_sqrt_payload_roundMode = decode_input_payload_roundMode;
  assign decode_fmaHit = (decode_input_payload_opcode == FpuOpcode_FMA);
  assign decode_mulHit = ((decode_input_payload_opcode == FpuOpcode_MUL) || decode_fmaHit);
  assign decode_divSqrtToMul_valid = 1'b0;
  assign decode_divSqrtToMul_payload_rs1_mantissa = 23'bxxxxxxxxxxxxxxxxxxxxxxx;
  assign decode_divSqrtToMul_payload_rs1_exponent = 9'bxxxxxxxxx;
  assign decode_divSqrtToMul_payload_rs1_sign = 1'bx;
  assign decode_divSqrtToMul_payload_rs1_special = 1'bx;
  assign decode_divSqrtToMul_payload_rs2_mantissa = 23'bxxxxxxxxxxxxxxxxxxxxxxx;
  assign decode_divSqrtToMul_payload_rs2_exponent = 9'bxxxxxxxxx;
  assign decode_divSqrtToMul_payload_rs2_sign = 1'bx;
  assign decode_divSqrtToMul_payload_rs2_special = 1'bx;
  assign decode_divSqrtToMul_payload_rs3_mantissa = 23'bxxxxxxxxxxxxxxxxxxxxxxx;
  assign decode_divSqrtToMul_payload_rs3_exponent = 9'bxxxxxxxxx;
  assign decode_divSqrtToMul_payload_rs3_sign = 1'bx;
  assign decode_divSqrtToMul_payload_rs3_special = 1'bx;
  assign decode_divSqrtToMul_payload_rd = 5'bxxxxx;
  assign decode_divSqrtToMul_payload_add = 1'bx;
  assign decode_divSqrtToMul_payload_divSqrt = 1'bx;
  assign decode_divSqrtToMul_payload_msb1 = 1'bx;
  assign decode_divSqrtToMul_payload_msb2 = 1'bx;
  assign decode_divSqrtToMul_payload_roundMode = (3'bxxx);
  assign when_FpuCore_l375 = ((decode_mulHit && decode_mul_ready) && (! decode_divSqrtToMul_valid));
  assign decode_mul_valid = ((decode_input_valid && decode_mulHit) || decode_divSqrtToMul_valid);
  assign decode_divSqrtToMul_ready = decode_mul_ready;
  always @(*) begin
    decode_mul_payload_rs1_mantissa = decode_divSqrtToMul_payload_rs1_mantissa;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs1_mantissa = decode_input_payload_rs1_mantissa;
    end
  end

  always @(*) begin
    decode_mul_payload_rs1_exponent = decode_divSqrtToMul_payload_rs1_exponent;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs1_exponent = decode_input_payload_rs1_exponent;
    end
  end

  always @(*) begin
    decode_mul_payload_rs1_sign = decode_divSqrtToMul_payload_rs1_sign;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs1_sign = decode_input_payload_rs1_sign;
    end
  end

  always @(*) begin
    decode_mul_payload_rs1_special = decode_divSqrtToMul_payload_rs1_special;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs1_special = decode_input_payload_rs1_special;
    end
  end

  always @(*) begin
    decode_mul_payload_rs2_mantissa = decode_divSqrtToMul_payload_rs2_mantissa;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs2_mantissa = decode_input_payload_rs2_mantissa;
    end
  end

  always @(*) begin
    decode_mul_payload_rs2_exponent = decode_divSqrtToMul_payload_rs2_exponent;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs2_exponent = decode_input_payload_rs2_exponent;
    end
  end

  always @(*) begin
    decode_mul_payload_rs2_sign = decode_divSqrtToMul_payload_rs2_sign;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs2_sign = decode_input_payload_rs2_sign;
      decode_mul_payload_rs2_sign = (decode_input_payload_rs2_sign ^ decode_input_payload_arg[0]);
    end
  end

  always @(*) begin
    decode_mul_payload_rs2_special = decode_divSqrtToMul_payload_rs2_special;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs2_special = decode_input_payload_rs2_special;
    end
  end

  always @(*) begin
    decode_mul_payload_rs3_mantissa = decode_divSqrtToMul_payload_rs3_mantissa;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs3_mantissa = decode_input_payload_rs3_mantissa;
    end
  end

  always @(*) begin
    decode_mul_payload_rs3_exponent = decode_divSqrtToMul_payload_rs3_exponent;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs3_exponent = decode_input_payload_rs3_exponent;
    end
  end

  always @(*) begin
    decode_mul_payload_rs3_sign = decode_divSqrtToMul_payload_rs3_sign;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs3_sign = decode_input_payload_rs3_sign;
      decode_mul_payload_rs3_sign = (decode_input_payload_rs3_sign ^ decode_input_payload_arg[1]);
    end
  end

  always @(*) begin
    decode_mul_payload_rs3_special = decode_divSqrtToMul_payload_rs3_special;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rs3_special = decode_input_payload_rs3_special;
    end
  end

  always @(*) begin
    decode_mul_payload_rd = decode_divSqrtToMul_payload_rd;
    if(when_FpuCore_l380) begin
      decode_mul_payload_rd = decode_input_payload_rd;
    end
  end

  always @(*) begin
    decode_mul_payload_add = decode_divSqrtToMul_payload_add;
    if(when_FpuCore_l380) begin
      decode_mul_payload_add = decode_fmaHit;
    end
  end

  always @(*) begin
    decode_mul_payload_divSqrt = decode_divSqrtToMul_payload_divSqrt;
    if(when_FpuCore_l380) begin
      decode_mul_payload_divSqrt = 1'b0;
    end
  end

  always @(*) begin
    decode_mul_payload_msb1 = decode_divSqrtToMul_payload_msb1;
    if(when_FpuCore_l380) begin
      decode_mul_payload_msb1 = 1'b1;
    end
  end

  always @(*) begin
    decode_mul_payload_msb2 = decode_divSqrtToMul_payload_msb2;
    if(when_FpuCore_l380) begin
      decode_mul_payload_msb2 = 1'b1;
    end
  end

  always @(*) begin
    decode_mul_payload_roundMode = decode_divSqrtToMul_payload_roundMode;
    if(when_FpuCore_l380) begin
      decode_mul_payload_roundMode = decode_input_payload_roundMode;
    end
  end

  assign when_FpuCore_l380 = (! decode_divSqrtToMul_valid);
  assign decode_addHit = (decode_input_payload_opcode == FpuOpcode_ADD);
  assign when_FpuCore_l399 = ((decode_addHit && decode_add_ready) && (! decode_mulToAdd_valid));
  assign decode_add_valid = ((decode_input_valid && decode_addHit) || decode_mulToAdd_valid);
  assign decode_mulToAdd_ready = decode_add_ready;
  always @(*) begin
    decode_add_payload_rs1_mantissa = decode_mulToAdd_payload_rs1_mantissa;
    if(when_FpuCore_l404) begin
      decode_add_payload_rs1_mantissa = ({2'd0,decode_input_payload_rs1_mantissa} <<< 2'd2);
    end
  end

  always @(*) begin
    decode_add_payload_rs1_exponent = decode_mulToAdd_payload_rs1_exponent;
    if(when_FpuCore_l404) begin
      decode_add_payload_rs1_exponent = decode_input_payload_rs1_exponent;
    end
  end

  always @(*) begin
    decode_add_payload_rs1_sign = decode_mulToAdd_payload_rs1_sign;
    if(when_FpuCore_l404) begin
      decode_add_payload_rs1_sign = decode_input_payload_rs1_sign;
    end
  end

  always @(*) begin
    decode_add_payload_rs1_special = decode_mulToAdd_payload_rs1_special;
    if(when_FpuCore_l404) begin
      decode_add_payload_rs1_special = decode_input_payload_rs1_special;
    end
  end

  always @(*) begin
    decode_add_payload_rs2_mantissa = decode_mulToAdd_payload_rs2_mantissa;
    if(when_FpuCore_l404) begin
      decode_add_payload_rs2_mantissa = ({2'd0,decode_input_payload_rs2_mantissa} <<< 2'd2);
    end
  end

  always @(*) begin
    decode_add_payload_rs2_exponent = decode_mulToAdd_payload_rs2_exponent;
    if(when_FpuCore_l404) begin
      decode_add_payload_rs2_exponent = decode_input_payload_rs2_exponent;
    end
  end

  always @(*) begin
    decode_add_payload_rs2_sign = decode_mulToAdd_payload_rs2_sign;
    if(when_FpuCore_l404) begin
      decode_add_payload_rs2_sign = (decode_input_payload_rs2_sign ^ decode_input_payload_arg[0]);
    end
  end

  always @(*) begin
    decode_add_payload_rs2_special = decode_mulToAdd_payload_rs2_special;
    if(when_FpuCore_l404) begin
      decode_add_payload_rs2_special = decode_input_payload_rs2_special;
    end
  end

  always @(*) begin
    decode_add_payload_rd = decode_mulToAdd_payload_rd;
    if(when_FpuCore_l404) begin
      decode_add_payload_rd = decode_input_payload_rd;
    end
  end

  always @(*) begin
    decode_add_payload_roundMode = decode_mulToAdd_payload_roundMode;
    if(when_FpuCore_l404) begin
      decode_add_payload_roundMode = decode_input_payload_roundMode;
    end
  end

  always @(*) begin
    decode_add_payload_needCommit = decode_mulToAdd_payload_needCommit;
    if(when_FpuCore_l404) begin
      decode_add_payload_needCommit = 1'b1;
    end
  end

  assign when_FpuCore_l404 = (! decode_mulToAdd_valid);
  assign decode_load_ready = decode_load_rValidN;
  assign decode_load_s2mPipe_valid = (decode_load_valid || (! decode_load_rValidN));
  assign _zz_decode_load_s2mPipe_payload_roundMode = (decode_load_rValidN ? decode_load_payload_roundMode : decode_load_rData_roundMode);
  assign decode_load_s2mPipe_payload_rd = (decode_load_rValidN ? decode_load_payload_rd : decode_load_rData_rd);
  assign decode_load_s2mPipe_payload_i2f = (decode_load_rValidN ? decode_load_payload_i2f : decode_load_rData_i2f);
  assign decode_load_s2mPipe_payload_arg = (decode_load_rValidN ? decode_load_payload_arg : decode_load_rData_arg);
  assign decode_load_s2mPipe_payload_roundMode = _zz_decode_load_s2mPipe_payload_roundMode;
  always @(*) begin
    decode_load_s2mPipe_ready = decode_load_s2mPipe_m2sPipe_ready;
    if(when_Stream_l375_1) begin
      decode_load_s2mPipe_ready = 1'b1;
    end
  end

  assign when_Stream_l375_1 = (! decode_load_s2mPipe_m2sPipe_valid);
  assign decode_load_s2mPipe_m2sPipe_valid = decode_load_s2mPipe_rValid;
  assign decode_load_s2mPipe_m2sPipe_payload_rd = decode_load_s2mPipe_rData_rd;
  assign decode_load_s2mPipe_m2sPipe_payload_i2f = decode_load_s2mPipe_rData_i2f;
  assign decode_load_s2mPipe_m2sPipe_payload_arg = decode_load_s2mPipe_rData_arg;
  assign decode_load_s2mPipe_m2sPipe_payload_roundMode = decode_load_s2mPipe_rData_roundMode;
  always @(*) begin
    decode_load_s2mPipe_m2sPipe_ready = load_s0_input_ready;
    if(when_Stream_l375_2) begin
      decode_load_s2mPipe_m2sPipe_ready = 1'b1;
    end
  end

  assign when_Stream_l375_2 = (! load_s0_input_valid);
  assign load_s0_input_valid = decode_load_s2mPipe_m2sPipe_rValid;
  assign load_s0_input_payload_rd = decode_load_s2mPipe_m2sPipe_rData_rd;
  assign load_s0_input_payload_i2f = decode_load_s2mPipe_m2sPipe_rData_i2f;
  assign load_s0_input_payload_arg = decode_load_s2mPipe_m2sPipe_rData_arg;
  assign load_s0_input_payload_roundMode = decode_load_s2mPipe_m2sPipe_rData_roundMode;
  assign when_Stream_l445 = (! (|{(commitFork_load_0_payload_opcode == FpuOpcode_I2F),{(commitFork_load_0_payload_opcode == FpuOpcode_FMV_W_X),(commitFork_load_0_payload_opcode == FpuOpcode_LOAD)}}));
  always @(*) begin
    load_s0_filtred_0_valid = commitFork_load_0_valid;
    if(when_Stream_l445) begin
      load_s0_filtred_0_valid = 1'b0;
    end
  end

  always @(*) begin
    commitFork_load_0_ready = load_s0_filtred_0_ready;
    if(when_Stream_l445) begin
      commitFork_load_0_ready = 1'b1;
    end
  end

  assign load_s0_filtred_0_payload_opcode = commitFork_load_0_payload_opcode;
  assign load_s0_filtred_0_payload_rd = commitFork_load_0_payload_rd;
  assign load_s0_filtred_0_payload_write = commitFork_load_0_payload_write;
  assign load_s0_filtred_0_payload_value = commitFork_load_0_payload_value;
  assign load_s0_hazard = (! load_s0_filtred_0_valid);
  assign _zz_load_s0_input_ready = (! load_s0_hazard);
  assign load_s0_input_ready = (load_s0_output_ready && _zz_load_s0_input_ready);
  assign load_s0_output_valid = (load_s0_input_valid && _zz_load_s0_input_ready);
  always @(*) begin
    load_s0_filtred_0_ready = 1'b0;
    if(_zz_when[0]) begin
      load_s0_filtred_0_ready = (load_s0_input_valid && load_s0_output_ready);
    end
  end

  assign load_s0_output_payload_rd = load_s0_input_payload_rd;
  assign load_s0_output_payload_value = load_s0_filtred_0_payload_value;
  assign load_s0_output_payload_i2f = load_s0_input_payload_i2f;
  assign load_s0_output_payload_arg = load_s0_input_payload_arg;
  assign load_s0_output_payload_roundMode = load_s0_input_payload_roundMode;
  always @(*) begin
    load_s0_output_ready = load_s1_input_ready;
    if(when_Stream_l375_3) begin
      load_s0_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_3 = (! load_s1_input_valid);
  assign load_s1_input_valid = load_s0_output_rValid;
  assign load_s1_input_payload_rd = load_s0_output_rData_rd;
  assign load_s1_input_payload_value = load_s0_output_rData_value;
  assign load_s1_input_payload_i2f = load_s0_output_rData_i2f;
  assign load_s1_input_payload_arg = load_s0_output_rData_arg;
  assign load_s1_input_payload_roundMode = load_s0_output_rData_roundMode;
  always @(*) begin
    load_s1_busy = 1'b0;
    if(when_FpuCore_l529) begin
      load_s1_busy = 1'b1;
    end
  end

  assign load_s1_f32_mantissa = load_s1_input_payload_value[22 : 0];
  assign load_s1_f32_exponent = load_s1_input_payload_value[30 : 23];
  assign load_s1_f32_sign = load_s1_input_payload_value[31];
  assign load_s1_passThroughFloat_special = 1'b0;
  assign load_s1_passThroughFloat_sign = load_s1_f32_sign;
  assign load_s1_passThroughFloat_exponent = {1'd0, load_s1_f32_exponent};
  assign load_s1_passThroughFloat_mantissa = load_s1_f32_mantissa;
  assign load_s1_recodedExpOffset = 9'h080;
  assign load_s1_manZero = (load_s1_passThroughFloat_mantissa == 23'h0);
  assign load_s1_expZero = (load_s1_passThroughFloat_exponent == 9'h0);
  assign load_s1_expOne = (&load_s1_passThroughFloat_exponent[7 : 0]);
  assign load_s1_isZero = (load_s1_expZero && load_s1_manZero);
  assign load_s1_isSubnormal = (load_s1_expZero && (! load_s1_manZero));
  assign load_s1_isInfinity = (load_s1_expOne && load_s1_manZero);
  assign load_s1_isNan = (load_s1_expOne && (! load_s1_manZero));
  always @(*) begin
    load_s1_fsm_ohInput = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    if(when_FpuCore_l508) begin
      load_s1_fsm_ohInput = ({9'd0,load_s1_input_payload_value[22 : 0]} <<< 4'd9);
    end else begin
      load_s1_fsm_ohInput[31 : 0] = load_s1_input_payload_value[31 : 0];
    end
  end

  assign when_FpuCore_l508 = (! load_s1_input_payload_i2f);
  always @(*) begin
    load_s1_fsm_shift_input = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    load_s1_fsm_shift_input = (load_s1_fsm_ohInput <<< 1);
  end

  assign when_FpuCore_l525 = (! load_s1_fsm_done);
  assign when_FpuCore_l529 = ((load_s1_input_valid && (load_s1_input_payload_i2f || load_s1_isSubnormal)) && (! load_s1_fsm_done));
  assign when_FpuCore_l532 = (((load_s1_input_payload_i2f && (! load_s1_fsm_patched)) && load_s1_input_payload_value[31]) && load_s1_input_payload_arg[0]);
  assign _zz_load_s0_output_rData_value = load_s1_input_payload_value;
  assign _zz_load_s0_output_rData_value_1 = 1'b1;
  assign _zz_load_s1_fsm_shift_by = {load_s1_fsm_ohInput[0],{load_s1_fsm_ohInput[1],{load_s1_fsm_ohInput[2],{load_s1_fsm_ohInput[3],{load_s1_fsm_ohInput[4],{load_s1_fsm_ohInput[5],{load_s1_fsm_ohInput[6],{_zz__zz_load_s1_fsm_shift_by,{_zz__zz_load_s1_fsm_shift_by_1,_zz__zz_load_s1_fsm_shift_by_2}}}}}}}}};
  assign _zz_load_s1_fsm_shift_by_1 = (_zz_load_s1_fsm_shift_by & (~ _zz__zz_load_s1_fsm_shift_by_1_1));
  assign _zz_load_s1_fsm_shift_by_2 = _zz_load_s1_fsm_shift_by_1[3];
  assign _zz_load_s1_fsm_shift_by_3 = _zz_load_s1_fsm_shift_by_1[5];
  assign _zz_load_s1_fsm_shift_by_4 = _zz_load_s1_fsm_shift_by_1[6];
  assign _zz_load_s1_fsm_shift_by_5 = _zz_load_s1_fsm_shift_by_1[7];
  assign _zz_load_s1_fsm_shift_by_6 = _zz_load_s1_fsm_shift_by_1[9];
  assign _zz_load_s1_fsm_shift_by_7 = _zz_load_s1_fsm_shift_by_1[10];
  assign _zz_load_s1_fsm_shift_by_8 = _zz_load_s1_fsm_shift_by_1[11];
  assign _zz_load_s1_fsm_shift_by_9 = _zz_load_s1_fsm_shift_by_1[12];
  assign _zz_load_s1_fsm_shift_by_10 = _zz_load_s1_fsm_shift_by_1[13];
  assign _zz_load_s1_fsm_shift_by_11 = _zz_load_s1_fsm_shift_by_1[14];
  assign _zz_load_s1_fsm_shift_by_12 = _zz_load_s1_fsm_shift_by_1[15];
  assign _zz_load_s1_fsm_shift_by_13 = _zz_load_s1_fsm_shift_by_1[17];
  assign _zz_load_s1_fsm_shift_by_14 = _zz_load_s1_fsm_shift_by_1[18];
  assign _zz_load_s1_fsm_shift_by_15 = _zz_load_s1_fsm_shift_by_1[19];
  assign _zz_load_s1_fsm_shift_by_16 = _zz_load_s1_fsm_shift_by_1[20];
  assign _zz_load_s1_fsm_shift_by_17 = _zz_load_s1_fsm_shift_by_1[21];
  assign _zz_load_s1_fsm_shift_by_18 = _zz_load_s1_fsm_shift_by_1[22];
  assign _zz_load_s1_fsm_shift_by_19 = _zz_load_s1_fsm_shift_by_1[23];
  assign _zz_load_s1_fsm_shift_by_20 = _zz_load_s1_fsm_shift_by_1[24];
  assign _zz_load_s1_fsm_shift_by_21 = _zz_load_s1_fsm_shift_by_1[25];
  assign _zz_load_s1_fsm_shift_by_22 = _zz_load_s1_fsm_shift_by_1[26];
  assign _zz_load_s1_fsm_shift_by_23 = _zz_load_s1_fsm_shift_by_1[27];
  assign _zz_load_s1_fsm_shift_by_24 = _zz_load_s1_fsm_shift_by_1[28];
  assign _zz_load_s1_fsm_shift_by_25 = _zz_load_s1_fsm_shift_by_1[29];
  assign _zz_load_s1_fsm_shift_by_26 = _zz_load_s1_fsm_shift_by_1[30];
  assign _zz_load_s1_fsm_shift_by_27 = _zz_load_s1_fsm_shift_by_1[31];
  assign _zz_load_s1_fsm_shift_by_28 = (((((((((((((((_zz_load_s1_fsm_shift_by_1[1] || _zz_load_s1_fsm_shift_by_2) || _zz_load_s1_fsm_shift_by_3) || _zz_load_s1_fsm_shift_by_5) || _zz_load_s1_fsm_shift_by_6) || _zz_load_s1_fsm_shift_by_8) || _zz_load_s1_fsm_shift_by_10) || _zz_load_s1_fsm_shift_by_12) || _zz_load_s1_fsm_shift_by_13) || _zz_load_s1_fsm_shift_by_15) || _zz_load_s1_fsm_shift_by_17) || _zz_load_s1_fsm_shift_by_19) || _zz_load_s1_fsm_shift_by_21) || _zz_load_s1_fsm_shift_by_23) || _zz_load_s1_fsm_shift_by_25) || _zz_load_s1_fsm_shift_by_27);
  assign _zz_load_s1_fsm_shift_by_29 = (((((((((((((((_zz_load_s1_fsm_shift_by_1[2] || _zz_load_s1_fsm_shift_by_2) || _zz_load_s1_fsm_shift_by_4) || _zz_load_s1_fsm_shift_by_5) || _zz_load_s1_fsm_shift_by_7) || _zz_load_s1_fsm_shift_by_8) || _zz_load_s1_fsm_shift_by_11) || _zz_load_s1_fsm_shift_by_12) || _zz_load_s1_fsm_shift_by_14) || _zz_load_s1_fsm_shift_by_15) || _zz_load_s1_fsm_shift_by_18) || _zz_load_s1_fsm_shift_by_19) || _zz_load_s1_fsm_shift_by_22) || _zz_load_s1_fsm_shift_by_23) || _zz_load_s1_fsm_shift_by_26) || _zz_load_s1_fsm_shift_by_27);
  assign _zz_load_s1_fsm_shift_by_30 = (((((((((((((((_zz_load_s1_fsm_shift_by_1[4] || _zz_load_s1_fsm_shift_by_3) || _zz_load_s1_fsm_shift_by_4) || _zz_load_s1_fsm_shift_by_5) || _zz_load_s1_fsm_shift_by_9) || _zz_load_s1_fsm_shift_by_10) || _zz_load_s1_fsm_shift_by_11) || _zz_load_s1_fsm_shift_by_12) || _zz_load_s1_fsm_shift_by_16) || _zz_load_s1_fsm_shift_by_17) || _zz_load_s1_fsm_shift_by_18) || _zz_load_s1_fsm_shift_by_19) || _zz_load_s1_fsm_shift_by_24) || _zz_load_s1_fsm_shift_by_25) || _zz_load_s1_fsm_shift_by_26) || _zz_load_s1_fsm_shift_by_27);
  assign _zz_load_s1_fsm_shift_by_31 = (((((((((((((((_zz_load_s1_fsm_shift_by_1[8] || _zz_load_s1_fsm_shift_by_6) || _zz_load_s1_fsm_shift_by_7) || _zz_load_s1_fsm_shift_by_8) || _zz_load_s1_fsm_shift_by_9) || _zz_load_s1_fsm_shift_by_10) || _zz_load_s1_fsm_shift_by_11) || _zz_load_s1_fsm_shift_by_12) || _zz_load_s1_fsm_shift_by_20) || _zz_load_s1_fsm_shift_by_21) || _zz_load_s1_fsm_shift_by_22) || _zz_load_s1_fsm_shift_by_23) || _zz_load_s1_fsm_shift_by_24) || _zz_load_s1_fsm_shift_by_25) || _zz_load_s1_fsm_shift_by_26) || _zz_load_s1_fsm_shift_by_27);
  assign _zz_load_s1_fsm_shift_by_32 = (((((((((((((((_zz_load_s1_fsm_shift_by_1[16] || _zz_load_s1_fsm_shift_by_13) || _zz_load_s1_fsm_shift_by_14) || _zz_load_s1_fsm_shift_by_15) || _zz_load_s1_fsm_shift_by_16) || _zz_load_s1_fsm_shift_by_17) || _zz_load_s1_fsm_shift_by_18) || _zz_load_s1_fsm_shift_by_19) || _zz_load_s1_fsm_shift_by_20) || _zz_load_s1_fsm_shift_by_21) || _zz_load_s1_fsm_shift_by_22) || _zz_load_s1_fsm_shift_by_23) || _zz_load_s1_fsm_shift_by_24) || _zz_load_s1_fsm_shift_by_25) || _zz_load_s1_fsm_shift_by_26) || _zz_load_s1_fsm_shift_by_27);
  always @(*) begin
    load_s1_fsm_expOffset = 9'h0;
    if(load_s1_isSubnormal) begin
      load_s1_fsm_expOffset = {4'd0, load_s1_fsm_shift_by};
    end
  end

  assign load_s1_input_isStall = (load_s1_input_valid && (! load_s1_input_ready));
  assign when_FpuCore_l551 = (! load_s1_input_isStall);
  assign load_s1_i2fHigh = load_s1_fsm_shift_output[31 : 8];
  assign load_s1_i2fLow = load_s1_fsm_shift_output[7 : 0];
  assign load_s1_scrap = (load_s1_i2fLow != 8'h0);
  assign load_s1_recoded_mantissa = load_s1_passThroughFloat_mantissa;
  always @(*) begin
    load_s1_recoded_exponent = _zz_load_s1_recoded_exponent[8:0];
    if(load_s1_isZero) begin
      load_s1_recoded_exponent[1 : 0] = 2'b00;
    end
    if(load_s1_isInfinity) begin
      load_s1_recoded_exponent[1 : 0] = 2'b01;
    end
    if(load_s1_isNan) begin
      load_s1_recoded_exponent[1 : 0] = 2'b10;
      load_s1_recoded_exponent[2] = 1'b0;
    end
  end

  assign load_s1_recoded_sign = load_s1_passThroughFloat_sign;
  always @(*) begin
    load_s1_recoded_special = 1'b0;
    if(load_s1_isZero) begin
      load_s1_recoded_special = 1'b1;
    end
    if(load_s1_isInfinity) begin
      load_s1_recoded_special = 1'b1;
    end
    if(load_s1_isNan) begin
      load_s1_recoded_special = 1'b1;
    end
  end

  assign _zz_load_s1_input_ready = (! load_s1_busy);
  assign load_s1_input_ready = (load_s1_output_ready && _zz_load_s1_input_ready);
  assign load_s1_output_valid = (load_s1_input_valid && _zz_load_s1_input_ready);
  assign load_s1_output_payload_roundMode = load_s1_input_payload_roundMode;
  assign load_s1_output_payload_rd = load_s1_input_payload_rd;
  always @(*) begin
    load_s1_output_payload_value_sign = load_s1_recoded_sign;
    if(load_s1_input_payload_i2f) begin
      load_s1_output_payload_value_sign = load_s1_fsm_patched;
    end
  end

  always @(*) begin
    load_s1_output_payload_value_exponent = load_s1_recoded_exponent;
    if(load_s1_input_payload_i2f) begin
      load_s1_output_payload_value_exponent = (9'h11e - _zz_load_s1_output_payload_value_exponent);
      if(load_s1_fsm_i2fZero) begin
        load_s1_output_payload_value_exponent[1 : 0] = 2'b00;
      end
    end
  end

  always @(*) begin
    load_s1_output_payload_value_mantissa = {load_s1_recoded_mantissa,1'b0};
    if(when_FpuCore_l594) begin
      load_s1_output_payload_value_mantissa = load_s1_i2fHigh;
    end
  end

  always @(*) begin
    load_s1_output_payload_value_special = load_s1_recoded_special;
    if(load_s1_input_payload_i2f) begin
      load_s1_output_payload_value_special = 1'b0;
      if(load_s1_fsm_i2fZero) begin
        load_s1_output_payload_value_special = 1'b1;
      end
    end
  end

  always @(*) begin
    load_s1_output_payload_scrap = 1'b0;
    if(load_s1_input_payload_i2f) begin
      load_s1_output_payload_scrap = load_s1_scrap;
    end
  end

  assign load_s1_output_payload_NV = 1'b0;
  assign load_s1_output_payload_DZ = 1'b0;
  assign when_FpuCore_l594 = (load_s1_input_payload_i2f || load_s1_isSubnormal);
  always @(*) begin
    decode_shortPip_ready = shortPip_input_ready;
    if(when_Stream_l375_4) begin
      decode_shortPip_ready = 1'b1;
    end
  end

  assign when_Stream_l375_4 = (! shortPip_input_valid);
  assign shortPip_input_valid = decode_shortPip_rValid;
  assign shortPip_input_payload_opcode = decode_shortPip_rData_opcode;
  assign shortPip_input_payload_rs1_mantissa = decode_shortPip_rData_rs1_mantissa;
  assign shortPip_input_payload_rs1_exponent = decode_shortPip_rData_rs1_exponent;
  assign shortPip_input_payload_rs1_sign = decode_shortPip_rData_rs1_sign;
  assign shortPip_input_payload_rs1_special = decode_shortPip_rData_rs1_special;
  assign shortPip_input_payload_rs2_mantissa = decode_shortPip_rData_rs2_mantissa;
  assign shortPip_input_payload_rs2_exponent = decode_shortPip_rData_rs2_exponent;
  assign shortPip_input_payload_rs2_sign = decode_shortPip_rData_rs2_sign;
  assign shortPip_input_payload_rs2_special = decode_shortPip_rData_rs2_special;
  assign shortPip_input_payload_rd = decode_shortPip_rData_rd;
  assign shortPip_input_payload_value = decode_shortPip_rData_value;
  assign shortPip_input_payload_arg = decode_shortPip_rData_arg;
  assign shortPip_input_payload_roundMode = decode_shortPip_rData_roundMode;
  assign shortPip_toFpuRf = (|{(shortPip_input_payload_opcode == FpuOpcode_FCVT_X_X),{(shortPip_input_payload_opcode == FpuOpcode_SGNJ),(shortPip_input_payload_opcode == FpuOpcode_MIN_MAX)}});
  assign shortPip_input_fire = (shortPip_input_valid && shortPip_input_ready);
  assign when_FpuCore_l221 = ((shortPip_input_fire && shortPip_toFpuRf) && 1'b1);
  assign shortPip_isCommited = commitLogic_0_short_notEmpty;
  assign _zz_shortPip_rfOutput_ready = (! (! shortPip_isCommited));
  assign shortPip_output_valid = (shortPip_rfOutput_valid && _zz_shortPip_rfOutput_ready);
  assign shortPip_rfOutput_ready = (shortPip_output_ready && _zz_shortPip_rfOutput_ready);
  assign shortPip_output_payload_rd = shortPip_rfOutput_payload_rd;
  assign shortPip_output_payload_value_mantissa = shortPip_rfOutput_payload_value_mantissa;
  assign shortPip_output_payload_value_exponent = shortPip_rfOutput_payload_value_exponent;
  assign shortPip_output_payload_value_sign = shortPip_rfOutput_payload_value_sign;
  assign shortPip_output_payload_value_special = shortPip_rfOutput_payload_value_special;
  assign shortPip_output_payload_scrap = shortPip_rfOutput_payload_scrap;
  assign shortPip_output_payload_roundMode = shortPip_rfOutput_payload_roundMode;
  assign shortPip_output_payload_NV = shortPip_rfOutput_payload_NV;
  assign shortPip_output_payload_DZ = shortPip_rfOutput_payload_DZ;
  always @(*) begin
    shortPip_result = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    case(shortPip_input_payload_opcode)
      FpuOpcode_STORE : begin
        shortPip_result = shortPip_recodedResult;
      end
      FpuOpcode_FMV_X_W : begin
        shortPip_result = shortPip_recodedResult;
      end
      FpuOpcode_F2I : begin
        shortPip_result[31 : 0] = shortPip_f2i_result;
      end
      FpuOpcode_CMP : begin
        shortPip_result[31 : 0] = {31'd0, shortPip_cmpResult};
      end
      FpuOpcode_FCLASS : begin
        shortPip_result[31 : 0] = shortPip_fclassResult;
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    shortPip_halt = 1'b0;
    if(when_FpuCore_l658) begin
      shortPip_halt = 1'b1;
    end
  end

  assign shortPip_f32_exp = _zz_shortPip_f32_exp[7:0];
  assign shortPip_f32_man = shortPip_input_payload_rs1_mantissa[22 : 0];
  always @(*) begin
    shortPip_recodedResult = {{shortPip_input_payload_rs1_sign,shortPip_f32_exp},shortPip_f32_man};
    if(shortPip_isSubnormal) begin
      shortPip_recodedResult[22 : 0] = shortPip_fsm_shift_output[22 : 0];
    end
    if(shortPip_mantissaForced) begin
      shortPip_recodedResult[22 : 0] = (shortPip_mantissaForcedValue ? 23'h7fffff : 23'h0);
    end
    if(shortPip_exponentForced) begin
      shortPip_recodedResult[30 : 23] = (shortPip_exponentForcedValue ? 8'hff : 8'h0);
    end
    if(shortPip_cononicalForced) begin
      shortPip_recodedResult[31] = 1'b0;
      shortPip_recodedResult[22] = 1'b1;
    end
  end

  assign shortPip_expSubnormalThreshold = 8'h80;
  assign shortPip_expInSubnormalRange = (shortPip_input_payload_rs1_exponent <= _zz_shortPip_expInSubnormalRange);
  assign shortPip_isSubnormal = ((! shortPip_input_payload_rs1_special) && shortPip_expInSubnormalRange);
  assign shortPip_isNormal = ((! shortPip_input_payload_rs1_special) && (! shortPip_expInSubnormalRange));
  assign shortPip_fsm_f2iShift = (shortPip_input_payload_rs1_exponent - 9'h0ff);
  assign shortPip_fsm_isF2i = (shortPip_input_payload_opcode == FpuOpcode_F2I);
  assign shortPip_fsm_needRecoding = ((|{(shortPip_input_payload_opcode == FpuOpcode_STORE),(shortPip_input_payload_opcode == FpuOpcode_FMV_X_W)}) && shortPip_isSubnormal);
  assign shortPip_fsm_isZero = (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b00));
  always @(*) begin
    shortPip_fsm_shift_input = 33'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    shortPip_fsm_shift_input = ({9'd0,{(! shortPip_fsm_isZero),shortPip_input_payload_rs1_mantissa}} <<< 4'd9);
  end

  assign when_FpuCore_l646 = (shortPip_fsm_shift_by[5] && (shortPip_fsm_shift_input[31 : 0] != 32'h0));
  assign when_FpuCore_l646_1 = (shortPip_fsm_shift_by[4] && (shortPip_fsm_shift_input_1[15 : 0] != 16'h0));
  assign when_FpuCore_l646_2 = (shortPip_fsm_shift_by[3] && (shortPip_fsm_shift_input_2[7 : 0] != 8'h0));
  assign when_FpuCore_l646_3 = (shortPip_fsm_shift_by[2] && (shortPip_fsm_shift_input_3[3 : 0] != 4'b0000));
  assign when_FpuCore_l646_4 = (shortPip_fsm_shift_by[1] && (shortPip_fsm_shift_input_4[1 : 0] != 2'b00));
  assign when_FpuCore_l646_5 = (shortPip_fsm_shift_by[0] && (shortPip_fsm_shift_input_5[0 : 0] != 1'b0));
  assign when_FpuCore_l652 = (! shortPip_fsm_done);
  assign shortPip_fsm_formatShiftOffset = 8'h8a;
  assign when_FpuCore_l658 = ((shortPip_input_valid && (shortPip_fsm_needRecoding || shortPip_fsm_isF2i)) && (! shortPip_fsm_done));
  assign _zz_shortPip_fsm_shift_by = (9'h11e - shortPip_input_payload_rs1_exponent);
  assign _zz_shortPip_fsm_shift_by_1 = 6'h21;
  assign shortPip_input_isStall = (shortPip_input_valid && (! shortPip_input_ready));
  assign when_FpuCore_l672 = (! shortPip_input_isStall);
  always @(*) begin
    shortPip_mantissaForced = 1'b0;
    if(shortPip_input_payload_rs1_special) begin
      case(switch_FpuCore_l686)
        2'b00 : begin
          shortPip_mantissaForced = 1'b1;
        end
        2'b01 : begin
          shortPip_mantissaForced = 1'b1;
        end
        2'b10 : begin
          if(when_FpuCore_l702) begin
            shortPip_mantissaForced = 1'b1;
          end
        end
        default : begin
        end
      endcase
    end
  end

  always @(*) begin
    shortPip_exponentForced = 1'b0;
    if(shortPip_input_payload_rs1_special) begin
      case(switch_FpuCore_l686)
        2'b00 : begin
          shortPip_exponentForced = 1'b1;
        end
        2'b01 : begin
          shortPip_exponentForced = 1'b1;
        end
        2'b10 : begin
          shortPip_exponentForced = 1'b1;
        end
        default : begin
        end
      endcase
    end
    if(shortPip_isSubnormal) begin
      shortPip_exponentForced = 1'b1;
    end
  end

  always @(*) begin
    shortPip_mantissaForcedValue = 1'bx;
    if(shortPip_input_payload_rs1_special) begin
      case(switch_FpuCore_l686)
        2'b00 : begin
          shortPip_mantissaForcedValue = 1'b0;
        end
        2'b01 : begin
          shortPip_mantissaForcedValue = 1'b0;
        end
        2'b10 : begin
          if(when_FpuCore_l702) begin
            shortPip_mantissaForcedValue = 1'b0;
          end
        end
        default : begin
        end
      endcase
    end
  end

  always @(*) begin
    shortPip_exponentForcedValue = 1'bx;
    if(shortPip_input_payload_rs1_special) begin
      case(switch_FpuCore_l686)
        2'b00 : begin
          shortPip_exponentForcedValue = 1'b0;
        end
        2'b01 : begin
          shortPip_exponentForcedValue = 1'b1;
        end
        2'b10 : begin
          shortPip_exponentForcedValue = 1'b1;
        end
        default : begin
        end
      endcase
    end
    if(shortPip_isSubnormal) begin
      shortPip_exponentForcedValue = 1'b0;
    end
  end

  always @(*) begin
    shortPip_cononicalForced = 1'b0;
    if(shortPip_input_payload_rs1_special) begin
      case(switch_FpuCore_l686)
        2'b10 : begin
          if(when_FpuCore_l702) begin
            shortPip_cononicalForced = 1'b1;
          end
        end
        default : begin
        end
      endcase
    end
  end

  assign switch_FpuCore_l686 = shortPip_input_payload_rs1_exponent[1 : 0];
  assign when_FpuCore_l702 = shortPip_input_payload_rs1_exponent[2];
  always @(*) begin
    shortPip_rspNv = 1'b0;
    if(!shortPip_f2i_isZero) begin
      if(when_FpuCore_l767) begin
        shortPip_rspNv = (((shortPip_input_valid && (shortPip_input_payload_opcode == FpuOpcode_F2I)) && shortPip_fsm_done) && (! shortPip_f2i_isZero));
      end
    end
    if(shortPip_NV) begin
      shortPip_rspNv = 1'b1;
    end
  end

  always @(*) begin
    shortPip_rspNx = 1'b0;
    if(!shortPip_f2i_isZero) begin
      if(!when_FpuCore_l767) begin
        shortPip_rspNx = (((shortPip_input_valid && (shortPip_input_payload_opcode == FpuOpcode_F2I)) && shortPip_fsm_done) && (shortPip_f2i_round != 2'b00));
      end
    end
  end

  assign shortPip_f2i_unsigned = (shortPip_fsm_shift_output[32 : 0] >>> 1'd1);
  assign shortPip_f2i_resign = (shortPip_input_payload_arg[0] && shortPip_input_payload_rs1_sign);
  assign shortPip_f2i_round = {shortPip_fsm_shift_output[0],shortPip_fsm_shift_scrap};
  always @(*) begin
    case(shortPip_input_payload_roundMode)
      FpuRoundMode_RNE : begin
        shortPip_f2i_increment = (shortPip_f2i_round[1] && (shortPip_f2i_round[0] || shortPip_f2i_unsigned[0]));
      end
      FpuRoundMode_RTZ : begin
        shortPip_f2i_increment = 1'b0;
      end
      FpuRoundMode_RDN : begin
        shortPip_f2i_increment = ((shortPip_f2i_round != 2'b00) && shortPip_input_payload_rs1_sign);
      end
      FpuRoundMode_RUP : begin
        shortPip_f2i_increment = ((shortPip_f2i_round != 2'b00) && (! shortPip_input_payload_rs1_sign));
      end
      default : begin
        shortPip_f2i_increment = shortPip_f2i_round[1];
      end
    endcase
  end

  always @(*) begin
    shortPip_f2i_result = ((shortPip_f2i_resign ? (~ shortPip_f2i_unsigned) : shortPip_f2i_unsigned) + _zz_shortPip_f2i_result);
    if(shortPip_f2i_isZero) begin
      shortPip_f2i_result = 32'h0;
    end else begin
      if(when_FpuCore_l767) begin
        shortPip_f2i_result = (shortPip_f2i_overflow ? 32'hffffffff : 32'h0);
        shortPip_f2i_result[31] = (shortPip_input_payload_arg[0] ^ shortPip_f2i_overflow);
      end
    end
  end

  assign shortPip_f2i_overflow = (((((shortPip_input_payload_arg[0] ? 9'h11d : 9'h11e) < shortPip_input_payload_rs1_exponent) || (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b01))) && (! shortPip_input_payload_rs1_sign)) || (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b10)));
  assign shortPip_f2i_underflow = (((((9'h11e < shortPip_input_payload_rs1_exponent) || ((shortPip_input_payload_arg[0] && shortPip_f2i_unsigned[31]) && ((_zz_shortPip_f2i_underflow != _zz_shortPip_f2i_underflow_1) || shortPip_f2i_increment))) || ((! shortPip_input_payload_arg[0]) && ((shortPip_f2i_unsigned != 32'h0) || shortPip_f2i_increment))) || (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b01))) && shortPip_input_payload_rs1_sign);
  assign shortPip_f2i_isZero = (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b00));
  assign when_FpuCore_l767 = (shortPip_f2i_underflow || shortPip_f2i_overflow);
  assign shortPip_bothZero = ((shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b00)) && (shortPip_input_payload_rs2_special && (shortPip_input_payload_rs2_exponent[1 : 0] == 2'b00)));
  always @(*) begin
    shortPip_rs1Equal = ((((shortPip_input_payload_rs1_mantissa == shortPip_input_payload_rs2_mantissa) && (shortPip_input_payload_rs1_exponent == shortPip_input_payload_rs2_exponent)) && (shortPip_input_payload_rs1_sign == shortPip_input_payload_rs2_sign)) && (shortPip_input_payload_rs1_special == shortPip_input_payload_rs2_special));
    if(when_FpuCore_l784) begin
      shortPip_rs1Equal = 1'b1;
    end
  end

  always @(*) begin
    shortPip_rs1AbsSmaller = ({shortPip_input_payload_rs1_exponent,shortPip_input_payload_rs1_mantissa} < {shortPip_input_payload_rs2_exponent,shortPip_input_payload_rs2_mantissa});
    if(when_FpuCore_l780) begin
      shortPip_rs1AbsSmaller = 1'b1;
    end
    if(when_FpuCore_l781) begin
      shortPip_rs1AbsSmaller = 1'b1;
    end
    if(when_FpuCore_l782) begin
      shortPip_rs1AbsSmaller = 1'b0;
    end
    if(when_FpuCore_l783) begin
      shortPip_rs1AbsSmaller = 1'b0;
    end
  end

  assign when_FpuCore_l780 = (shortPip_input_payload_rs2_special && (shortPip_input_payload_rs2_exponent[1 : 0] == 2'b01));
  assign when_FpuCore_l781 = (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b00));
  assign when_FpuCore_l782 = (shortPip_input_payload_rs2_special && (shortPip_input_payload_rs2_exponent[1 : 0] == 2'b00));
  assign when_FpuCore_l783 = (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b01));
  assign when_FpuCore_l784 = (((shortPip_input_payload_rs1_sign == shortPip_input_payload_rs2_sign) && (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b01))) && (shortPip_input_payload_rs2_special && (shortPip_input_payload_rs2_exponent[1 : 0] == 2'b01)));
  assign switch_Misc_l241 = {shortPip_input_payload_rs1_sign,shortPip_input_payload_rs2_sign};
  always @(*) begin
    case(switch_Misc_l241)
      2'b00 : begin
        shortPip_rs1Smaller = shortPip_rs1AbsSmaller;
      end
      2'b01 : begin
        shortPip_rs1Smaller = 1'b0;
      end
      2'b10 : begin
        shortPip_rs1Smaller = 1'b1;
      end
      default : begin
        shortPip_rs1Smaller = ((! shortPip_rs1AbsSmaller) && (! shortPip_rs1Equal));
      end
    endcase
  end

  assign shortPip_minMaxSelectRs2 = (! (((shortPip_rs1Smaller ^ shortPip_input_payload_arg[0]) && (! (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b10)))) || (shortPip_input_payload_rs2_special && (shortPip_input_payload_rs2_exponent[1 : 0] == 2'b10))));
  assign shortPip_minMaxSelectNanQuiet = ((shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b10)) && (shortPip_input_payload_rs2_special && (shortPip_input_payload_rs2_exponent[1 : 0] == 2'b10)));
  always @(*) begin
    shortPip_cmpResult = (((shortPip_rs1Smaller && (! shortPip_bothZero)) && (! shortPip_input_payload_arg[1])) || ((shortPip_rs1Equal || shortPip_bothZero) && (! shortPip_input_payload_arg[0])));
    if(when_FpuCore_l796) begin
      shortPip_cmpResult = 1'b0;
    end
  end

  assign when_FpuCore_l796 = ((shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b10)) || (shortPip_input_payload_rs2_special && (shortPip_input_payload_rs2_exponent[1 : 0] == 2'b10)));
  assign shortPip_sgnjRs1Sign = shortPip_input_payload_rs1_sign;
  assign shortPip_sgnjRs2Sign = shortPip_input_payload_rs2_sign;
  assign shortPip_sgnjResult = (((shortPip_sgnjRs1Sign && shortPip_input_payload_arg[1]) ^ shortPip_sgnjRs2Sign) ^ shortPip_input_payload_arg[0]);
  always @(*) begin
    shortPip_fclassResult = 32'h0;
    shortPip_fclassResult[0] = (shortPip_input_payload_rs1_sign && shortPip_decoded_isInfinity);
    shortPip_fclassResult[1] = (shortPip_input_payload_rs1_sign && shortPip_isNormal);
    shortPip_fclassResult[2] = (shortPip_input_payload_rs1_sign && shortPip_isSubnormal);
    shortPip_fclassResult[3] = (shortPip_input_payload_rs1_sign && shortPip_decoded_isZero);
    shortPip_fclassResult[4] = ((! shortPip_input_payload_rs1_sign) && shortPip_decoded_isZero);
    shortPip_fclassResult[5] = ((! shortPip_input_payload_rs1_sign) && shortPip_isSubnormal);
    shortPip_fclassResult[6] = ((! shortPip_input_payload_rs1_sign) && shortPip_isNormal);
    shortPip_fclassResult[7] = ((! shortPip_input_payload_rs1_sign) && shortPip_decoded_isInfinity);
    shortPip_fclassResult[8] = (shortPip_decoded_isNan && (! shortPip_decoded_isQuiet));
    shortPip_fclassResult[9] = (shortPip_decoded_isNan && shortPip_decoded_isQuiet);
  end

  assign shortPip_decoded_isZero = (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b00));
  assign shortPip_decoded_isNormal = (! shortPip_input_payload_rs1_special);
  assign shortPip_decoded_isInfinity = (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b01));
  assign shortPip_decoded_isNan = (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b10));
  assign shortPip_decoded_isQuiet = shortPip_input_payload_rs1_mantissa[22];
  assign shortPip_rfOutput_valid = ((shortPip_input_valid && shortPip_toFpuRf) && (! shortPip_halt));
  assign shortPip_rfOutput_payload_rd = shortPip_input_payload_rd;
  assign shortPip_rfOutput_payload_roundMode = shortPip_input_payload_roundMode;
  assign shortPip_rfOutput_payload_scrap = 1'b0;
  always @(*) begin
    shortPip_rfOutput_payload_value_sign = shortPip_input_payload_rs1_sign;
    case(shortPip_input_payload_opcode)
      FpuOpcode_MIN_MAX : begin
        if(shortPip_minMaxSelectRs2) begin
          shortPip_rfOutput_payload_value_sign = shortPip_input_payload_rs2_sign;
        end
      end
      FpuOpcode_SGNJ : begin
        if(when_FpuCore_l850) begin
          shortPip_rfOutput_payload_value_sign = shortPip_sgnjResult;
        end
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    shortPip_rfOutput_payload_value_exponent = shortPip_input_payload_rs1_exponent;
    case(shortPip_input_payload_opcode)
      FpuOpcode_MIN_MAX : begin
        if(shortPip_minMaxSelectRs2) begin
          shortPip_rfOutput_payload_value_exponent = shortPip_input_payload_rs2_exponent;
        end
        if(shortPip_minMaxSelectNanQuiet) begin
          shortPip_rfOutput_payload_value_exponent[1 : 0] = 2'b10;
          shortPip_rfOutput_payload_value_exponent[2] = 1'b1;
        end
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    shortPip_rfOutput_payload_value_mantissa = {shortPip_input_payload_rs1_mantissa,1'b0};
    case(shortPip_input_payload_opcode)
      FpuOpcode_MIN_MAX : begin
        if(shortPip_minMaxSelectRs2) begin
          shortPip_rfOutput_payload_value_mantissa = {shortPip_input_payload_rs2_mantissa,1'b0};
        end
        if(shortPip_minMaxSelectNanQuiet) begin
          shortPip_rfOutput_payload_value_mantissa[23] = 1'b1;
        end
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    shortPip_rfOutput_payload_value_special = shortPip_input_payload_rs1_special;
    case(shortPip_input_payload_opcode)
      FpuOpcode_MIN_MAX : begin
        if(shortPip_minMaxSelectRs2) begin
          shortPip_rfOutput_payload_value_special = shortPip_input_payload_rs2_special;
        end
        if(shortPip_minMaxSelectNanQuiet) begin
          shortPip_rfOutput_payload_value_special = 1'b1;
        end
      end
      default : begin
      end
    endcase
  end

  assign when_FpuCore_l850 = (! (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b10)));
  assign shortPip_signalQuiet = ((shortPip_input_payload_opcode == FpuOpcode_CMP) && (shortPip_input_payload_arg != 2'b10));
  assign shortPip_rs1Nan = (shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b10));
  assign shortPip_rs2Nan = (shortPip_input_payload_rs2_special && (shortPip_input_payload_rs2_exponent[1 : 0] == 2'b10));
  assign shortPip_rs1NanNv = ((shortPip_input_payload_rs1_special && (shortPip_input_payload_rs1_exponent[1 : 0] == 2'b10)) && ((! shortPip_input_payload_rs1_mantissa[22]) || shortPip_signalQuiet));
  assign shortPip_rs2NanNv = ((shortPip_input_payload_rs2_special && (shortPip_input_payload_rs2_exponent[1 : 0] == 2'b10)) && ((! shortPip_input_payload_rs2_mantissa[22]) || shortPip_signalQuiet));
  assign shortPip_NV = (((|{(shortPip_input_payload_opcode == FpuOpcode_FCVT_X_X),{(shortPip_input_payload_opcode == FpuOpcode_MIN_MAX),(shortPip_input_payload_opcode == FpuOpcode_CMP)}}) && shortPip_rs1NanNv) || ((|{(shortPip_input_payload_opcode == FpuOpcode_MIN_MAX),(shortPip_input_payload_opcode == FpuOpcode_CMP)}) && shortPip_rs2NanNv));
  assign shortPip_input_ready = ((! shortPip_halt) && (shortPip_toFpuRf ? shortPip_rfOutput_ready : shortPip_rspStreams_0_ready));
  assign shortPip_rspStreams_0_valid = (((shortPip_input_valid && 1'b1) && (! shortPip_toFpuRf)) && (! shortPip_halt));
  assign shortPip_rspStreams_0_payload_value = shortPip_result;
  assign shortPip_rspStreams_0_payload_NV = shortPip_rspNv;
  assign shortPip_rspStreams_0_payload_NX = shortPip_rspNx;
  always @(*) begin
    shortPip_rspStreams_0_ready = shortPip_rspStreams_0_m2sPipe_ready;
    if(when_Stream_l375_5) begin
      shortPip_rspStreams_0_ready = 1'b1;
    end
  end

  assign when_Stream_l375_5 = (! shortPip_rspStreams_0_m2sPipe_valid);
  assign shortPip_rspStreams_0_m2sPipe_valid = shortPip_rspStreams_0_rValid;
  assign shortPip_rspStreams_0_m2sPipe_payload_value = shortPip_rspStreams_0_rData_value;
  assign shortPip_rspStreams_0_m2sPipe_payload_NV = shortPip_rspStreams_0_rData_NV;
  assign shortPip_rspStreams_0_m2sPipe_payload_NX = shortPip_rspStreams_0_rData_NX;
  assign io_port_0_rsp_valid = shortPip_rspStreams_0_m2sPipe_valid;
  assign shortPip_rspStreams_0_m2sPipe_ready = io_port_0_rsp_ready;
  assign io_port_0_rsp_payload_value = shortPip_rspStreams_0_m2sPipe_payload_value;
  assign io_port_0_rsp_payload_NV = shortPip_rspStreams_0_m2sPipe_payload_NV;
  assign io_port_0_rsp_payload_NX = shortPip_rspStreams_0_m2sPipe_payload_NX;
  assign shortPip_rfOutput_payload_NV = shortPip_NV;
  assign shortPip_rfOutput_payload_DZ = 1'b0;
  always @(*) begin
    decode_mul_ready = mul_preMul_input_ready;
    if(when_Stream_l375_6) begin
      decode_mul_ready = 1'b1;
    end
  end

  assign when_Stream_l375_6 = (! mul_preMul_input_valid);
  assign mul_preMul_input_valid = decode_mul_rValid;
  assign mul_preMul_input_payload_rs1_mantissa = decode_mul_rData_rs1_mantissa;
  assign mul_preMul_input_payload_rs1_exponent = decode_mul_rData_rs1_exponent;
  assign mul_preMul_input_payload_rs1_sign = decode_mul_rData_rs1_sign;
  assign mul_preMul_input_payload_rs1_special = decode_mul_rData_rs1_special;
  assign mul_preMul_input_payload_rs2_mantissa = decode_mul_rData_rs2_mantissa;
  assign mul_preMul_input_payload_rs2_exponent = decode_mul_rData_rs2_exponent;
  assign mul_preMul_input_payload_rs2_sign = decode_mul_rData_rs2_sign;
  assign mul_preMul_input_payload_rs2_special = decode_mul_rData_rs2_special;
  assign mul_preMul_input_payload_rs3_mantissa = decode_mul_rData_rs3_mantissa;
  assign mul_preMul_input_payload_rs3_exponent = decode_mul_rData_rs3_exponent;
  assign mul_preMul_input_payload_rs3_sign = decode_mul_rData_rs3_sign;
  assign mul_preMul_input_payload_rs3_special = decode_mul_rData_rs3_special;
  assign mul_preMul_input_payload_rd = decode_mul_rData_rd;
  assign mul_preMul_input_payload_add = decode_mul_rData_add;
  assign mul_preMul_input_payload_divSqrt = decode_mul_rData_divSqrt;
  assign mul_preMul_input_payload_msb1 = decode_mul_rData_msb1;
  assign mul_preMul_input_payload_msb2 = decode_mul_rData_msb2;
  assign mul_preMul_input_payload_roundMode = decode_mul_rData_roundMode;
  assign mul_preMul_output_valid = mul_preMul_input_valid;
  assign mul_preMul_input_ready = mul_preMul_output_ready;
  assign mul_preMul_output_payload_rs1_mantissa = mul_preMul_input_payload_rs1_mantissa;
  assign mul_preMul_output_payload_rs1_exponent = mul_preMul_input_payload_rs1_exponent;
  assign mul_preMul_output_payload_rs1_sign = mul_preMul_input_payload_rs1_sign;
  assign mul_preMul_output_payload_rs1_special = mul_preMul_input_payload_rs1_special;
  assign mul_preMul_output_payload_rs2_mantissa = mul_preMul_input_payload_rs2_mantissa;
  assign mul_preMul_output_payload_rs2_exponent = mul_preMul_input_payload_rs2_exponent;
  assign mul_preMul_output_payload_rs2_sign = mul_preMul_input_payload_rs2_sign;
  assign mul_preMul_output_payload_rs2_special = mul_preMul_input_payload_rs2_special;
  assign mul_preMul_output_payload_rs3_mantissa = mul_preMul_input_payload_rs3_mantissa;
  assign mul_preMul_output_payload_rs3_exponent = mul_preMul_input_payload_rs3_exponent;
  assign mul_preMul_output_payload_rs3_sign = mul_preMul_input_payload_rs3_sign;
  assign mul_preMul_output_payload_rs3_special = mul_preMul_input_payload_rs3_special;
  assign mul_preMul_output_payload_rd = mul_preMul_input_payload_rd;
  assign mul_preMul_output_payload_add = mul_preMul_input_payload_add;
  assign mul_preMul_output_payload_divSqrt = mul_preMul_input_payload_divSqrt;
  assign mul_preMul_output_payload_msb1 = mul_preMul_input_payload_msb1;
  assign mul_preMul_output_payload_msb2 = mul_preMul_input_payload_msb2;
  assign mul_preMul_output_payload_roundMode = mul_preMul_input_payload_roundMode;
  assign mul_preMul_output_payload_exp = ({1'b0,mul_preMul_input_payload_rs1_exponent} + {1'b0,mul_preMul_input_payload_rs2_exponent});
  always @(*) begin
    mul_preMul_output_ready = mul_mul_input_ready;
    if(when_Stream_l375_7) begin
      mul_preMul_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_7 = (! mul_mul_input_valid);
  assign mul_mul_input_valid = mul_preMul_output_rValid;
  assign mul_mul_input_payload_rs1_mantissa = mul_preMul_output_rData_rs1_mantissa;
  assign mul_mul_input_payload_rs1_exponent = mul_preMul_output_rData_rs1_exponent;
  assign mul_mul_input_payload_rs1_sign = mul_preMul_output_rData_rs1_sign;
  assign mul_mul_input_payload_rs1_special = mul_preMul_output_rData_rs1_special;
  assign mul_mul_input_payload_rs2_mantissa = mul_preMul_output_rData_rs2_mantissa;
  assign mul_mul_input_payload_rs2_exponent = mul_preMul_output_rData_rs2_exponent;
  assign mul_mul_input_payload_rs2_sign = mul_preMul_output_rData_rs2_sign;
  assign mul_mul_input_payload_rs2_special = mul_preMul_output_rData_rs2_special;
  assign mul_mul_input_payload_rs3_mantissa = mul_preMul_output_rData_rs3_mantissa;
  assign mul_mul_input_payload_rs3_exponent = mul_preMul_output_rData_rs3_exponent;
  assign mul_mul_input_payload_rs3_sign = mul_preMul_output_rData_rs3_sign;
  assign mul_mul_input_payload_rs3_special = mul_preMul_output_rData_rs3_special;
  assign mul_mul_input_payload_rd = mul_preMul_output_rData_rd;
  assign mul_mul_input_payload_add = mul_preMul_output_rData_add;
  assign mul_mul_input_payload_divSqrt = mul_preMul_output_rData_divSqrt;
  assign mul_mul_input_payload_msb1 = mul_preMul_output_rData_msb1;
  assign mul_mul_input_payload_msb2 = mul_preMul_output_rData_msb2;
  assign mul_mul_input_payload_roundMode = mul_preMul_output_rData_roundMode;
  assign mul_mul_input_payload_exp = mul_preMul_output_rData_exp;
  assign mul_mul_output_valid = mul_mul_input_valid;
  assign mul_mul_input_ready = mul_mul_output_ready;
  assign mul_mul_mulA = {mul_mul_input_payload_msb1,mul_mul_input_payload_rs1_mantissa};
  assign mul_mul_mulB = {mul_mul_input_payload_msb2,mul_mul_input_payload_rs2_mantissa};
  assign mul_mul_output_payload_rs1_mantissa = mul_mul_input_payload_rs1_mantissa;
  assign mul_mul_output_payload_rs1_exponent = mul_mul_input_payload_rs1_exponent;
  assign mul_mul_output_payload_rs1_sign = mul_mul_input_payload_rs1_sign;
  assign mul_mul_output_payload_rs1_special = mul_mul_input_payload_rs1_special;
  assign mul_mul_output_payload_rs2_mantissa = mul_mul_input_payload_rs2_mantissa;
  assign mul_mul_output_payload_rs2_exponent = mul_mul_input_payload_rs2_exponent;
  assign mul_mul_output_payload_rs2_sign = mul_mul_input_payload_rs2_sign;
  assign mul_mul_output_payload_rs2_special = mul_mul_input_payload_rs2_special;
  assign mul_mul_output_payload_rs3_mantissa = mul_mul_input_payload_rs3_mantissa;
  assign mul_mul_output_payload_rs3_exponent = mul_mul_input_payload_rs3_exponent;
  assign mul_mul_output_payload_rs3_sign = mul_mul_input_payload_rs3_sign;
  assign mul_mul_output_payload_rs3_special = mul_mul_input_payload_rs3_special;
  assign mul_mul_output_payload_rd = mul_mul_input_payload_rd;
  assign mul_mul_output_payload_add = mul_mul_input_payload_add;
  assign mul_mul_output_payload_divSqrt = mul_mul_input_payload_divSqrt;
  assign mul_mul_output_payload_msb1 = mul_mul_input_payload_msb1;
  assign mul_mul_output_payload_msb2 = mul_mul_input_payload_msb2;
  assign mul_mul_output_payload_roundMode = mul_mul_input_payload_roundMode;
  assign mul_mul_output_payload_exp = mul_mul_input_payload_exp;
  assign mul_mul_output_payload_muls_0 = (mul_mul_mulA[17 : 0] * mul_mul_mulB[17 : 0]);
  assign mul_mul_output_payload_muls_1 = (mul_mul_mulA[17 : 0] * mul_mul_mulB[23 : 18]);
  assign mul_mul_output_payload_muls_2 = (mul_mul_mulA[23 : 18] * mul_mul_mulB[17 : 0]);
  assign mul_mul_output_payload_muls_3 = (mul_mul_mulA[23 : 18] * mul_mul_mulB[23 : 18]);
  always @(*) begin
    mul_mul_output_ready = mul_sum1_input_ready;
    if(when_Stream_l375_8) begin
      mul_mul_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_8 = (! mul_sum1_input_valid);
  assign mul_sum1_input_valid = mul_mul_output_rValid;
  assign mul_sum1_input_payload_rs1_mantissa = mul_mul_output_rData_rs1_mantissa;
  assign mul_sum1_input_payload_rs1_exponent = mul_mul_output_rData_rs1_exponent;
  assign mul_sum1_input_payload_rs1_sign = mul_mul_output_rData_rs1_sign;
  assign mul_sum1_input_payload_rs1_special = mul_mul_output_rData_rs1_special;
  assign mul_sum1_input_payload_rs2_mantissa = mul_mul_output_rData_rs2_mantissa;
  assign mul_sum1_input_payload_rs2_exponent = mul_mul_output_rData_rs2_exponent;
  assign mul_sum1_input_payload_rs2_sign = mul_mul_output_rData_rs2_sign;
  assign mul_sum1_input_payload_rs2_special = mul_mul_output_rData_rs2_special;
  assign mul_sum1_input_payload_rs3_mantissa = mul_mul_output_rData_rs3_mantissa;
  assign mul_sum1_input_payload_rs3_exponent = mul_mul_output_rData_rs3_exponent;
  assign mul_sum1_input_payload_rs3_sign = mul_mul_output_rData_rs3_sign;
  assign mul_sum1_input_payload_rs3_special = mul_mul_output_rData_rs3_special;
  assign mul_sum1_input_payload_rd = mul_mul_output_rData_rd;
  assign mul_sum1_input_payload_add = mul_mul_output_rData_add;
  assign mul_sum1_input_payload_divSqrt = mul_mul_output_rData_divSqrt;
  assign mul_sum1_input_payload_msb1 = mul_mul_output_rData_msb1;
  assign mul_sum1_input_payload_msb2 = mul_mul_output_rData_msb2;
  assign mul_sum1_input_payload_roundMode = mul_mul_output_rData_roundMode;
  assign mul_sum1_input_payload_exp = mul_mul_output_rData_exp;
  assign mul_sum1_input_payload_muls_0 = mul_mul_output_rData_muls_0;
  assign mul_sum1_input_payload_muls_1 = mul_mul_output_rData_muls_1;
  assign mul_sum1_input_payload_muls_2 = mul_mul_output_rData_muls_2;
  assign mul_sum1_input_payload_muls_3 = mul_mul_output_rData_muls_3;
  assign mul_sum1_sum = (_zz_mul_sum1_sum + _zz_mul_sum1_sum_1);
  assign mul_sum1_output_valid = mul_sum1_input_valid;
  assign mul_sum1_input_ready = mul_sum1_output_ready;
  assign mul_sum1_output_payload_rs1_mantissa = mul_sum1_input_payload_rs1_mantissa;
  assign mul_sum1_output_payload_rs1_exponent = mul_sum1_input_payload_rs1_exponent;
  assign mul_sum1_output_payload_rs1_sign = mul_sum1_input_payload_rs1_sign;
  assign mul_sum1_output_payload_rs1_special = mul_sum1_input_payload_rs1_special;
  assign mul_sum1_output_payload_rs2_mantissa = mul_sum1_input_payload_rs2_mantissa;
  assign mul_sum1_output_payload_rs2_exponent = mul_sum1_input_payload_rs2_exponent;
  assign mul_sum1_output_payload_rs2_sign = mul_sum1_input_payload_rs2_sign;
  assign mul_sum1_output_payload_rs2_special = mul_sum1_input_payload_rs2_special;
  assign mul_sum1_output_payload_rs3_mantissa = mul_sum1_input_payload_rs3_mantissa;
  assign mul_sum1_output_payload_rs3_exponent = mul_sum1_input_payload_rs3_exponent;
  assign mul_sum1_output_payload_rs3_sign = mul_sum1_input_payload_rs3_sign;
  assign mul_sum1_output_payload_rs3_special = mul_sum1_input_payload_rs3_special;
  assign mul_sum1_output_payload_rd = mul_sum1_input_payload_rd;
  assign mul_sum1_output_payload_add = mul_sum1_input_payload_add;
  assign mul_sum1_output_payload_divSqrt = mul_sum1_input_payload_divSqrt;
  assign mul_sum1_output_payload_msb1 = mul_sum1_input_payload_msb1;
  assign mul_sum1_output_payload_msb2 = mul_sum1_input_payload_msb2;
  assign mul_sum1_output_payload_roundMode = mul_sum1_input_payload_roundMode;
  assign mul_sum1_output_payload_exp = mul_sum1_input_payload_exp;
  assign mul_sum1_output_payload_mulC2 = mul_sum1_sum;
  assign mul_sum1_output_payload_muls2_0 = mul_sum1_input_payload_muls_2;
  assign mul_sum1_output_payload_muls2_1 = mul_sum1_input_payload_muls_3;
  always @(*) begin
    mul_sum1_output_ready = mul_sum2_input_ready;
    if(when_Stream_l375_9) begin
      mul_sum1_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_9 = (! mul_sum2_input_valid);
  assign mul_sum2_input_valid = mul_sum1_output_rValid;
  assign mul_sum2_input_payload_rs1_mantissa = mul_sum1_output_rData_rs1_mantissa;
  assign mul_sum2_input_payload_rs1_exponent = mul_sum1_output_rData_rs1_exponent;
  assign mul_sum2_input_payload_rs1_sign = mul_sum1_output_rData_rs1_sign;
  assign mul_sum2_input_payload_rs1_special = mul_sum1_output_rData_rs1_special;
  assign mul_sum2_input_payload_rs2_mantissa = mul_sum1_output_rData_rs2_mantissa;
  assign mul_sum2_input_payload_rs2_exponent = mul_sum1_output_rData_rs2_exponent;
  assign mul_sum2_input_payload_rs2_sign = mul_sum1_output_rData_rs2_sign;
  assign mul_sum2_input_payload_rs2_special = mul_sum1_output_rData_rs2_special;
  assign mul_sum2_input_payload_rs3_mantissa = mul_sum1_output_rData_rs3_mantissa;
  assign mul_sum2_input_payload_rs3_exponent = mul_sum1_output_rData_rs3_exponent;
  assign mul_sum2_input_payload_rs3_sign = mul_sum1_output_rData_rs3_sign;
  assign mul_sum2_input_payload_rs3_special = mul_sum1_output_rData_rs3_special;
  assign mul_sum2_input_payload_rd = mul_sum1_output_rData_rd;
  assign mul_sum2_input_payload_add = mul_sum1_output_rData_add;
  assign mul_sum2_input_payload_divSqrt = mul_sum1_output_rData_divSqrt;
  assign mul_sum2_input_payload_msb1 = mul_sum1_output_rData_msb1;
  assign mul_sum2_input_payload_msb2 = mul_sum1_output_rData_msb2;
  assign mul_sum2_input_payload_roundMode = mul_sum1_output_rData_roundMode;
  assign mul_sum2_input_payload_exp = mul_sum1_output_rData_exp;
  assign mul_sum2_input_payload_muls2_0 = mul_sum1_output_rData_muls2_0;
  assign mul_sum2_input_payload_muls2_1 = mul_sum1_output_rData_muls2_1;
  assign mul_sum2_input_payload_mulC2 = mul_sum1_output_rData_mulC2;
  assign mul_sum2_sum = (mul_sum2_input_payload_mulC2 + _zz_mul_sum2_sum);
  assign mul_sum2_input_fire = (mul_sum2_input_valid && mul_sum2_input_ready);
  assign when_FpuCore_l221_1 = (mul_sum2_input_fire && 1'b1);
  assign mul_sum2_isCommited = commitLogic_0_mul_notEmpty;
  assign _zz_mul_sum2_input_ready = (! (! mul_sum2_isCommited));
  assign mul_sum2_input_ready = (mul_sum2_output_ready && _zz_mul_sum2_input_ready);
  assign mul_sum2_output_valid = (mul_sum2_input_valid && _zz_mul_sum2_input_ready);
  assign mul_sum2_output_payload_rs1_mantissa = mul_sum2_input_payload_rs1_mantissa;
  assign mul_sum2_output_payload_rs1_exponent = mul_sum2_input_payload_rs1_exponent;
  assign mul_sum2_output_payload_rs1_sign = mul_sum2_input_payload_rs1_sign;
  assign mul_sum2_output_payload_rs1_special = mul_sum2_input_payload_rs1_special;
  assign mul_sum2_output_payload_rs2_mantissa = mul_sum2_input_payload_rs2_mantissa;
  assign mul_sum2_output_payload_rs2_exponent = mul_sum2_input_payload_rs2_exponent;
  assign mul_sum2_output_payload_rs2_sign = mul_sum2_input_payload_rs2_sign;
  assign mul_sum2_output_payload_rs2_special = mul_sum2_input_payload_rs2_special;
  assign mul_sum2_output_payload_rs3_mantissa = mul_sum2_input_payload_rs3_mantissa;
  assign mul_sum2_output_payload_rs3_exponent = mul_sum2_input_payload_rs3_exponent;
  assign mul_sum2_output_payload_rs3_sign = mul_sum2_input_payload_rs3_sign;
  assign mul_sum2_output_payload_rs3_special = mul_sum2_input_payload_rs3_special;
  assign mul_sum2_output_payload_rd = mul_sum2_input_payload_rd;
  assign mul_sum2_output_payload_add = mul_sum2_input_payload_add;
  assign mul_sum2_output_payload_divSqrt = mul_sum2_input_payload_divSqrt;
  assign mul_sum2_output_payload_msb1 = mul_sum2_input_payload_msb1;
  assign mul_sum2_output_payload_msb2 = mul_sum2_input_payload_msb2;
  assign mul_sum2_output_payload_roundMode = mul_sum2_input_payload_roundMode;
  assign mul_sum2_output_payload_exp = mul_sum2_input_payload_exp;
  assign mul_sum2_output_payload_mulC = mul_sum2_sum;
  always @(*) begin
    mul_sum2_output_ready = mul_norm_input_ready;
    if(when_Stream_l375_10) begin
      mul_sum2_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_10 = (! mul_norm_input_valid);
  assign mul_norm_input_valid = mul_sum2_output_rValid;
  assign mul_norm_input_payload_rs1_mantissa = mul_sum2_output_rData_rs1_mantissa;
  assign mul_norm_input_payload_rs1_exponent = mul_sum2_output_rData_rs1_exponent;
  assign mul_norm_input_payload_rs1_sign = mul_sum2_output_rData_rs1_sign;
  assign mul_norm_input_payload_rs1_special = mul_sum2_output_rData_rs1_special;
  assign mul_norm_input_payload_rs2_mantissa = mul_sum2_output_rData_rs2_mantissa;
  assign mul_norm_input_payload_rs2_exponent = mul_sum2_output_rData_rs2_exponent;
  assign mul_norm_input_payload_rs2_sign = mul_sum2_output_rData_rs2_sign;
  assign mul_norm_input_payload_rs2_special = mul_sum2_output_rData_rs2_special;
  assign mul_norm_input_payload_rs3_mantissa = mul_sum2_output_rData_rs3_mantissa;
  assign mul_norm_input_payload_rs3_exponent = mul_sum2_output_rData_rs3_exponent;
  assign mul_norm_input_payload_rs3_sign = mul_sum2_output_rData_rs3_sign;
  assign mul_norm_input_payload_rs3_special = mul_sum2_output_rData_rs3_special;
  assign mul_norm_input_payload_rd = mul_sum2_output_rData_rd;
  assign mul_norm_input_payload_add = mul_sum2_output_rData_add;
  assign mul_norm_input_payload_divSqrt = mul_sum2_output_rData_divSqrt;
  assign mul_norm_input_payload_msb1 = mul_sum2_output_rData_msb1;
  assign mul_norm_input_payload_msb2 = mul_sum2_output_rData_msb2;
  assign mul_norm_input_payload_roundMode = mul_sum2_output_rData_roundMode;
  assign mul_norm_input_payload_exp = mul_sum2_output_rData_exp;
  assign mul_norm_input_payload_mulC = mul_sum2_output_rData_mulC;
  assign mul_norm_mulHigh = mul_norm_input_payload_mulC[47 : 22];
  assign mul_norm_mulLow = mul_norm_input_payload_mulC[21 : 0];
  always @(*) begin
    mul_norm_scrap = (mul_norm_mulLow != 22'h0);
    if(when_FpuCore_l967) begin
      mul_norm_scrap = 1'b1;
    end
  end

  assign mul_norm_needShift = mul_norm_mulHigh[25];
  assign mul_norm_exp = (mul_norm_input_payload_exp + _zz_mul_norm_exp);
  assign mul_norm_man = (mul_norm_needShift ? mul_norm_mulHigh[24 : 1] : mul_norm_mulHigh[23 : 0]);
  assign when_FpuCore_l967 = (mul_norm_needShift && mul_norm_mulHigh[0]);
  assign mul_norm_forceZero = ((mul_norm_input_payload_rs1_special && (mul_norm_input_payload_rs1_exponent[1 : 0] == 2'b00)) || (mul_norm_input_payload_rs2_special && (mul_norm_input_payload_rs2_exponent[1 : 0] == 2'b00)));
  assign mul_norm_underflowThreshold = 9'h167;
  assign mul_norm_underflowExp = 7'h67;
  assign mul_norm_forceUnderflow = (mul_norm_exp < _zz_mul_norm_forceUnderflow);
  assign mul_norm_forceOverflow = ((mul_norm_input_payload_rs1_special && (mul_norm_input_payload_rs1_exponent[1 : 0] == 2'b01)) || (mul_norm_input_payload_rs2_special && (mul_norm_input_payload_rs2_exponent[1 : 0] == 2'b01)));
  assign mul_norm_infinitynan = (((mul_norm_input_payload_rs1_special && (mul_norm_input_payload_rs1_exponent[1 : 0] == 2'b01)) || (mul_norm_input_payload_rs2_special && (mul_norm_input_payload_rs2_exponent[1 : 0] == 2'b01))) && ((mul_norm_input_payload_rs1_special && (mul_norm_input_payload_rs1_exponent[1 : 0] == 2'b00)) || (mul_norm_input_payload_rs2_special && (mul_norm_input_payload_rs2_exponent[1 : 0] == 2'b00))));
  assign mul_norm_forceNan = (((mul_norm_input_payload_rs1_special && (mul_norm_input_payload_rs1_exponent[1 : 0] == 2'b10)) || (mul_norm_input_payload_rs2_special && (mul_norm_input_payload_rs2_exponent[1 : 0] == 2'b10))) || mul_norm_infinitynan);
  assign mul_norm_output_sign = (mul_norm_input_payload_rs1_sign ^ mul_norm_input_payload_rs2_sign);
  always @(*) begin
    mul_norm_output_exponent = _zz_mul_norm_output_exponent[8:0];
    if(when_FpuCore_l983) begin
      mul_norm_output_exponent[8 : 7] = 2'b11;
    end
    if(mul_norm_forceNan) begin
      mul_norm_output_exponent[1 : 0] = 2'b10;
      mul_norm_output_exponent[2] = 1'b1;
    end else begin
      if(mul_norm_forceOverflow) begin
        mul_norm_output_exponent[1 : 0] = 2'b01;
      end else begin
        if(mul_norm_forceZero) begin
          mul_norm_output_exponent[1 : 0] = 2'b00;
        end else begin
          if(mul_norm_forceUnderflow) begin
            mul_norm_output_exponent = {2'd0, mul_norm_underflowExp};
          end
        end
      end
    end
  end

  always @(*) begin
    mul_norm_output_mantissa = mul_norm_man;
    if(mul_norm_forceNan) begin
      mul_norm_output_mantissa[23] = 1'b1;
    end
  end

  always @(*) begin
    mul_norm_output_special = 1'b0;
    if(mul_norm_forceNan) begin
      mul_norm_output_special = 1'b1;
    end else begin
      if(mul_norm_forceOverflow) begin
        mul_norm_output_special = 1'b1;
      end else begin
        if(mul_norm_forceZero) begin
          mul_norm_output_special = 1'b1;
        end
      end
    end
  end

  always @(*) begin
    mul_norm_NV = 1'b0;
    if(mul_norm_forceNan) begin
      if(when_FpuCore_l987) begin
        mul_norm_NV = 1'b1;
      end
    end
  end

  assign when_FpuCore_l983 = (3'b101 <= mul_norm_exp[9 : 7]);
  assign when_FpuCore_l987 = ((mul_norm_infinitynan || ((mul_norm_input_payload_rs1_special && (mul_norm_input_payload_rs1_exponent[1 : 0] == 2'b10)) && (! mul_norm_input_payload_rs1_mantissa[22]))) || ((mul_norm_input_payload_rs2_special && (mul_norm_input_payload_rs2_exponent[1 : 0] == 2'b10)) && (! mul_norm_input_payload_rs2_mantissa[22])));
  assign mul_result_notMul_output_valid = (mul_norm_input_valid && mul_norm_input_payload_divSqrt);
  assign mul_result_notMul_output_payload = mul_norm_input_payload_mulC[46 : 23];
  assign mul_result_output_valid = ((mul_norm_input_valid && (! mul_norm_input_payload_add)) && (! mul_norm_input_payload_divSqrt));
  assign mul_result_output_payload_rd = mul_norm_input_payload_rd;
  assign mul_result_output_payload_roundMode = mul_norm_input_payload_roundMode;
  assign mul_result_output_payload_scrap = mul_norm_scrap;
  assign mul_result_output_payload_value_mantissa = mul_norm_output_mantissa;
  assign mul_result_output_payload_value_exponent = mul_norm_output_exponent;
  assign mul_result_output_payload_value_sign = mul_norm_output_sign;
  assign mul_result_output_payload_value_special = mul_norm_output_special;
  assign mul_result_output_payload_NV = mul_norm_NV;
  assign mul_result_output_payload_DZ = 1'b0;
  always @(*) begin
    mul_result_mulToAdd_ready = mul_result_mulToAdd_m2sPipe_ready;
    if(when_Stream_l375_11) begin
      mul_result_mulToAdd_ready = 1'b1;
    end
  end

  assign when_Stream_l375_11 = (! mul_result_mulToAdd_m2sPipe_valid);
  assign mul_result_mulToAdd_m2sPipe_valid = mul_result_mulToAdd_rValid;
  assign mul_result_mulToAdd_m2sPipe_payload_rs1_mantissa = mul_result_mulToAdd_rData_rs1_mantissa;
  assign mul_result_mulToAdd_m2sPipe_payload_rs1_exponent = mul_result_mulToAdd_rData_rs1_exponent;
  assign mul_result_mulToAdd_m2sPipe_payload_rs1_sign = mul_result_mulToAdd_rData_rs1_sign;
  assign mul_result_mulToAdd_m2sPipe_payload_rs1_special = mul_result_mulToAdd_rData_rs1_special;
  assign mul_result_mulToAdd_m2sPipe_payload_rs2_mantissa = mul_result_mulToAdd_rData_rs2_mantissa;
  assign mul_result_mulToAdd_m2sPipe_payload_rs2_exponent = mul_result_mulToAdd_rData_rs2_exponent;
  assign mul_result_mulToAdd_m2sPipe_payload_rs2_sign = mul_result_mulToAdd_rData_rs2_sign;
  assign mul_result_mulToAdd_m2sPipe_payload_rs2_special = mul_result_mulToAdd_rData_rs2_special;
  assign mul_result_mulToAdd_m2sPipe_payload_rd = mul_result_mulToAdd_rData_rd;
  assign mul_result_mulToAdd_m2sPipe_payload_roundMode = mul_result_mulToAdd_rData_roundMode;
  assign mul_result_mulToAdd_m2sPipe_payload_needCommit = mul_result_mulToAdd_rData_needCommit;
  assign decode_mulToAdd_valid = mul_result_mulToAdd_m2sPipe_valid;
  assign mul_result_mulToAdd_m2sPipe_ready = decode_mulToAdd_ready;
  assign decode_mulToAdd_payload_rs1_mantissa = mul_result_mulToAdd_m2sPipe_payload_rs1_mantissa;
  assign decode_mulToAdd_payload_rs1_exponent = mul_result_mulToAdd_m2sPipe_payload_rs1_exponent;
  assign decode_mulToAdd_payload_rs1_sign = mul_result_mulToAdd_m2sPipe_payload_rs1_sign;
  assign decode_mulToAdd_payload_rs1_special = mul_result_mulToAdd_m2sPipe_payload_rs1_special;
  assign decode_mulToAdd_payload_rs2_mantissa = mul_result_mulToAdd_m2sPipe_payload_rs2_mantissa;
  assign decode_mulToAdd_payload_rs2_exponent = mul_result_mulToAdd_m2sPipe_payload_rs2_exponent;
  assign decode_mulToAdd_payload_rs2_sign = mul_result_mulToAdd_m2sPipe_payload_rs2_sign;
  assign decode_mulToAdd_payload_rs2_special = mul_result_mulToAdd_m2sPipe_payload_rs2_special;
  assign decode_mulToAdd_payload_rd = mul_result_mulToAdd_m2sPipe_payload_rd;
  assign decode_mulToAdd_payload_roundMode = mul_result_mulToAdd_m2sPipe_payload_roundMode;
  assign decode_mulToAdd_payload_needCommit = mul_result_mulToAdd_m2sPipe_payload_needCommit;
  assign mul_result_mulToAdd_valid = (mul_norm_input_valid && mul_norm_input_payload_add);
  always @(*) begin
    mul_result_mulToAdd_payload_rs1_mantissa = {mul_norm_output_mantissa,mul_norm_scrap};
    if(mul_norm_NV) begin
      mul_result_mulToAdd_payload_rs1_mantissa[24] = 1'b0;
    end
  end

  assign mul_result_mulToAdd_payload_rs1_exponent = mul_norm_output_exponent;
  assign mul_result_mulToAdd_payload_rs1_sign = mul_norm_output_sign;
  assign mul_result_mulToAdd_payload_rs1_special = mul_norm_output_special;
  assign mul_result_mulToAdd_payload_rs2_exponent = mul_norm_input_payload_rs3_exponent;
  assign mul_result_mulToAdd_payload_rs2_sign = mul_norm_input_payload_rs3_sign;
  assign mul_result_mulToAdd_payload_rs2_special = mul_norm_input_payload_rs3_special;
  assign mul_result_mulToAdd_payload_rs2_mantissa = ({2'd0,mul_norm_input_payload_rs3_mantissa} <<< 2'd2);
  assign mul_result_mulToAdd_payload_rd = mul_norm_input_payload_rd;
  assign mul_result_mulToAdd_payload_roundMode = mul_norm_input_payload_roundMode;
  assign mul_result_mulToAdd_payload_needCommit = 1'b0;
  assign mul_norm_input_ready = ((mul_norm_input_payload_add ? mul_result_mulToAdd_ready : mul_result_output_ready) || mul_norm_input_payload_divSqrt);
  assign div_input_fire = (div_input_valid && div_input_ready);
  assign decode_div_ready = (! decode_div_rValid);
  assign div_input_valid = decode_div_rValid;
  assign div_input_payload_rs1_mantissa = decode_div_rData_rs1_mantissa;
  assign div_input_payload_rs1_exponent = decode_div_rData_rs1_exponent;
  assign div_input_payload_rs1_sign = decode_div_rData_rs1_sign;
  assign div_input_payload_rs1_special = decode_div_rData_rs1_special;
  assign div_input_payload_rs2_mantissa = decode_div_rData_rs2_mantissa;
  assign div_input_payload_rs2_exponent = decode_div_rData_rs2_exponent;
  assign div_input_payload_rs2_sign = decode_div_rData_rs2_sign;
  assign div_input_payload_rs2_special = decode_div_rData_rs2_special;
  assign div_input_payload_rd = decode_div_rData_rd;
  assign div_input_payload_roundMode = decode_div_rData_roundMode;
  always @(*) begin
    div_haltIt = 1'b1;
    if(div_divider_io_output_valid) begin
      div_haltIt = 1'b0;
    end
  end

  assign when_FpuCore_l221_2 = (div_input_fire && 1'b1);
  assign _zz_div_input_ready = (! (div_haltIt || (! div_isCommited)));
  assign div_input_ready = (div_output_ready && _zz_div_input_ready);
  assign div_output_valid = (div_input_valid && _zz_div_input_ready);
  assign div_divider_io_input_payload_a = ({1'd0,div_input_payload_rs1_mantissa} <<< 1'd1);
  assign div_divider_io_input_payload_b = ({1'd0,div_input_payload_rs2_mantissa} <<< 1'd1);
  assign div_dividerResult = (div_divider_io_output_payload_result >>> 1'd1);
  assign div_dividerScrap = ((div_divider_io_output_payload_remain != 25'h0) || (div_divider_io_output_payload_result[0 : 0] != 1'b0));
  assign FpuPlugin_fpu_div_divider_io_input_fire = (div_divider_io_input_valid && div_divider_io_input_ready);
  assign when_FpuCore_l1056 = (! div_haltIt);
  assign div_divider_io_input_valid = (div_input_valid && (! div_cmdSent));
  assign div_output_payload_rd = div_input_payload_rd;
  assign div_output_payload_roundMode = div_input_payload_roundMode;
  assign div_needShift = (! div_dividerResult[25]);
  assign div_mantissa = (div_needShift ? div_dividerResult[23 : 0] : div_dividerResult[24 : 1]);
  assign div_scrap = (div_dividerScrap || ((! div_needShift) && div_dividerResult[0]));
  assign div_exponent = (_zz_div_exponent - _zz_div_exponent_4);
  always @(*) begin
    div_output_payload_value_special = 1'b0;
    if(div_forceNan) begin
      div_output_payload_value_special = 1'b1;
    end else begin
      if(div_forceOverflow) begin
        div_output_payload_value_special = 1'b1;
      end else begin
        if(div_forceZero) begin
          div_output_payload_value_special = 1'b1;
        end
      end
    end
  end

  assign div_output_payload_value_sign = (div_input_payload_rs1_sign ^ div_input_payload_rs2_sign);
  always @(*) begin
    div_output_payload_value_exponent = div_exponent[8:0];
    if(when_FpuCore_l1072) begin
      div_output_payload_value_exponent[8 : 6] = 3'b111;
    end
    if(when_FpuCore_l1089) begin
      div_output_payload_value_exponent[8 : 7] = 2'b11;
    end
    if(div_forceNan) begin
      div_output_payload_value_exponent[1 : 0] = 2'b10;
      div_output_payload_value_exponent[2] = 1'b1;
    end else begin
      if(div_forceOverflow) begin
        div_output_payload_value_exponent[1 : 0] = 2'b01;
      end else begin
        if(div_forceZero) begin
          div_output_payload_value_exponent[1 : 0] = 2'b00;
        end else begin
          if(div_forceUnderflow) begin
            div_output_payload_value_exponent = div_underflowExp[8:0];
          end
        end
      end
    end
  end

  always @(*) begin
    div_output_payload_value_mantissa = div_mantissa;
    if(div_forceNan) begin
      div_output_payload_value_mantissa[23] = 1'b1;
    end
  end

  assign div_output_payload_scrap = div_scrap;
  assign when_FpuCore_l1072 = (div_exponent[10 : 9] == 2'b11);
  assign div_underflowThreshold = 11'h468;
  assign div_underflowExp = 11'h467;
  assign div_forceUnderflow = (div_exponent < div_underflowThreshold);
  assign div_forceOverflow = ((div_input_payload_rs1_special && (div_input_payload_rs1_exponent[1 : 0] == 2'b01)) || (div_input_payload_rs2_special && (div_input_payload_rs2_exponent[1 : 0] == 2'b00)));
  assign div_infinitynan = (((div_input_payload_rs1_special && (div_input_payload_rs1_exponent[1 : 0] == 2'b00)) && (div_input_payload_rs2_special && (div_input_payload_rs2_exponent[1 : 0] == 2'b00))) || ((div_input_payload_rs1_special && (div_input_payload_rs1_exponent[1 : 0] == 2'b01)) && (div_input_payload_rs2_special && (div_input_payload_rs2_exponent[1 : 0] == 2'b01))));
  assign div_forceNan = (((div_input_payload_rs1_special && (div_input_payload_rs1_exponent[1 : 0] == 2'b10)) || (div_input_payload_rs2_special && (div_input_payload_rs2_exponent[1 : 0] == 2'b10))) || div_infinitynan);
  assign div_forceZero = ((div_input_payload_rs1_special && (div_input_payload_rs1_exponent[1 : 0] == 2'b00)) || (div_input_payload_rs2_special && (div_input_payload_rs2_exponent[1 : 0] == 2'b01)));
  always @(*) begin
    div_output_payload_NV = 1'b0;
    if(div_forceNan) begin
      if(when_FpuCore_l1093) begin
        div_output_payload_NV = 1'b1;
      end
    end
  end

  assign div_output_payload_DZ = (((! div_forceNan) && (! (div_input_payload_rs1_special && (div_input_payload_rs1_exponent[1 : 0] == 2'b01)))) && (div_input_payload_rs2_special && (div_input_payload_rs2_exponent[1 : 0] == 2'b00)));
  assign when_FpuCore_l1089 = (div_exponent[10 : 8] == 3'b111);
  assign when_FpuCore_l1093 = ((div_infinitynan || ((div_input_payload_rs1_special && (div_input_payload_rs1_exponent[1 : 0] == 2'b10)) && (! div_input_payload_rs1_mantissa[22]))) || ((div_input_payload_rs2_special && (div_input_payload_rs2_exponent[1 : 0] == 2'b10)) && (! div_input_payload_rs2_mantissa[22])));
  assign sqrt_input_fire = (sqrt_input_valid && sqrt_input_ready);
  assign decode_sqrt_ready = (! decode_sqrt_rValid);
  assign sqrt_input_valid = decode_sqrt_rValid;
  assign sqrt_input_payload_rs1_mantissa = decode_sqrt_rData_rs1_mantissa;
  assign sqrt_input_payload_rs1_exponent = decode_sqrt_rData_rs1_exponent;
  assign sqrt_input_payload_rs1_sign = decode_sqrt_rData_rs1_sign;
  assign sqrt_input_payload_rs1_special = decode_sqrt_rData_rs1_special;
  assign sqrt_input_payload_rd = decode_sqrt_rData_rd;
  assign sqrt_input_payload_roundMode = decode_sqrt_rData_roundMode;
  always @(*) begin
    sqrt_haltIt = 1'b1;
    if(sqrt_sqrt_io_output_valid) begin
      sqrt_haltIt = 1'b0;
    end
  end

  assign when_FpuCore_l221_3 = (sqrt_input_fire && 1'b1);
  assign _zz_sqrt_input_ready = (! (sqrt_haltIt || (! sqrt_isCommited)));
  assign sqrt_input_ready = (sqrt_output_ready && _zz_sqrt_input_ready);
  assign sqrt_output_valid = (sqrt_input_valid && _zz_sqrt_input_ready);
  assign sqrt_needShift = (! sqrt_input_payload_rs1_exponent[0]);
  assign sqrt_sqrt_io_input_payload_a = (sqrt_needShift ? {{1'b1,sqrt_input_payload_rs1_mantissa},1'b0} : {2'b01,sqrt_input_payload_rs1_mantissa});
  assign FpuPlugin_fpu_sqrt_sqrt_io_input_fire = (sqrt_sqrt_io_input_valid && sqrt_sqrt_io_input_ready);
  assign when_FpuCore_l1118 = (! sqrt_haltIt);
  assign sqrt_sqrt_io_input_valid = (sqrt_input_valid && (! sqrt_cmdSent));
  assign sqrt_output_payload_rd = sqrt_input_payload_rd;
  assign sqrt_output_payload_roundMode = sqrt_input_payload_roundMode;
  assign sqrt_scrap = (sqrt_sqrt_io_output_payload_remain != 28'h0);
  always @(*) begin
    sqrt_output_payload_value_special = 1'b0;
    if(when_FpuCore_l1137) begin
      sqrt_output_payload_value_special = 1'b1;
    end
    if(sqrt_negative) begin
      sqrt_output_payload_value_special = 1'b1;
    end
    if(when_FpuCore_l1144) begin
      sqrt_output_payload_value_special = 1'b1;
    end
    if(when_FpuCore_l1148) begin
      sqrt_output_payload_value_special = 1'b1;
    end
  end

  assign sqrt_output_payload_value_sign = sqrt_input_payload_rs1_sign;
  always @(*) begin
    sqrt_output_payload_value_exponent = sqrt_exponent;
    if(when_FpuCore_l1137) begin
      sqrt_output_payload_value_exponent[1 : 0] = 2'b01;
    end
    if(sqrt_negative) begin
      sqrt_output_payload_value_exponent[1 : 0] = 2'b10;
      sqrt_output_payload_value_exponent[2] = 1'b1;
    end
    if(when_FpuCore_l1144) begin
      sqrt_output_payload_value_exponent[1 : 0] = 2'b10;
      sqrt_output_payload_value_exponent[2] = 1'b1;
    end
    if(when_FpuCore_l1148) begin
      sqrt_output_payload_value_exponent[1 : 0] = 2'b00;
    end
  end

  always @(*) begin
    sqrt_output_payload_value_mantissa = sqrt_sqrt_io_output_payload_result;
    if(sqrt_negative) begin
      sqrt_output_payload_value_mantissa[23] = 1'b1;
    end
    if(when_FpuCore_l1144) begin
      sqrt_output_payload_value_mantissa[23] = 1'b1;
    end
  end

  assign sqrt_output_payload_scrap = sqrt_scrap;
  always @(*) begin
    sqrt_output_payload_NV = 1'b0;
    if(sqrt_negative) begin
      sqrt_output_payload_NV = 1'b1;
    end
    if(when_FpuCore_l1144) begin
      sqrt_output_payload_NV = (! sqrt_input_payload_rs1_mantissa[22]);
    end
  end

  assign sqrt_output_payload_DZ = 1'b0;
  assign sqrt_negative = (((! (sqrt_input_payload_rs1_special && (sqrt_input_payload_rs1_exponent[1 : 0] == 2'b10))) && (! (sqrt_input_payload_rs1_special && (sqrt_input_payload_rs1_exponent[1 : 0] == 2'b00)))) && sqrt_input_payload_rs1_sign);
  assign when_FpuCore_l1137 = (sqrt_input_payload_rs1_special && (sqrt_input_payload_rs1_exponent[1 : 0] == 2'b01));
  assign when_FpuCore_l1144 = (sqrt_input_payload_rs1_special && (sqrt_input_payload_rs1_exponent[1 : 0] == 2'b10));
  assign when_FpuCore_l1148 = (sqrt_input_payload_rs1_special && (sqrt_input_payload_rs1_exponent[1 : 0] == 2'b00));
  assign add_preShifter_input_valid = decode_add_valid;
  assign decode_add_ready = add_preShifter_input_ready;
  assign add_preShifter_input_payload_rs1_mantissa = decode_add_payload_rs1_mantissa;
  assign add_preShifter_input_payload_rs1_exponent = decode_add_payload_rs1_exponent;
  assign add_preShifter_input_payload_rs1_sign = decode_add_payload_rs1_sign;
  assign add_preShifter_input_payload_rs1_special = decode_add_payload_rs1_special;
  assign add_preShifter_input_payload_rs2_mantissa = decode_add_payload_rs2_mantissa;
  assign add_preShifter_input_payload_rs2_exponent = decode_add_payload_rs2_exponent;
  assign add_preShifter_input_payload_rs2_sign = decode_add_payload_rs2_sign;
  assign add_preShifter_input_payload_rs2_special = decode_add_payload_rs2_special;
  assign add_preShifter_input_payload_rd = decode_add_payload_rd;
  assign add_preShifter_input_payload_roundMode = decode_add_payload_roundMode;
  assign add_preShifter_input_payload_needCommit = decode_add_payload_needCommit;
  assign add_preShifter_output_valid = add_preShifter_input_valid;
  assign add_preShifter_input_ready = add_preShifter_output_ready;
  assign add_preShifter_exp21 = ({1'b0,add_preShifter_input_payload_rs2_exponent} - {1'b0,add_preShifter_input_payload_rs1_exponent});
  assign add_preShifter_rs1ExponentBigger = ((add_preShifter_exp21[9] || (add_preShifter_input_payload_rs2_special && (add_preShifter_input_payload_rs2_exponent[1 : 0] == 2'b00))) && (! (add_preShifter_input_payload_rs1_special && (add_preShifter_input_payload_rs1_exponent[1 : 0] == 2'b00))));
  assign add_preShifter_rs1ExponentEqual = (add_preShifter_input_payload_rs1_exponent == add_preShifter_input_payload_rs2_exponent);
  assign add_preShifter_rs1MantissaBigger = (add_preShifter_input_payload_rs2_mantissa < add_preShifter_input_payload_rs1_mantissa);
  assign add_preShifter_absRs1Bigger = ((((add_preShifter_rs1ExponentBigger || (add_preShifter_rs1ExponentEqual && add_preShifter_rs1MantissaBigger)) && (! (add_preShifter_input_payload_rs1_special && (add_preShifter_input_payload_rs1_exponent[1 : 0] == 2'b00)))) || (add_preShifter_input_payload_rs1_special && (add_preShifter_input_payload_rs1_exponent[1 : 0] == 2'b01))) && (! (add_preShifter_input_payload_rs2_special && (add_preShifter_input_payload_rs2_exponent[1 : 0] == 2'b01))));
  assign add_preShifter_output_payload_rs1_mantissa = add_preShifter_input_payload_rs1_mantissa;
  assign add_preShifter_output_payload_rs1_exponent = add_preShifter_input_payload_rs1_exponent;
  assign add_preShifter_output_payload_rs1_sign = add_preShifter_input_payload_rs1_sign;
  assign add_preShifter_output_payload_rs1_special = add_preShifter_input_payload_rs1_special;
  assign add_preShifter_output_payload_rs2_mantissa = add_preShifter_input_payload_rs2_mantissa;
  assign add_preShifter_output_payload_rs2_exponent = add_preShifter_input_payload_rs2_exponent;
  assign add_preShifter_output_payload_rs2_sign = add_preShifter_input_payload_rs2_sign;
  assign add_preShifter_output_payload_rs2_special = add_preShifter_input_payload_rs2_special;
  assign add_preShifter_output_payload_rd = add_preShifter_input_payload_rd;
  assign add_preShifter_output_payload_roundMode = add_preShifter_input_payload_roundMode;
  assign add_preShifter_output_payload_needCommit = add_preShifter_input_payload_needCommit;
  assign add_preShifter_output_payload_absRs1Bigger = add_preShifter_absRs1Bigger;
  assign add_preShifter_output_payload_rs1ExponentBigger = add_preShifter_rs1ExponentBigger;
  always @(*) begin
    add_preShifter_output_ready = add_shifter_input_ready;
    if(when_Stream_l375_12) begin
      add_preShifter_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_12 = (! add_shifter_input_valid);
  assign add_shifter_input_valid = add_preShifter_output_rValid;
  assign add_shifter_input_payload_rs1_mantissa = add_preShifter_output_rData_rs1_mantissa;
  assign add_shifter_input_payload_rs1_exponent = add_preShifter_output_rData_rs1_exponent;
  assign add_shifter_input_payload_rs1_sign = add_preShifter_output_rData_rs1_sign;
  assign add_shifter_input_payload_rs1_special = add_preShifter_output_rData_rs1_special;
  assign add_shifter_input_payload_rs2_mantissa = add_preShifter_output_rData_rs2_mantissa;
  assign add_shifter_input_payload_rs2_exponent = add_preShifter_output_rData_rs2_exponent;
  assign add_shifter_input_payload_rs2_sign = add_preShifter_output_rData_rs2_sign;
  assign add_shifter_input_payload_rs2_special = add_preShifter_output_rData_rs2_special;
  assign add_shifter_input_payload_rd = add_preShifter_output_rData_rd;
  assign add_shifter_input_payload_roundMode = add_preShifter_output_rData_roundMode;
  assign add_shifter_input_payload_needCommit = add_preShifter_output_rData_needCommit;
  assign add_shifter_input_payload_absRs1Bigger = add_preShifter_output_rData_absRs1Bigger;
  assign add_shifter_input_payload_rs1ExponentBigger = add_preShifter_output_rData_rs1ExponentBigger;
  assign add_shifter_output_valid = add_shifter_input_valid;
  assign add_shifter_input_ready = add_shifter_output_ready;
  assign add_shifter_output_payload_rs1_mantissa = add_shifter_input_payload_rs1_mantissa;
  assign add_shifter_output_payload_rs1_exponent = add_shifter_input_payload_rs1_exponent;
  assign add_shifter_output_payload_rs1_sign = add_shifter_input_payload_rs1_sign;
  assign add_shifter_output_payload_rs1_special = add_shifter_input_payload_rs1_special;
  assign add_shifter_output_payload_rs2_mantissa = add_shifter_input_payload_rs2_mantissa;
  assign add_shifter_output_payload_rs2_exponent = add_shifter_input_payload_rs2_exponent;
  assign add_shifter_output_payload_rs2_sign = add_shifter_input_payload_rs2_sign;
  assign add_shifter_output_payload_rs2_special = add_shifter_input_payload_rs2_special;
  assign add_shifter_output_payload_rd = add_shifter_input_payload_rd;
  assign add_shifter_output_payload_roundMode = add_shifter_input_payload_roundMode;
  assign add_shifter_output_payload_needCommit = add_shifter_input_payload_needCommit;
  assign add_shifter_exp21 = ({1'b0,add_shifter_input_payload_rs2_exponent} - {1'b0,add_shifter_input_payload_rs1_exponent});
  assign _zz_add_shifter_shiftBy = add_shifter_exp21;
  assign add_shifter_shiftBy = (_zz_add_shifter_shiftBy_1 + _zz_add_shifter_shiftBy_3);
  assign add_shifter_shiftOverflow = (10'h01a <= add_shifter_shiftBy);
  assign add_shifter_passThrough = ((add_shifter_shiftOverflow || (add_shifter_input_payload_rs1_special && (add_shifter_input_payload_rs1_exponent[1 : 0] == 2'b00))) || (add_shifter_input_payload_rs2_special && (add_shifter_input_payload_rs2_exponent[1 : 0] == 2'b00)));
  assign add_shifter_xySign = (add_shifter_input_payload_absRs1Bigger ? add_shifter_input_payload_rs1_sign : add_shifter_input_payload_rs2_sign);
  assign add_shifter_output_payload_xSign = (add_shifter_xySign ^ (add_shifter_input_payload_rs1ExponentBigger ? add_shifter_input_payload_rs1_sign : add_shifter_input_payload_rs2_sign));
  assign add_shifter_output_payload_ySign = (add_shifter_xySign ^ (add_shifter_input_payload_rs1ExponentBigger ? add_shifter_input_payload_rs2_sign : add_shifter_input_payload_rs1_sign));
  assign add_shifter_xMantissa = {1'b1,(add_shifter_input_payload_rs1ExponentBigger ? add_shifter_input_payload_rs1_mantissa : add_shifter_input_payload_rs2_mantissa)};
  assign add_shifter_yMantissaUnshifted = {1'b1,(add_shifter_input_payload_rs1ExponentBigger ? add_shifter_input_payload_rs2_mantissa : add_shifter_input_payload_rs1_mantissa)};
  assign add_shifter_yMantissa = add_shifter_yMantissaUnshifted;
  always @(*) begin
    add_shifter_roundingScrap = 1'b0;
    if(when_FpuCore_l1419) begin
      add_shifter_roundingScrap = 1'b1;
    end
    if(when_FpuCore_l1419_1) begin
      add_shifter_roundingScrap = 1'b1;
    end
    if(when_FpuCore_l1419_2) begin
      add_shifter_roundingScrap = 1'b1;
    end
    if(when_FpuCore_l1419_3) begin
      add_shifter_roundingScrap = 1'b1;
    end
    if(when_FpuCore_l1419_4) begin
      add_shifter_roundingScrap = 1'b1;
    end
    if(add_shifter_shiftOverflow) begin
      add_shifter_roundingScrap = 1'b1;
    end
    if(when_FpuCore_l1424) begin
      add_shifter_roundingScrap = 1'b0;
    end
  end

  assign when_FpuCore_l1419 = (add_shifter_shiftBy[4] && (add_shifter_yMantissa[15 : 0] != 16'h0));
  assign when_FpuCore_l1419_1 = (add_shifter_shiftBy[3] && (add_shifter_yMantissa_1[7 : 0] != 8'h0));
  assign when_FpuCore_l1419_2 = (add_shifter_shiftBy[2] && (add_shifter_yMantissa_2[3 : 0] != 4'b0000));
  assign when_FpuCore_l1419_3 = (add_shifter_shiftBy[1] && (add_shifter_yMantissa_3[1 : 0] != 2'b00));
  assign when_FpuCore_l1419_4 = (add_shifter_shiftBy[0] && (add_shifter_yMantissa_4[0 : 0] != 1'b0));
  assign when_FpuCore_l1424 = (add_shifter_input_payload_rs1_special || add_shifter_input_payload_rs2_special);
  assign add_shifter_output_payload_xyExponent = (add_shifter_input_payload_rs1ExponentBigger ? add_shifter_input_payload_rs1_exponent : add_shifter_input_payload_rs2_exponent);
  assign add_shifter_output_payload_xMantissa = add_shifter_xMantissa;
  assign add_shifter_output_payload_yMantissa = add_shifter_yMantissa_5;
  assign add_shifter_output_payload_xySign = add_shifter_xySign;
  assign add_shifter_output_payload_roundingScrap = add_shifter_roundingScrap;
  always @(*) begin
    add_shifter_output_ready = add_math_input_ready;
    if(when_Stream_l375_13) begin
      add_shifter_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_13 = (! add_math_input_valid);
  assign add_math_input_valid = add_shifter_output_rValid;
  assign add_math_input_payload_rs1_mantissa = add_shifter_output_rData_rs1_mantissa;
  assign add_math_input_payload_rs1_exponent = add_shifter_output_rData_rs1_exponent;
  assign add_math_input_payload_rs1_sign = add_shifter_output_rData_rs1_sign;
  assign add_math_input_payload_rs1_special = add_shifter_output_rData_rs1_special;
  assign add_math_input_payload_rs2_mantissa = add_shifter_output_rData_rs2_mantissa;
  assign add_math_input_payload_rs2_exponent = add_shifter_output_rData_rs2_exponent;
  assign add_math_input_payload_rs2_sign = add_shifter_output_rData_rs2_sign;
  assign add_math_input_payload_rs2_special = add_shifter_output_rData_rs2_special;
  assign add_math_input_payload_rd = add_shifter_output_rData_rd;
  assign add_math_input_payload_roundMode = add_shifter_output_rData_roundMode;
  assign add_math_input_payload_needCommit = add_shifter_output_rData_needCommit;
  assign add_math_input_payload_xSign = add_shifter_output_rData_xSign;
  assign add_math_input_payload_ySign = add_shifter_output_rData_ySign;
  assign add_math_input_payload_xMantissa = add_shifter_output_rData_xMantissa;
  assign add_math_input_payload_yMantissa = add_shifter_output_rData_yMantissa;
  assign add_math_input_payload_xyExponent = add_shifter_output_rData_xyExponent;
  assign add_math_input_payload_xySign = add_shifter_output_rData_xySign;
  assign add_math_input_payload_roundingScrap = add_shifter_output_rData_roundingScrap;
  assign add_math_output_valid = add_math_input_valid;
  assign add_math_input_ready = add_math_output_ready;
  assign add_math_output_payload_rs1_mantissa = add_math_input_payload_rs1_mantissa;
  assign add_math_output_payload_rs1_exponent = add_math_input_payload_rs1_exponent;
  assign add_math_output_payload_rs1_sign = add_math_input_payload_rs1_sign;
  assign add_math_output_payload_rs1_special = add_math_input_payload_rs1_special;
  assign add_math_output_payload_rs2_mantissa = add_math_input_payload_rs2_mantissa;
  assign add_math_output_payload_rs2_exponent = add_math_input_payload_rs2_exponent;
  assign add_math_output_payload_rs2_sign = add_math_input_payload_rs2_sign;
  assign add_math_output_payload_rs2_special = add_math_input_payload_rs2_special;
  assign add_math_output_payload_rd = add_math_input_payload_rd;
  assign add_math_output_payload_roundMode = add_math_input_payload_roundMode;
  assign add_math_output_payload_needCommit = add_math_input_payload_needCommit;
  assign add_math_output_payload_xSign = add_math_input_payload_xSign;
  assign add_math_output_payload_ySign = add_math_input_payload_ySign;
  assign add_math_output_payload_xMantissa = add_math_input_payload_xMantissa;
  assign add_math_output_payload_yMantissa = add_math_input_payload_yMantissa;
  assign add_math_output_payload_xyExponent = add_math_input_payload_xyExponent;
  assign add_math_output_payload_xySign = add_math_input_payload_xySign;
  assign add_math_output_payload_roundingScrap = add_math_input_payload_roundingScrap;
  assign add_math_xSigned = _zz_add_math_xSigned;
  assign add_math_ySigned = _zz_add_math_ySigned;
  assign add_math_output_payload_xyMantissa = _zz_add_math_output_payload_xyMantissa[26 : 0];
  always @(*) begin
    add_math_output_ready = add_oh_input_ready;
    if(when_Stream_l375_14) begin
      add_math_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_14 = (! add_oh_input_valid);
  assign add_oh_input_valid = add_math_output_rValid;
  assign add_oh_input_payload_rs1_mantissa = add_math_output_rData_rs1_mantissa;
  assign add_oh_input_payload_rs1_exponent = add_math_output_rData_rs1_exponent;
  assign add_oh_input_payload_rs1_sign = add_math_output_rData_rs1_sign;
  assign add_oh_input_payload_rs1_special = add_math_output_rData_rs1_special;
  assign add_oh_input_payload_rs2_mantissa = add_math_output_rData_rs2_mantissa;
  assign add_oh_input_payload_rs2_exponent = add_math_output_rData_rs2_exponent;
  assign add_oh_input_payload_rs2_sign = add_math_output_rData_rs2_sign;
  assign add_oh_input_payload_rs2_special = add_math_output_rData_rs2_special;
  assign add_oh_input_payload_rd = add_math_output_rData_rd;
  assign add_oh_input_payload_roundMode = add_math_output_rData_roundMode;
  assign add_oh_input_payload_needCommit = add_math_output_rData_needCommit;
  assign add_oh_input_payload_xSign = add_math_output_rData_xSign;
  assign add_oh_input_payload_ySign = add_math_output_rData_ySign;
  assign add_oh_input_payload_xMantissa = add_math_output_rData_xMantissa;
  assign add_oh_input_payload_yMantissa = add_math_output_rData_yMantissa;
  assign add_oh_input_payload_xyExponent = add_math_output_rData_xyExponent;
  assign add_oh_input_payload_xySign = add_math_output_rData_xySign;
  assign add_oh_input_payload_roundingScrap = add_math_output_rData_roundingScrap;
  assign add_oh_input_payload_xyMantissa = add_math_output_rData_xyMantissa;
  assign add_oh_input_fire = (add_oh_input_valid && add_oh_input_ready);
  assign when_FpuCore_l221_4 = ((add_oh_input_fire && add_oh_input_payload_needCommit) && 1'b1);
  assign add_oh_isCommited = commitLogic_0_add_notEmpty;
  assign _zz_add_oh_input_ready = (! (add_oh_input_payload_needCommit && (! add_oh_isCommited)));
  assign add_oh_input_ready = (add_oh_output_ready && _zz_add_oh_input_ready);
  assign add_oh_output_valid = (add_oh_input_valid && _zz_add_oh_input_ready);
  assign add_oh_output_payload_rs1_mantissa = add_oh_input_payload_rs1_mantissa;
  assign add_oh_output_payload_rs1_exponent = add_oh_input_payload_rs1_exponent;
  assign add_oh_output_payload_rs1_sign = add_oh_input_payload_rs1_sign;
  assign add_oh_output_payload_rs1_special = add_oh_input_payload_rs1_special;
  assign add_oh_output_payload_rs2_mantissa = add_oh_input_payload_rs2_mantissa;
  assign add_oh_output_payload_rs2_exponent = add_oh_input_payload_rs2_exponent;
  assign add_oh_output_payload_rs2_sign = add_oh_input_payload_rs2_sign;
  assign add_oh_output_payload_rs2_special = add_oh_input_payload_rs2_special;
  assign add_oh_output_payload_rd = add_oh_input_payload_rd;
  assign add_oh_output_payload_roundMode = add_oh_input_payload_roundMode;
  assign add_oh_output_payload_needCommit = add_oh_input_payload_needCommit;
  assign add_oh_output_payload_xSign = add_oh_input_payload_xSign;
  assign add_oh_output_payload_ySign = add_oh_input_payload_ySign;
  assign add_oh_output_payload_xMantissa = add_oh_input_payload_xMantissa;
  assign add_oh_output_payload_yMantissa = add_oh_input_payload_yMantissa;
  assign add_oh_output_payload_xyExponent = add_oh_input_payload_xyExponent;
  assign add_oh_output_payload_xySign = add_oh_input_payload_xySign;
  assign add_oh_output_payload_roundingScrap = add_oh_input_payload_roundingScrap;
  assign add_oh_output_payload_xyMantissa = add_oh_input_payload_xyMantissa;
  assign _zz_add_oh_shift = {add_oh_output_payload_xyMantissa[0],{add_oh_output_payload_xyMantissa[1],{add_oh_output_payload_xyMantissa[2],{add_oh_output_payload_xyMantissa[3],{add_oh_output_payload_xyMantissa[4],{add_oh_output_payload_xyMantissa[5],{add_oh_output_payload_xyMantissa[6],{_zz__zz_add_oh_shift,{_zz__zz_add_oh_shift_1,_zz__zz_add_oh_shift_2}}}}}}}}};
  assign _zz_add_oh_shift_1 = (_zz_add_oh_shift & (~ _zz__zz_add_oh_shift_1_1));
  assign _zz_add_oh_shift_2 = _zz_add_oh_shift_1[3];
  assign _zz_add_oh_shift_3 = _zz_add_oh_shift_1[5];
  assign _zz_add_oh_shift_4 = _zz_add_oh_shift_1[6];
  assign _zz_add_oh_shift_5 = _zz_add_oh_shift_1[7];
  assign _zz_add_oh_shift_6 = _zz_add_oh_shift_1[9];
  assign _zz_add_oh_shift_7 = _zz_add_oh_shift_1[10];
  assign _zz_add_oh_shift_8 = _zz_add_oh_shift_1[11];
  assign _zz_add_oh_shift_9 = _zz_add_oh_shift_1[12];
  assign _zz_add_oh_shift_10 = _zz_add_oh_shift_1[13];
  assign _zz_add_oh_shift_11 = _zz_add_oh_shift_1[14];
  assign _zz_add_oh_shift_12 = _zz_add_oh_shift_1[15];
  assign _zz_add_oh_shift_13 = _zz_add_oh_shift_1[17];
  assign _zz_add_oh_shift_14 = _zz_add_oh_shift_1[18];
  assign _zz_add_oh_shift_15 = _zz_add_oh_shift_1[19];
  assign _zz_add_oh_shift_16 = _zz_add_oh_shift_1[20];
  assign _zz_add_oh_shift_17 = _zz_add_oh_shift_1[21];
  assign _zz_add_oh_shift_18 = _zz_add_oh_shift_1[22];
  assign _zz_add_oh_shift_19 = _zz_add_oh_shift_1[23];
  assign _zz_add_oh_shift_20 = _zz_add_oh_shift_1[24];
  assign _zz_add_oh_shift_21 = _zz_add_oh_shift_1[25];
  assign _zz_add_oh_shift_22 = _zz_add_oh_shift_1[26];
  assign _zz_add_oh_shift_23 = ((((((((((((_zz_add_oh_shift_1[1] || _zz_add_oh_shift_2) || _zz_add_oh_shift_3) || _zz_add_oh_shift_5) || _zz_add_oh_shift_6) || _zz_add_oh_shift_8) || _zz_add_oh_shift_10) || _zz_add_oh_shift_12) || _zz_add_oh_shift_13) || _zz_add_oh_shift_15) || _zz_add_oh_shift_17) || _zz_add_oh_shift_19) || _zz_add_oh_shift_21);
  assign _zz_add_oh_shift_24 = ((((((((((((_zz_add_oh_shift_1[2] || _zz_add_oh_shift_2) || _zz_add_oh_shift_4) || _zz_add_oh_shift_5) || _zz_add_oh_shift_7) || _zz_add_oh_shift_8) || _zz_add_oh_shift_11) || _zz_add_oh_shift_12) || _zz_add_oh_shift_14) || _zz_add_oh_shift_15) || _zz_add_oh_shift_18) || _zz_add_oh_shift_19) || _zz_add_oh_shift_22);
  assign _zz_add_oh_shift_25 = (((((((((((_zz_add_oh_shift_1[4] || _zz_add_oh_shift_3) || _zz_add_oh_shift_4) || _zz_add_oh_shift_5) || _zz_add_oh_shift_9) || _zz_add_oh_shift_10) || _zz_add_oh_shift_11) || _zz_add_oh_shift_12) || _zz_add_oh_shift_16) || _zz_add_oh_shift_17) || _zz_add_oh_shift_18) || _zz_add_oh_shift_19);
  assign _zz_add_oh_shift_26 = ((((((((((_zz_add_oh_shift_1[8] || _zz_add_oh_shift_6) || _zz_add_oh_shift_7) || _zz_add_oh_shift_8) || _zz_add_oh_shift_9) || _zz_add_oh_shift_10) || _zz_add_oh_shift_11) || _zz_add_oh_shift_12) || _zz_add_oh_shift_20) || _zz_add_oh_shift_21) || _zz_add_oh_shift_22);
  assign _zz_add_oh_shift_27 = ((((((((((_zz_add_oh_shift_1[16] || _zz_add_oh_shift_13) || _zz_add_oh_shift_14) || _zz_add_oh_shift_15) || _zz_add_oh_shift_16) || _zz_add_oh_shift_17) || _zz_add_oh_shift_18) || _zz_add_oh_shift_19) || _zz_add_oh_shift_20) || _zz_add_oh_shift_21) || _zz_add_oh_shift_22);
  assign add_oh_shift = {_zz_add_oh_shift_27,{_zz_add_oh_shift_26,{_zz_add_oh_shift_25,{_zz_add_oh_shift_24,_zz_add_oh_shift_23}}}};
  assign add_oh_output_payload_shift = add_oh_shift;
  always @(*) begin
    add_oh_output_ready = add_norm_input_ready;
    if(when_Stream_l375_15) begin
      add_oh_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_15 = (! add_norm_input_valid);
  assign add_norm_input_valid = add_oh_output_rValid;
  assign add_norm_input_payload_rs1_mantissa = add_oh_output_rData_rs1_mantissa;
  assign add_norm_input_payload_rs1_exponent = add_oh_output_rData_rs1_exponent;
  assign add_norm_input_payload_rs1_sign = add_oh_output_rData_rs1_sign;
  assign add_norm_input_payload_rs1_special = add_oh_output_rData_rs1_special;
  assign add_norm_input_payload_rs2_mantissa = add_oh_output_rData_rs2_mantissa;
  assign add_norm_input_payload_rs2_exponent = add_oh_output_rData_rs2_exponent;
  assign add_norm_input_payload_rs2_sign = add_oh_output_rData_rs2_sign;
  assign add_norm_input_payload_rs2_special = add_oh_output_rData_rs2_special;
  assign add_norm_input_payload_rd = add_oh_output_rData_rd;
  assign add_norm_input_payload_roundMode = add_oh_output_rData_roundMode;
  assign add_norm_input_payload_needCommit = add_oh_output_rData_needCommit;
  assign add_norm_input_payload_xSign = add_oh_output_rData_xSign;
  assign add_norm_input_payload_ySign = add_oh_output_rData_ySign;
  assign add_norm_input_payload_xMantissa = add_oh_output_rData_xMantissa;
  assign add_norm_input_payload_yMantissa = add_oh_output_rData_yMantissa;
  assign add_norm_input_payload_xyExponent = add_oh_output_rData_xyExponent;
  assign add_norm_input_payload_xySign = add_oh_output_rData_xySign;
  assign add_norm_input_payload_roundingScrap = add_oh_output_rData_roundingScrap;
  assign add_norm_input_payload_xyMantissa = add_oh_output_rData_xyMantissa;
  assign add_norm_input_payload_shift = add_oh_output_rData_shift;
  assign add_norm_output_valid = add_norm_input_valid;
  assign add_norm_input_ready = add_norm_output_ready;
  assign add_norm_output_payload_rs1_mantissa = add_norm_input_payload_rs1_mantissa;
  assign add_norm_output_payload_rs1_exponent = add_norm_input_payload_rs1_exponent;
  assign add_norm_output_payload_rs1_sign = add_norm_input_payload_rs1_sign;
  assign add_norm_output_payload_rs1_special = add_norm_input_payload_rs1_special;
  assign add_norm_output_payload_rs2_mantissa = add_norm_input_payload_rs2_mantissa;
  assign add_norm_output_payload_rs2_exponent = add_norm_input_payload_rs2_exponent;
  assign add_norm_output_payload_rs2_sign = add_norm_input_payload_rs2_sign;
  assign add_norm_output_payload_rs2_special = add_norm_input_payload_rs2_special;
  assign add_norm_output_payload_rd = add_norm_input_payload_rd;
  assign add_norm_output_payload_roundMode = add_norm_input_payload_roundMode;
  assign add_norm_output_payload_needCommit = add_norm_input_payload_needCommit;
  assign add_norm_output_payload_xySign = add_norm_input_payload_xySign;
  assign add_norm_output_payload_roundingScrap = add_norm_input_payload_roundingScrap;
  assign add_norm_output_payload_mantissa = (add_norm_input_payload_xyMantissa <<< add_norm_input_payload_shift);
  assign add_norm_output_payload_exponent = (_zz_add_norm_output_payload_exponent + 10'h001);
  assign add_norm_output_payload_forceInfinity = ((add_norm_input_payload_rs1_special && (add_norm_input_payload_rs1_exponent[1 : 0] == 2'b01)) || (add_norm_input_payload_rs2_special && (add_norm_input_payload_rs2_exponent[1 : 0] == 2'b01)));
  assign add_norm_output_payload_forceZero = ((add_norm_input_payload_xyMantissa == 27'h0) || ((add_norm_input_payload_rs1_special && (add_norm_input_payload_rs1_exponent[1 : 0] == 2'b00)) && (add_norm_input_payload_rs2_special && (add_norm_input_payload_rs2_exponent[1 : 0] == 2'b00))));
  assign add_norm_output_payload_infinityNan = (((add_norm_input_payload_rs1_special && (add_norm_input_payload_rs1_exponent[1 : 0] == 2'b01)) && (add_norm_input_payload_rs2_special && (add_norm_input_payload_rs2_exponent[1 : 0] == 2'b01))) && (add_norm_input_payload_rs1_sign ^ add_norm_input_payload_rs2_sign));
  assign add_norm_output_payload_forceNan = (((add_norm_input_payload_rs1_special && (add_norm_input_payload_rs1_exponent[1 : 0] == 2'b10)) || (add_norm_input_payload_rs2_special && (add_norm_input_payload_rs2_exponent[1 : 0] == 2'b10))) || add_norm_output_payload_infinityNan);
  assign add_norm_output_payload_xyMantissaZero = (add_norm_input_payload_xyMantissa == 27'h0);
  assign add_result_input_valid = add_norm_output_valid;
  assign add_norm_output_ready = add_result_input_ready;
  assign add_result_input_payload_rs1_mantissa = add_norm_output_payload_rs1_mantissa;
  assign add_result_input_payload_rs1_exponent = add_norm_output_payload_rs1_exponent;
  assign add_result_input_payload_rs1_sign = add_norm_output_payload_rs1_sign;
  assign add_result_input_payload_rs1_special = add_norm_output_payload_rs1_special;
  assign add_result_input_payload_rs2_mantissa = add_norm_output_payload_rs2_mantissa;
  assign add_result_input_payload_rs2_exponent = add_norm_output_payload_rs2_exponent;
  assign add_result_input_payload_rs2_sign = add_norm_output_payload_rs2_sign;
  assign add_result_input_payload_rs2_special = add_norm_output_payload_rs2_special;
  assign add_result_input_payload_rd = add_norm_output_payload_rd;
  assign add_result_input_payload_roundMode = add_norm_output_payload_roundMode;
  assign add_result_input_payload_needCommit = add_norm_output_payload_needCommit;
  assign add_result_input_payload_mantissa = add_norm_output_payload_mantissa;
  assign add_result_input_payload_exponent = add_norm_output_payload_exponent;
  assign add_result_input_payload_infinityNan = add_norm_output_payload_infinityNan;
  assign add_result_input_payload_forceNan = add_norm_output_payload_forceNan;
  assign add_result_input_payload_forceZero = add_norm_output_payload_forceZero;
  assign add_result_input_payload_forceInfinity = add_norm_output_payload_forceInfinity;
  assign add_result_input_payload_xySign = add_norm_output_payload_xySign;
  assign add_result_input_payload_roundingScrap = add_norm_output_payload_roundingScrap;
  assign add_result_input_payload_xyMantissaZero = add_norm_output_payload_xyMantissaZero;
  assign add_result_output_valid = add_result_input_valid;
  assign add_result_input_ready = add_result_output_ready;
  assign add_result_output_payload_rd = add_result_input_payload_rd;
  always @(*) begin
    add_result_output_payload_value_sign = add_result_input_payload_xySign;
    if(!add_result_input_payload_forceNan) begin
      if(!add_result_input_payload_forceInfinity) begin
        if(add_result_input_payload_forceZero) begin
          if(when_FpuCore_l1513) begin
            add_result_output_payload_value_sign = (add_result_input_payload_rs1_sign && add_result_input_payload_rs2_sign);
          end
          if(when_FpuCore_l1516) begin
            add_result_output_payload_value_sign = 1'b1;
          end
        end
      end
    end
  end

  always @(*) begin
    add_result_output_payload_value_mantissa = _zz_add_result_output_payload_value_mantissa[23:0];
    if(add_result_input_payload_forceNan) begin
      add_result_output_payload_value_mantissa[23] = 1'b1;
    end
  end

  always @(*) begin
    add_result_output_payload_value_exponent = add_result_input_payload_exponent[8:0];
    if(add_result_input_payload_forceNan) begin
      add_result_output_payload_value_exponent[1 : 0] = 2'b10;
      add_result_output_payload_value_exponent[2] = 1'b1;
    end else begin
      if(add_result_input_payload_forceInfinity) begin
        add_result_output_payload_value_exponent[1 : 0] = 2'b01;
      end else begin
        if(add_result_input_payload_forceZero) begin
          add_result_output_payload_value_exponent[1 : 0] = 2'b00;
        end
      end
    end
  end

  always @(*) begin
    add_result_output_payload_value_special = 1'b0;
    if(add_result_input_payload_forceNan) begin
      add_result_output_payload_value_special = 1'b1;
    end else begin
      if(add_result_input_payload_forceInfinity) begin
        add_result_output_payload_value_special = 1'b1;
      end else begin
        if(add_result_input_payload_forceZero) begin
          add_result_output_payload_value_special = 1'b1;
        end
      end
    end
  end

  assign add_result_output_payload_roundMode = add_result_input_payload_roundMode;
  assign add_result_output_payload_scrap = ((add_result_input_payload_mantissa[1] || add_result_input_payload_mantissa[0]) || add_result_input_payload_roundingScrap);
  assign add_result_output_payload_NV = ((add_result_input_payload_infinityNan || ((add_result_input_payload_rs1_special && (add_result_input_payload_rs1_exponent[1 : 0] == 2'b10)) && (! add_result_input_payload_rs1_mantissa[24]))) || ((add_result_input_payload_rs2_special && (add_result_input_payload_rs2_exponent[1 : 0] == 2'b10)) && (! add_result_input_payload_rs2_mantissa[24])));
  assign add_result_output_payload_DZ = 1'b0;
  assign when_FpuCore_l1513 = (add_result_input_payload_xyMantissaZero || ((add_result_input_payload_rs1_special && (add_result_input_payload_rs1_exponent[1 : 0] == 2'b00)) && (add_result_input_payload_rs2_special && (add_result_input_payload_rs2_exponent[1 : 0] == 2'b00))));
  assign when_FpuCore_l1516 = ((add_result_input_payload_rs1_sign || add_result_input_payload_rs2_sign) && (add_result_input_payload_roundMode == FpuRoundMode_RDN));
  always @(*) begin
    load_s1_output_ready = load_s1_output_m2sPipe_ready;
    if(when_Stream_l375_16) begin
      load_s1_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_16 = (! load_s1_output_m2sPipe_valid);
  assign load_s1_output_m2sPipe_valid = load_s1_output_rValid;
  assign load_s1_output_m2sPipe_payload_rd = load_s1_output_rData_rd;
  assign load_s1_output_m2sPipe_payload_value_mantissa = load_s1_output_rData_value_mantissa;
  assign load_s1_output_m2sPipe_payload_value_exponent = load_s1_output_rData_value_exponent;
  assign load_s1_output_m2sPipe_payload_value_sign = load_s1_output_rData_value_sign;
  assign load_s1_output_m2sPipe_payload_value_special = load_s1_output_rData_value_special;
  assign load_s1_output_m2sPipe_payload_scrap = load_s1_output_rData_scrap;
  assign load_s1_output_m2sPipe_payload_roundMode = load_s1_output_rData_roundMode;
  assign load_s1_output_m2sPipe_payload_NV = load_s1_output_rData_NV;
  assign load_s1_output_m2sPipe_payload_DZ = load_s1_output_rData_DZ;
  always @(*) begin
    shortPip_output_ready = shortPip_output_m2sPipe_ready;
    if(when_Stream_l375_17) begin
      shortPip_output_ready = 1'b1;
    end
  end

  assign when_Stream_l375_17 = (! shortPip_output_m2sPipe_valid);
  assign shortPip_output_m2sPipe_valid = shortPip_output_rValid;
  assign shortPip_output_m2sPipe_payload_rd = shortPip_output_rData_rd;
  assign shortPip_output_m2sPipe_payload_value_mantissa = shortPip_output_rData_value_mantissa;
  assign shortPip_output_m2sPipe_payload_value_exponent = shortPip_output_rData_value_exponent;
  assign shortPip_output_m2sPipe_payload_value_sign = shortPip_output_rData_value_sign;
  assign shortPip_output_m2sPipe_payload_value_special = shortPip_output_rData_value_special;
  assign shortPip_output_m2sPipe_payload_scrap = shortPip_output_rData_scrap;
  assign shortPip_output_m2sPipe_payload_roundMode = shortPip_output_rData_roundMode;
  assign shortPip_output_m2sPipe_payload_NV = shortPip_output_rData_NV;
  assign shortPip_output_m2sPipe_payload_DZ = shortPip_output_rData_DZ;
  assign load_s1_output_m2sPipe_ready = streamArbiter_2_io_inputs_0_ready;
  assign sqrt_output_ready = streamArbiter_2_io_inputs_1_ready;
  assign div_output_ready = streamArbiter_2_io_inputs_2_ready;
  assign add_result_output_ready = streamArbiter_2_io_inputs_3_ready;
  assign mul_result_output_ready = streamArbiter_2_io_inputs_4_ready;
  assign shortPip_output_m2sPipe_ready = streamArbiter_2_io_inputs_5_ready;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_valid = streamArbiter_2_io_output_valid;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_rd = streamArbiter_2_io_output_payload_rd;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_mantissa = streamArbiter_2_io_output_payload_value_mantissa;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_exponent = streamArbiter_2_io_output_payload_value_exponent;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_sign = streamArbiter_2_io_output_payload_value_sign;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_special = streamArbiter_2_io_output_payload_value_special;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_scrap = streamArbiter_2_io_output_payload_scrap;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode = streamArbiter_2_io_output_payload_roundMode;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_NV = streamArbiter_2_io_output_payload_NV;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_DZ = streamArbiter_2_io_output_payload_DZ;
  assign FpuPlugin_fpu_streamArbiter_2_io_output_combStage_ready = 1'b1;
  assign merge_arbitrated_valid = FpuPlugin_fpu_streamArbiter_2_io_output_combStage_valid;
  assign merge_arbitrated_payload_rd = FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_rd;
  assign merge_arbitrated_payload_value_mantissa = FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_mantissa;
  assign merge_arbitrated_payload_value_exponent = FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_exponent;
  assign merge_arbitrated_payload_value_sign = FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_sign;
  assign merge_arbitrated_payload_value_special = FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_value_special;
  assign merge_arbitrated_payload_scrap = FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_scrap;
  assign merge_arbitrated_payload_roundMode = FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_roundMode;
  assign merge_arbitrated_payload_NV = FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_NV;
  assign merge_arbitrated_payload_DZ = FpuPlugin_fpu_streamArbiter_2_io_output_combStage_payload_DZ;
  assign roundFront_output_valid = roundFront_input_valid;
  assign roundFront_output_payload_rd = roundFront_input_payload_rd;
  assign roundFront_output_payload_value_mantissa = roundFront_input_payload_value_mantissa;
  assign roundFront_output_payload_value_exponent = roundFront_input_payload_value_exponent;
  assign roundFront_output_payload_value_sign = roundFront_input_payload_value_sign;
  assign roundFront_output_payload_value_special = roundFront_input_payload_value_special;
  assign roundFront_output_payload_scrap = roundFront_input_payload_scrap;
  assign roundFront_output_payload_roundMode = roundFront_input_payload_roundMode;
  assign roundFront_output_payload_NV = roundFront_input_payload_NV;
  assign roundFront_output_payload_DZ = roundFront_input_payload_DZ;
  assign roundFront_manAggregate = {roundFront_input_payload_value_mantissa,roundFront_input_payload_scrap};
  assign roundFront_expBase = 8'h81;
  assign roundFront_expDif = (_zz_roundFront_expDif - {1'b0,roundFront_input_payload_value_exponent});
  assign roundFront_expSubnormal = ((! roundFront_input_payload_value_special) && (! roundFront_expDif[9]));
  assign roundFront_discardCount = (roundFront_expSubnormal ? roundFront_expDif : 10'h0);
  assign roundFront_discardCountTrunk = roundFront_discardCount[4:0];
  always @(*) begin
    roundFront_exactMask = {(5'h17 < roundFront_discardCountTrunk),{(5'h16 < roundFront_discardCountTrunk),{(5'h15 < roundFront_discardCountTrunk),{(5'h14 < roundFront_discardCountTrunk),{(_zz_roundFront_exactMask < roundFront_discardCountTrunk),{_zz_roundFront_exactMask_1,{_zz_roundFront_exactMask_2,_zz_roundFront_exactMask_3}}}}}}};
    if(when_FpuCore_l1559) begin
      roundFront_exactMask = 25'h1ffffff;
    end
  end

  always @(*) begin
    roundFront_roundAdjusted = {_zz_roundFront_roundAdjusted[roundFront_discardCountTrunk],((roundFront_manAggregate & roundFront_exactMask) != 25'h0)};
    if(when_FpuCore_l1559) begin
      roundFront_roundAdjusted[1] = 1'b0;
    end
  end

  always @(*) begin
    roundFront_rneBit = _zz_roundFront_rneBit[roundFront_discardCountTrunk];
    if(when_FpuCore_l1559) begin
      roundFront_rneBit = 1'b0;
    end
  end

  assign when_FpuCore_l1559 = (10'h019 <= roundFront_discardCount);
  always @(*) begin
    case(roundFront_input_payload_roundMode)
      FpuRoundMode_RNE : begin
        _zz_roundFront_mantissaIncrement = (roundFront_roundAdjusted[1] && (roundFront_roundAdjusted[0] || roundFront_rneBit));
      end
      FpuRoundMode_RTZ : begin
        _zz_roundFront_mantissaIncrement = 1'b0;
      end
      FpuRoundMode_RDN : begin
        _zz_roundFront_mantissaIncrement = ((roundFront_roundAdjusted != 2'b00) && roundFront_input_payload_value_sign);
      end
      FpuRoundMode_RUP : begin
        _zz_roundFront_mantissaIncrement = ((roundFront_roundAdjusted != 2'b00) && (! roundFront_input_payload_value_sign));
      end
      default : begin
        _zz_roundFront_mantissaIncrement = roundFront_roundAdjusted[1];
      end
    endcase
  end

  assign roundFront_mantissaIncrement = ((! roundFront_input_payload_value_special) && _zz_roundFront_mantissaIncrement);
  assign roundFront_output_payload_mantissaIncrement = roundFront_mantissaIncrement;
  assign roundFront_output_payload_roundAdjusted = roundFront_roundAdjusted;
  assign roundFront_output_payload_exactMask = roundFront_exactMask;
  assign roundBack_output_valid = roundBack_input_valid;
  assign roundBack_adderMantissa = (roundBack_input_payload_value_mantissa[23 : 1] & (roundBack_input_payload_mantissaIncrement ? (~ _zz_roundBack_adderMantissa) : 23'h7fffff));
  assign roundBack_adderRightOp = _zz_roundBack_adderRightOp[22:0];
  assign _zz_roundBack_adder = {roundBack_input_payload_value_exponent,roundBack_adderMantissa};
  assign _zz_roundBack_adder_1 = roundBack_input_payload_mantissaIncrement;
  assign roundBack_adder = (_zz_roundBack_adder_2 + _zz_roundBack_adder_4);
  assign roundBack_masked = (roundBack_adder & (~ _zz_roundBack_masked));
  assign roundBack_math_special = roundBack_input_payload_value_special;
  assign roundBack_math_sign = roundBack_input_payload_value_sign;
  assign roundBack_math_exponent = roundBack_masked[31 : 23];
  assign roundBack_math_mantissa = roundBack_masked[22 : 0];
  always @(*) begin
    roundBack_patched_mantissa = roundBack_math_mantissa;
    if(when_FpuCore_l1619) begin
      if(when_FpuCore_l1629) begin
        roundBack_patched_mantissa = 23'h7fffff;
      end
    end
    if(when_FpuCore_l1638) begin
      if(when_FpuCore_l1648) begin
        roundBack_patched_mantissa = 23'h0;
      end
    end
  end

  always @(*) begin
    roundBack_patched_exponent = roundBack_math_exponent;
    if(when_FpuCore_l1619) begin
      if(when_FpuCore_l1629) begin
        roundBack_patched_exponent = roundBack_ofThreshold;
      end else begin
        roundBack_patched_exponent[1 : 0] = 2'b01;
      end
    end
    if(when_FpuCore_l1638) begin
      if(when_FpuCore_l1648) begin
        roundBack_patched_exponent = {2'd0, roundBack_ufThreshold};
      end else begin
        roundBack_patched_exponent[1 : 0] = 2'b00;
      end
    end
  end

  assign roundBack_patched_sign = roundBack_math_sign;
  always @(*) begin
    roundBack_patched_special = roundBack_math_special;
    if(when_FpuCore_l1619) begin
      if(!when_FpuCore_l1629) begin
        roundBack_patched_special = 1'b1;
      end
    end
    if(when_FpuCore_l1638) begin
      if(!when_FpuCore_l1648) begin
        roundBack_patched_special = 1'b1;
      end
    end
  end

  always @(*) begin
    roundBack_nx = 1'b0;
    if(when_FpuCore_l1619) begin
      roundBack_nx = 1'b1;
    end
    if(when_FpuCore_l1638) begin
      roundBack_nx = 1'b1;
    end
    if(when_FpuCore_l1657) begin
      roundBack_nx = 1'b1;
    end
  end

  always @(*) begin
    roundBack_of = 1'b0;
    if(when_FpuCore_l1619) begin
      roundBack_of = 1'b1;
    end
  end

  always @(*) begin
    roundBack_uf = 1'b0;
    if(when_FpuCore_l1616) begin
      roundBack_uf = 1'b1;
    end
    if(when_FpuCore_l1638) begin
      roundBack_uf = 1'b1;
    end
  end

  assign roundBack_ufSubnormalThreshold = 8'h80;
  assign roundBack_ufThreshold = 7'h6a;
  assign roundBack_ofThreshold = 9'h17e;
  always @(*) begin
    case(roundBack_input_payload_roundMode)
      FpuRoundMode_RNE : begin
        roundBack_threshold = 3'b110;
      end
      FpuRoundMode_RTZ : begin
        roundBack_threshold = 3'b110;
      end
      FpuRoundMode_RDN : begin
        roundBack_threshold = (roundBack_input_payload_value_sign ? 3'b101 : 3'b111);
      end
      FpuRoundMode_RUP : begin
        roundBack_threshold = (roundBack_input_payload_value_sign ? 3'b111 : 3'b101);
      end
      default : begin
        roundBack_threshold = 3'b110;
      end
    endcase
  end

  assign roundBack_borringRound = {roundBack_input_payload_value_mantissa[1 : 0],roundBack_input_payload_scrap};
  assign roundBack_borringCase = ((roundBack_input_payload_value_exponent == _zz_roundBack_borringCase) && (roundBack_borringRound < roundBack_threshold));
  assign when_FpuCore_l1616 = (((! roundBack_math_special) && ((roundBack_math_exponent <= _zz_when_FpuCore_l1616) || roundBack_borringCase)) && (roundBack_input_payload_roundAdjusted != 2'b00));
  assign when_FpuCore_l1619 = ((! roundBack_math_special) && (roundBack_ofThreshold < roundBack_math_exponent));
  always @(*) begin
    case(roundBack_input_payload_roundMode)
      FpuRoundMode_RNE : begin
        when_FpuCore_l1629 = 1'b0;
      end
      FpuRoundMode_RTZ : begin
        when_FpuCore_l1629 = 1'b1;
      end
      FpuRoundMode_RDN : begin
        when_FpuCore_l1629 = (! roundBack_math_sign);
      end
      FpuRoundMode_RUP : begin
        when_FpuCore_l1629 = roundBack_math_sign;
      end
      default : begin
        when_FpuCore_l1629 = 1'b0;
      end
    endcase
  end

  assign when_FpuCore_l1638 = ((! roundBack_math_special) && (roundBack_math_exponent < _zz_when_FpuCore_l1638));
  always @(*) begin
    case(roundBack_input_payload_roundMode)
      FpuRoundMode_RNE : begin
        when_FpuCore_l1648 = 1'b0;
      end
      FpuRoundMode_RTZ : begin
        when_FpuCore_l1648 = 1'b0;
      end
      FpuRoundMode_RDN : begin
        when_FpuCore_l1648 = roundBack_math_sign;
      end
      FpuRoundMode_RUP : begin
        when_FpuCore_l1648 = (! roundBack_math_sign);
      end
      default : begin
        when_FpuCore_l1648 = 1'b0;
      end
    endcase
  end

  assign when_FpuCore_l1657 = ((! roundBack_input_payload_value_special) && (roundBack_input_payload_roundAdjusted != 2'b00));
  assign roundBack_writes_0 = rf_scoreboards_0_writes_spinal_port1[0];
  assign roundBack_write = roundBack_writes_0;
  assign roundBack_output_payload_NX = (roundBack_nx && roundBack_write);
  assign roundBack_output_payload_OF = (roundBack_of && roundBack_write);
  assign roundBack_output_payload_UF = (roundBack_uf && roundBack_write);
  assign roundBack_output_payload_NV = (roundBack_input_payload_NV && roundBack_write);
  assign roundBack_output_payload_DZ = (roundBack_input_payload_DZ && roundBack_write);
  assign roundBack_output_payload_rd = roundBack_input_payload_rd;
  assign roundBack_output_payload_write = roundBack_write;
  assign roundBack_output_payload_value_mantissa = roundBack_patched_mantissa;
  assign roundBack_output_payload_value_exponent = roundBack_patched_exponent;
  assign roundBack_output_payload_value_sign = roundBack_patched_sign;
  assign roundBack_output_payload_value_special = roundBack_patched_special;
  assign io_port_0_completion_valid = (writeback_input_valid && 1'b1);
  assign io_port_0_completion_payload_flags_NX = writeback_input_payload_NX;
  assign io_port_0_completion_payload_flags_OF = writeback_input_payload_OF;
  assign io_port_0_completion_payload_flags_UF = writeback_input_payload_UF;
  assign io_port_0_completion_payload_flags_NV = writeback_input_payload_NV;
  assign io_port_0_completion_payload_flags_DZ = writeback_input_payload_DZ;
  assign io_port_0_completion_payload_written = writeback_input_payload_write;
  assign when_FpuCore_l1689 = 1'b1;
  assign writeback_port_valid = (writeback_input_valid && writeback_input_payload_write);
  assign writeback_port_payload_address = writeback_input_payload_rd;
  assign writeback_port_payload_data_value_mantissa = writeback_input_payload_value_mantissa;
  assign writeback_port_payload_data_value_exponent = writeback_input_payload_value_exponent;
  assign writeback_port_payload_data_value_sign = writeback_input_payload_value_sign;
  assign writeback_port_payload_data_value_special = writeback_input_payload_value_special;
  always @(posedge clk or posedge reset) begin
    if(reset) begin
      rf_init_counter <= 6'h0;
      FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN <= 1'b1;
      commitLogic_0_pending_counter <= 4'b0000;
      commitLogic_0_add_counter <= 4'b0000;
      commitLogic_0_mul_counter <= 4'b0000;
      commitLogic_0_div_counter <= 4'b0000;
      commitLogic_0_sqrt_counter <= 4'b0000;
      commitLogic_0_short_counter <= 4'b0000;
      io_port_0_cmd_rValidN <= 1'b1;
      read_s0_rValid <= 1'b0;
      decode_load_rValidN <= 1'b1;
      decode_load_s2mPipe_rValid <= 1'b0;
      decode_load_s2mPipe_m2sPipe_rValid <= 1'b0;
      load_s0_output_rValid <= 1'b0;
      decode_shortPip_rValid <= 1'b0;
      shortPip_rspStreams_0_rValid <= 1'b0;
      decode_mul_rValid <= 1'b0;
      mul_preMul_output_rValid <= 1'b0;
      mul_mul_output_rValid <= 1'b0;
      mul_sum1_output_rValid <= 1'b0;
      mul_sum2_output_rValid <= 1'b0;
      mul_result_mulToAdd_rValid <= 1'b0;
      decode_div_rValid <= 1'b0;
      div_cmdSent <= 1'b0;
      decode_sqrt_rValid <= 1'b0;
      sqrt_cmdSent <= 1'b0;
      add_preShifter_output_rValid <= 1'b0;
      add_shifter_output_rValid <= 1'b0;
      add_math_output_rValid <= 1'b0;
      add_oh_output_rValid <= 1'b0;
      load_s1_output_rValid <= 1'b0;
      shortPip_output_rValid <= 1'b0;
      roundFront_input_valid <= 1'b0;
      roundBack_input_valid <= 1'b0;
      writeback_input_valid <= 1'b0;
    end else begin
      if(when_FpuCore_l163) begin
        rf_init_counter <= (rf_init_counter + 6'h01);
      end
      if(streamFork_1_io_outputs_1_valid) begin
        FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN <= 1'b0;
      end
      if(FpuPlugin_fpu_streamFork_1_io_outputs_1_s2mPipe_ready) begin
        FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN <= 1'b1;
      end
      commitLogic_0_pending_counter <= (_zz_commitLogic_0_pending_counter - _zz_commitLogic_0_pending_counter_3);
      commitLogic_0_add_counter <= (_zz_commitLogic_0_add_counter - _zz_commitLogic_0_add_counter_3);
      commitLogic_0_mul_counter <= (_zz_commitLogic_0_mul_counter - _zz_commitLogic_0_mul_counter_3);
      commitLogic_0_div_counter <= (_zz_commitLogic_0_div_counter - _zz_commitLogic_0_div_counter_3);
      commitLogic_0_sqrt_counter <= (_zz_commitLogic_0_sqrt_counter - _zz_commitLogic_0_sqrt_counter_3);
      commitLogic_0_short_counter <= (_zz_commitLogic_0_short_counter - _zz_commitLogic_0_short_counter_3);
      if(io_port_0_cmd_valid) begin
        io_port_0_cmd_rValidN <= 1'b0;
      end
      if(scheduler_0_input_ready) begin
        io_port_0_cmd_rValidN <= 1'b1;
      end
      if(read_s0_ready) begin
        read_s0_rValid <= read_s0_valid;
      end
      if(decode_load_valid) begin
        decode_load_rValidN <= 1'b0;
      end
      if(decode_load_s2mPipe_ready) begin
        decode_load_rValidN <= 1'b1;
      end
      if(decode_load_s2mPipe_ready) begin
        decode_load_s2mPipe_rValid <= decode_load_s2mPipe_valid;
      end
      if(decode_load_s2mPipe_m2sPipe_ready) begin
        decode_load_s2mPipe_m2sPipe_rValid <= decode_load_s2mPipe_m2sPipe_valid;
      end
      if(load_s0_output_ready) begin
        load_s0_output_rValid <= load_s0_output_valid;
      end
      if(decode_shortPip_ready) begin
        decode_shortPip_rValid <= decode_shortPip_valid;
      end
      if(shortPip_rspStreams_0_ready) begin
        shortPip_rspStreams_0_rValid <= shortPip_rspStreams_0_valid;
      end
      if(decode_mul_ready) begin
        decode_mul_rValid <= decode_mul_valid;
      end
      if(mul_preMul_output_ready) begin
        mul_preMul_output_rValid <= mul_preMul_output_valid;
      end
      if(mul_mul_output_ready) begin
        mul_mul_output_rValid <= mul_mul_output_valid;
      end
      if(mul_sum1_output_ready) begin
        mul_sum1_output_rValid <= mul_sum1_output_valid;
      end
      if(mul_sum2_output_ready) begin
        mul_sum2_output_rValid <= mul_sum2_output_valid;
      end
      if(mul_result_mulToAdd_ready) begin
        mul_result_mulToAdd_rValid <= mul_result_mulToAdd_valid;
      end
      if(decode_div_valid) begin
        decode_div_rValid <= 1'b1;
      end
      if(div_input_fire) begin
        decode_div_rValid <= 1'b0;
      end
      if(FpuPlugin_fpu_div_divider_io_input_fire) begin
        div_cmdSent <= 1'b1;
      end
      if(when_FpuCore_l1056) begin
        div_cmdSent <= 1'b0;
      end
      if(decode_sqrt_valid) begin
        decode_sqrt_rValid <= 1'b1;
      end
      if(sqrt_input_fire) begin
        decode_sqrt_rValid <= 1'b0;
      end
      if(FpuPlugin_fpu_sqrt_sqrt_io_input_fire) begin
        sqrt_cmdSent <= 1'b1;
      end
      if(when_FpuCore_l1118) begin
        sqrt_cmdSent <= 1'b0;
      end
      if(add_preShifter_output_ready) begin
        add_preShifter_output_rValid <= add_preShifter_output_valid;
      end
      if(add_shifter_output_ready) begin
        add_shifter_output_rValid <= add_shifter_output_valid;
      end
      if(add_math_output_ready) begin
        add_math_output_rValid <= add_math_output_valid;
      end
      if(add_oh_output_ready) begin
        add_oh_output_rValid <= add_oh_output_valid;
      end
      if(load_s1_output_ready) begin
        load_s1_output_rValid <= load_s1_output_valid;
      end
      if(shortPip_output_ready) begin
        shortPip_output_rValid <= shortPip_output_valid;
      end
      roundFront_input_valid <= merge_arbitrated_valid;
      roundBack_input_valid <= roundFront_output_valid;
      writeback_input_valid <= roundBack_output_valid;
      if(writeback_port_valid) begin
        `ifndef SYNTHESIS
          `ifdef FORMAL
            assert((! ((writeback_port_payload_data_value_exponent == 9'h0) && (! writeback_port_payload_data_value_special)))); // FpuCore.scala:L1718
          `else
            if(!(! ((writeback_port_payload_data_value_exponent == 9'h0) && (! writeback_port_payload_data_value_special)))) begin
              $display("FAILURE Special violation"); // FpuCore.scala:L1718
              $finish;
            end
          `endif
        `endif
        `ifndef SYNTHESIS
          `ifdef FORMAL
            assert((! ((writeback_port_payload_data_value_exponent == 9'h1ff) && (! writeback_port_payload_data_value_special)))); // FpuCore.scala:L1719
          `else
            if(!(! ((writeback_port_payload_data_value_exponent == 9'h1ff) && (! writeback_port_payload_data_value_special)))) begin
              $display("FAILURE Special violation"); // FpuCore.scala:L1719
              $finish;
            end
          `endif
        `endif
      end
    end
  end

  always @(posedge clk) begin
    if(FpuPlugin_fpu_streamFork_1_io_outputs_1_rValidN) begin
      FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_opcode <= streamFork_1_io_outputs_1_payload_opcode;
      FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_rd <= streamFork_1_io_outputs_1_payload_rd;
      FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_write <= streamFork_1_io_outputs_1_payload_write;
      FpuPlugin_fpu_streamFork_1_io_outputs_1_rData_value <= streamFork_1_io_outputs_1_payload_value;
    end
    if(io_port_0_cmd_ready) begin
      io_port_0_cmd_rData_opcode <= io_port_0_cmd_payload_opcode;
      io_port_0_cmd_rData_arg <= io_port_0_cmd_payload_arg;
      io_port_0_cmd_rData_rs1 <= io_port_0_cmd_payload_rs1;
      io_port_0_cmd_rData_rs2 <= io_port_0_cmd_payload_rs2;
      io_port_0_cmd_rData_rs3 <= io_port_0_cmd_payload_rs3;
      io_port_0_cmd_rData_rd <= io_port_0_cmd_payload_rd;
      io_port_0_cmd_rData_format <= io_port_0_cmd_payload_format;
      io_port_0_cmd_rData_roundMode <= io_port_0_cmd_payload_roundMode;
    end
    if(read_s0_ready) begin
      read_s0_rData_opcode <= read_s0_payload_opcode;
      read_s0_rData_rs1 <= read_s0_payload_rs1;
      read_s0_rData_rs2 <= read_s0_payload_rs2;
      read_s0_rData_rs3 <= read_s0_payload_rs3;
      read_s0_rData_rd <= read_s0_payload_rd;
      read_s0_rData_arg <= read_s0_payload_arg;
      read_s0_rData_roundMode <= read_s0_payload_roundMode;
    end
    if(decode_load_ready) begin
      decode_load_rData_rd <= decode_load_payload_rd;
      decode_load_rData_i2f <= decode_load_payload_i2f;
      decode_load_rData_arg <= decode_load_payload_arg;
      decode_load_rData_roundMode <= decode_load_payload_roundMode;
    end
    if(decode_load_s2mPipe_ready) begin
      decode_load_s2mPipe_rData_rd <= decode_load_s2mPipe_payload_rd;
      decode_load_s2mPipe_rData_i2f <= decode_load_s2mPipe_payload_i2f;
      decode_load_s2mPipe_rData_arg <= decode_load_s2mPipe_payload_arg;
      decode_load_s2mPipe_rData_roundMode <= decode_load_s2mPipe_payload_roundMode;
    end
    if(decode_load_s2mPipe_m2sPipe_ready) begin
      decode_load_s2mPipe_m2sPipe_rData_rd <= decode_load_s2mPipe_m2sPipe_payload_rd;
      decode_load_s2mPipe_m2sPipe_rData_i2f <= decode_load_s2mPipe_m2sPipe_payload_i2f;
      decode_load_s2mPipe_m2sPipe_rData_arg <= decode_load_s2mPipe_m2sPipe_payload_arg;
      decode_load_s2mPipe_m2sPipe_rData_roundMode <= decode_load_s2mPipe_m2sPipe_payload_roundMode;
    end
    if(load_s0_output_ready) begin
      load_s0_output_rData_rd <= load_s0_output_payload_rd;
      load_s0_output_rData_value <= load_s0_output_payload_value;
      load_s0_output_rData_i2f <= load_s0_output_payload_i2f;
      load_s0_output_rData_arg <= load_s0_output_payload_arg;
      load_s0_output_rData_roundMode <= load_s0_output_payload_roundMode;
    end
    if(when_FpuCore_l525) begin
      load_s1_fsm_shift_output <= load_s1_fsm_shift_input_5;
    end
    if(when_FpuCore_l529) begin
      if(load_s1_fsm_boot) begin
        if(when_FpuCore_l532) begin
          load_s0_output_rData_value[31 : 0] <= _zz_load_s0_output_rData_value_2;
          load_s1_fsm_patched <= 1'b1;
        end else begin
          load_s1_fsm_shift_by <= {_zz_load_s1_fsm_shift_by_32,{_zz_load_s1_fsm_shift_by_31,{_zz_load_s1_fsm_shift_by_30,{_zz_load_s1_fsm_shift_by_29,_zz_load_s1_fsm_shift_by_28}}}};
          load_s1_fsm_boot <= 1'b0;
          load_s1_fsm_i2fZero <= (load_s1_input_payload_value[31 : 0] == 32'h0);
        end
      end else begin
        load_s1_fsm_done <= 1'b1;
      end
    end
    if(when_FpuCore_l551) begin
      load_s1_fsm_done <= 1'b0;
      load_s1_fsm_boot <= 1'b1;
      load_s1_fsm_patched <= 1'b0;
    end
    if(decode_shortPip_ready) begin
      decode_shortPip_rData_opcode <= decode_shortPip_payload_opcode;
      decode_shortPip_rData_rs1_mantissa <= decode_shortPip_payload_rs1_mantissa;
      decode_shortPip_rData_rs1_exponent <= decode_shortPip_payload_rs1_exponent;
      decode_shortPip_rData_rs1_sign <= decode_shortPip_payload_rs1_sign;
      decode_shortPip_rData_rs1_special <= decode_shortPip_payload_rs1_special;
      decode_shortPip_rData_rs2_mantissa <= decode_shortPip_payload_rs2_mantissa;
      decode_shortPip_rData_rs2_exponent <= decode_shortPip_payload_rs2_exponent;
      decode_shortPip_rData_rs2_sign <= decode_shortPip_payload_rs2_sign;
      decode_shortPip_rData_rs2_special <= decode_shortPip_payload_rs2_special;
      decode_shortPip_rData_rd <= decode_shortPip_payload_rd;
      decode_shortPip_rData_value <= decode_shortPip_payload_value;
      decode_shortPip_rData_arg <= decode_shortPip_payload_arg;
      decode_shortPip_rData_roundMode <= decode_shortPip_payload_roundMode;
    end
    if(when_FpuCore_l646) begin
      shortPip_fsm_shift_scrap <= 1'b1;
    end
    if(when_FpuCore_l646_1) begin
      shortPip_fsm_shift_scrap <= 1'b1;
    end
    if(when_FpuCore_l646_2) begin
      shortPip_fsm_shift_scrap <= 1'b1;
    end
    if(when_FpuCore_l646_3) begin
      shortPip_fsm_shift_scrap <= 1'b1;
    end
    if(when_FpuCore_l646_4) begin
      shortPip_fsm_shift_scrap <= 1'b1;
    end
    if(when_FpuCore_l646_5) begin
      shortPip_fsm_shift_scrap <= 1'b1;
    end
    if(shortPip_fsm_boot) begin
      shortPip_fsm_shift_scrap <= 1'b0;
    end
    if(when_FpuCore_l652) begin
      shortPip_fsm_shift_output <= shortPip_fsm_shift_input_6;
    end
    if(when_FpuCore_l658) begin
      if(shortPip_fsm_boot) begin
        if(shortPip_fsm_isF2i) begin
          shortPip_fsm_shift_by <= _zz_shortPip_fsm_shift_by_2[5:0];
        end else begin
          shortPip_fsm_shift_by <= _zz_shortPip_fsm_shift_by_5[5:0];
        end
        shortPip_fsm_boot <= 1'b0;
      end else begin
        shortPip_fsm_done <= 1'b1;
      end
    end
    if(when_FpuCore_l672) begin
      shortPip_fsm_done <= 1'b0;
      shortPip_fsm_boot <= 1'b1;
    end
    if(shortPip_rspStreams_0_ready) begin
      shortPip_rspStreams_0_rData_value <= shortPip_rspStreams_0_payload_value;
      shortPip_rspStreams_0_rData_NV <= shortPip_rspStreams_0_payload_NV;
      shortPip_rspStreams_0_rData_NX <= shortPip_rspStreams_0_payload_NX;
    end
    if(decode_mul_ready) begin
      decode_mul_rData_rs1_mantissa <= decode_mul_payload_rs1_mantissa;
      decode_mul_rData_rs1_exponent <= decode_mul_payload_rs1_exponent;
      decode_mul_rData_rs1_sign <= decode_mul_payload_rs1_sign;
      decode_mul_rData_rs1_special <= decode_mul_payload_rs1_special;
      decode_mul_rData_rs2_mantissa <= decode_mul_payload_rs2_mantissa;
      decode_mul_rData_rs2_exponent <= decode_mul_payload_rs2_exponent;
      decode_mul_rData_rs2_sign <= decode_mul_payload_rs2_sign;
      decode_mul_rData_rs2_special <= decode_mul_payload_rs2_special;
      decode_mul_rData_rs3_mantissa <= decode_mul_payload_rs3_mantissa;
      decode_mul_rData_rs3_exponent <= decode_mul_payload_rs3_exponent;
      decode_mul_rData_rs3_sign <= decode_mul_payload_rs3_sign;
      decode_mul_rData_rs3_special <= decode_mul_payload_rs3_special;
      decode_mul_rData_rd <= decode_mul_payload_rd;
      decode_mul_rData_add <= decode_mul_payload_add;
      decode_mul_rData_divSqrt <= decode_mul_payload_divSqrt;
      decode_mul_rData_msb1 <= decode_mul_payload_msb1;
      decode_mul_rData_msb2 <= decode_mul_payload_msb2;
      decode_mul_rData_roundMode <= decode_mul_payload_roundMode;
    end
    if(mul_preMul_output_ready) begin
      mul_preMul_output_rData_rs1_mantissa <= mul_preMul_output_payload_rs1_mantissa;
      mul_preMul_output_rData_rs1_exponent <= mul_preMul_output_payload_rs1_exponent;
      mul_preMul_output_rData_rs1_sign <= mul_preMul_output_payload_rs1_sign;
      mul_preMul_output_rData_rs1_special <= mul_preMul_output_payload_rs1_special;
      mul_preMul_output_rData_rs2_mantissa <= mul_preMul_output_payload_rs2_mantissa;
      mul_preMul_output_rData_rs2_exponent <= mul_preMul_output_payload_rs2_exponent;
      mul_preMul_output_rData_rs2_sign <= mul_preMul_output_payload_rs2_sign;
      mul_preMul_output_rData_rs2_special <= mul_preMul_output_payload_rs2_special;
      mul_preMul_output_rData_rs3_mantissa <= mul_preMul_output_payload_rs3_mantissa;
      mul_preMul_output_rData_rs3_exponent <= mul_preMul_output_payload_rs3_exponent;
      mul_preMul_output_rData_rs3_sign <= mul_preMul_output_payload_rs3_sign;
      mul_preMul_output_rData_rs3_special <= mul_preMul_output_payload_rs3_special;
      mul_preMul_output_rData_rd <= mul_preMul_output_payload_rd;
      mul_preMul_output_rData_add <= mul_preMul_output_payload_add;
      mul_preMul_output_rData_divSqrt <= mul_preMul_output_payload_divSqrt;
      mul_preMul_output_rData_msb1 <= mul_preMul_output_payload_msb1;
      mul_preMul_output_rData_msb2 <= mul_preMul_output_payload_msb2;
      mul_preMul_output_rData_roundMode <= mul_preMul_output_payload_roundMode;
      mul_preMul_output_rData_exp <= mul_preMul_output_payload_exp;
    end
    if(mul_mul_output_ready) begin
      mul_mul_output_rData_rs1_mantissa <= mul_mul_output_payload_rs1_mantissa;
      mul_mul_output_rData_rs1_exponent <= mul_mul_output_payload_rs1_exponent;
      mul_mul_output_rData_rs1_sign <= mul_mul_output_payload_rs1_sign;
      mul_mul_output_rData_rs1_special <= mul_mul_output_payload_rs1_special;
      mul_mul_output_rData_rs2_mantissa <= mul_mul_output_payload_rs2_mantissa;
      mul_mul_output_rData_rs2_exponent <= mul_mul_output_payload_rs2_exponent;
      mul_mul_output_rData_rs2_sign <= mul_mul_output_payload_rs2_sign;
      mul_mul_output_rData_rs2_special <= mul_mul_output_payload_rs2_special;
      mul_mul_output_rData_rs3_mantissa <= mul_mul_output_payload_rs3_mantissa;
      mul_mul_output_rData_rs3_exponent <= mul_mul_output_payload_rs3_exponent;
      mul_mul_output_rData_rs3_sign <= mul_mul_output_payload_rs3_sign;
      mul_mul_output_rData_rs3_special <= mul_mul_output_payload_rs3_special;
      mul_mul_output_rData_rd <= mul_mul_output_payload_rd;
      mul_mul_output_rData_add <= mul_mul_output_payload_add;
      mul_mul_output_rData_divSqrt <= mul_mul_output_payload_divSqrt;
      mul_mul_output_rData_msb1 <= mul_mul_output_payload_msb1;
      mul_mul_output_rData_msb2 <= mul_mul_output_payload_msb2;
      mul_mul_output_rData_roundMode <= mul_mul_output_payload_roundMode;
      mul_mul_output_rData_exp <= mul_mul_output_payload_exp;
      mul_mul_output_rData_muls_0 <= mul_mul_output_payload_muls_0;
      mul_mul_output_rData_muls_1 <= mul_mul_output_payload_muls_1;
      mul_mul_output_rData_muls_2 <= mul_mul_output_payload_muls_2;
      mul_mul_output_rData_muls_3 <= mul_mul_output_payload_muls_3;
    end
    if(mul_sum1_output_ready) begin
      mul_sum1_output_rData_rs1_mantissa <= mul_sum1_output_payload_rs1_mantissa;
      mul_sum1_output_rData_rs1_exponent <= mul_sum1_output_payload_rs1_exponent;
      mul_sum1_output_rData_rs1_sign <= mul_sum1_output_payload_rs1_sign;
      mul_sum1_output_rData_rs1_special <= mul_sum1_output_payload_rs1_special;
      mul_sum1_output_rData_rs2_mantissa <= mul_sum1_output_payload_rs2_mantissa;
      mul_sum1_output_rData_rs2_exponent <= mul_sum1_output_payload_rs2_exponent;
      mul_sum1_output_rData_rs2_sign <= mul_sum1_output_payload_rs2_sign;
      mul_sum1_output_rData_rs2_special <= mul_sum1_output_payload_rs2_special;
      mul_sum1_output_rData_rs3_mantissa <= mul_sum1_output_payload_rs3_mantissa;
      mul_sum1_output_rData_rs3_exponent <= mul_sum1_output_payload_rs3_exponent;
      mul_sum1_output_rData_rs3_sign <= mul_sum1_output_payload_rs3_sign;
      mul_sum1_output_rData_rs3_special <= mul_sum1_output_payload_rs3_special;
      mul_sum1_output_rData_rd <= mul_sum1_output_payload_rd;
      mul_sum1_output_rData_add <= mul_sum1_output_payload_add;
      mul_sum1_output_rData_divSqrt <= mul_sum1_output_payload_divSqrt;
      mul_sum1_output_rData_msb1 <= mul_sum1_output_payload_msb1;
      mul_sum1_output_rData_msb2 <= mul_sum1_output_payload_msb2;
      mul_sum1_output_rData_roundMode <= mul_sum1_output_payload_roundMode;
      mul_sum1_output_rData_exp <= mul_sum1_output_payload_exp;
      mul_sum1_output_rData_muls2_0 <= mul_sum1_output_payload_muls2_0;
      mul_sum1_output_rData_muls2_1 <= mul_sum1_output_payload_muls2_1;
      mul_sum1_output_rData_mulC2 <= mul_sum1_output_payload_mulC2;
    end
    if(mul_sum2_output_ready) begin
      mul_sum2_output_rData_rs1_mantissa <= mul_sum2_output_payload_rs1_mantissa;
      mul_sum2_output_rData_rs1_exponent <= mul_sum2_output_payload_rs1_exponent;
      mul_sum2_output_rData_rs1_sign <= mul_sum2_output_payload_rs1_sign;
      mul_sum2_output_rData_rs1_special <= mul_sum2_output_payload_rs1_special;
      mul_sum2_output_rData_rs2_mantissa <= mul_sum2_output_payload_rs2_mantissa;
      mul_sum2_output_rData_rs2_exponent <= mul_sum2_output_payload_rs2_exponent;
      mul_sum2_output_rData_rs2_sign <= mul_sum2_output_payload_rs2_sign;
      mul_sum2_output_rData_rs2_special <= mul_sum2_output_payload_rs2_special;
      mul_sum2_output_rData_rs3_mantissa <= mul_sum2_output_payload_rs3_mantissa;
      mul_sum2_output_rData_rs3_exponent <= mul_sum2_output_payload_rs3_exponent;
      mul_sum2_output_rData_rs3_sign <= mul_sum2_output_payload_rs3_sign;
      mul_sum2_output_rData_rs3_special <= mul_sum2_output_payload_rs3_special;
      mul_sum2_output_rData_rd <= mul_sum2_output_payload_rd;
      mul_sum2_output_rData_add <= mul_sum2_output_payload_add;
      mul_sum2_output_rData_divSqrt <= mul_sum2_output_payload_divSqrt;
      mul_sum2_output_rData_msb1 <= mul_sum2_output_payload_msb1;
      mul_sum2_output_rData_msb2 <= mul_sum2_output_payload_msb2;
      mul_sum2_output_rData_roundMode <= mul_sum2_output_payload_roundMode;
      mul_sum2_output_rData_exp <= mul_sum2_output_payload_exp;
      mul_sum2_output_rData_mulC <= mul_sum2_output_payload_mulC;
    end
    if(mul_result_mulToAdd_ready) begin
      mul_result_mulToAdd_rData_rs1_mantissa <= mul_result_mulToAdd_payload_rs1_mantissa;
      mul_result_mulToAdd_rData_rs1_exponent <= mul_result_mulToAdd_payload_rs1_exponent;
      mul_result_mulToAdd_rData_rs1_sign <= mul_result_mulToAdd_payload_rs1_sign;
      mul_result_mulToAdd_rData_rs1_special <= mul_result_mulToAdd_payload_rs1_special;
      mul_result_mulToAdd_rData_rs2_mantissa <= mul_result_mulToAdd_payload_rs2_mantissa;
      mul_result_mulToAdd_rData_rs2_exponent <= mul_result_mulToAdd_payload_rs2_exponent;
      mul_result_mulToAdd_rData_rs2_sign <= mul_result_mulToAdd_payload_rs2_sign;
      mul_result_mulToAdd_rData_rs2_special <= mul_result_mulToAdd_payload_rs2_special;
      mul_result_mulToAdd_rData_rd <= mul_result_mulToAdd_payload_rd;
      mul_result_mulToAdd_rData_roundMode <= mul_result_mulToAdd_payload_roundMode;
      mul_result_mulToAdd_rData_needCommit <= mul_result_mulToAdd_payload_needCommit;
    end
    if(decode_div_ready) begin
      decode_div_rData_rs1_mantissa <= decode_div_payload_rs1_mantissa;
      decode_div_rData_rs1_exponent <= decode_div_payload_rs1_exponent;
      decode_div_rData_rs1_sign <= decode_div_payload_rs1_sign;
      decode_div_rData_rs1_special <= decode_div_payload_rs1_special;
      decode_div_rData_rs2_mantissa <= decode_div_payload_rs2_mantissa;
      decode_div_rData_rs2_exponent <= decode_div_payload_rs2_exponent;
      decode_div_rData_rs2_sign <= decode_div_payload_rs2_sign;
      decode_div_rData_rs2_special <= decode_div_payload_rs2_special;
      decode_div_rData_rd <= decode_div_payload_rd;
      decode_div_rData_roundMode <= decode_div_payload_roundMode;
    end
    div_isCommited <= commitLogic_0_div_notEmpty;
    if(decode_sqrt_ready) begin
      decode_sqrt_rData_rs1_mantissa <= decode_sqrt_payload_rs1_mantissa;
      decode_sqrt_rData_rs1_exponent <= decode_sqrt_payload_rs1_exponent;
      decode_sqrt_rData_rs1_sign <= decode_sqrt_payload_rs1_sign;
      decode_sqrt_rData_rs1_special <= decode_sqrt_payload_rs1_special;
      decode_sqrt_rData_rd <= decode_sqrt_payload_rd;
      decode_sqrt_rData_roundMode <= decode_sqrt_payload_roundMode;
    end
    sqrt_isCommited <= commitLogic_0_sqrt_notEmpty;
    sqrt_exponent <= (_zz_sqrt_exponent + _zz_sqrt_exponent_4);
    if(add_preShifter_output_ready) begin
      add_preShifter_output_rData_rs1_mantissa <= add_preShifter_output_payload_rs1_mantissa;
      add_preShifter_output_rData_rs1_exponent <= add_preShifter_output_payload_rs1_exponent;
      add_preShifter_output_rData_rs1_sign <= add_preShifter_output_payload_rs1_sign;
      add_preShifter_output_rData_rs1_special <= add_preShifter_output_payload_rs1_special;
      add_preShifter_output_rData_rs2_mantissa <= add_preShifter_output_payload_rs2_mantissa;
      add_preShifter_output_rData_rs2_exponent <= add_preShifter_output_payload_rs2_exponent;
      add_preShifter_output_rData_rs2_sign <= add_preShifter_output_payload_rs2_sign;
      add_preShifter_output_rData_rs2_special <= add_preShifter_output_payload_rs2_special;
      add_preShifter_output_rData_rd <= add_preShifter_output_payload_rd;
      add_preShifter_output_rData_roundMode <= add_preShifter_output_payload_roundMode;
      add_preShifter_output_rData_needCommit <= add_preShifter_output_payload_needCommit;
      add_preShifter_output_rData_absRs1Bigger <= add_preShifter_output_payload_absRs1Bigger;
      add_preShifter_output_rData_rs1ExponentBigger <= add_preShifter_output_payload_rs1ExponentBigger;
    end
    if(add_shifter_output_ready) begin
      add_shifter_output_rData_rs1_mantissa <= add_shifter_output_payload_rs1_mantissa;
      add_shifter_output_rData_rs1_exponent <= add_shifter_output_payload_rs1_exponent;
      add_shifter_output_rData_rs1_sign <= add_shifter_output_payload_rs1_sign;
      add_shifter_output_rData_rs1_special <= add_shifter_output_payload_rs1_special;
      add_shifter_output_rData_rs2_mantissa <= add_shifter_output_payload_rs2_mantissa;
      add_shifter_output_rData_rs2_exponent <= add_shifter_output_payload_rs2_exponent;
      add_shifter_output_rData_rs2_sign <= add_shifter_output_payload_rs2_sign;
      add_shifter_output_rData_rs2_special <= add_shifter_output_payload_rs2_special;
      add_shifter_output_rData_rd <= add_shifter_output_payload_rd;
      add_shifter_output_rData_roundMode <= add_shifter_output_payload_roundMode;
      add_shifter_output_rData_needCommit <= add_shifter_output_payload_needCommit;
      add_shifter_output_rData_xSign <= add_shifter_output_payload_xSign;
      add_shifter_output_rData_ySign <= add_shifter_output_payload_ySign;
      add_shifter_output_rData_xMantissa <= add_shifter_output_payload_xMantissa;
      add_shifter_output_rData_yMantissa <= add_shifter_output_payload_yMantissa;
      add_shifter_output_rData_xyExponent <= add_shifter_output_payload_xyExponent;
      add_shifter_output_rData_xySign <= add_shifter_output_payload_xySign;
      add_shifter_output_rData_roundingScrap <= add_shifter_output_payload_roundingScrap;
    end
    if(add_math_output_ready) begin
      add_math_output_rData_rs1_mantissa <= add_math_output_payload_rs1_mantissa;
      add_math_output_rData_rs1_exponent <= add_math_output_payload_rs1_exponent;
      add_math_output_rData_rs1_sign <= add_math_output_payload_rs1_sign;
      add_math_output_rData_rs1_special <= add_math_output_payload_rs1_special;
      add_math_output_rData_rs2_mantissa <= add_math_output_payload_rs2_mantissa;
      add_math_output_rData_rs2_exponent <= add_math_output_payload_rs2_exponent;
      add_math_output_rData_rs2_sign <= add_math_output_payload_rs2_sign;
      add_math_output_rData_rs2_special <= add_math_output_payload_rs2_special;
      add_math_output_rData_rd <= add_math_output_payload_rd;
      add_math_output_rData_roundMode <= add_math_output_payload_roundMode;
      add_math_output_rData_needCommit <= add_math_output_payload_needCommit;
      add_math_output_rData_xSign <= add_math_output_payload_xSign;
      add_math_output_rData_ySign <= add_math_output_payload_ySign;
      add_math_output_rData_xMantissa <= add_math_output_payload_xMantissa;
      add_math_output_rData_yMantissa <= add_math_output_payload_yMantissa;
      add_math_output_rData_xyExponent <= add_math_output_payload_xyExponent;
      add_math_output_rData_xySign <= add_math_output_payload_xySign;
      add_math_output_rData_roundingScrap <= add_math_output_payload_roundingScrap;
      add_math_output_rData_xyMantissa <= add_math_output_payload_xyMantissa;
    end
    if(add_oh_output_ready) begin
      add_oh_output_rData_rs1_mantissa <= add_oh_output_payload_rs1_mantissa;
      add_oh_output_rData_rs1_exponent <= add_oh_output_payload_rs1_exponent;
      add_oh_output_rData_rs1_sign <= add_oh_output_payload_rs1_sign;
      add_oh_output_rData_rs1_special <= add_oh_output_payload_rs1_special;
      add_oh_output_rData_rs2_mantissa <= add_oh_output_payload_rs2_mantissa;
      add_oh_output_rData_rs2_exponent <= add_oh_output_payload_rs2_exponent;
      add_oh_output_rData_rs2_sign <= add_oh_output_payload_rs2_sign;
      add_oh_output_rData_rs2_special <= add_oh_output_payload_rs2_special;
      add_oh_output_rData_rd <= add_oh_output_payload_rd;
      add_oh_output_rData_roundMode <= add_oh_output_payload_roundMode;
      add_oh_output_rData_needCommit <= add_oh_output_payload_needCommit;
      add_oh_output_rData_xSign <= add_oh_output_payload_xSign;
      add_oh_output_rData_ySign <= add_oh_output_payload_ySign;
      add_oh_output_rData_xMantissa <= add_oh_output_payload_xMantissa;
      add_oh_output_rData_yMantissa <= add_oh_output_payload_yMantissa;
      add_oh_output_rData_xyExponent <= add_oh_output_payload_xyExponent;
      add_oh_output_rData_xySign <= add_oh_output_payload_xySign;
      add_oh_output_rData_roundingScrap <= add_oh_output_payload_roundingScrap;
      add_oh_output_rData_xyMantissa <= add_oh_output_payload_xyMantissa;
      add_oh_output_rData_shift <= add_oh_output_payload_shift;
    end
    if(load_s1_output_ready) begin
      load_s1_output_rData_rd <= load_s1_output_payload_rd;
      load_s1_output_rData_value_mantissa <= load_s1_output_payload_value_mantissa;
      load_s1_output_rData_value_exponent <= load_s1_output_payload_value_exponent;
      load_s1_output_rData_value_sign <= load_s1_output_payload_value_sign;
      load_s1_output_rData_value_special <= load_s1_output_payload_value_special;
      load_s1_output_rData_scrap <= load_s1_output_payload_scrap;
      load_s1_output_rData_roundMode <= load_s1_output_payload_roundMode;
      load_s1_output_rData_NV <= load_s1_output_payload_NV;
      load_s1_output_rData_DZ <= load_s1_output_payload_DZ;
    end
    if(shortPip_output_ready) begin
      shortPip_output_rData_rd <= shortPip_output_payload_rd;
      shortPip_output_rData_value_mantissa <= shortPip_output_payload_value_mantissa;
      shortPip_output_rData_value_exponent <= shortPip_output_payload_value_exponent;
      shortPip_output_rData_value_sign <= shortPip_output_payload_value_sign;
      shortPip_output_rData_value_special <= shortPip_output_payload_value_special;
      shortPip_output_rData_scrap <= shortPip_output_payload_scrap;
      shortPip_output_rData_roundMode <= shortPip_output_payload_roundMode;
      shortPip_output_rData_NV <= shortPip_output_payload_NV;
      shortPip_output_rData_DZ <= shortPip_output_payload_DZ;
    end
    roundFront_input_payload_rd <= merge_arbitrated_payload_rd;
    roundFront_input_payload_value_mantissa <= merge_arbitrated_payload_value_mantissa;
    roundFront_input_payload_value_exponent <= merge_arbitrated_payload_value_exponent;
    roundFront_input_payload_value_sign <= merge_arbitrated_payload_value_sign;
    roundFront_input_payload_value_special <= merge_arbitrated_payload_value_special;
    roundFront_input_payload_scrap <= merge_arbitrated_payload_scrap;
    roundFront_input_payload_roundMode <= merge_arbitrated_payload_roundMode;
    roundFront_input_payload_NV <= merge_arbitrated_payload_NV;
    roundFront_input_payload_DZ <= merge_arbitrated_payload_DZ;
    roundBack_input_payload_rd <= roundFront_output_payload_rd;
    roundBack_input_payload_value_mantissa <= roundFront_output_payload_value_mantissa;
    roundBack_input_payload_value_exponent <= roundFront_output_payload_value_exponent;
    roundBack_input_payload_value_sign <= roundFront_output_payload_value_sign;
    roundBack_input_payload_value_special <= roundFront_output_payload_value_special;
    roundBack_input_payload_scrap <= roundFront_output_payload_scrap;
    roundBack_input_payload_roundMode <= roundFront_output_payload_roundMode;
    roundBack_input_payload_NV <= roundFront_output_payload_NV;
    roundBack_input_payload_DZ <= roundFront_output_payload_DZ;
    roundBack_input_payload_mantissaIncrement <= roundFront_output_payload_mantissaIncrement;
    roundBack_input_payload_roundAdjusted <= roundFront_output_payload_roundAdjusted;
    roundBack_input_payload_exactMask <= roundFront_output_payload_exactMask;
    writeback_input_payload_rd <= roundBack_output_payload_rd;
    writeback_input_payload_value_mantissa <= roundBack_output_payload_value_mantissa;
    writeback_input_payload_value_exponent <= roundBack_output_payload_value_exponent;
    writeback_input_payload_value_sign <= roundBack_output_payload_value_sign;
    writeback_input_payload_value_special <= roundBack_output_payload_value_special;
    writeback_input_payload_NV <= roundBack_output_payload_NV;
    writeback_input_payload_NX <= roundBack_output_payload_NX;
    writeback_input_payload_OF <= roundBack_output_payload_OF;
    writeback_input_payload_UF <= roundBack_output_payload_UF;
    writeback_input_payload_DZ <= roundBack_output_payload_DZ;
    writeback_input_payload_write <= roundBack_output_payload_write;
  end


endmodule

module BufferCC (
  input  wire          io_dataIn,
  output wire          io_dataOut,
  input  wire          clk,
  input  wire          reset
);

  (* async_reg = "true" *) reg                 buffers_0;
  (* async_reg = "true" *) reg                 buffers_1;

  assign io_dataOut = buffers_1;
  always @(posedge clk) begin
    buffers_0 <= io_dataIn;
    buffers_1 <= buffers_0;
  end


endmodule

module DataCache (
  input  wire          io_cpu_execute_isValid,
  input  wire [31:0]   io_cpu_execute_address,
  output reg           io_cpu_execute_haltIt,
  input  wire          io_cpu_execute_args_wr,
  input  wire [1:0]    io_cpu_execute_args_size,
  input  wire          io_cpu_execute_args_totalyConsistent,
  output wire          io_cpu_execute_refilling,
  input  wire          io_cpu_memory_isValid,
  input  wire          io_cpu_memory_isStuck,
  output wire          io_cpu_memory_isWrite,
  input  wire [31:0]   io_cpu_memory_address,
  input  wire [31:0]   io_cpu_memory_mmuRsp_physicalAddress,
  input  wire          io_cpu_memory_mmuRsp_isIoAccess,
  input  wire          io_cpu_memory_mmuRsp_isPaging,
  input  wire          io_cpu_memory_mmuRsp_allowRead,
  input  wire          io_cpu_memory_mmuRsp_allowWrite,
  input  wire          io_cpu_memory_mmuRsp_allowExecute,
  input  wire          io_cpu_memory_mmuRsp_exception,
  input  wire          io_cpu_memory_mmuRsp_refilling,
  input  wire          io_cpu_memory_mmuRsp_bypassTranslation,
  input  wire          io_cpu_writeBack_isValid,
  input  wire          io_cpu_writeBack_isStuck,
  input  wire          io_cpu_writeBack_isFiring,
  input  wire          io_cpu_writeBack_isUser,
  output reg           io_cpu_writeBack_haltIt,
  output wire          io_cpu_writeBack_isWrite,
  input  wire [31:0]   io_cpu_writeBack_storeData,
  output reg  [31:0]   io_cpu_writeBack_data,
  input  wire [31:0]   io_cpu_writeBack_address,
  output wire          io_cpu_writeBack_mmuException,
  output wire          io_cpu_writeBack_unalignedAccess,
  output reg           io_cpu_writeBack_accessError,
  output wire          io_cpu_writeBack_keepMemRspData,
  input  wire          io_cpu_writeBack_fence_SW,
  input  wire          io_cpu_writeBack_fence_SR,
  input  wire          io_cpu_writeBack_fence_SO,
  input  wire          io_cpu_writeBack_fence_SI,
  input  wire          io_cpu_writeBack_fence_PW,
  input  wire          io_cpu_writeBack_fence_PR,
  input  wire          io_cpu_writeBack_fence_PO,
  input  wire          io_cpu_writeBack_fence_PI,
  input  wire [3:0]    io_cpu_writeBack_fence_FM,
  output wire          io_cpu_writeBack_exclusiveOk,
  output reg           io_cpu_redo,
  input  wire          io_cpu_flush_valid,
  output wire          io_cpu_flush_ready,
  input  wire          io_cpu_flush_payload_singleLine,
  output wire          io_cpu_writesPending,
  output reg           io_mem_cmd_valid,
  input  wire          io_mem_cmd_ready,
  output reg           io_mem_cmd_payload_wr,
  output wire          io_mem_cmd_payload_uncached,
  output reg  [31:0]   io_mem_cmd_payload_address,
  output wire [31:0]   io_mem_cmd_payload_data,
  output wire [3:0]    io_mem_cmd_payload_mask,
  output reg  [1:0]    io_mem_cmd_payload_size,
  output wire          io_mem_cmd_payload_last,
  input  wire          io_mem_rsp_valid,
  input  wire          io_mem_rsp_payload_last,
  input  wire [31:0]   io_mem_rsp_payload_data,
  input  wire          io_mem_rsp_payload_error,
  input  wire          clk,
  input  wire          reset
);

  reg        [31:0]   ways_0_data_spinal_port0;
  wire       [0:0]    _zz_when;
  wire       [1:0]    _zz_loader_waysAllocator;
  reg                 _zz_1;
  reg                 when_Phase_l768;
  wire                haltCpu;
  reg                 tagsReadCmd_valid;
  reg                 tagsWriteCmd_valid;
  reg        [0:0]    tagsWriteCmd_payload_way;
  reg                 tagsWriteCmd_payload_data_valid;
  reg                 tagsWriteCmd_payload_data_error;
  reg        [28:0]   tagsWriteCmd_payload_data_address;
  reg                 tagsWriteLastCmd_valid;
  reg        [0:0]    tagsWriteLastCmd_payload_way;
  reg                 tagsWriteLastCmd_payload_data_valid;
  reg                 tagsWriteLastCmd_payload_data_error;
  reg        [28:0]   tagsWriteLastCmd_payload_data_address;
  reg                 dataReadCmd_valid;
  reg        [0:0]    dataReadCmd_payload;
  reg                 dataWriteCmd_valid;
  reg        [0:0]    dataWriteCmd_payload_way;
  reg        [0:0]    dataWriteCmd_payload_address;
  reg        [31:0]   dataWriteCmd_payload_data;
  reg        [3:0]    dataWriteCmd_payload_mask;
  wire                when_Phase_l795;
  wire                ways_0_tagsReadRsp_valid;
  wire                ways_0_tagsReadRsp_error;
  wire       [28:0]   ways_0_tagsReadRsp_address;
  wire       [30:0]   _zz_ways_0_tagsReadRsp_valid;
  wire                _zz_ways_0_dataReadRspMem;
  wire       [31:0]   ways_0_dataReadRspMem;
  wire       [31:0]   ways_0_dataReadRsp;
  wire                when_DataCache_l645;
  wire                when_DataCache_l648;
  wire                when_DataCache_l667;
  wire                rspSync;
  wire                rspLast;
  reg                 memCmdSent;
  wire                io_mem_cmd_fire;
  wire                when_DataCache_l689;
  reg        [3:0]    _zz_stage0_mask;
  wire       [3:0]    stage0_mask;
  wire       [0:0]    stage0_dataColisions;
  wire       [0:0]    stage0_wayInvalidate;
  wire                stage0_isAmo;
  wire                when_DataCache_l776;
  reg                 stageA_request_wr;
  reg        [1:0]    stageA_request_size;
  reg                 stageA_request_totalyConsistent;
  wire                when_DataCache_l776_1;
  reg        [3:0]    stageA_mask;
  wire                stageA_isAmo;
  wire                stageA_isLrsc;
  wire       [0:0]    stageA_wayHits;
  wire                when_DataCache_l776_2;
  reg        [0:0]    stageA_wayInvalidate;
  wire                when_DataCache_l776_3;
  reg        [0:0]    stage0_dataColisions_regNextWhen;
  wire       [0:0]    _zz_stageA_dataColisions;
  wire       [0:0]    stageA_dataColisions;
  wire                when_DataCache_l827;
  reg                 stageB_request_wr;
  reg        [1:0]    stageB_request_size;
  reg                 stageB_request_totalyConsistent;
  reg                 stageB_mmuRspFreeze;
  wire                when_DataCache_l829;
  reg        [31:0]   stageB_mmuRsp_physicalAddress;
  reg                 stageB_mmuRsp_isIoAccess;
  reg                 stageB_mmuRsp_isPaging;
  reg                 stageB_mmuRsp_allowRead;
  reg                 stageB_mmuRsp_allowWrite;
  reg                 stageB_mmuRsp_allowExecute;
  reg                 stageB_mmuRsp_exception;
  reg                 stageB_mmuRsp_refilling;
  reg                 stageB_mmuRsp_bypassTranslation;
  wire                when_DataCache_l826;
  reg                 stageB_tagsReadRsp_0_valid;
  reg                 stageB_tagsReadRsp_0_error;
  reg        [28:0]   stageB_tagsReadRsp_0_address;
  wire                when_DataCache_l826_1;
  reg        [31:0]   stageB_dataReadRsp_0;
  wire                when_DataCache_l825;
  reg        [0:0]    stageB_wayInvalidate;
  wire                stageB_consistancyHazard;
  wire                when_DataCache_l825_1;
  reg        [0:0]    stageB_dataColisions;
  wire                when_DataCache_l825_2;
  reg                 stageB_unaligned;
  wire                when_DataCache_l825_3;
  reg        [0:0]    stageB_waysHitsBeforeInvalidate;
  wire       [0:0]    stageB_waysHits;
  wire                stageB_waysHit;
  wire       [31:0]   stageB_dataMux;
  wire                when_DataCache_l825_4;
  reg        [3:0]    stageB_mask;
  reg                 stageB_loaderValid;
  wire       [31:0]   stageB_ioMemRspMuxed;
  reg                 stageB_flusher_waitDone;
  wire                stageB_flusher_hold;
  reg        [0:0]    stageB_flusher_counter;
  wire                when_DataCache_l855;
  wire                when_DataCache_l861;
  wire                when_DataCache_l863;
  reg                 stageB_flusher_start;
  wire                when_DataCache_l877;
  wire                stageB_isAmo;
  wire                stageB_isAmoCached;
  wire                stageB_isExternalLsrc;
  wire                stageB_isExternalAmo;
  wire       [31:0]   stageB_requestDataBypass;
  reg                 stageB_cpuWriteToCache;
  wire                when_DataCache_l931;
  wire                stageB_badPermissions;
  wire                stageB_loadStoreFault;
  wire                stageB_bypassCache;
  wire                when_DataCache_l1000;
  wire                when_DataCache_l1009;
  wire                when_DataCache_l1014;
  wire                when_DataCache_l1025;
  wire                when_DataCache_l1037;
  wire                when_DataCache_l996;
  wire                when_DataCache_l1072;
  wire                when_DataCache_l1081;
  reg                 loader_valid;
  reg                 loader_counter_willIncrement;
  wire                loader_counter_willClear;
  reg        [0:0]    loader_counter_valueNext;
  reg        [0:0]    loader_counter_value;
  wire                loader_counter_willOverflowIfInc;
  wire                loader_counter_willOverflow;
  reg        [0:0]    loader_waysAllocator;
  reg                 loader_error;
  wire                loader_kill;
  reg                 loader_killReg;
  wire                when_DataCache_l1097;
  wire                loader_done;
  wire                when_DataCache_l1125;
  reg                 loader_valid_regNext;
  wire                when_DataCache_l1129;
  wire                when_DataCache_l1132;
  wire       [30:0]   _zz_ways_0_tagsReadRsp_valid_1;
  reg        [30:0]   _zz_ways_0_tagsReadRsp_valid_2;
  reg        [30:0]   _zz_ways_0_tagsReadRsp_valid_3;
  reg [7:0] ways_0_data_symbol0 [0:1];
  reg [7:0] ways_0_data_symbol1 [0:1];
  reg [7:0] ways_0_data_symbol2 [0:1];
  reg [7:0] ways_0_data_symbol3 [0:1];
  reg [7:0] _zz_ways_0_datasymbol_read;
  reg [7:0] _zz_ways_0_datasymbol_read_1;
  reg [7:0] _zz_ways_0_datasymbol_read_2;
  reg [7:0] _zz_ways_0_datasymbol_read_3;

  assign _zz_when = 1'b1;
  assign _zz_loader_waysAllocator = {loader_waysAllocator,loader_waysAllocator[0]};
  always @(*) begin
    ways_0_data_spinal_port0 = {_zz_ways_0_datasymbol_read_3, _zz_ways_0_datasymbol_read_2, _zz_ways_0_datasymbol_read_1, _zz_ways_0_datasymbol_read};
  end
  always @(posedge clk) begin
    if(_zz_ways_0_dataReadRspMem) begin
      _zz_ways_0_datasymbol_read <= ways_0_data_symbol0[dataReadCmd_payload];
      _zz_ways_0_datasymbol_read_1 <= ways_0_data_symbol1[dataReadCmd_payload];
      _zz_ways_0_datasymbol_read_2 <= ways_0_data_symbol2[dataReadCmd_payload];
      _zz_ways_0_datasymbol_read_3 <= ways_0_data_symbol3[dataReadCmd_payload];
    end
  end

  always @(posedge clk) begin
    if(dataWriteCmd_payload_mask[0] && _zz_1) begin
      ways_0_data_symbol0[dataWriteCmd_payload_address] <= dataWriteCmd_payload_data[7 : 0];
    end
    if(dataWriteCmd_payload_mask[1] && _zz_1) begin
      ways_0_data_symbol1[dataWriteCmd_payload_address] <= dataWriteCmd_payload_data[15 : 8];
    end
    if(dataWriteCmd_payload_mask[2] && _zz_1) begin
      ways_0_data_symbol2[dataWriteCmd_payload_address] <= dataWriteCmd_payload_data[23 : 16];
    end
    if(dataWriteCmd_payload_mask[3] && _zz_1) begin
      ways_0_data_symbol3[dataWriteCmd_payload_address] <= dataWriteCmd_payload_data[31 : 24];
    end
  end

  always @(*) begin
    _zz_1 = 1'b0;
    if(when_DataCache_l648) begin
      _zz_1 = 1'b1;
    end
  end

  always @(*) begin
    when_Phase_l768 = 1'b0;
    if(when_DataCache_l645) begin
      when_Phase_l768 = 1'b1;
    end
  end

  assign haltCpu = 1'b0;
  assign when_Phase_l795 = (tagsReadCmd_valid && (! io_cpu_memory_isStuck));
  assign _zz_ways_0_tagsReadRsp_valid = _zz_ways_0_tagsReadRsp_valid_2;
  assign ways_0_tagsReadRsp_valid = _zz_ways_0_tagsReadRsp_valid[0];
  assign ways_0_tagsReadRsp_error = _zz_ways_0_tagsReadRsp_valid[1];
  assign ways_0_tagsReadRsp_address = _zz_ways_0_tagsReadRsp_valid[30 : 2];
  assign _zz_ways_0_dataReadRspMem = (dataReadCmd_valid && (! io_cpu_memory_isStuck));
  assign ways_0_dataReadRspMem = ways_0_data_spinal_port0;
  assign ways_0_dataReadRsp = ways_0_dataReadRspMem[31 : 0];
  assign when_DataCache_l645 = (tagsWriteCmd_valid && tagsWriteCmd_payload_way[0]);
  assign when_DataCache_l648 = (dataWriteCmd_valid && dataWriteCmd_payload_way[0]);
  always @(*) begin
    tagsReadCmd_valid = 1'b0;
    if(when_DataCache_l667) begin
      tagsReadCmd_valid = 1'b1;
    end
  end

  always @(*) begin
    dataReadCmd_valid = 1'b0;
    if(when_DataCache_l667) begin
      dataReadCmd_valid = 1'b1;
    end
  end

  always @(*) begin
    dataReadCmd_payload = 1'bx;
    if(when_DataCache_l667) begin
      dataReadCmd_payload = io_cpu_execute_address[2 : 2];
    end
  end

  always @(*) begin
    tagsWriteCmd_valid = 1'b0;
    if(when_DataCache_l855) begin
      tagsWriteCmd_valid = 1'b1;
    end
    if(io_cpu_writeBack_isValid) begin
      if(when_DataCache_l1072) begin
        tagsWriteCmd_valid = 1'b0;
      end
    end
    if(loader_done) begin
      tagsWriteCmd_valid = 1'b1;
    end
  end

  always @(*) begin
    tagsWriteCmd_payload_way = 1'bx;
    if(when_DataCache_l855) begin
      tagsWriteCmd_payload_way = 1'b1;
    end
    if(loader_done) begin
      tagsWriteCmd_payload_way = loader_waysAllocator;
    end
  end

  always @(*) begin
    tagsWriteCmd_payload_data_valid = 1'bx;
    if(when_DataCache_l855) begin
      tagsWriteCmd_payload_data_valid = 1'b0;
    end
    if(loader_done) begin
      tagsWriteCmd_payload_data_valid = (! (loader_kill || loader_killReg));
    end
  end

  always @(*) begin
    tagsWriteCmd_payload_data_error = 1'bx;
    if(loader_done) begin
      tagsWriteCmd_payload_data_error = (loader_error || (io_mem_rsp_valid && io_mem_rsp_payload_error));
    end
  end

  always @(*) begin
    tagsWriteCmd_payload_data_address = 29'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    if(loader_done) begin
      tagsWriteCmd_payload_data_address = stageB_mmuRsp_physicalAddress[31 : 3];
    end
  end

  always @(*) begin
    dataWriteCmd_valid = 1'b0;
    if(stageB_cpuWriteToCache) begin
      if(when_DataCache_l931) begin
        dataWriteCmd_valid = 1'b1;
      end
    end
    if(io_cpu_writeBack_isValid) begin
      if(when_DataCache_l1072) begin
        dataWriteCmd_valid = 1'b0;
      end
    end
    if(when_DataCache_l1097) begin
      dataWriteCmd_valid = 1'b1;
    end
  end

  always @(*) begin
    dataWriteCmd_payload_way = 1'bx;
    if(stageB_cpuWriteToCache) begin
      dataWriteCmd_payload_way = stageB_waysHits;
    end
    if(when_DataCache_l1097) begin
      dataWriteCmd_payload_way = loader_waysAllocator;
    end
  end

  always @(*) begin
    dataWriteCmd_payload_address = 1'bx;
    if(stageB_cpuWriteToCache) begin
      dataWriteCmd_payload_address = stageB_mmuRsp_physicalAddress[2 : 2];
    end
    if(when_DataCache_l1097) begin
      dataWriteCmd_payload_address = loader_counter_value;
    end
  end

  always @(*) begin
    dataWriteCmd_payload_data = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    if(stageB_cpuWriteToCache) begin
      dataWriteCmd_payload_data[31 : 0] = stageB_requestDataBypass;
    end
    if(when_DataCache_l1097) begin
      dataWriteCmd_payload_data = io_mem_rsp_payload_data;
    end
  end

  always @(*) begin
    dataWriteCmd_payload_mask = 4'bxxxx;
    if(stageB_cpuWriteToCache) begin
      dataWriteCmd_payload_mask = 4'b0000;
      if(_zz_when[0]) begin
        dataWriteCmd_payload_mask[3 : 0] = stageB_mask;
      end
    end
    if(when_DataCache_l1097) begin
      dataWriteCmd_payload_mask = 4'b1111;
    end
  end

  assign when_DataCache_l667 = (io_cpu_execute_isValid && (! io_cpu_memory_isStuck));
  always @(*) begin
    io_cpu_execute_haltIt = 1'b0;
    if(when_DataCache_l855) begin
      io_cpu_execute_haltIt = 1'b1;
    end
  end

  assign rspSync = 1'b1;
  assign rspLast = 1'b1;
  assign io_mem_cmd_fire = (io_mem_cmd_valid && io_mem_cmd_ready);
  assign when_DataCache_l689 = (! io_cpu_writeBack_isStuck);
  always @(*) begin
    _zz_stage0_mask = 4'bxxxx;
    case(io_cpu_execute_args_size)
      2'b00 : begin
        _zz_stage0_mask = 4'b0001;
      end
      2'b01 : begin
        _zz_stage0_mask = 4'b0011;
      end
      2'b10 : begin
        _zz_stage0_mask = 4'b1111;
      end
      default : begin
      end
    endcase
  end

  assign stage0_mask = (_zz_stage0_mask <<< io_cpu_execute_address[1 : 0]);
  assign stage0_dataColisions[0] = (((dataWriteCmd_valid && dataWriteCmd_payload_way[0]) && (dataWriteCmd_payload_address == io_cpu_execute_address[2 : 2])) && ((stage0_mask & dataWriteCmd_payload_mask[3 : 0]) != 4'b0000));
  assign stage0_wayInvalidate = 1'b0;
  assign stage0_isAmo = 1'b0;
  assign when_DataCache_l776 = (! io_cpu_memory_isStuck);
  assign when_DataCache_l776_1 = (! io_cpu_memory_isStuck);
  assign io_cpu_memory_isWrite = stageA_request_wr;
  assign stageA_isAmo = 1'b0;
  assign stageA_isLrsc = 1'b0;
  assign stageA_wayHits = ((io_cpu_memory_mmuRsp_physicalAddress[31 : 3] == ways_0_tagsReadRsp_address) && ways_0_tagsReadRsp_valid);
  assign when_DataCache_l776_2 = (! io_cpu_memory_isStuck);
  assign when_DataCache_l776_3 = (! io_cpu_memory_isStuck);
  assign _zz_stageA_dataColisions[0] = (((dataWriteCmd_valid && dataWriteCmd_payload_way[0]) && (dataWriteCmd_payload_address == io_cpu_memory_address[2 : 2])) && ((stageA_mask & dataWriteCmd_payload_mask[3 : 0]) != 4'b0000));
  assign stageA_dataColisions = (stage0_dataColisions_regNextWhen | _zz_stageA_dataColisions);
  assign when_DataCache_l827 = (! io_cpu_writeBack_isStuck);
  always @(*) begin
    stageB_mmuRspFreeze = 1'b0;
    if(when_DataCache_l1132) begin
      stageB_mmuRspFreeze = 1'b1;
    end
  end

  assign when_DataCache_l829 = ((! io_cpu_writeBack_isStuck) && (! stageB_mmuRspFreeze));
  assign when_DataCache_l826 = (! io_cpu_writeBack_isStuck);
  assign when_DataCache_l826_1 = (! io_cpu_writeBack_isStuck);
  assign when_DataCache_l825 = (! io_cpu_writeBack_isStuck);
  assign stageB_consistancyHazard = 1'b0;
  assign when_DataCache_l825_1 = (! io_cpu_writeBack_isStuck);
  assign when_DataCache_l825_2 = (! io_cpu_writeBack_isStuck);
  assign when_DataCache_l825_3 = (! io_cpu_writeBack_isStuck);
  assign stageB_waysHits = (stageB_waysHitsBeforeInvalidate & (~ stageB_wayInvalidate));
  assign stageB_waysHit = (|stageB_waysHits);
  assign stageB_dataMux = stageB_dataReadRsp_0;
  assign when_DataCache_l825_4 = (! io_cpu_writeBack_isStuck);
  always @(*) begin
    stageB_loaderValid = 1'b0;
    if(io_cpu_writeBack_isValid) begin
      if(!stageB_isExternalAmo) begin
        if(!when_DataCache_l996) begin
          if(!when_DataCache_l1009) begin
            if(io_mem_cmd_ready) begin
              stageB_loaderValid = 1'b1;
            end
          end
        end
      end
    end
    if(io_cpu_writeBack_isValid) begin
      if(when_DataCache_l1072) begin
        stageB_loaderValid = 1'b0;
      end
    end
  end

  assign stageB_ioMemRspMuxed = io_mem_rsp_payload_data[31 : 0];
  always @(*) begin
    io_cpu_writeBack_haltIt = 1'b1;
    if(io_cpu_writeBack_isValid) begin
      if(!stageB_isExternalAmo) begin
        if(when_DataCache_l996) begin
          if(when_DataCache_l1000) begin
            io_cpu_writeBack_haltIt = 1'b0;
          end
        end else begin
          if(when_DataCache_l1009) begin
            if(when_DataCache_l1014) begin
              io_cpu_writeBack_haltIt = 1'b0;
            end
          end
        end
      end
    end
    if(io_cpu_writeBack_isValid) begin
      if(when_DataCache_l1072) begin
        io_cpu_writeBack_haltIt = 1'b0;
      end
    end
  end

  assign stageB_flusher_hold = 1'b0;
  assign when_DataCache_l855 = (! stageB_flusher_counter[0]);
  assign when_DataCache_l861 = (! stageB_flusher_hold);
  assign when_DataCache_l863 = (io_cpu_flush_valid && io_cpu_flush_payload_singleLine);
  assign io_cpu_flush_ready = (stageB_flusher_waitDone && stageB_flusher_counter[0]);
  assign when_DataCache_l877 = (io_cpu_flush_valid && io_cpu_flush_payload_singleLine);
  assign stageB_isAmo = 1'b0;
  assign stageB_isAmoCached = 1'b0;
  assign stageB_isExternalLsrc = 1'b0;
  assign stageB_isExternalAmo = 1'b0;
  assign stageB_requestDataBypass = io_cpu_writeBack_storeData;
  always @(*) begin
    stageB_cpuWriteToCache = 1'b0;
    if(io_cpu_writeBack_isValid) begin
      if(!stageB_isExternalAmo) begin
        if(!when_DataCache_l996) begin
          if(when_DataCache_l1009) begin
            stageB_cpuWriteToCache = 1'b1;
          end
        end
      end
    end
  end

  assign when_DataCache_l931 = (stageB_request_wr && stageB_waysHit);
  assign stageB_badPermissions = (((! stageB_mmuRsp_allowWrite) && stageB_request_wr) || ((! stageB_mmuRsp_allowRead) && ((! stageB_request_wr) || stageB_isAmo)));
  assign stageB_loadStoreFault = (io_cpu_writeBack_isValid && (stageB_mmuRsp_exception || stageB_badPermissions));
  always @(*) begin
    io_cpu_redo = 1'b0;
    if(io_cpu_writeBack_isValid) begin
      if(!stageB_isExternalAmo) begin
        if(!when_DataCache_l996) begin
          if(when_DataCache_l1009) begin
            if(when_DataCache_l1025) begin
              io_cpu_redo = 1'b1;
            end
          end
        end
      end
    end
    if(io_cpu_writeBack_isValid) begin
      if(when_DataCache_l1081) begin
        io_cpu_redo = 1'b1;
      end
    end
    if(when_DataCache_l1129) begin
      io_cpu_redo = 1'b1;
    end
  end

  always @(*) begin
    io_cpu_writeBack_accessError = 1'b0;
    if(stageB_bypassCache) begin
      io_cpu_writeBack_accessError = ((((! stageB_request_wr) && 1'b1) && io_mem_rsp_valid) && io_mem_rsp_payload_error);
    end else begin
      io_cpu_writeBack_accessError = (((stageB_waysHits & stageB_tagsReadRsp_0_error) != 1'b0) || (stageB_loadStoreFault && (! stageB_mmuRsp_isPaging)));
    end
  end

  assign io_cpu_writeBack_mmuException = (stageB_loadStoreFault && stageB_mmuRsp_isPaging);
  assign io_cpu_writeBack_unalignedAccess = (io_cpu_writeBack_isValid && stageB_unaligned);
  assign io_cpu_writeBack_isWrite = stageB_request_wr;
  always @(*) begin
    io_mem_cmd_valid = 1'b0;
    if(io_cpu_writeBack_isValid) begin
      if(!stageB_isExternalAmo) begin
        if(when_DataCache_l996) begin
          io_mem_cmd_valid = (! memCmdSent);
        end else begin
          if(when_DataCache_l1009) begin
            if(stageB_request_wr) begin
              io_mem_cmd_valid = 1'b1;
            end
          end else begin
            if(when_DataCache_l1037) begin
              io_mem_cmd_valid = 1'b1;
            end
          end
        end
      end
    end
    if(io_cpu_writeBack_isValid) begin
      if(when_DataCache_l1072) begin
        io_mem_cmd_valid = 1'b0;
      end
    end
  end

  always @(*) begin
    io_mem_cmd_payload_address = stageB_mmuRsp_physicalAddress;
    if(io_cpu_writeBack_isValid) begin
      if(!stageB_isExternalAmo) begin
        if(!when_DataCache_l996) begin
          if(!when_DataCache_l1009) begin
            io_mem_cmd_payload_address[2 : 0] = 3'b000;
          end
        end
      end
    end
  end

  assign io_mem_cmd_payload_last = 1'b1;
  always @(*) begin
    io_mem_cmd_payload_wr = stageB_request_wr;
    if(io_cpu_writeBack_isValid) begin
      if(!stageB_isExternalAmo) begin
        if(!when_DataCache_l996) begin
          if(!when_DataCache_l1009) begin
            io_mem_cmd_payload_wr = 1'b0;
          end
        end
      end
    end
  end

  assign io_mem_cmd_payload_mask = stageB_mask;
  assign io_mem_cmd_payload_data = stageB_requestDataBypass;
  assign io_mem_cmd_payload_uncached = stageB_mmuRsp_isIoAccess;
  always @(*) begin
    io_mem_cmd_payload_size = stageB_request_size;
    if(io_cpu_writeBack_isValid) begin
      if(!stageB_isExternalAmo) begin
        if(!when_DataCache_l996) begin
          if(!when_DataCache_l1009) begin
            io_mem_cmd_payload_size = 2'b11;
          end
        end
      end
    end
  end

  assign stageB_bypassCache = ((stageB_mmuRsp_isIoAccess || stageB_isExternalLsrc) || stageB_isExternalAmo);
  assign io_cpu_writeBack_keepMemRspData = 1'b0;
  assign when_DataCache_l1000 = ((! stageB_request_wr) ? (io_mem_rsp_valid && rspSync) : io_mem_cmd_ready);
  assign when_DataCache_l1009 = (stageB_waysHit || (stageB_request_wr && (! stageB_isAmoCached)));
  assign when_DataCache_l1014 = ((! stageB_request_wr) || io_mem_cmd_ready);
  assign when_DataCache_l1025 = (((! stageB_request_wr) || stageB_isAmoCached) && ((stageB_dataColisions & stageB_waysHits) != 1'b0));
  assign when_DataCache_l1037 = (! memCmdSent);
  assign when_DataCache_l996 = (stageB_mmuRsp_isIoAccess || stageB_isExternalLsrc);
  always @(*) begin
    if(stageB_bypassCache) begin
      io_cpu_writeBack_data = stageB_ioMemRspMuxed;
    end else begin
      io_cpu_writeBack_data = stageB_dataMux;
    end
  end

  assign when_DataCache_l1072 = ((((stageB_consistancyHazard || stageB_mmuRsp_refilling) || io_cpu_writeBack_accessError) || io_cpu_writeBack_mmuException) || io_cpu_writeBack_unalignedAccess);
  assign when_DataCache_l1081 = (stageB_mmuRsp_refilling || stageB_consistancyHazard);
  always @(*) begin
    loader_counter_willIncrement = 1'b0;
    if(when_DataCache_l1097) begin
      loader_counter_willIncrement = 1'b1;
    end
  end

  assign loader_counter_willClear = 1'b0;
  assign loader_counter_willOverflowIfInc = (loader_counter_value == 1'b1);
  assign loader_counter_willOverflow = (loader_counter_willOverflowIfInc && loader_counter_willIncrement);
  always @(*) begin
    loader_counter_valueNext = (loader_counter_value + loader_counter_willIncrement);
    if(loader_counter_willClear) begin
      loader_counter_valueNext = 1'b0;
    end
  end

  assign loader_kill = 1'b0;
  assign when_DataCache_l1097 = ((loader_valid && io_mem_rsp_valid) && rspLast);
  assign loader_done = loader_counter_willOverflow;
  assign when_DataCache_l1125 = (! loader_valid);
  assign when_DataCache_l1129 = (loader_valid && (! loader_valid_regNext));
  assign io_cpu_execute_refilling = loader_valid;
  assign when_DataCache_l1132 = (stageB_loaderValid || loader_valid);
  assign _zz_ways_0_tagsReadRsp_valid_1 = _zz_ways_0_tagsReadRsp_valid_3;
  always @(posedge clk) begin
    tagsWriteLastCmd_valid <= tagsWriteCmd_valid;
    tagsWriteLastCmd_payload_way <= tagsWriteCmd_payload_way;
    tagsWriteLastCmd_payload_data_valid <= tagsWriteCmd_payload_data_valid;
    tagsWriteLastCmd_payload_data_error <= tagsWriteCmd_payload_data_error;
    tagsWriteLastCmd_payload_data_address <= tagsWriteCmd_payload_data_address;
    if(when_DataCache_l776) begin
      stageA_request_wr <= io_cpu_execute_args_wr;
      stageA_request_size <= io_cpu_execute_args_size;
      stageA_request_totalyConsistent <= io_cpu_execute_args_totalyConsistent;
    end
    if(when_DataCache_l776_1) begin
      stageA_mask <= stage0_mask;
    end
    if(when_DataCache_l776_2) begin
      stageA_wayInvalidate <= stage0_wayInvalidate;
    end
    if(when_DataCache_l776_3) begin
      stage0_dataColisions_regNextWhen <= stage0_dataColisions;
    end
    if(when_DataCache_l827) begin
      stageB_request_wr <= stageA_request_wr;
      stageB_request_size <= stageA_request_size;
      stageB_request_totalyConsistent <= stageA_request_totalyConsistent;
    end
    if(when_DataCache_l829) begin
      stageB_mmuRsp_physicalAddress <= io_cpu_memory_mmuRsp_physicalAddress;
      stageB_mmuRsp_isIoAccess <= io_cpu_memory_mmuRsp_isIoAccess;
      stageB_mmuRsp_isPaging <= io_cpu_memory_mmuRsp_isPaging;
      stageB_mmuRsp_allowRead <= io_cpu_memory_mmuRsp_allowRead;
      stageB_mmuRsp_allowWrite <= io_cpu_memory_mmuRsp_allowWrite;
      stageB_mmuRsp_allowExecute <= io_cpu_memory_mmuRsp_allowExecute;
      stageB_mmuRsp_exception <= io_cpu_memory_mmuRsp_exception;
      stageB_mmuRsp_refilling <= io_cpu_memory_mmuRsp_refilling;
      stageB_mmuRsp_bypassTranslation <= io_cpu_memory_mmuRsp_bypassTranslation;
    end
    if(when_DataCache_l826) begin
      stageB_tagsReadRsp_0_valid <= ways_0_tagsReadRsp_valid;
      stageB_tagsReadRsp_0_error <= ways_0_tagsReadRsp_error;
      stageB_tagsReadRsp_0_address <= ways_0_tagsReadRsp_address;
    end
    if(when_DataCache_l826_1) begin
      stageB_dataReadRsp_0 <= ways_0_dataReadRsp;
    end
    if(when_DataCache_l825) begin
      stageB_wayInvalidate <= stageA_wayInvalidate;
    end
    if(when_DataCache_l825_1) begin
      stageB_dataColisions <= stageA_dataColisions;
    end
    if(when_DataCache_l825_2) begin
      stageB_unaligned <= (|{((stageA_request_size == 2'b10) && (io_cpu_memory_address[1 : 0] != 2'b00)),((stageA_request_size == 2'b01) && (io_cpu_memory_address[0 : 0] != 1'b0))});
    end
    if(when_DataCache_l825_3) begin
      stageB_waysHitsBeforeInvalidate <= stageA_wayHits;
    end
    if(when_DataCache_l825_4) begin
      stageB_mask <= stageA_mask;
    end
    loader_valid_regNext <= loader_valid;
    if(when_Phase_l795) begin
      _zz_ways_0_tagsReadRsp_valid_2 <= _zz_ways_0_tagsReadRsp_valid_1;
    end
    if(when_Phase_l768) begin
      _zz_ways_0_tagsReadRsp_valid_3 <= {tagsWriteCmd_payload_data_address,{tagsWriteCmd_payload_data_error,tagsWriteCmd_payload_data_valid}};
    end
  end

  always @(posedge clk or posedge reset) begin
    if(reset) begin
      memCmdSent <= 1'b0;
      stageB_flusher_waitDone <= 1'b0;
      stageB_flusher_counter <= 1'b0;
      stageB_flusher_start <= 1'b1;
      loader_valid <= 1'b0;
      loader_counter_value <= 1'b0;
      loader_waysAllocator <= 1'b1;
      loader_error <= 1'b0;
      loader_killReg <= 1'b0;
    end else begin
      if(io_mem_cmd_fire) begin
        memCmdSent <= 1'b1;
      end
      if(when_DataCache_l689) begin
        memCmdSent <= 1'b0;
      end
      if(io_cpu_flush_ready) begin
        stageB_flusher_waitDone <= 1'b0;
      end
      if(when_DataCache_l855) begin
        if(when_DataCache_l861) begin
          stageB_flusher_counter <= (stageB_flusher_counter + 1'b1);
          if(when_DataCache_l863) begin
            stageB_flusher_counter[0] <= 1'b1;
          end
        end
      end
      stageB_flusher_start <= (((((((! stageB_flusher_waitDone) && (! stageB_flusher_start)) && io_cpu_flush_valid) && (! io_cpu_execute_isValid)) && (! io_cpu_memory_isValid)) && (! io_cpu_writeBack_isValid)) && (! io_cpu_redo));
      if(stageB_flusher_start) begin
        stageB_flusher_waitDone <= 1'b1;
        stageB_flusher_counter <= 1'b0;
        if(when_DataCache_l877) begin
          stageB_flusher_counter <= 1'b0;
        end
      end
      `ifndef SYNTHESIS
        `ifdef FORMAL
          assert((! ((io_cpu_writeBack_isValid && (! io_cpu_writeBack_haltIt)) && io_cpu_writeBack_isStuck))); // DataCache.scala:L1084
        `else
          if(!(! ((io_cpu_writeBack_isValid && (! io_cpu_writeBack_haltIt)) && io_cpu_writeBack_isStuck))) begin
            $display("ERROR writeBack stuck by another plugin is not allowed"); // DataCache.scala:L1084
          end
        `endif
      `endif
      if(stageB_loaderValid) begin
        loader_valid <= 1'b1;
      end
      loader_counter_value <= loader_counter_valueNext;
      if(loader_kill) begin
        loader_killReg <= 1'b1;
      end
      if(when_DataCache_l1097) begin
        loader_error <= (loader_error || io_mem_rsp_payload_error);
      end
      if(loader_done) begin
        loader_valid <= 1'b0;
        loader_error <= 1'b0;
        loader_killReg <= 1'b0;
      end
      if(when_DataCache_l1125) begin
        loader_waysAllocator <= _zz_loader_waysAllocator[0:0];
      end
    end
  end


endmodule

module StreamFifoLowLatency (
  input  wire          io_push_valid,
  output wire          io_push_ready,
  input  wire          io_push_payload_error,
  input  wire [31:0]   io_push_payload_inst,
  output wire          io_pop_valid,
  input  wire          io_pop_ready,
  output wire          io_pop_payload_error,
  output wire [31:0]   io_pop_payload_inst,
  input  wire          io_flush,
  output wire [0:0]    io_occupancy,
  output wire [0:0]    io_availability,
  input  wire          clk,
  input  wire          reset
);

  wire                fifo_io_push_ready;
  wire                fifo_io_pop_valid;
  wire                fifo_io_pop_payload_error;
  wire       [31:0]   fifo_io_pop_payload_inst;
  wire       [0:0]    fifo_io_occupancy;
  wire       [0:0]    fifo_io_availability;

  StreamFifo fifo (
    .io_push_valid         (io_push_valid                 ), //i
    .io_push_ready         (fifo_io_push_ready            ), //o
    .io_push_payload_error (io_push_payload_error         ), //i
    .io_push_payload_inst  (io_push_payload_inst[31:0]    ), //i
    .io_pop_valid          (fifo_io_pop_valid             ), //o
    .io_pop_ready          (io_pop_ready                  ), //i
    .io_pop_payload_error  (fifo_io_pop_payload_error     ), //o
    .io_pop_payload_inst   (fifo_io_pop_payload_inst[31:0]), //o
    .io_flush              (io_flush                      ), //i
    .io_occupancy          (fifo_io_occupancy             ), //o
    .io_availability       (fifo_io_availability          ), //o
    .clk                   (clk                           ), //i
    .reset                 (reset                         )  //i
  );
  assign io_push_ready = fifo_io_push_ready;
  assign io_pop_valid = fifo_io_pop_valid;
  assign io_pop_payload_error = fifo_io_pop_payload_error;
  assign io_pop_payload_inst = fifo_io_pop_payload_inst;
  assign io_occupancy = fifo_io_occupancy;
  assign io_availability = fifo_io_availability;

endmodule

module FlowCCByToggle_1 (
  input  wire          io_input_valid,
  input  wire          io_input_payload_error,
  input  wire [31:0]   io_input_payload_data,
  output wire          io_output_valid,
  output wire          io_output_payload_error,
  output wire [31:0]   io_output_payload_data,
  input  wire          clk,
  input  wire          reset,
  input  wire          io_jtag_tck
);

  wire                bufferCC_4_io_dataIn;
  wire                bufferCC_4_io_dataOut;
  wire                inputArea_target_buffercc_io_dataOut;
  wire                reset_synchronized;
  reg                 inputArea_target;
  reg                 inputArea_data_error;
  reg        [31:0]   inputArea_data_data;
  wire                outputArea_target;
  reg                 outputArea_hit;
  wire                outputArea_flow_valid;
  wire                outputArea_flow_payload_error;
  wire       [31:0]   outputArea_flow_payload_data;
  reg                 outputArea_flow_m2sPipe_valid;
  (* async_reg = "true" *) reg                 outputArea_flow_m2sPipe_payload_error;
  (* async_reg = "true" *) reg        [31:0]   outputArea_flow_m2sPipe_payload_data;

  (* keep_hierarchy = "TRUE" *) BufferCC_1 bufferCC_4 (
    .io_dataIn   (bufferCC_4_io_dataIn ), //i
    .io_dataOut  (bufferCC_4_io_dataOut), //o
    .io_jtag_tck (io_jtag_tck          ), //i
    .reset       (reset                )  //i
  );
  (* keep_hierarchy = "TRUE" *) BufferCC_2 inputArea_target_buffercc (
    .io_dataIn          (inputArea_target                    ), //i
    .io_dataOut         (inputArea_target_buffercc_io_dataOut), //o
    .io_jtag_tck        (io_jtag_tck                         ), //i
    .reset_synchronized (reset_synchronized                  )  //i
  );
  assign bufferCC_4_io_dataIn = (1'b0 ^ 1'b0);
  assign reset_synchronized = bufferCC_4_io_dataOut;
  assign outputArea_target = inputArea_target_buffercc_io_dataOut;
  assign outputArea_flow_valid = (outputArea_target != outputArea_hit);
  assign outputArea_flow_payload_error = inputArea_data_error;
  assign outputArea_flow_payload_data = inputArea_data_data;
  assign io_output_valid = outputArea_flow_m2sPipe_valid;
  assign io_output_payload_error = outputArea_flow_m2sPipe_payload_error;
  assign io_output_payload_data = outputArea_flow_m2sPipe_payload_data;
  always @(posedge clk or posedge reset) begin
    if(reset) begin
      inputArea_target <= 1'b0;
    end else begin
      if(io_input_valid) begin
        inputArea_target <= (! inputArea_target);
      end
    end
  end

  always @(posedge clk) begin
    if(io_input_valid) begin
      inputArea_data_error <= io_input_payload_error;
      inputArea_data_data <= io_input_payload_data;
    end
  end

  always @(posedge io_jtag_tck or posedge reset_synchronized) begin
    if(reset_synchronized) begin
      outputArea_flow_m2sPipe_valid <= 1'b0;
      outputArea_hit <= 1'b0;
    end else begin
      outputArea_hit <= outputArea_target;
      outputArea_flow_m2sPipe_valid <= outputArea_flow_valid;
    end
  end

  always @(posedge io_jtag_tck) begin
    if(outputArea_flow_valid) begin
      outputArea_flow_m2sPipe_payload_error <= outputArea_flow_payload_error;
      outputArea_flow_m2sPipe_payload_data <= outputArea_flow_payload_data;
    end
  end


endmodule

module FlowCCByToggle (
  input  wire          io_input_valid,
  input  wire          io_input_payload_write,
  input  wire [31:0]   io_input_payload_data,
  input  wire [6:0]    io_input_payload_address,
  output wire          io_output_valid,
  output wire          io_output_payload_write,
  output wire [31:0]   io_output_payload_data,
  output wire [6:0]    io_output_payload_address,
  input  wire          io_jtag_tck,
  input  wire          clk,
  input  wire          reset
);

  wire                inputArea_target_buffercc_io_dataOut;
  reg                 inputArea_target;
  reg                 inputArea_data_write;
  reg        [31:0]   inputArea_data_data;
  reg        [6:0]    inputArea_data_address;
  wire                outputArea_target;
  reg                 outputArea_hit;
  wire                outputArea_flow_valid;
  wire                outputArea_flow_payload_write;
  wire       [31:0]   outputArea_flow_payload_data;
  wire       [6:0]    outputArea_flow_payload_address;

  (* keep_hierarchy = "TRUE" *) BufferCC_3 inputArea_target_buffercc (
    .io_dataIn  (inputArea_target                    ), //i
    .io_dataOut (inputArea_target_buffercc_io_dataOut), //o
    .clk        (clk                                 ), //i
    .reset      (reset                               )  //i
  );
  initial begin
  `ifndef SYNTHESIS
    inputArea_target = $urandom;
    outputArea_hit = $urandom;
  `endif
  end

  assign outputArea_target = inputArea_target_buffercc_io_dataOut;
  assign outputArea_flow_valid = (outputArea_target != outputArea_hit);
  assign outputArea_flow_payload_write = inputArea_data_write;
  assign outputArea_flow_payload_data = inputArea_data_data;
  assign outputArea_flow_payload_address = inputArea_data_address;
  assign io_output_valid = outputArea_flow_valid;
  assign io_output_payload_write = outputArea_flow_payload_write;
  assign io_output_payload_data = outputArea_flow_payload_data;
  assign io_output_payload_address = outputArea_flow_payload_address;
  always @(posedge io_jtag_tck) begin
    if(io_input_valid) begin
      inputArea_target <= (! inputArea_target);
      inputArea_data_write <= io_input_payload_write;
      inputArea_data_data <= io_input_payload_data;
      inputArea_data_address <= io_input_payload_address;
    end
  end

  always @(posedge clk) begin
    outputArea_hit <= outputArea_target;
  end


endmodule

module StreamArbiter_1 (
  input  wire          io_inputs_0_valid,
  output wire          io_inputs_0_ready,
  input  wire [4:0]    io_inputs_0_payload_rd,
  input  wire [23:0]   io_inputs_0_payload_value_mantissa,
  input  wire [8:0]    io_inputs_0_payload_value_exponent,
  input  wire          io_inputs_0_payload_value_sign,
  input  wire          io_inputs_0_payload_value_special,
  input  wire          io_inputs_0_payload_scrap,
  input  wire [2:0]    io_inputs_0_payload_roundMode,
  input  wire          io_inputs_0_payload_NV,
  input  wire          io_inputs_0_payload_DZ,
  input  wire          io_inputs_1_valid,
  output wire          io_inputs_1_ready,
  input  wire [4:0]    io_inputs_1_payload_rd,
  input  wire [23:0]   io_inputs_1_payload_value_mantissa,
  input  wire [8:0]    io_inputs_1_payload_value_exponent,
  input  wire          io_inputs_1_payload_value_sign,
  input  wire          io_inputs_1_payload_value_special,
  input  wire          io_inputs_1_payload_scrap,
  input  wire [2:0]    io_inputs_1_payload_roundMode,
  input  wire          io_inputs_1_payload_NV,
  input  wire          io_inputs_1_payload_DZ,
  input  wire          io_inputs_2_valid,
  output wire          io_inputs_2_ready,
  input  wire [4:0]    io_inputs_2_payload_rd,
  input  wire [23:0]   io_inputs_2_payload_value_mantissa,
  input  wire [8:0]    io_inputs_2_payload_value_exponent,
  input  wire          io_inputs_2_payload_value_sign,
  input  wire          io_inputs_2_payload_value_special,
  input  wire          io_inputs_2_payload_scrap,
  input  wire [2:0]    io_inputs_2_payload_roundMode,
  input  wire          io_inputs_2_payload_NV,
  input  wire          io_inputs_2_payload_DZ,
  input  wire          io_inputs_3_valid,
  output wire          io_inputs_3_ready,
  input  wire [4:0]    io_inputs_3_payload_rd,
  input  wire [23:0]   io_inputs_3_payload_value_mantissa,
  input  wire [8:0]    io_inputs_3_payload_value_exponent,
  input  wire          io_inputs_3_payload_value_sign,
  input  wire          io_inputs_3_payload_value_special,
  input  wire          io_inputs_3_payload_scrap,
  input  wire [2:0]    io_inputs_3_payload_roundMode,
  input  wire          io_inputs_3_payload_NV,
  input  wire          io_inputs_3_payload_DZ,
  input  wire          io_inputs_4_valid,
  output wire          io_inputs_4_ready,
  input  wire [4:0]    io_inputs_4_payload_rd,
  input  wire [23:0]   io_inputs_4_payload_value_mantissa,
  input  wire [8:0]    io_inputs_4_payload_value_exponent,
  input  wire          io_inputs_4_payload_value_sign,
  input  wire          io_inputs_4_payload_value_special,
  input  wire          io_inputs_4_payload_scrap,
  input  wire [2:0]    io_inputs_4_payload_roundMode,
  input  wire          io_inputs_4_payload_NV,
  input  wire          io_inputs_4_payload_DZ,
  input  wire          io_inputs_5_valid,
  output wire          io_inputs_5_ready,
  input  wire [4:0]    io_inputs_5_payload_rd,
  input  wire [23:0]   io_inputs_5_payload_value_mantissa,
  input  wire [8:0]    io_inputs_5_payload_value_exponent,
  input  wire          io_inputs_5_payload_value_sign,
  input  wire          io_inputs_5_payload_value_special,
  input  wire          io_inputs_5_payload_scrap,
  input  wire [2:0]    io_inputs_5_payload_roundMode,
  input  wire          io_inputs_5_payload_NV,
  input  wire          io_inputs_5_payload_DZ,
  output wire          io_output_valid,
  input  wire          io_output_ready,
  output wire [4:0]    io_output_payload_rd,
  output wire [23:0]   io_output_payload_value_mantissa,
  output wire [8:0]    io_output_payload_value_exponent,
  output wire          io_output_payload_value_sign,
  output wire          io_output_payload_value_special,
  output wire          io_output_payload_scrap,
  output wire [2:0]    io_output_payload_roundMode,
  output wire          io_output_payload_NV,
  output wire          io_output_payload_DZ,
  output wire [2:0]    io_chosen,
  output wire [5:0]    io_chosenOH,
  input  wire          clk,
  input  wire          reset
);
  localparam FpuRoundMode_RNE = 3'd0;
  localparam FpuRoundMode_RTZ = 3'd1;
  localparam FpuRoundMode_RDN = 3'd2;
  localparam FpuRoundMode_RUP = 3'd3;
  localparam FpuRoundMode_RMM = 3'd4;

  wire       [5:0]    _zz__zz_maskProposal_1_1;
  reg        [2:0]    _zz__zz_io_output_payload_roundMode;
  reg        [4:0]    _zz_io_output_payload_rd_4;
  reg        [23:0]   _zz_io_output_payload_value_mantissa;
  reg        [8:0]    _zz_io_output_payload_value_exponent;
  reg                 _zz_io_output_payload_value_sign;
  reg                 _zz_io_output_payload_value_special;
  reg                 _zz_io_output_payload_scrap;
  reg                 _zz_io_output_payload_NV;
  reg                 _zz_io_output_payload_DZ;
  wire                locked;
  wire                maskProposal_0;
  wire                maskProposal_1;
  wire                maskProposal_2;
  wire                maskProposal_3;
  wire                maskProposal_4;
  wire                maskProposal_5;
  reg                 maskLocked_0;
  reg                 maskLocked_1;
  reg                 maskLocked_2;
  reg                 maskLocked_3;
  reg                 maskLocked_4;
  reg                 maskLocked_5;
  wire                maskRouted_0;
  wire                maskRouted_1;
  wire                maskRouted_2;
  wire                maskRouted_3;
  wire                maskRouted_4;
  wire                maskRouted_5;
  wire       [5:0]    _zz_maskProposal_1;
  wire       [5:0]    _zz_maskProposal_1_1;
  wire                _zz_io_output_payload_rd;
  wire                _zz_io_output_payload_rd_1;
  wire                _zz_io_output_payload_rd_2;
  wire       [2:0]    _zz_io_output_payload_rd_3;
  wire       [2:0]    _zz_io_output_payload_roundMode;
  wire                _zz_io_chosen;
  wire                _zz_io_chosen_1;
  wire                _zz_io_chosen_2;
  wire                _zz_io_chosen_3;
  wire                _zz_io_chosen_4;
  `ifndef SYNTHESIS
  reg [23:0] io_inputs_0_payload_roundMode_string;
  reg [23:0] io_inputs_1_payload_roundMode_string;
  reg [23:0] io_inputs_2_payload_roundMode_string;
  reg [23:0] io_inputs_3_payload_roundMode_string;
  reg [23:0] io_inputs_4_payload_roundMode_string;
  reg [23:0] io_inputs_5_payload_roundMode_string;
  reg [23:0] io_output_payload_roundMode_string;
  reg [23:0] _zz_io_output_payload_roundMode_string;
  `endif


  assign _zz__zz_maskProposal_1_1 = (_zz_maskProposal_1 - 6'h01);
  always @(*) begin
    case(_zz_io_output_payload_rd_3)
      3'b000 : begin
        _zz__zz_io_output_payload_roundMode = io_inputs_0_payload_roundMode;
        _zz_io_output_payload_rd_4 = io_inputs_0_payload_rd;
        _zz_io_output_payload_value_mantissa = io_inputs_0_payload_value_mantissa;
        _zz_io_output_payload_value_exponent = io_inputs_0_payload_value_exponent;
        _zz_io_output_payload_value_sign = io_inputs_0_payload_value_sign;
        _zz_io_output_payload_value_special = io_inputs_0_payload_value_special;
        _zz_io_output_payload_scrap = io_inputs_0_payload_scrap;
        _zz_io_output_payload_NV = io_inputs_0_payload_NV;
        _zz_io_output_payload_DZ = io_inputs_0_payload_DZ;
      end
      3'b001 : begin
        _zz__zz_io_output_payload_roundMode = io_inputs_1_payload_roundMode;
        _zz_io_output_payload_rd_4 = io_inputs_1_payload_rd;
        _zz_io_output_payload_value_mantissa = io_inputs_1_payload_value_mantissa;
        _zz_io_output_payload_value_exponent = io_inputs_1_payload_value_exponent;
        _zz_io_output_payload_value_sign = io_inputs_1_payload_value_sign;
        _zz_io_output_payload_value_special = io_inputs_1_payload_value_special;
        _zz_io_output_payload_scrap = io_inputs_1_payload_scrap;
        _zz_io_output_payload_NV = io_inputs_1_payload_NV;
        _zz_io_output_payload_DZ = io_inputs_1_payload_DZ;
      end
      3'b010 : begin
        _zz__zz_io_output_payload_roundMode = io_inputs_2_payload_roundMode;
        _zz_io_output_payload_rd_4 = io_inputs_2_payload_rd;
        _zz_io_output_payload_value_mantissa = io_inputs_2_payload_value_mantissa;
        _zz_io_output_payload_value_exponent = io_inputs_2_payload_value_exponent;
        _zz_io_output_payload_value_sign = io_inputs_2_payload_value_sign;
        _zz_io_output_payload_value_special = io_inputs_2_payload_value_special;
        _zz_io_output_payload_scrap = io_inputs_2_payload_scrap;
        _zz_io_output_payload_NV = io_inputs_2_payload_NV;
        _zz_io_output_payload_DZ = io_inputs_2_payload_DZ;
      end
      3'b011 : begin
        _zz__zz_io_output_payload_roundMode = io_inputs_3_payload_roundMode;
        _zz_io_output_payload_rd_4 = io_inputs_3_payload_rd;
        _zz_io_output_payload_value_mantissa = io_inputs_3_payload_value_mantissa;
        _zz_io_output_payload_value_exponent = io_inputs_3_payload_value_exponent;
        _zz_io_output_payload_value_sign = io_inputs_3_payload_value_sign;
        _zz_io_output_payload_value_special = io_inputs_3_payload_value_special;
        _zz_io_output_payload_scrap = io_inputs_3_payload_scrap;
        _zz_io_output_payload_NV = io_inputs_3_payload_NV;
        _zz_io_output_payload_DZ = io_inputs_3_payload_DZ;
      end
      3'b100 : begin
        _zz__zz_io_output_payload_roundMode = io_inputs_4_payload_roundMode;
        _zz_io_output_payload_rd_4 = io_inputs_4_payload_rd;
        _zz_io_output_payload_value_mantissa = io_inputs_4_payload_value_mantissa;
        _zz_io_output_payload_value_exponent = io_inputs_4_payload_value_exponent;
        _zz_io_output_payload_value_sign = io_inputs_4_payload_value_sign;
        _zz_io_output_payload_value_special = io_inputs_4_payload_value_special;
        _zz_io_output_payload_scrap = io_inputs_4_payload_scrap;
        _zz_io_output_payload_NV = io_inputs_4_payload_NV;
        _zz_io_output_payload_DZ = io_inputs_4_payload_DZ;
      end
      default : begin
        _zz__zz_io_output_payload_roundMode = io_inputs_5_payload_roundMode;
        _zz_io_output_payload_rd_4 = io_inputs_5_payload_rd;
        _zz_io_output_payload_value_mantissa = io_inputs_5_payload_value_mantissa;
        _zz_io_output_payload_value_exponent = io_inputs_5_payload_value_exponent;
        _zz_io_output_payload_value_sign = io_inputs_5_payload_value_sign;
        _zz_io_output_payload_value_special = io_inputs_5_payload_value_special;
        _zz_io_output_payload_scrap = io_inputs_5_payload_scrap;
        _zz_io_output_payload_NV = io_inputs_5_payload_NV;
        _zz_io_output_payload_DZ = io_inputs_5_payload_DZ;
      end
    endcase
  end

  `ifndef SYNTHESIS
  always @(*) begin
    case(io_inputs_0_payload_roundMode)
      FpuRoundMode_RNE : io_inputs_0_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_inputs_0_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_inputs_0_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_inputs_0_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_inputs_0_payload_roundMode_string = "RMM";
      default : io_inputs_0_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(io_inputs_1_payload_roundMode)
      FpuRoundMode_RNE : io_inputs_1_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_inputs_1_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_inputs_1_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_inputs_1_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_inputs_1_payload_roundMode_string = "RMM";
      default : io_inputs_1_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(io_inputs_2_payload_roundMode)
      FpuRoundMode_RNE : io_inputs_2_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_inputs_2_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_inputs_2_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_inputs_2_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_inputs_2_payload_roundMode_string = "RMM";
      default : io_inputs_2_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(io_inputs_3_payload_roundMode)
      FpuRoundMode_RNE : io_inputs_3_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_inputs_3_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_inputs_3_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_inputs_3_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_inputs_3_payload_roundMode_string = "RMM";
      default : io_inputs_3_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(io_inputs_4_payload_roundMode)
      FpuRoundMode_RNE : io_inputs_4_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_inputs_4_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_inputs_4_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_inputs_4_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_inputs_4_payload_roundMode_string = "RMM";
      default : io_inputs_4_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(io_inputs_5_payload_roundMode)
      FpuRoundMode_RNE : io_inputs_5_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_inputs_5_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_inputs_5_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_inputs_5_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_inputs_5_payload_roundMode_string = "RMM";
      default : io_inputs_5_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(io_output_payload_roundMode)
      FpuRoundMode_RNE : io_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_output_payload_roundMode_string = "RMM";
      default : io_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(_zz_io_output_payload_roundMode)
      FpuRoundMode_RNE : _zz_io_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : _zz_io_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : _zz_io_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : _zz_io_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : _zz_io_output_payload_roundMode_string = "RMM";
      default : _zz_io_output_payload_roundMode_string = "???";
    endcase
  end
  `endif

  assign locked = 1'b0;
  assign maskRouted_0 = (locked ? maskLocked_0 : maskProposal_0);
  assign maskRouted_1 = (locked ? maskLocked_1 : maskProposal_1);
  assign maskRouted_2 = (locked ? maskLocked_2 : maskProposal_2);
  assign maskRouted_3 = (locked ? maskLocked_3 : maskProposal_3);
  assign maskRouted_4 = (locked ? maskLocked_4 : maskProposal_4);
  assign maskRouted_5 = (locked ? maskLocked_5 : maskProposal_5);
  assign _zz_maskProposal_1 = {io_inputs_5_valid,{io_inputs_4_valid,{io_inputs_3_valid,{io_inputs_2_valid,{io_inputs_1_valid,io_inputs_0_valid}}}}};
  assign _zz_maskProposal_1_1 = (_zz_maskProposal_1 & (~ _zz__zz_maskProposal_1_1));
  assign maskProposal_0 = io_inputs_0_valid;
  assign maskProposal_1 = _zz_maskProposal_1_1[1];
  assign maskProposal_2 = _zz_maskProposal_1_1[2];
  assign maskProposal_3 = _zz_maskProposal_1_1[3];
  assign maskProposal_4 = _zz_maskProposal_1_1[4];
  assign maskProposal_5 = _zz_maskProposal_1_1[5];
  assign io_output_valid = ((((((io_inputs_0_valid && maskRouted_0) || (io_inputs_1_valid && maskRouted_1)) || (io_inputs_2_valid && maskRouted_2)) || (io_inputs_3_valid && maskRouted_3)) || (io_inputs_4_valid && maskRouted_4)) || (io_inputs_5_valid && maskRouted_5));
  assign _zz_io_output_payload_rd = ((maskRouted_1 || maskRouted_3) || maskRouted_5);
  assign _zz_io_output_payload_rd_1 = (maskRouted_2 || maskRouted_3);
  assign _zz_io_output_payload_rd_2 = (maskRouted_4 || maskRouted_5);
  assign _zz_io_output_payload_rd_3 = {_zz_io_output_payload_rd_2,{_zz_io_output_payload_rd_1,_zz_io_output_payload_rd}};
  assign _zz_io_output_payload_roundMode = _zz__zz_io_output_payload_roundMode;
  assign io_output_payload_rd = _zz_io_output_payload_rd_4;
  assign io_output_payload_value_mantissa = _zz_io_output_payload_value_mantissa;
  assign io_output_payload_value_exponent = _zz_io_output_payload_value_exponent;
  assign io_output_payload_value_sign = _zz_io_output_payload_value_sign;
  assign io_output_payload_value_special = _zz_io_output_payload_value_special;
  assign io_output_payload_scrap = _zz_io_output_payload_scrap;
  assign io_output_payload_roundMode = _zz_io_output_payload_roundMode;
  assign io_output_payload_NV = _zz_io_output_payload_NV;
  assign io_output_payload_DZ = _zz_io_output_payload_DZ;
  assign io_inputs_0_ready = (maskRouted_0 && io_output_ready);
  assign io_inputs_1_ready = (maskRouted_1 && io_output_ready);
  assign io_inputs_2_ready = (maskRouted_2 && io_output_ready);
  assign io_inputs_3_ready = (maskRouted_3 && io_output_ready);
  assign io_inputs_4_ready = (maskRouted_4 && io_output_ready);
  assign io_inputs_5_ready = (maskRouted_5 && io_output_ready);
  assign io_chosenOH = {maskRouted_5,{maskRouted_4,{maskRouted_3,{maskRouted_2,{maskRouted_1,maskRouted_0}}}}};
  assign _zz_io_chosen = io_chosenOH[3];
  assign _zz_io_chosen_1 = io_chosenOH[5];
  assign _zz_io_chosen_2 = ((io_chosenOH[1] || _zz_io_chosen) || _zz_io_chosen_1);
  assign _zz_io_chosen_3 = (io_chosenOH[2] || _zz_io_chosen);
  assign _zz_io_chosen_4 = (io_chosenOH[4] || _zz_io_chosen_1);
  assign io_chosen = {_zz_io_chosen_4,{_zz_io_chosen_3,_zz_io_chosen_2}};
  always @(posedge clk) begin
    if(io_output_valid) begin
      maskLocked_0 <= maskRouted_0;
      maskLocked_1 <= maskRouted_1;
      maskLocked_2 <= maskRouted_2;
      maskLocked_3 <= maskRouted_3;
      maskLocked_4 <= maskRouted_4;
      maskLocked_5 <= maskRouted_5;
    end
  end


endmodule

module FpuSqrt (
  input  wire          io_input_valid,
  output wire          io_input_ready,
  input  wire [24:0]   io_input_payload_a,
  output wire          io_output_valid,
  input  wire          io_output_ready,
  output wire [23:0]   io_output_payload_result,
  output wire [27:0]   io_output_payload_remain,
  input  wire          clk,
  input  wire          reset
);

  wire       [27:0]   _zz_t;
  wire       [25:0]   _zz_t_1;
  wire       [24:0]   _zz_q;
  wire       [29:0]   _zz_a_1;
  wire       [1:0]    _zz_a_2;
  reg        [4:0]    counter;
  reg                 busy;
  wire                io_output_fire;
  reg                 done;
  wire                when_FpuSqrt_l28;
  reg        [27:0]   a;
  reg        [22:0]   x;
  reg        [23:0]   q;
  wire       [27:0]   t;
  wire                when_FpuSqrt_l41;
  reg        [27:0]   _zz_a;
  wire                when_FpuSqrt_l44;
  wire                when_FpuSqrt_l52;

  assign _zz_t_1 = {q,2'b01};
  assign _zz_t = {2'd0, _zz_t_1};
  assign _zz_q = {q,(! t[27])};
  assign _zz_a_1 = {_zz_a,x[22 : 21]};
  assign _zz_a_2 = io_input_payload_a[24 : 23];
  assign io_output_fire = (io_output_valid && io_output_ready);
  assign when_FpuSqrt_l28 = (busy && (counter == 5'h18));
  assign t = (a - _zz_t);
  assign io_output_valid = done;
  assign io_output_payload_result = q;
  assign io_output_payload_remain = a;
  assign io_input_ready = (! busy);
  assign when_FpuSqrt_l41 = (! done);
  always @(*) begin
    _zz_a = a;
    if(when_FpuSqrt_l44) begin
      _zz_a = t;
    end
  end

  assign when_FpuSqrt_l44 = (! t[27]);
  assign when_FpuSqrt_l52 = (! busy);
  always @(posedge clk or posedge reset) begin
    if(reset) begin
      busy <= 1'b0;
      done <= 1'b0;
    end else begin
      if(io_output_fire) begin
        busy <= 1'b0;
      end
      if(when_FpuSqrt_l28) begin
        done <= 1'b1;
      end
      if(io_output_fire) begin
        done <= 1'b0;
      end
      if(when_FpuSqrt_l52) begin
        if(io_input_valid) begin
          busy <= 1'b1;
        end
      end
    end
  end

  always @(posedge clk) begin
    if(when_FpuSqrt_l41) begin
      counter <= (counter + 5'h01);
      q <= _zz_q[23:0];
      a <= _zz_a_1[27:0];
      x <= (x <<< 2);
    end
    if(when_FpuSqrt_l52) begin
      q <= 24'h0;
      a <= {26'd0, _zz_a_2};
      x <= io_input_payload_a[22:0];
      counter <= 5'h0;
    end
  end


endmodule

module FpuDiv (
  input  wire          io_input_valid,
  output wire          io_input_ready,
  input  wire [23:0]   io_input_payload_a,
  input  wire [23:0]   io_input_payload_b,
  output wire          io_output_valid,
  input  wire          io_output_ready,
  output wire [26:0]   io_output_payload_result,
  output wire [24:0]   io_output_payload_remain,
  input  wire          clk,
  input  wire          reset
);

  wire       [24:0]   _zz_shifter_1;
  wire       [24:0]   _zz_div1;
  wire       [26:0]   _zz_div3;
  wire       [25:0]   _zz_div3_1;
  wire       [25:0]   _zz_div3_2;
  reg        [3:0]    counter;
  reg                 busy;
  wire                io_output_fire;
  reg                 done;
  wire                when_FpuDiv_l31;
  reg        [26:0]   shifter;
  reg        [26:0]   result;
  reg        [26:0]   div1;
  reg        [26:0]   div3;
  wire       [26:0]   div2;
  wire       [27:0]   sub1;
  wire       [27:0]   sub2;
  wire       [27:0]   sub3;
  wire                when_FpuDiv_l48;
  reg        [26:0]   _zz_shifter;
  wire                when_FpuDiv_l52;
  wire                when_FpuDiv_l56;
  wire                when_FpuDiv_l60;
  wire                when_FpuDiv_l67;

  assign _zz_shifter_1 = {1'b1,io_input_payload_a};
  assign _zz_div1 = {1'b1,io_input_payload_b};
  assign _zz_div3_1 = {1'b0,{1'b1,io_input_payload_b}};
  assign _zz_div3 = {1'd0, _zz_div3_1};
  assign _zz_div3_2 = ({1'd0,{1'b1,io_input_payload_b}} <<< 1'd1);
  assign io_output_fire = (io_output_valid && io_output_ready);
  assign when_FpuDiv_l31 = (busy && (counter == 4'b1101));
  assign div2 = (div1 <<< 1);
  assign sub1 = ({1'b0,shifter} - {1'b0,div1});
  assign sub2 = ({1'b0,shifter} - {1'b0,div2});
  assign sub3 = ({1'b0,shifter} - {1'b0,div3});
  assign io_output_valid = done;
  assign io_output_payload_result = result;
  assign io_output_payload_remain = (shifter >>> 2'd2);
  assign io_input_ready = (! busy);
  assign when_FpuDiv_l48 = (! done);
  always @(*) begin
    _zz_shifter = shifter;
    if(when_FpuDiv_l52) begin
      _zz_shifter = sub1[26:0];
    end
    if(when_FpuDiv_l56) begin
      _zz_shifter = sub2[26:0];
    end
    if(when_FpuDiv_l60) begin
      _zz_shifter = sub3[26:0];
    end
  end

  assign when_FpuDiv_l52 = (! sub1[27]);
  assign when_FpuDiv_l56 = (! sub2[27]);
  assign when_FpuDiv_l60 = (! sub3[27]);
  assign when_FpuDiv_l67 = (! busy);
  always @(posedge clk or posedge reset) begin
    if(reset) begin
      busy <= 1'b0;
      done <= 1'b0;
    end else begin
      if(io_output_fire) begin
        busy <= 1'b0;
      end
      if(when_FpuDiv_l31) begin
        done <= 1'b1;
      end
      if(io_output_fire) begin
        done <= 1'b0;
      end
      if(when_FpuDiv_l67) begin
        busy <= io_input_valid;
      end
    end
  end

  always @(posedge clk) begin
    if(when_FpuDiv_l48) begin
      counter <= (counter + 4'b0001);
      result <= (result <<< 2);
      if(when_FpuDiv_l52) begin
        result[1 : 0] <= 2'b01;
      end
      if(when_FpuDiv_l56) begin
        result[1 : 0] <= 2'b10;
      end
      if(when_FpuDiv_l60) begin
        result[1 : 0] <= 2'b11;
      end
      shifter <= (_zz_shifter <<< 2);
    end
    if(when_FpuDiv_l67) begin
      counter <= 4'b0000;
      shifter <= {2'd0, _zz_shifter_1};
      div1 <= {2'd0, _zz_div1};
      div3 <= (_zz_div3 + {1'b0,_zz_div3_2});
    end
  end


endmodule

module StreamArbiter (
  input  wire          io_inputs_0_valid,
  output wire          io_inputs_0_ready,
  input  wire [3:0]    io_inputs_0_payload_opcode,
  input  wire [1:0]    io_inputs_0_payload_arg,
  input  wire [4:0]    io_inputs_0_payload_rs1,
  input  wire [4:0]    io_inputs_0_payload_rs2,
  input  wire [4:0]    io_inputs_0_payload_rs3,
  input  wire [4:0]    io_inputs_0_payload_rd,
  input  wire [0:0]    io_inputs_0_payload_format,
  input  wire [2:0]    io_inputs_0_payload_roundMode,
  output wire          io_output_valid,
  input  wire          io_output_ready,
  output wire [3:0]    io_output_payload_opcode,
  output wire [1:0]    io_output_payload_arg,
  output wire [4:0]    io_output_payload_rs1,
  output wire [4:0]    io_output_payload_rs2,
  output wire [4:0]    io_output_payload_rs3,
  output wire [4:0]    io_output_payload_rd,
  output wire [0:0]    io_output_payload_format,
  output wire [2:0]    io_output_payload_roundMode,
  output wire [0:0]    io_chosenOH,
  input  wire          clk,
  input  wire          reset
);
  localparam FpuOpcode_LOAD = 4'd0;
  localparam FpuOpcode_STORE = 4'd1;
  localparam FpuOpcode_MUL = 4'd2;
  localparam FpuOpcode_ADD = 4'd3;
  localparam FpuOpcode_FMA = 4'd4;
  localparam FpuOpcode_I2F = 4'd5;
  localparam FpuOpcode_F2I = 4'd6;
  localparam FpuOpcode_CMP = 4'd7;
  localparam FpuOpcode_DIV = 4'd8;
  localparam FpuOpcode_SQRT = 4'd9;
  localparam FpuOpcode_MIN_MAX = 4'd10;
  localparam FpuOpcode_SGNJ = 4'd11;
  localparam FpuOpcode_FMV_X_W = 4'd12;
  localparam FpuOpcode_FMV_W_X = 4'd13;
  localparam FpuOpcode_FCLASS = 4'd14;
  localparam FpuOpcode_FCVT_X_X = 4'd15;
  localparam FpuFormat_FLOAT = 1'd0;
  localparam FpuFormat_DOUBLE = 1'd1;
  localparam FpuRoundMode_RNE = 3'd0;
  localparam FpuRoundMode_RTZ = 3'd1;
  localparam FpuRoundMode_RDN = 3'd2;
  localparam FpuRoundMode_RUP = 3'd3;
  localparam FpuRoundMode_RMM = 3'd4;

  wire       [1:0]    _zz__zz_maskProposal_0_2;
  wire       [1:0]    _zz__zz_maskProposal_0_2_1;
  wire       [0:0]    _zz__zz_maskProposal_0_2_2;
  wire       [0:0]    _zz_maskProposal_0_3;
  wire                locked;
  wire                maskProposal_0;
  reg                 maskLocked_0;
  wire                maskRouted_0;
  wire       [0:0]    _zz_maskProposal_0;
  wire       [1:0]    _zz_maskProposal_0_1;
  wire       [1:0]    _zz_maskProposal_0_2;
  wire       [3:0]    _zz_io_output_payload_opcode;
  wire       [0:0]    _zz_io_output_payload_format;
  wire       [2:0]    _zz_io_output_payload_roundMode;
  `ifndef SYNTHESIS
  reg [63:0] io_inputs_0_payload_opcode_string;
  reg [47:0] io_inputs_0_payload_format_string;
  reg [23:0] io_inputs_0_payload_roundMode_string;
  reg [63:0] io_output_payload_opcode_string;
  reg [47:0] io_output_payload_format_string;
  reg [23:0] io_output_payload_roundMode_string;
  reg [63:0] _zz_io_output_payload_opcode_string;
  reg [47:0] _zz_io_output_payload_format_string;
  reg [23:0] _zz_io_output_payload_roundMode_string;
  `endif


  assign _zz__zz_maskProposal_0_2 = (_zz_maskProposal_0_1 - _zz__zz_maskProposal_0_2_1);
  assign _zz__zz_maskProposal_0_2_2 = maskLocked_0;
  assign _zz__zz_maskProposal_0_2_1 = {1'd0, _zz__zz_maskProposal_0_2_2};
  assign _zz_maskProposal_0_3 = (_zz_maskProposal_0_2[1 : 1] | _zz_maskProposal_0_2[0 : 0]);
  `ifndef SYNTHESIS
  always @(*) begin
    case(io_inputs_0_payload_opcode)
      FpuOpcode_LOAD : io_inputs_0_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : io_inputs_0_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : io_inputs_0_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : io_inputs_0_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : io_inputs_0_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : io_inputs_0_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : io_inputs_0_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : io_inputs_0_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : io_inputs_0_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : io_inputs_0_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : io_inputs_0_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : io_inputs_0_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : io_inputs_0_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : io_inputs_0_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : io_inputs_0_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : io_inputs_0_payload_opcode_string = "FCVT_X_X";
      default : io_inputs_0_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(io_inputs_0_payload_format)
      FpuFormat_FLOAT : io_inputs_0_payload_format_string = "FLOAT ";
      FpuFormat_DOUBLE : io_inputs_0_payload_format_string = "DOUBLE";
      default : io_inputs_0_payload_format_string = "??????";
    endcase
  end
  always @(*) begin
    case(io_inputs_0_payload_roundMode)
      FpuRoundMode_RNE : io_inputs_0_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_inputs_0_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_inputs_0_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_inputs_0_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_inputs_0_payload_roundMode_string = "RMM";
      default : io_inputs_0_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(io_output_payload_opcode)
      FpuOpcode_LOAD : io_output_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : io_output_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : io_output_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : io_output_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : io_output_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : io_output_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : io_output_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : io_output_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : io_output_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : io_output_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : io_output_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : io_output_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : io_output_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : io_output_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : io_output_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : io_output_payload_opcode_string = "FCVT_X_X";
      default : io_output_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(io_output_payload_format)
      FpuFormat_FLOAT : io_output_payload_format_string = "FLOAT ";
      FpuFormat_DOUBLE : io_output_payload_format_string = "DOUBLE";
      default : io_output_payload_format_string = "??????";
    endcase
  end
  always @(*) begin
    case(io_output_payload_roundMode)
      FpuRoundMode_RNE : io_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : io_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : io_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : io_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : io_output_payload_roundMode_string = "RMM";
      default : io_output_payload_roundMode_string = "???";
    endcase
  end
  always @(*) begin
    case(_zz_io_output_payload_opcode)
      FpuOpcode_LOAD : _zz_io_output_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : _zz_io_output_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : _zz_io_output_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : _zz_io_output_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : _zz_io_output_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : _zz_io_output_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : _zz_io_output_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : _zz_io_output_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : _zz_io_output_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : _zz_io_output_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : _zz_io_output_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : _zz_io_output_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : _zz_io_output_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : _zz_io_output_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : _zz_io_output_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : _zz_io_output_payload_opcode_string = "FCVT_X_X";
      default : _zz_io_output_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(_zz_io_output_payload_format)
      FpuFormat_FLOAT : _zz_io_output_payload_format_string = "FLOAT ";
      FpuFormat_DOUBLE : _zz_io_output_payload_format_string = "DOUBLE";
      default : _zz_io_output_payload_format_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_io_output_payload_roundMode)
      FpuRoundMode_RNE : _zz_io_output_payload_roundMode_string = "RNE";
      FpuRoundMode_RTZ : _zz_io_output_payload_roundMode_string = "RTZ";
      FpuRoundMode_RDN : _zz_io_output_payload_roundMode_string = "RDN";
      FpuRoundMode_RUP : _zz_io_output_payload_roundMode_string = "RUP";
      FpuRoundMode_RMM : _zz_io_output_payload_roundMode_string = "RMM";
      default : _zz_io_output_payload_roundMode_string = "???";
    endcase
  end
  `endif

  assign locked = 1'b0;
  assign maskRouted_0 = (locked ? maskLocked_0 : maskProposal_0);
  assign _zz_maskProposal_0 = io_inputs_0_valid;
  assign _zz_maskProposal_0_1 = {_zz_maskProposal_0,_zz_maskProposal_0};
  assign _zz_maskProposal_0_2 = (_zz_maskProposal_0_1 & (~ _zz__zz_maskProposal_0_2));
  assign maskProposal_0 = _zz_maskProposal_0_3[0];
  assign io_output_valid = (io_inputs_0_valid && maskRouted_0);
  assign _zz_io_output_payload_opcode = io_inputs_0_payload_opcode;
  assign _zz_io_output_payload_format = io_inputs_0_payload_format;
  assign _zz_io_output_payload_roundMode = io_inputs_0_payload_roundMode;
  assign io_output_payload_opcode = _zz_io_output_payload_opcode;
  assign io_output_payload_arg = io_inputs_0_payload_arg;
  assign io_output_payload_rs1 = io_inputs_0_payload_rs1;
  assign io_output_payload_rs2 = io_inputs_0_payload_rs2;
  assign io_output_payload_rs3 = io_inputs_0_payload_rs3;
  assign io_output_payload_rd = io_inputs_0_payload_rd;
  assign io_output_payload_format = _zz_io_output_payload_format;
  assign io_output_payload_roundMode = _zz_io_output_payload_roundMode;
  assign io_inputs_0_ready = (maskRouted_0 && io_output_ready);
  assign io_chosenOH = maskRouted_0;
  always @(posedge clk or posedge reset) begin
    if(reset) begin
      maskLocked_0 <= 1'b1;
    end else begin
      if(io_output_valid) begin
        maskLocked_0 <= maskRouted_0;
      end
    end
  end


endmodule

module StreamFork (
  input  wire          io_input_valid,
  output wire          io_input_ready,
  input  wire [3:0]    io_input_payload_opcode,
  input  wire [4:0]    io_input_payload_rd,
  input  wire          io_input_payload_write,
  input  wire [31:0]   io_input_payload_value,
  output wire          io_outputs_0_valid,
  input  wire          io_outputs_0_ready,
  output wire [3:0]    io_outputs_0_payload_opcode,
  output wire [4:0]    io_outputs_0_payload_rd,
  output wire          io_outputs_0_payload_write,
  output wire [31:0]   io_outputs_0_payload_value,
  output wire          io_outputs_1_valid,
  input  wire          io_outputs_1_ready,
  output wire [3:0]    io_outputs_1_payload_opcode,
  output wire [4:0]    io_outputs_1_payload_rd,
  output wire          io_outputs_1_payload_write,
  output wire [31:0]   io_outputs_1_payload_value
);
  localparam FpuOpcode_LOAD = 4'd0;
  localparam FpuOpcode_STORE = 4'd1;
  localparam FpuOpcode_MUL = 4'd2;
  localparam FpuOpcode_ADD = 4'd3;
  localparam FpuOpcode_FMA = 4'd4;
  localparam FpuOpcode_I2F = 4'd5;
  localparam FpuOpcode_F2I = 4'd6;
  localparam FpuOpcode_CMP = 4'd7;
  localparam FpuOpcode_DIV = 4'd8;
  localparam FpuOpcode_SQRT = 4'd9;
  localparam FpuOpcode_MIN_MAX = 4'd10;
  localparam FpuOpcode_SGNJ = 4'd11;
  localparam FpuOpcode_FMV_X_W = 4'd12;
  localparam FpuOpcode_FMV_W_X = 4'd13;
  localparam FpuOpcode_FCLASS = 4'd14;
  localparam FpuOpcode_FCVT_X_X = 4'd15;

  `ifndef SYNTHESIS
  reg [63:0] io_input_payload_opcode_string;
  reg [63:0] io_outputs_0_payload_opcode_string;
  reg [63:0] io_outputs_1_payload_opcode_string;
  `endif


  `ifndef SYNTHESIS
  always @(*) begin
    case(io_input_payload_opcode)
      FpuOpcode_LOAD : io_input_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : io_input_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : io_input_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : io_input_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : io_input_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : io_input_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : io_input_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : io_input_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : io_input_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : io_input_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : io_input_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : io_input_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : io_input_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : io_input_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : io_input_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : io_input_payload_opcode_string = "FCVT_X_X";
      default : io_input_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(io_outputs_0_payload_opcode)
      FpuOpcode_LOAD : io_outputs_0_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : io_outputs_0_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : io_outputs_0_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : io_outputs_0_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : io_outputs_0_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : io_outputs_0_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : io_outputs_0_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : io_outputs_0_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : io_outputs_0_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : io_outputs_0_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : io_outputs_0_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : io_outputs_0_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : io_outputs_0_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : io_outputs_0_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : io_outputs_0_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : io_outputs_0_payload_opcode_string = "FCVT_X_X";
      default : io_outputs_0_payload_opcode_string = "????????";
    endcase
  end
  always @(*) begin
    case(io_outputs_1_payload_opcode)
      FpuOpcode_LOAD : io_outputs_1_payload_opcode_string = "LOAD    ";
      FpuOpcode_STORE : io_outputs_1_payload_opcode_string = "STORE   ";
      FpuOpcode_MUL : io_outputs_1_payload_opcode_string = "MUL     ";
      FpuOpcode_ADD : io_outputs_1_payload_opcode_string = "ADD     ";
      FpuOpcode_FMA : io_outputs_1_payload_opcode_string = "FMA     ";
      FpuOpcode_I2F : io_outputs_1_payload_opcode_string = "I2F     ";
      FpuOpcode_F2I : io_outputs_1_payload_opcode_string = "F2I     ";
      FpuOpcode_CMP : io_outputs_1_payload_opcode_string = "CMP     ";
      FpuOpcode_DIV : io_outputs_1_payload_opcode_string = "DIV     ";
      FpuOpcode_SQRT : io_outputs_1_payload_opcode_string = "SQRT    ";
      FpuOpcode_MIN_MAX : io_outputs_1_payload_opcode_string = "MIN_MAX ";
      FpuOpcode_SGNJ : io_outputs_1_payload_opcode_string = "SGNJ    ";
      FpuOpcode_FMV_X_W : io_outputs_1_payload_opcode_string = "FMV_X_W ";
      FpuOpcode_FMV_W_X : io_outputs_1_payload_opcode_string = "FMV_W_X ";
      FpuOpcode_FCLASS : io_outputs_1_payload_opcode_string = "FCLASS  ";
      FpuOpcode_FCVT_X_X : io_outputs_1_payload_opcode_string = "FCVT_X_X";
      default : io_outputs_1_payload_opcode_string = "????????";
    endcase
  end
  `endif

  assign io_input_ready = (io_outputs_0_ready && io_outputs_1_ready);
  assign io_outputs_0_valid = (io_input_valid && io_input_ready);
  assign io_outputs_1_valid = (io_input_valid && io_input_ready);
  assign io_outputs_0_payload_opcode = io_input_payload_opcode;
  assign io_outputs_0_payload_rd = io_input_payload_rd;
  assign io_outputs_0_payload_write = io_input_payload_write;
  assign io_outputs_0_payload_value = io_input_payload_value;
  assign io_outputs_1_payload_opcode = io_input_payload_opcode;
  assign io_outputs_1_payload_rd = io_input_payload_rd;
  assign io_outputs_1_payload_write = io_input_payload_write;
  assign io_outputs_1_payload_value = io_input_payload_value;

endmodule

module StreamFifo (
  input  wire          io_push_valid,
  output reg           io_push_ready,
  input  wire          io_push_payload_error,
  input  wire [31:0]   io_push_payload_inst,
  output reg           io_pop_valid,
  input  wire          io_pop_ready,
  output reg           io_pop_payload_error,
  output reg  [31:0]   io_pop_payload_inst,
  input  wire          io_flush,
  output wire [0:0]    io_occupancy,
  output wire [0:0]    io_availability,
  input  wire          clk,
  input  wire          reset
);

  reg                 oneStage_doFlush;
  wire                oneStage_buffer_valid;
  wire                oneStage_buffer_ready;
  wire                oneStage_buffer_payload_error;
  wire       [31:0]   oneStage_buffer_payload_inst;
  reg                 io_push_rValid;
  reg                 io_push_rData_error;
  reg        [31:0]   io_push_rData_inst;
  wire                when_Stream_l375;
  wire                when_Stream_l1230;

  always @(*) begin
    oneStage_doFlush = io_flush;
    if(when_Stream_l1230) begin
      if(io_pop_ready) begin
        oneStage_doFlush = 1'b1;
      end
    end
  end

  always @(*) begin
    io_push_ready = oneStage_buffer_ready;
    if(when_Stream_l375) begin
      io_push_ready = 1'b1;
    end
  end

  assign when_Stream_l375 = (! oneStage_buffer_valid);
  assign oneStage_buffer_valid = io_push_rValid;
  assign oneStage_buffer_payload_error = io_push_rData_error;
  assign oneStage_buffer_payload_inst = io_push_rData_inst;
  always @(*) begin
    io_pop_valid = oneStage_buffer_valid;
    if(when_Stream_l1230) begin
      io_pop_valid = io_push_valid;
    end
  end

  assign oneStage_buffer_ready = io_pop_ready;
  always @(*) begin
    io_pop_payload_error = oneStage_buffer_payload_error;
    if(when_Stream_l1230) begin
      io_pop_payload_error = io_push_payload_error;
    end
  end

  always @(*) begin
    io_pop_payload_inst = oneStage_buffer_payload_inst;
    if(when_Stream_l1230) begin
      io_pop_payload_inst = io_push_payload_inst;
    end
  end

  assign io_occupancy = oneStage_buffer_valid;
  assign io_availability = (! oneStage_buffer_valid);
  assign when_Stream_l1230 = (! oneStage_buffer_valid);
  always @(posedge clk or posedge reset) begin
    if(reset) begin
      io_push_rValid <= 1'b0;
    end else begin
      if(io_push_ready) begin
        io_push_rValid <= io_push_valid;
      end
      if(oneStage_doFlush) begin
        io_push_rValid <= 1'b0;
      end
    end
  end

  always @(posedge clk) begin
    if(io_push_ready) begin
      io_push_rData_error <= io_push_payload_error;
      io_push_rData_inst <= io_push_payload_inst;
    end
  end


endmodule

module BufferCC_2 (
  input  wire          io_dataIn,
  output wire          io_dataOut,
  input  wire          io_jtag_tck,
  input  wire          reset_synchronized
);

  (* async_reg = "true" *) reg                 buffers_0;
  (* async_reg = "true" *) reg                 buffers_1;

  assign io_dataOut = buffers_1;
  always @(posedge io_jtag_tck or posedge reset_synchronized) begin
    if(reset_synchronized) begin
      buffers_0 <= 1'b0;
      buffers_1 <= 1'b0;
    end else begin
      buffers_0 <= io_dataIn;
      buffers_1 <= buffers_0;
    end
  end


endmodule

module BufferCC_1 (
  input  wire          io_dataIn,
  output wire          io_dataOut,
  input  wire          io_jtag_tck,
  input  wire          reset
);

  (* async_reg = "true" *) reg                 buffers_0;
  (* async_reg = "true" *) reg                 buffers_1;

  assign io_dataOut = buffers_1;
  always @(posedge io_jtag_tck or posedge reset) begin
    if(reset) begin
      buffers_0 <= 1'b1;
      buffers_1 <= 1'b1;
    end else begin
      buffers_0 <= io_dataIn;
      buffers_1 <= buffers_0;
    end
  end


endmodule

module BufferCC_3 (
  input  wire          io_dataIn,
  output wire          io_dataOut,
  input  wire          clk,
  input  wire          reset
);

  (* async_reg = "true" *) reg                 buffers_0;
  (* async_reg = "true" *) reg                 buffers_1;

  initial begin
  `ifndef SYNTHESIS
    buffers_0 = $urandom;
    buffers_1 = $urandom;
  `endif
  end

  assign io_dataOut = buffers_1;
  always @(posedge clk) begin
    buffers_0 <= io_dataIn;
    buffers_1 <= buffers_0;
  end


endmodule
