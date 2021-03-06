from enum import Enum
from dataclasses import dataclass

# RISC-V RV32I Instructions
class Rv32iInstructions(Enum):
    # --- R-type Instruction Formats ---
    # (funt7) | (funct3) | (opcode)
    #
    ADD  = (0b0000000 << 10) | (0b000 << 7) | (0b0110011)
    SUB  = (0b0100000 << 10) | (0b000 << 7) | (0b0110011)
    SLL  = (0b0000000 << 10) | (0b001 << 7) | (0b0110011)
    SLT  = (0b0000000 << 10) | (0b010 << 7) | (0b0110011)
    SLTU = (0b0000000 << 10) | (0b011 << 7) | (0b0110011)
    XOR  = (0b0000000 << 10) | (0b100 << 7) | (0b0110011)
    SRL  = (0b0000000 << 10) | (0b101 << 7) | (0b0110011)
    SRA  = (0b0100000 << 10) | (0b101 << 7) | (0b0110011)
    OR   = (0b0000000 << 10) | (0b110 << 7) | (0b0110011)
    AND  = (0b0000000 << 10) | (0b111 << 7) | (0b0110011)

    # --- I-type Instruction Formats ---
    # (funct3)  | (opcode)
    # (imm)     | (funct3) | (opcode)
    #
    JALR    = (0b000 << 7)   | (0b1100111)
    LB      = (0b000 << 7)   | (0b0000011)
    LH      = (0b001 << 7)   | (0b0000011)
    LW      = (0b010 << 7)   | (0b0000011)
    LBU     = (0b100 << 7)   | (0b0000011)
    LHU     = (0b101 << 7)   | (0b0000011)
    ADDI    = (0b000 << 7)   | (0b0010011)
    SLTI    = (0b010 << 7)   | (0b0010011)
    SLTIU   = (0b011 << 7)   | (0b0010011)
    XORI    = (0b100 << 7)   | (0b0010011)
    ORI     = (0b110 << 7)   | (0b0010011)
    ANDI    = (0b111 << 7)   | (0b0010011)
    FENCE   = (0b000 << 7)   | (0b0001111)
    ECALL   = (0b000 << 7)   | (0b1110011)
    SLLI    = (0b0000000 << 10) | (0b001 << 7) | (0b0010011)
    SRLI    = (0b0000000 << 10) | (0b101 << 7) | (0b0010011)
    SRAI    = (0b0100000 << 10) | (0b101 << 7) | (0b0010011)
    EBREAK  = (0b000000000001 << 10) | (0b000 << 7) | (0b1110011)

    # --- S-type Instruction Formats ---
    # (funct3) | (opcode)
    #
    SB = (0b000 << 7) | (0b0100011)
    SH = (0b001 << 7) | (0b0100011)
    SW = (0b010 << 7) | (0b0100011)

    # --- B-type Instruction Formats ---
    # (funct3) | (opcode)
    #
    BEQ  = (0b000 << 7) | (0b1100011)
    BNE  = (0b001 << 7) | (0b1100011)
    BLT  = (0b100 << 7) | (0b1100011)
    BGE  = (0b101 << 7) | (0b1100011)
    BLTU = (0b110 << 7) | (0b1100011)
    BGEU = (0b111 << 7) | (0b1100011)

    # --- U-type Instruction Formats ---
    # (opcode)
    #
    LUI   = (0b0110111)
    AUIPC = (0b0010111)

    # --- J-type Instruction Formats ---
    # (opcode)
    #
    JAL = (0b1101111)

# RV32I Instruction Types
class Rv32iTypes(Enum):
    R       = 0b0110011
    I_Jump  = 0b1100111
    I_Load  = 0b0000011
    I_Arith = 0b0010011
    I_Sys   = 0b1110011
    I_Sync  = 0b0001111
    S       = 0b0100011
    B       = 0b1100011
    U_Load  = 0b0110111
    U_Add   = 0b0010111
    J       = 0b1101111

# CPU control signal types
class AluOp(Enum):
    ADD      = 0b0000
    SUB      = 0b0001
    AND      = 0b0010
    OR       = 0b0011
    XOR      = 0b0100
    SLL      = 0b0101 # Shift Left Logically
    SRL      = 0b0110 # Shift Right Logically
    SRA      = 0b0111 # Shift Right Arithmetically
    EQUAL    = 0b1000
    NEQUAL   = 0b1001
    SLT      = 0b1010 # Set if Less Than
    SLTU     = 0b1011 # Set if Less Than (Unsigned)
    SGTE     = 0b1100 # Set if Greater Than or Equal
    SGTEU    = 0b1101 # Set if Greater Than or Equal (Unsigned)
    PASS_B   = 0b1110 # Pass the B input to output
    ADD4_A   = 0b1111 # Add 4 to A input

# Mem2Reg mux select types
class Mem2RegCtrl(Enum):
    FROM_MEM = 0
    FROM_ALU = 1

class AluASrcCtrl(Enum):
    FROM_RS1    = 0b0
    FROM_PC     = 0b1

class AluBSrcCtrl(Enum):
    FROM_RS2    = 0b0
    FROM_IMM    = 0b1

# ALU Input Data Hazard Forward Selection mux Ctrl types
class AluForwardCtrl(Enum):
    NO_FWD      = 0b00
    EX_MEM      = 0b01
    MEM_WB      = 0b10
    WB_BUFFER   = 0b11

# Load-Store Unit control types
class LSUStoreCtrl(Enum):
    LSU_SB = 0b00
    LSU_SH = 0b01
    LSU_SW = 0b10

class LSULoadCtrl(Enum):
    LSU_LB  = 0b000
    LSU_LH  = 0b001
    LSU_LW  = 0b010
    LSU_LBU = 0b011
    LSU_LHU = 0b100

# Supported ISAs
class CoreISAconfigs(Enum):
    RV32I   = 0
    RV32IM  = 1
    RV32IF  = 2
    RV32IMF = 3
## ISA table
ISAtable = {
    "RV32I"     : CoreISAconfigs.RV32I,
    "RV32IF"    : CoreISAconfigs.RV32IM,
    "RV32IM"    : CoreISAconfigs.RV32IF,
    "RV32IFM"   : CoreISAconfigs.RV32IMF
}

class Imm32Ranges(Enum):
    I_MIN    = -((2**12)//2)
    I_MAX    = ((2**12)//2)-1
    # Unsigned max
    I_MAX_U  = 2**12

    UJ_MIN   = -((2**20)//2)
    UJ_MAX   = ((2**20)//2)-1
    # Unsigned max
    UJ_MAX_U = 2**20

@dataclass
class MipyfiveConfig:
    # Soft-core configs
    core_isa                : CoreISAconfigs = None
    core_data_width         : int = None
    core_reg_count          : int = None
    core_pc_start           : int = None