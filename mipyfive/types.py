from enum import Enum

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
    ADD     = 0b0000
    SUB     = 0b0001
    AND     = 0b0010
    OR      = 0b0011
    XOR     = 0b0100
    SLL     = 0b0101 # Shift Left Logically
    SRL     = 0b0110 # Shift Right Logically
    SRA     = 0b0111 # Shift Right Arithmetically
    SLT     = 0b1000 # Set if Less Than
    SLTU    = 0b1001 # Set if Less Than (Unsigned)

# Mem2Reg mux select types
class Mem2RegCtrl(Enum):
    FROM_MEM = 0
    FROM_ALU = 1

class AluASrcCtrl(Enum):
    FROM_RS1    = 0b00
    FROM_ZERO   = 0b01
    FROM_PC     = 0b10

class AluBSrcCtrl(Enum):
    FROM_RS2    = 0
    FROM_IMM    = 1

# ALU Input Data Hazard Forward Selection mux Ctrl types
class AluForwardCtrl(Enum):
    NO_FWD  = 0b00
    MEM_WB  = 0b01
    EX_MEM  = 0b10

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

# Regfile Output Control Hazard Forward Selection mux Ctrl types
class RegfileOutForwardCtrl(Enum):
    NO_FWD  = 0
    EX_MEM  = 1

# Compare unit types
class CompareTypes(Enum):
    EQUAL           = 0b000
    NOT_EQUAL       = 0b001
    LESS_THAN       = 0b010
    LESS_THAN_U     = 0b011
    GREATER_EQUAL   = 0b100
    GREATER_EQUAL_U = 0b101

# Supported ISAs
class CoreISAconfigs(Enum):
    RV32I   = 0
    RV32IM  = 1
    RV32IF  = 2
    RV32IMF = 3
