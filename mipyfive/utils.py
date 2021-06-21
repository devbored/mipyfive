import math
from riscv_assembler.utils import *

def ceilLog2(value):
    ''' Return ceiling of Log2\n\n NOTE: "value" is always treated as --> abs(value) '''
    if abs(value) == 1:
        return 1
    return math.ceil(math.log(abs(value), 2))

def asm2binR(instr, rd, rs1, rs2):
    ''' Simple wrapper around riscv_assembler.utils R-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.R_type(instr, rs1, rs2, rd)
    return int(bitstring, 2)

def asm2binI(instr, rd, imm, rs1):
    ''' Simple wrapper around riscv_assembler.utils I-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.I_type(instr, rs1, imm, rd)
    return int(bitstring, 2)

def asm2binS(instr, rs2, imm, rs1):
    ''' Simple wrapper around riscv_assembler.utils S-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.S_type(instr, rs1, rs2, imm)
    return int(bitstring, 2)

def asm2binB(instr, rs2, imm, rs1):
    ''' Simple wrapper around riscv_assembler.utils B/SB-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.SB_type(instr, rs1, rs2, imm)
    return int(bitstring, 2)

def asm2binU(instr, rd, imm):
    ''' Simple wrapper around riscv_assembler.utils U-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.U_type(instr, imm, rd)
    return int(bitstring, 2)

def asm2binJ(instr, rd, imm):
    ''' Simple wrapper around riscv_assembler.utils J/UJ-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.UJ_type(instr, imm, rd)
    return int(bitstring, 2)