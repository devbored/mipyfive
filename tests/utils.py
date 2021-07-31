import os
import textwrap
from riscv_assembler.utils import *

def asm2binR(instr, rd, rs1, rs2):
    ''' Simple wrapper around riscv_assembler.utils R-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.R_type(instr, rs1, rs2, rd)
    return int(bitstring, 2)

def asm2binI(instr, rd, rs1, imm):
    ''' Simple wrapper around riscv_assembler.utils I-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.I_type(instr, rs1, imm, rd)

    # Bug in generated srai instruction generated - this is a quick workaround
    if instr == "srai":
        return int(bitstring, 2) | 0x40000000

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

    # Possible bug with returned bitstring? Imm value seems to disappear - this is a workaround for now
    return (int(bitstring, 2) | (int(imm) << 12))

def asm2binJ(instr, rd, imm):
    ''' Simple wrapper around riscv_assembler.utils J/UJ-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.UJ_type(instr, imm, rd)
    return int(bitstring, 2)

def asm2Bin(instructions, verbose=False):
    '''Convert RV32I asm program str to binary list\n
    (Operand order follows same arg orders as asm2bin<RISBUJ> util functions)
    '''
    instructionsList = textwrap.dedent(instructions).split(os.linesep)
    instructionsList = [value for value in instructionsList if value != '']

    tk = Toolkit()
    binaryList = []
    for i, instr in enumerate(instructionsList):
        mnemonic = instr[:instr.find(' ')]
        operands = instr[instr.find(' '):].replace(' ', '').split(',')
        if mnemonic in tk.R_instr:
            binaryList.append(asm2binR(mnemonic, operands[0], operands[1], operands[2]))
        elif mnemonic in tk.I_instr:
            binaryList.append(asm2binI(mnemonic, operands[0], operands[1], operands[2]))
        elif mnemonic in tk.S_instr:
            binaryList.append(asm2binS(mnemonic, operands[0], operands[1], operands[2]))
        elif mnemonic in tk.SB_instr:
            binaryList.append(asm2binB(mnemonic, operands[0], operands[1], operands[2]))
        elif mnemonic in tk.U_instr:
            binaryList.append(asm2binU(mnemonic, operands[0], operands[1]))
        elif mnemonic in tk.UJ_instr:
            binaryList.append(asm2binJ(mnemonic, operands[0], operands[1]))
        else:
            print(f"[mipyfive - Error]: assembleRv32iProgram - Unrecognized mnemonic ({mnemonic})")
            return None
        if verbose is True:
            if len(operands) == 3:
                print(f"Instr: {mnemonic} {operands[0]}, {operands[1]}, {operands[2]} --> {hex(binaryList[i])}")
            else:
                print(f"Instr: {mnemonic} {operands[0]}, {operands[1]} --> {hex(binaryList[i])}")

    return binaryList
