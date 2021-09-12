import os
import textwrap
from enum import Enum
from riscv_assembler.utils import *

class JumpLabelTableOffsets(Enum):
    LABEL_OFFSET        = 0
    LABEL_USED_OFFSET   = 1

def asm2binR(instr, rd, rs1, rs2):
    ''' Simple wrapper around riscv_assembler.utils R-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.R_type(instr, rs1, rs2, rd)
    return int(bitstring, 2)

def asm2binI(instr, rd, rs1, imm):
    ''' Simple wrapper around riscv_assembler.utils I-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.I_type(instr, rs1, imm, rd)

    # NOTE: Bug in generated srai instruction generated - this is a quick workaround
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

    # NOTE: Possible bug? Flip operand order when using this SB_type method
    bitstring = tk.SB_type(instr, rs2, rs1, imm)
    return int(bitstring, 2)

def asm2binU(instr, rd, imm):
    ''' Simple wrapper around riscv_assembler.utils U-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.U_type(instr, imm, rd)

    # Possible bug with returned bitstring? Imm value seems to disappear - this is a workaround for now
    return (int(bitstring,2) & ~(0xff << 12)) | (int(imm) << 12)

def asm2binJ(instr, rd, imm):
    ''' Simple wrapper around riscv_assembler.utils J/UJ-type assembler to convert bitstring --> raw int'''
    tk = Toolkit()
    bitstring = tk.UJ_type(instr, imm, rd)
    return int(bitstring, 2)

def asm2Bin(instructions, verbose=False):
    '''Convert RV32I asm program str to binary list\n
    (Operand order follows same arg orders as asm2bin<RISBUJ> util functions)
    '''
    instrList = textwrap.dedent(instructions).splitlines()

    tk = Toolkit()
    srcLine = 0
    binaryList = []
    jumpLabelTable = {}
    processedInstrList = []

    # Pre-process the instrList for comments and jump labels
    for instr in instrList:
        instr = instr.lstrip()

        # Remove comments and blank lines
        hashIdx = instr.find('#')
        semiIdx = instr.find(';')
        if hashIdx >= 0:
            instr = instr[:hashIdx]
        if semiIdx >= 0:
            instr = instr[:semiIdx]
        if len(instr) == 0:
            continue

        # A very crude find jump labels and store values (assuming first instr is at addr 0)
        labelIdx = instr.find(':')
        if labelIdx > -1:
            if labelIdx == 0:
                print(f"[mipyfive - Error]: asm2Bin - Jump label is empty at line {srcLine}")
                return None
            if instr[:labelIdx].find(" ") > 0:
                print(f"[mipyfive - Error]: asm2Bin - Jump label cannot contain whitespace at line {srcLine}")
                return None
            if labelIdx > 0:
                jumpLabelTable[instr[:labelIdx]] = len(processedInstrList) * 2
                instr = instr[labelIdx+1:]
                instr = instr[(len(instr) - len(instr.lstrip())):]
                if instr[:instr.find(' ')] == "":
                    continue

        processedInstrList.append(instr)

    # Main ASM loop
    addrCounter = 0
    for instr in processedInstrList:
        mnemonic = instr[:instr.find(' ')]
        operands = instr[instr.find(' '):].replace(' ', '').split(',')

        if mnemonic in tk.R_instr:
            binaryList.append(asm2binR(mnemonic, operands[0], operands[1], operands[2]))
        elif mnemonic in tk.I_instr:
            binaryList.append(asm2binI(mnemonic, operands[0], operands[1], operands[2]))
        elif mnemonic in tk.S_instr:
            binaryList.append(asm2binS(mnemonic, operands[0], operands[1], operands[2]))
        elif mnemonic in tk.SB_instr:
            # Check jumplist to replace label with relative jump location for branch instructions
            if operands[2] in jumpLabelTable:
                currIdx = len(binaryList) * 2
                operands[2] = int(jumpLabelTable[operands[2]]) - currIdx
            binaryList.append(asm2binB(mnemonic, operands[0], operands[2], operands[1]))
        elif mnemonic in tk.U_instr:
            binaryList.append(asm2binU(mnemonic, operands[0], operands[1]))
        elif mnemonic in tk.UJ_instr:
            # Check jumplist to replace label with relative jump location for jump instruction
            if operands[1] in jumpLabelTable:
                currIdx = len(binaryList) * 2
                operands[1] = int(jumpLabelTable[operands[1]]) - currIdx
            binaryList.append(asm2binJ(mnemonic, operands[0], operands[1]))
        else:
            print(f"[mipyfive - Error]: asm2Bin - Unrecognized mnemonic ({mnemonic})")
            return None

        if verbose is True:
            if len(operands) == 3:
                print(f"{str(hex(addrCounter)).rjust(10,' ')} |", end='')
                print(f" {mnemonic} {operands[0]}, {operands[1]}, {operands[2]}".ljust(40,' '), end='')
                print(f" | {hex(binaryList[len(binaryList)-1])}".ljust(16,' '))
            else:
                print(f"{str(hex(addrCounter)).rjust(10,' ')} |", end='')
                print(f" {mnemonic} {operands[0]}, {operands[1]}".ljust(40,' '), end='')
                print(f" | {hex(binaryList[len(binaryList)-1])}".ljust(16,' '))

        srcLine     += 1
        addrCounter += 4

    return binaryList
