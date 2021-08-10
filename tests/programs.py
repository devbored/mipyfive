import os
import sys
import random

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from mipyfive.types import *

# --- Define test programs --------------------------------------------------------------------------------------------

# Arithmetic test
arithVal1           = random.randint(Imm32Ranges.I_MIN.value//2, Imm32Ranges.I_MAX.value//2)
arithVal2           = random.randint(Imm32Ranges.I_MIN.value//2, Imm32Ranges.I_MAX.value//2)
arithRs1            = arithVal1
arithRs2            = arithVal2
arithRs3            = arithVal1 + arithVal2
arithRs4            = arithVal1 - arithVal2
arithRs5            = (arithVal1 << (abs(arithVal2) % 32)) & 0xffffffff
arithRs6            = (arithVal2 << (abs(arithVal1) % 32)) & 0xffffffff
arithRs7            = (arithVal1 & 0xffffffff) >> (abs(arithVal2) % 32) & 0xffffffff
arithRs8            = (arithVal2 & 0xffffffff) >> (abs(arithVal1) % 32) & 0xffffffff
arithRs9            = arithVal1 >> (abs(arithVal2) % 32) & 0xffffffff
arithRs10           = arithVal2 >> (abs(arithVal1) % 32) & 0xffffffff
arithRs11           = (abs(arithVal1) % 32)
arithRs12           = (abs(arithVal2) % 32)
arithTestProgram    = f'''
    # --- Add/Sub tests ---
    addi  x13, x1, {arithVal2}
    bne   x13, x3, FAIL
    addi  x30, x30, 1
    add   x13, x1, x2
    bne   x13, x3, FAIL
    addi  x30, x30, 1
    sub   x13, x1, x2
    bne   x13, x4, FAIL
    addi  x30, x30, 1

    # --- Shift tests ---
    slli  x13, x1, {abs(arithVal2) % 32}
    bne   x13, x5, FAIL
    addi  x30, x30, 1
    sll   x13, x1, x12
    bne   x13, x5, FAIL
    addi  x30, x30, 1
    slli  x13, x2, {abs(arithVal1) % 32}
    bne   x13, x6, FAIL
    addi  x30, x30, 1
    sll   x13, x2, x11
    bne   x13, x6, FAIL
    addi  x30, x30, 1
    slri  x13, x1, {abs(arithVal2) % 32}
    bne   x13, x7, FAIL
    addi  x30, x30, 1
    srl   x13, x1, x12
    bne   x13, x7, FAIL
    addi  x30, x30, 1
    slri  x13, x2, {abs(arithVal1) % 32}
    bne   x13, x8, FAIL
    addi  x30, x30, 1
    srl   x13, x2, x11
    bne   x13, x8, FAIL
    addi  x30, x30, 1
    srai  x13, x1, {abs(arithVal2) % 32}
    bne   x13, x9, FAIL
    addi  x30, x30, 1
    sra   x13, x1, x12
    bne   x13, x9, FAIL
    addi  x30, x30, 1
    srai  x13, x2, {abs(arithVal1) % 32}
    bne   x13, x10, FAIL
    addi  x30, x30, 1
    sra   x13, x2, x11
    bne   x13, x10, FAIL
    addi  x30, x30, 1
    jal   x29, STALL
    FAIL:   add x0, x0, x0  ; NOP
            add x31, x0, x30
    STALL:  add x0, x0, x0  ; NOP
            jal x0, STALL
'''

# TODO: Add more tests...
