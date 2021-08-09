import os
import sys
import random

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from mipyfive.types import *

# --- Define test programs --------------------------------------------------------------------------------------------

# Arithmetic
#arithTestVal1 = random.randint(Imm32Ranges.I_MIN.value, Imm32Ranges.I_MAX.value)
#arithTestVal2 = random.randint(Imm32Ranges.I_MIN.value, Imm32Ranges.I_MAX.value)
arithTestVal1 = random.randint(0, Imm32Ranges.I_MAX.value//2)
arithTestVal2 = random.randint(0, Imm32Ranges.I_MAX.value//2)
arithTestProgram = f'''
    # --- Init test ---
    addi x1,  x0, {arithTestVal1}
    addi x2,  x0, {arithTestVal2}
    addi x3,  x0, {abs(arithTestVal1) % 32}     ; Clamp max shift value by mod 32
    addi x4,  x0, {abs(arithTestVal2) % 32}     ; Clamp max shift value by mod 32
    addi x30, x0, 1                             ; Test index
    and  x31, x0, x0

    # --- Add/Sub tests ---
    addi  x5, x1, {arithTestVal2}               ; Test1
    add   x6, x1, x2                            ; Test2
    sub   x7, x1, x2                            ; Test3

    # --- Shift tests ---
    slli  x8,  x1, {abs(arithTestVal2) % 32}    ; Test8  (Clamp max shift value by mod 32)
    sll   x9,  x3, x4                           ; Test9
    slli  x10, x2, {abs(arithTestVal1) % 32}    ; Test10 (Clamp max shift value by mod 32)
    sll   x11, x4, x3                           ; Test11
    slri  x12, x1, {abs(arithTestVal2) % 32}    ; Test12 (Clamp max shift value by mod 32)
    srl   x13, x3, x4                           ; Test13
    slri  x14, x2, {abs(arithTestVal1) % 32}    ; Test14 (Clamp max shift value by mod 32)
    srl   x15, x4, x3                           ; Test15
    srai  x16, x1, {abs(arithTestVal2) % 32}    ; Test16 (Clamp max shift value by mod 32)
    sra   x17, x3, x4                           ; Test17
    srai  x18, x2, {abs(arithTestVal1) % 32}    ; Test18 (Clamp max shift value by mod 32)
    sra   x19, x4, x3                           ; Test19

    # --- Verify tests, set x32 as register index to the first failed test ---
    addi  x20, x0, {arithTestVal1 + arithTestVal2}
    bne   x20, x5, FAIL
    addi  x30, x30, 1
    bne   x20, x6, FAIL
    addi  x30, x30, 1
    addi  x20, x0, {arithTestVal1 - arithTestVal2}
    bne   x20, x7, FAIL
    addi  x30, x30, 1
    addi  x20, x0, {(arithTestVal1 << (abs(arithTestVal2) % 32)) & 0xfff}
    bne   x20, x8, FAIL
    addi  x30, x30, 1
    bne   x20, x9, FAIL
    addi  x30, x30, 1
    addi  x20, x0, {(arithTestVal2 << (abs(arithTestVal1) % 32)) & 0xfff}
    bne   x20, x10, FAIL
    addi  x30, x30, 1
    bne   x20, x11, FAIL
    addi  x30, x30, 1
    addi  x20, x0, {((arithTestVal1 & 0xffffffff) >> (abs(arithTestVal2) % 32) & 0xfff)}
    bne   x20, x12, FAIL
    addi  x30, x30, 1
    bne   x20, x13, FAIL
    addi  x30, x30, 1
    addi  x20, x0, {((arithTestVal2 & 0xffffffff) >> (abs(arithTestVal1) % 32) & 0xfff)}
    bne   x20, x14, FAIL
    addi  x30, x30, 1
    bne   x20, x15, FAIL
    addi  x30, x30, 1
    addi  x20, x0, {(arithTestVal1 >> (abs(arithTestVal2) % 32) & 0xfff)}
    bne   x20, x16, FAIL
    addi  x30, x30, 1
    bne   x20, x17, FAIL
    addi  x30, x30, 1
    addi  x20, x0, {(arithTestVal2 >> (abs(arithTestVal1) % 32) & 0xfff)}
    bne   x20, x18, FAIL
    addi  x30, x30, 1
    bne   x20, x19, FAIL
    addi  x30, x30, 1
    jal   x29, STALL
    FAIL:   add x0, x0, x0  ; NOP
            add x31, x0, x30
    STALL:  add x0, x0, x0  ; NOP
            jal x0, STALL
'''

# TODO: Add more tests...
