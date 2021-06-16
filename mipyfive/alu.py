from enum import Enum
from nmigen import *
import math

# TODO: Move this enum to "types.py"
class AluOp(Enum):
    ADD     = 0
    SUB     = 1
    AND     = 2
    OR      = 3
    XOR     = 4
    SLT     = 5 # Set if Less Than
    SLTU    = 6 # Set if Less Than (Unsigned)
    SLL     = 7 # Shift Left Logically
    SRL     = 8 # Shift Right Logically
    SRA     = 9 # Shift Right Arithmetically

class ALU(Elaboratable):
    def __init__(self, width):
        self.aluOp = Signal(4)
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)
        self.zflag = Signal(1)

    def elaborate(self, platform):
        m = Module()

        with m.If(self.aluOp == AluOp.ADD):
            m.d.comb += self.out.eq(self.in1 + self.in2)
        with m.Elif(self.aluOp == AluOp.SUB):
            m.d.comb += self.out.eq(self.in1 + ~(self.in2) + 1)
        with m.Elif(self.aluOp == AluOp.AND):
            m.d.comb += self.out.eq(self.in1 & self.in2)
        with m.Elif(self.aluOp == AluOp.OR):
            m.d.comb += self.out.eq(self.in1 | self.in2)
        with m.Elif(self.aluOp == AluOp.XOR):
            m.d.comb += self.out.eq(self.in1 ^ self.in2)
        with m.Elif(self.aluOp == AluOp.SLT):
            m.d.comb += self.out.eq(self.in1.as_signed() < self.in2.as_signed())
        with m.Elif(self.aluOp == AluOp.SLTU):
            m.d.comb += self.out.eq(self.in1 < self.in2)
        with m.Elif(self.aluOp == AluOp.SLL):
            m.d.comb += self.out.eq(self.in1 << self.in2[:int(math.log(self.in2.width, 2))])
        with m.Elif(self.aluOp == AluOp.SRL):
            m.d.comb += self.out.eq(self.in1 >> self.in2)
        with m.Elif(self.aluOp == AluOp.SRA):
            m.d.comb += self.out.eq(self.in1.as_signed() >> self.in2)
        # Default: Invalid AluOp should just resort to ADD
        with m.Else():
            m.d.comb += self.out.eq(0)

        # ALU flag(s)
        with m.If(self.out == 0):
            m.d.comb += self.zflag.eq(1)

        return m
