from nmigen import *
from .types import *
from .utils import *

class ALU(Elaboratable):
    def __init__(self, width):
        self.aluOp = Signal(ceilLog2(len(AluOp)))
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)
        self.zflag = Signal()

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
        with m.Elif(self.aluOp == AluOp.SLL):
            m.d.comb += self.out.eq(self.in1 << self.in2[:ceilLog2(self.in2.width)])
        with m.Elif(self.aluOp == AluOp.SRL):
            m.d.comb += self.out.eq(self.in1 >> self.in2)
        with m.Elif(self.aluOp == AluOp.SRA):
            m.d.comb += self.out.eq(self.in1.as_signed() >> self.in2)
        # Default: Unknown AluOp should just resort to ADD
        with m.Else():
            m.d.comb += self.out.eq(self.in1 + self.in2)

        # ALU flag(s)
        with m.If(self.out == 0):
            m.d.comb += self.zflag.eq(1)

        return m
