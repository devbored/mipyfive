from nmigen import *
from .types import *
from .utils import *

class ALU(Elaboratable):
    def __init__(self, width):
        self.aluOp  = Signal(ceilLog2(len(AluOp)))
        self.in1    = Signal(width)
        self.in2    = Signal(width)

        self.out    = Signal(width)

    def elaborate(self, platform):
        m = Module()

        addLogic    = self.in1 + self.in2
        subLogic    = self.in1 + ~(self.in2) + 1
        andLogic    = self.in1 & self.in2
        orLogic     = self.in1 | self.in2
        xorLogic    = self.in1 ^ self.in2
        sllLogic    = self.in1 << self.in2[:ceilLog2(self.in2.width)]
        srlLogic    = self.in1 >> self.in2
        sraLogic    = self.in1.as_signed() >> self.in2
        sltLogic    = self.in1.as_signed() < self.in2.as_signed()
        sltuLogic   = self.in1 < self.in2
        sgteLogic   = Cat(~sltLogic[0],  Repl(0, 31))
        sgteuLogic  = Cat(~sltuLogic[0], Repl(0, 31))
        equalLogic  = (xorLogic) == 0
        nequalLogic = (xorLogic) != 0

        with m.If(self.aluOp == AluOp.ADD):
            m.d.comb += self.out.eq(addLogic)
        with m.Elif(self.aluOp == AluOp.SUB):
            m.d.comb += self.out.eq(subLogic)
        with m.Elif(self.aluOp == AluOp.AND):
            m.d.comb += self.out.eq(andLogic)
        with m.Elif(self.aluOp == AluOp.OR):
            m.d.comb += self.out.eq(orLogic)
        with m.Elif(self.aluOp == AluOp.XOR):
            m.d.comb += self.out.eq(xorLogic)
        with m.Elif(self.aluOp == AluOp.SLL):
            m.d.comb += self.out.eq(sllLogic)
        with m.Elif(self.aluOp == AluOp.SRL):
            m.d.comb += self.out.eq(srlLogic)
        with m.Elif(self.aluOp == AluOp.SRA):
            m.d.comb += self.out.eq(sraLogic)
        with m.Elif(self.aluOp == AluOp.SLT):
            m.d.comb += self.out.eq(sltLogic)
        with m.Elif(self.aluOp == AluOp.SLTU):
            m.d.comb += self.out.eq(sltuLogic)
        with m.Elif(self.aluOp == AluOp.SGTE):
            m.d.comb += self.out.eq(sgteLogic)
        with m.Elif(self.aluOp == AluOp.SGTEU):
            m.d.comb += self.out.eq(sgteuLogic)
        with m.Elif(self.aluOp == AluOp.EQUAL):
            m.d.comb += self.out.eq(equalLogic)
        with m.Elif(self.aluOp == AluOp.NEQUAL):
            m.d.comb += self.out.eq(nequalLogic)
        # Default: Unknown AluOp should just resort to ADD
        with m.Else():
            m.d.comb += self.out.eq(self.in1 + self.in2)

        return m
