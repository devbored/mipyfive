from nmigen import *
from .types import *
from .utils import *

class ALU(Elaboratable):
    def __init__(self, width):
        self.width      = width
        self.aluOpWidth = ceilLog2(len(AluOp))
        self.aluOp      = Signal(ceilLog2(len(AluOp)))
        self.in1        = Signal(width)
        self.in2        = Signal(width)

        self.out        = Signal(width)

    def elaborate(self, platform):
        m = Module()

        addLogic    = self.in1 + self.in2
        subLogic    = self.in1 + (~(self.in2) + 1)[:32]
        andLogic    = self.in1 & self.in2
        orLogic     = self.in1 | self.in2
        xorLogic    = self.in1 ^ self.in2
        sllLogic    = self.in1 << self.in2[:ceilLog2(self.in2.width)]
        srlLogic    = self.in1 >> self.in2
        sraLogic    = self.in1.as_signed() >> self.in2
        sltLogic    = Cat((self.in1.as_signed() < self.in2.as_signed())[0], Repl(0, self.width - 1))
        sltuLogic   = Cat((self.in1 < self.in2)[0], Repl(0, self.width - 1))
        sgteLogic   = Cat(~sltLogic[0],  Repl(0, 31))
        sgteuLogic  = Cat(~sltuLogic[0], Repl(0, 31))
        equalLogic  = Cat(((xorLogic) == C(0, self.width))[0], Repl(0, self.width - 1))
        nequalLogic = Cat(((xorLogic) != C(0, self.width))[0], Repl(0, self.width - 1))
        passBLogic  = self.in2
        add4ALogic  = self.in1 + C(4, self.width)

        with m.If(self.aluOp == C(AluOp.ADD.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(addLogic)
        with m.Elif(self.aluOp == C(AluOp.SUB.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(subLogic)
        with m.Elif(self.aluOp == C(AluOp.AND.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(andLogic)
        with m.Elif(self.aluOp == C(AluOp.OR.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(orLogic)
        with m.Elif(self.aluOp == C(AluOp.XOR.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(xorLogic)
        with m.Elif(self.aluOp == C(AluOp.SLL.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(sllLogic)
        with m.Elif(self.aluOp == C(AluOp.SRL.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(srlLogic)
        with m.Elif(self.aluOp == C(AluOp.SRA.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(sraLogic)
        with m.Elif(self.aluOp == C(AluOp.SLT.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(sltLogic)
        with m.Elif(self.aluOp == C(AluOp.SLTU.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(sltuLogic)
        with m.Elif(self.aluOp == C(AluOp.SGTE.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(sgteLogic)
        with m.Elif(self.aluOp == C(AluOp.SGTEU.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(sgteuLogic)
        with m.Elif(self.aluOp == C(AluOp.EQUAL.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(equalLogic)
        with m.Elif(self.aluOp == C(AluOp.NEQUAL.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(nequalLogic)
        with m.Elif(self.aluOp == C(AluOp.PASS_B.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(passBLogic)
        with m.Elif(self.aluOp == C(AluOp.ADD4_A.value, self.aluOpWidth)):
            m.d.comb += self.out.eq(add4ALogic)
        # Default: Unknown AluOp should just resort to ADD
        with m.Else():
            m.d.comb += self.out.eq(addLogic)

        return m
