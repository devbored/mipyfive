from enum import Enum
from nmigen import *

class AluOp(Enum):
    ADD = 0
    SUB = 1
    AND = 2
    OR  = 3
    XOR = 4

class Adder(Elaboratable):
    def __init__(self, width):
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()
        m.d.comb += self.out.eq(self.in1 + self.in2)
        return m

class Subtractor(Elaboratable):
    def __init__(self, width):
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()
        m.d.comb += self.out.eq(self.in1 - self.in2)
        return m

class AND(Elaboratable):
    def __init__(self, width):
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()
        m.d.comb += self.out.eq(self.in1 & self.in2)
        return m

class OR(Elaboratable):
    def __init__(self, width):
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()
        m.d.comb += self.out.eq(self.in1 | self.in2)
        return m

class XOR(Elaboratable):
    def __init__(self, width):
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()
        m.d.comb += self.out.eq(self.in1 ^ self.in2)
        return m

class ALU(Elaboratable):
    def __init__(self, width):
        self.aluOp = Signal(4)
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)

        # Submodule handles
        self.add = Adder(width)
        self.sub = Subtractor(width)
        self.AND = AND(width)
        self.OR  = OR(width)
        self.XOR = XOR(width)

    def elaborate(self, platform):
        m = Module()
        m.submodules.add = self.add
        m.submodules.sub = self.sub
        m.submodules.AND = self.AND
        m.submodules.OR  = self.OR
        m.submodules.XOR = self.XOR

        # Define ALU logic
        m.d.comb += [
            # Connect Adder submodule
            self.add.in1.eq(self.in1),
            self.add.in2.eq(self.in2),
            # Connect Subtractor submodule
            self.sub.in1.eq(self.in1),
            self.sub.in2.eq(self.in2),
            # Connect AND submodule
            self.AND.in1.eq(self.in1),
            self.AND.in2.eq(self.in2),
            # Connect OR submodule
            self.OR.in1.eq(self.in1),
            self.OR.in2.eq(self.in2),
            # Connect XOR submodule
            self.XOR.in1.eq(self.in1),
            self.XOR.in2.eq(self.in2)
        ]
        with m.If(self.aluOp == AluOp.ADD):
            m.d.comb += self.out.eq(self.add.out)
        with m.Elif(self.aluOp == AluOp.SUB):
            m.d.comb += self.out.eq(self.sub.out)
        with m.Elif(self.aluOp == AluOp.AND):
            m.d.comb += self.out.eq(self.AND.out)
        with m.Elif(self.aluOp == AluOp.OR):
            m.d.comb += self.out.eq(self.OR.out)
        with m.Elif(self.aluOp == AluOp.XOR):
            m.d.comb += self.out.eq(self.XOR.out)
        # Invalid AluOp should just output 0
        with m.Else():
            m.d.comb += self.out.eq(0)

        return m
