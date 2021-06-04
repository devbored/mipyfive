from nmigen import *
from nmigen.cli import main
from nmigen.back.pysim import *

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

class ALU(Elaboratable):
    def __init__(self, width):
        self.aluOp = Signal()
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)

        # Submodule handles
        self.add = Adder(width)
        self.sub = Subtractor(width)

    def elaborate(self, platform):
        m = Module()
        m.submodules.add = self.add
        m.submodules.sub = self.sub

        # Define ALU logic
        m.d.comb += [
            # Connect inputs to Adder submodule
            self.add.in1.eq(self.in1),
            self.add.in2.eq(self.in2),
            # Connect inputs to Subtractor submodule
            self.sub.in1.eq(self.in1),
            self.sub.in2.eq(self.in2),
        ]
        with m.If(self.aluOp):
            m.d.comb += self.out.eq(self.sub.out)
        with m.Else():
            m.d.comb += self.out.eq(self.add.out)
        return m

if __name__ == "__main__":
    alu = ALU(width=32)
    main(alu, ports=[alu.aluOp, alu.in1, alu.in2, alu.out])
