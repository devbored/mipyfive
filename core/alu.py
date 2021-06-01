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
    m = Module()
    aluWidth = 32
    m.submodules.alu = alu = ALU(width=aluWidth)

    # Formal assertion for adding
    with m.If(alu.aluOp == 0):
        m.d.comb += Assert(alu.out == (alu.in1 + alu.in2)[:aluWidth])
        m.d.comb += Cover((alu.out == 0x00) & (alu.in1 == 0xfe))

    # Formal assertion for subtracting
    with m.If(alu.aluOp == 1):
        m.d.comb += Assert(alu.out == (alu.in1 - alu.in2)[:aluWidth])
        m.d.comb += Cover((alu.out == 0x00) & (alu.in1 == 0xfe))
    
    main(m, ports=[alu.aluOp, alu.in1, alu.in2, alu.out])

    #with Simulator(alu) as sim:
    #    def process():
    #        yield alu.in1.eq(5)
    #        yield alu.in2.eq(6)
    #        yield alu.aluOp.eq(0)
    #        yield Delay(1e-6)
    #    
    #    sim.add_process(process)
    #    with sim.write_vcd(vcd_file="alu.vcd"):
    #        sim.run()

