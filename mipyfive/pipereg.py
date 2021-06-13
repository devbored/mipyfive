from re import I
from nmigen import *
from nmigen.cli import main

# Adjustable-width pipeline register
class PipeReg(Elaboratable):
    def __init__(self, width):
        self.din    = Signal(width)
        self.dout   = Signal(width)
        self.reg    = Memory(width=width, depth=1)

    def elaborate(self, platform):
        m = Module()

        m.d.sync += self.reg[0].eq(self.din)
        m.d.comb += self.dout.eq(self.reg[0])

        return m