from nmigen import *

# Adjustable-width pipeline register
class PipeReg(Elaboratable):
    def __init__(self, width):
        self.din    = Signal(width)
        self.en     = Signal()
        self.rst    = ResetSignal()
        self.dout   = Signal(width)
        self.reg    = Memory(width=width, depth=1)

    def elaborate(self, platform):
        m = Module()

        # Combinational logic
        m.d.comb += self.dout.eq(self.reg[0])
        
        # Sequential logic
        with m.If(self.rst):
            m.d.sync += self.reg[0].eq(0)
        with m.Elif(self.en):
            m.d.sync += self.reg[0].eq(self.din)

        return m
