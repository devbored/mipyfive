from nmigen import *

# Adjustable-width pipeline register
class PipeReg(Elaboratable):
    def __init__(self, **inputs):
        self.width = 0
        self.inputs = {}
        if inputs is None:
            self.width = 1
        else:
            for input in inputs.items():
                self.inputs[str(input[0])] = (self.width, self.width+input[1])
                self.width += input[1]
        self.din    = Signal(self.width)
        self.en     = Signal()
        self.rst    = ResetSignal()
        self.dout   = Signal(self.width)
        self.reg    = Memory(width=self.width, depth=1)

    def doutSlice(self, din):
        return self.dout[self.inputs[din][0]:self.inputs[din][1]]

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.dout.eq(self.reg[0])
        
        with m.If(self.rst):
            m.d.sync += self.reg[0].eq(0)
        with m.Elif(self.en):
            m.d.sync += self.reg[0].eq(self.din)
        with m.Else():
            m.d.sync += self.reg[0].eq(self.reg[0])

        return m
