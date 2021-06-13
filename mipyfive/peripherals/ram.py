from nmigen import *
import math

# A generic single-port synchronous RAM
class RAM(Elaboratable):
    def __init__(self, width, depth):
        self.readData       = Signal(width)
        self.writeData      = Signal(width)
        self.writeData      = Signal(width)
        self.writeEnable    = Signal()
        self.memory         = Memory(width=width, depth=depth, init=None, name=None, attrs=None)
        if depth == 1:
            self.readAddr   = Signal()
            self.writeAddr  = Signal()
        else:
            self.readAddr   = Signal(math.ceil(math.log(depth, 2)))
            self.writeAddr  = Signal(math.ceil(math.log(depth, 2)))

    def elaborate(self, platform):
        m = Module()

        with m.If(self.writeEnable):
            m.d.sync += self.memory[self.writeAddr].eq(self.writeData)
        with m.Else():
            m.d.sync += self.readData.eq(self.memory[self.readAddr])

        return m