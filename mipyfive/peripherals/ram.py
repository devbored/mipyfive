from nmigen import *
import math

# A generic single-port synchronous RAM
class RAM(Elaboratable):
    def __init__(self, width, depth):
        if depth == 1:
            words = 1
        else:
            words = math.ceil(math.log(depth, 2))
        self.writeEnable    = Signal()
        self.readData       = Signal(width)
        self.writeData      = Signal(width)
        self.writeData      = Signal(width)
        self.readAddr       = Signal(words)
        self.writeAddr      = Signal(words)
        self.memory         = Memory(width=width, depth=depth, init=None, name=None, attrs=None)

    def elaborate(self, platform):
        m = Module()

        with m.If(self.writeEnable):
            m.d.sync += self.memory[self.writeAddr].eq(self.writeData)

        m.d.sync += self.readData.eq(self.memory[self.readAddr])

        return m
