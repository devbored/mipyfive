from nmigen import *
from .utils import *

# A generic single-port synchronous RAM
class RAM(Elaboratable):
    def __init__(self, width, depth):
        addrBits            = ceilLog2(depth)
        self.writeEnable    = Signal()
        self.readData       = Signal(width)
        self.writeData      = Signal(width)
        self.writeData      = Signal(width)
        self.readAddr       = Signal(addrBits)
        self.writeAddr      = Signal(addrBits)
        self.memory         = Memory(width=width, depth=depth)

    def elaborate(self, platform):
        m = Module()

        with m.If(self.writeEnable):
            m.d.sync += self.memory[self.writeAddr].eq(self.writeData)

        m.d.sync += self.readData.eq(self.memory[self.readAddr])

        return m
