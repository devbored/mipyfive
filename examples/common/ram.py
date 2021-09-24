from nmigen import *
from mipyfive.utils import *

# RAM used for simulation purposes at the moment
# TODO: Redo this as BRAM (i.e. read_port(), write_port(), etc.)
class RAM(Elaboratable):
    def __init__(self, width, depth, init=None, wordAligned=False):
        addrBits            = ceilLog2(depth)
        self.wordAligned    = wordAligned
        self.writeEnable    = Signal()
        self.readData       = Signal(width)
        self.writeData      = Signal(width)
        self.readAddr       = Signal(addrBits)
        self.writeAddr      = Signal(addrBits)
        self.memory         = Memory(width=width, depth=depth, init=init)

    def elaborate(self, platform):
        m = Module()

        with m.If(self.writeEnable):
            m.d.sync += self.memory[self.writeAddr].eq(self.writeData)

        if self.wordAligned:
            m.d.sync += self.readData.eq(self.memory[self.readAddr[2:]])
        else:
            m.d.sync += self.readData.eq(self.memory[self.readAddr])

        return m
