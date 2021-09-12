from nmigen import *
from mipyfive.utils import *

# A generic synchronous RAM
class RAM(Elaboratable):
    def __init__(self, width, depth, init=None, wordAligned=False, dualPort=False):
        addrBits            = ceilLog2(depth)
        self.wordAligned    = wordAligned
        self.dualPort       = dualPort
        self.writeEnable    = Signal()
        self.readData       = Signal(width)
        self.writeData      = Signal(width)
        self.readAddr       = Signal(addrBits)
        self.writeAddr      = Signal(addrBits)
        self.memory         = Memory(width=width, depth=depth, init=init)

        # Not true dual port RAM - instead simple dual port RAM (i.e. dual read only)
        if dualPort:
            self.readAddr2  = Signal(addrBits)
            self.readData2  = Signal(width)

    def elaborate(self, platform):
        m = Module()

        with m.If(self.writeEnable):
            m.d.sync += self.memory[self.writeAddr].eq(self.writeData)

        if self.wordAligned:
            m.d.sync += self.readData.eq(self.memory[self.readAddr[2:]])
            if self.dualPort:
                m.d.sync += self.readData2.eq(self.memory[self.readAddr2[2:]])
        else:
            m.d.sync += self.readData.eq(self.memory[self.readAddr])
            if self.dualPort:
                m.d.sync += self.readData2.eq(self.memory[self.readAddr2])

        return m
