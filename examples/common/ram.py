from nmigen import *
from mipyfive.utils import *

# A basic BRAM module
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

        self.readPort       = self.memory.read_port()
        self.writePort      = self.memory.write_port()

    def elaborate(self, platform):
        m = Module()

        m.submodules.readPort   = self.readPort
        m.submodules.writePort  = self.writePort

        if self.wordAligned:
            m.d.comb += [
                self.readPort.addr.eq(self.readAddr[2:]),
                self.readData.eq(self.readPort.data),
                self.writePort.addr.eq(self.writeAddr),
                self.writePort.data.eq(self.writeData),
                self.writePort.en.eq(self.writeEnable)
            ]
        else:
            m.d.comb += [
                self.readPort.addr.eq(self.readAddr),
                self.readData.eq(self.readPort.data),
                self.writePort.addr.eq(self.writeAddr),
                self.writePort.data.eq(self.writeData),
                self.writePort.en.eq(self.writeEnable)
            ]

        return m
