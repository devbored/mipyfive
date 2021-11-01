from nmigen import *
from mipyfive.utils import *

# A basic BRAM module
class RAM(Elaboratable):
    def __init__(self, width, depth, init=None, dualRead=False):
        addrBits            = ceilLog2(depth)
        self.dualRead       = dualRead
        self.writeEnable    = Signal()
        self.readData       = Signal(width)
        if dualRead:
            self.readData2  = Signal(width)
        self.writeData      = Signal(width)
        self.readAddr       = Signal(addrBits)
        if dualRead:
            self.readAddr2  = Signal(addrBits)
        self.writeAddr      = Signal(addrBits)
        self.memory         = Memory(width=width, depth=depth, init=init)

        self.readPort       = self.memory.read_port()
        if dualRead:
            self.readPort2  = self.memory.read_port()
        self.writePort      = self.memory.write_port()

    def elaborate(self, platform):
        m = Module()

        m.submodules.readPort       = self.readPort
        if self.dualRead:
            m.submodules.readPort2  = self.readPort2
        m.submodules.writePort      = self.writePort

        m.d.comb += [
            self.writePort.addr.eq(self.writeAddr),
            self.writePort.data.eq(self.writeData),
            self.writePort.en.eq(self.writeEnable)
        ]
        if self.dualRead:
            m.d.comb += [
                self.readPort.addr.eq(self.readAddr),
                self.readPort2.addr.eq(self.readAddr2),
                self.readData.eq(self.readPort.data),
                self.readData2.eq(self.readPort2.data),
            ]
        else:
            m.d.comb += [
                self.readPort.addr.eq(self.readAddr),
                self.readData.eq(self.readPort.data),
            ]

        return m
