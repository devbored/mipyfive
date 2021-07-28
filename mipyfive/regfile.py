from nmigen import *
from .utils import *

class RegFile(Elaboratable):
    def __init__(self, width, regCount):
        self.addrBits       = ceilLog2(regCount)
        self.rs1Data        = Signal(width)
        self.rs2Data        = Signal(width)
        self.writeData      = Signal(width)
        self.writeEnable    = Signal()
        self.regArray       = Memory(width=width, depth=regCount)
        self.rs1Addr        = Signal(self.addrBits)
        self.rs2Addr        = Signal(self.addrBits)
        self.writeAddr      = Signal(self.addrBits)

    def elaborate(self, platform):
        m = Module()

        with m.If((self.rs1Addr == self.writeAddr) & self.writeEnable):
            m.d.comb += self.rs1Data.eq(self.writeData)
        with m.Else():
            m.d.comb += self.rs1Data.eq(self.regArray[self.rs1Addr])

        with m.If((self.rs2Addr == self.writeAddr) & self.writeEnable):
            m.d.comb += self.rs2Data.eq(self.writeData)
        with m.Else():
            m.d.comb += self.rs2Data.eq(self.regArray[self.rs2Addr])

        with m.If(self.writeEnable):
            m.d.sync += self.regArray[self.writeAddr].eq(self.writeData)

        return m
