from enum import Enum
from nmigen import *
import math

class RegFile(Elaboratable):
    def __init__(self, width, regCount):
        if regCount == 1:
            addrBits = 1
        else:
            addrBits = math.ceil(math.log(regCount, 2))
        self.rs1Data        = Signal(width)
        self.rs2Data        = Signal(width)
        self.writeData      = Signal(width)
        self.writeEnable    = Signal()
        self.regArray       = Memory(width=width, depth=regCount)
        self.rs1Addr        = Signal(addrBits)
        self.rs2Addr        = Signal(addrBits)
        self.writeAddr      = Signal(addrBits)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += [
            self.rs1Data.eq(self.regArray[self.rs1Addr]),
            self.rs2Data.eq(self.regArray[self.rs2Addr])
        ]
        with m.If(self.writeEnable):
            m.d.sync += self.regArray[self.writeAddr].eq(self.writeData)

        return m
