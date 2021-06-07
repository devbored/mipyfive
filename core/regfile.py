from enum import Enum
from nmigen import *
from nmigen.cli import main
import math

class RegFile(Elaboratable):
    def __init__(self, width, regCount):
        self.rs1Data        = Signal(width)
        self.rs2Data        = Signal(width)
        self.writeData      = Signal(width)
        self.writeEnable    = Signal()
        self.regArray       = Memory(width=width, depth=regCount, init=None, name=None, attrs=None)
        if regCount == 1:
            self.rs1Addr        = Signal()
            self.rs2Addr        = Signal()
            self.writeAddr      = Signal()
        else:
            self.rs1Addr        = Signal(int(math.log(regCount, 2)))
            self.rs2Addr        = Signal(int(math.log(regCount, 2)))
            self.writeAddr      = Signal(int(math.log(regCount, 2)))

    def elaborate(self, platform):
        m = Module()

        # Regfile logic
        m.d.comb += [
            self.rs1Data.eq(self.regArray[self.rs1Addr]),
            self.rs2Data.eq(self.regArray[self.rs2Addr])
        ]
        with m.If(self.writeEnable):
            m.d.sync += self.regArray[self.writeAddr].eq(self.writeData)

        return m
