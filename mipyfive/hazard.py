from nmigen import *
from .utils import *

# Hazard Detection Unit
class HazardUnit(Elaboratable):
    def __init__(self, regCount):
        addrBits            = ceilLog2(regCount)
        self.ID_EX_memRead  = Signal()
        self.ID_EX_rd       = Signal(addrBits)
        self.IF_ID_rs1      = Signal(addrBits)
        self.IF_ID_rs2      = Signal(addrBits)
        self.PCwrite        = Signal()
        self.IF_ID_write    = Signal()
        self.ctrlBubble     = Signal()

    def elaborate(self, platform):
        m = Module()

        with m.If(self.ID_EX_memRead &
            ((self.ID_EX_rd == self.IF_ID_rs1) | (self.ID_EX_rd == self.IF_ID_rs2))):
                m.d.comb += [ # Stall
                    self.PCwrite.eq(0),
                    self.IF_ID_write.eq(0),
                    self.ctrlBubble.eq(1)
                ]
        with m.Else():
            m.d.comb += [
                self.PCwrite.eq(1),
                self.IF_ID_write.eq(1),
                self.ctrlBubble.eq(0)
            ]

        return m
