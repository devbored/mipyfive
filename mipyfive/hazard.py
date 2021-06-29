from nmigen import *
from .utils import *

# Hazard Detection Unit
class HazardUnit(Elaboratable):
    def __init__(self, regCount):
        addrBits                = ceilLog2(regCount)
        self.ID_EX_memRead      = Signal()
        self.Branch             = Signal()
        self.EX_MEM_memToReg    = Signal()
        self.ID_EX_regWrite     = Signal()
        self.ID_EX_rd           = Signal(addrBits)
        self.EX_MEM_rd          = Signal(addrBits)
        self.IF_ID_rs1          = Signal(addrBits)
        self.IF_ID_rs2          = Signal(addrBits)

        self.IF_stall           = Signal()
        self.IF_ID_stall        = Signal()
        self.ID_EX_flush        = Signal()

    def elaborate(self, platform):
        m = Module()

        branchStall = (self.Branch & self.ID_EX_regWrite &
            ((self.ID_EX_rd == self.IF_ID_rs1) | (self.ID_EX_rd == self.IF_ID_rs2)) |
                self.Branch & self.EX_MEM_memToReg &
                    ((self.EX_MEM_rd == self.IF_ID_rs1) | (self.EX_MEM_rd == self.IF_ID_rs2)))

        loadStall = (self.ID_EX_memRead &
            ((self.ID_EX_rd == self.IF_ID_rs1) | (self.ID_EX_rd == self.IF_ID_rs2)))

        # Stall logic is inverted since this is the writeEnable control line for their respective pipeline regs.
        with m.If(branchStall | loadStall):
                m.d.comb += [
                    self.IF_stall.eq(~1),
                    self.IF_ID_stall.eq(~1),
                    self.ID_EX_flush.eq(1)
                ]
        with m.Else():
            m.d.comb += [
                self.IF_stall.eq(~0),
                self.IF_ID_stall.eq(~0),
                self.ID_EX_flush.eq(0)
            ]

        return m
