from amaranth import *
from .utils import *

class HazardUnit(Elaboratable):
    def __init__(self, regCount):
        addrBits                = ceilLog2(regCount)
        self.Jump               = Signal()
        self.Branch             = Signal()
        self.BranchMispredict   = Signal()
        self.IF_valid           = Signal()
        self.MEM_valid          = Signal()
        self.ID_EX_memRead      = Signal()
        self.IF_ID_rs1          = Signal(addrBits)
        self.IF_ID_rs2          = Signal(addrBits)
        self.ID_EX_rd           = Signal(addrBits)

        self.IF_stall           = Signal()
        self.IF_ID_stall        = Signal()
        self.ID_EX_stall        = Signal()
        self.EX_MEM_stall       = Signal()
        self.MEM_WB_stall       = Signal()
        self.IF_ID_flush        = Signal()
        self.ID_EX_flush        = Signal()
        self.EX_MEM_flush       = Signal()

    def elaborate(self, platform):
        m = Module()

        # --- Hazard logic for pipeline stalls and flushes ---
        branchFlush = self.Branch & self.BranchMispredict
        jumpFlush   = self.Jump
        loadStall   = (
            self.ID_EX_memRead & ((self.ID_EX_rd == self.IF_ID_rs1) | (self.ID_EX_rd == self.IF_ID_rs2))
        )
        ifetchStall = ~self.IF_valid
        dfetchStall = ~self.MEM_valid

        with m.If(branchFlush):
            m.d.comb += [
                self.IF_stall.eq(0),
                self.IF_ID_stall.eq(0),
                self.ID_EX_stall.eq(0),
                self.EX_MEM_stall.eq(0),
                self.MEM_WB_stall.eq(0),
                self.IF_ID_flush.eq(1),
                self.ID_EX_flush.eq(1),
                self.EX_MEM_flush.eq(1)
            ]
        with m.Elif(jumpFlush):
            m.d.comb += [
                self.IF_stall.eq(0),
                self.IF_ID_stall.eq(0),
                self.ID_EX_stall.eq(0),
                self.EX_MEM_stall.eq(0),
                self.MEM_WB_stall.eq(0),
                self.IF_ID_flush.eq(1),
                self.ID_EX_flush.eq(1),
                self.EX_MEM_flush.eq(1)
            ]
        with m.Elif(loadStall):
            m.d.comb += [
                self.IF_stall.eq(1),
                self.IF_ID_stall.eq(1),
                self.ID_EX_stall.eq(0),
                self.EX_MEM_stall.eq(0),
                self.MEM_WB_stall.eq(0),
                self.IF_ID_flush.eq(0),
                self.ID_EX_flush.eq(1),
                self.EX_MEM_flush.eq(0)
            ]
        with m.Elif(ifetchStall):
            m.d.comb += [
                self.IF_stall.eq(1),
                self.IF_ID_stall.eq(1),
                self.ID_EX_stall.eq(0),
                self.EX_MEM_stall.eq(0),
                self.MEM_WB_stall.eq(0),
                self.IF_ID_flush.eq(0),
                self.ID_EX_flush.eq(0),
                self.EX_MEM_flush.eq(0)
            ]
        with m.Elif(dfetchStall):
            m.d.comb += [
                self.IF_stall.eq(1),
                self.IF_ID_stall.eq(1),
                self.ID_EX_stall.eq(1),
                self.EX_MEM_stall.eq(1),
                self.MEM_WB_stall.eq(1),
                self.IF_ID_flush.eq(0),
                self.ID_EX_flush.eq(0),
                self.EX_MEM_flush.eq(0)
            ]
        with m.Else():
            m.d.comb += [
                self.IF_stall.eq(0),
                self.IF_ID_stall.eq(0),
                self.ID_EX_stall.eq(0),
                self.EX_MEM_stall.eq(0),
                self.MEM_WB_stall.eq(0),
                self.IF_ID_flush.eq(0),
                self.ID_EX_flush.eq(0),
                self.EX_MEM_flush.eq(0)
            ]

        return m
