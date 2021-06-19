from nmigen import *
from .utils import *
from .types import *

# Forwarding Unit to resolve Data Hazards
class ForwardingUnit(Elaboratable):
    def __init__(self, width, regCount):
        addrBits                = ceilLog2(regCount)
        self.ID_EX_rs1          = Signal(addrBits)
        self.ID_EX_rs2          = Signal(addrBits)
        self.EX_MEM_rd          = Signal(addrBits)
        self.MEM_WB_rd          = Signal(addrBits)
        self.EX_MEM_reg_write   = Signal()
        self.MEM_WB_reg_write   = Signal()
        self.fwdAluA            = Signal(2)
        self.fwdAluB            = Signal(2)

    def elaborate(self, platform):
        m = Module()

        # MEM/WB Hazard
        with m.If((self.MEM_WB_rd != self.EX_MEM_rd) & self.MEM_WB_reg_write & (self.MEM_WB_rd != 0)):
            with m.If(self.MEM_WB_rd == self.ID_EX_rs1):
                m.d.comb += self.fwdAluA.eq(AluForwardCtrl.MEM_WB)
            with m.Else():
                m.d.comb += self.fwdAluA.eq(AluForwardCtrl.NO_FWD)
            
            with m.If(self.MEM_WB_rd == self.ID_EX_rs2):
                m.d.comb += self.fwdAluB.eq(AluForwardCtrl.MEM_WB)
            with m.Else():
                m.d.comb += self.fwdAluB.eq(AluForwardCtrl.NO_FWD)
            

        # EX/MEM Hazard
        with m.Elif(self.EX_MEM_reg_write & (self.EX_MEM_rd != 0)):
            with m.If(self.EX_MEM_rd == self.ID_EX_rs1):
                m.d.comb += self.fwdAluA.eq(AluForwardCtrl.EX_MEM)
            with m.Else():
                m.d.comb += self.fwdAluA.eq(AluForwardCtrl.NO_FWD)

            with m.If(self.EX_MEM_rd == self.ID_EX_rs2):
                m.d.comb += self.fwdAluB.eq(AluForwardCtrl.EX_MEM)
            with m.Else():
                m.d.comb += self.fwdAluB.eq(AluForwardCtrl.NO_FWD)

        # No Data Hazards detected
        with m.Else():
            m.d.comb += [
                self.fwdAluA.eq(AluForwardCtrl.NO_FWD),
                self.fwdAluB.eq(AluForwardCtrl.NO_FWD)
            ]

        return m
