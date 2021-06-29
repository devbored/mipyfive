from nmigen import *
from .utils import *
from .types import *

class ForwardingUnit(Elaboratable):
    def __init__(self, regCount):
        addrBits                = ceilLog2(regCount)
        self.IF_ID_rs1          = Signal(addrBits)
        self.ID_EX_rs1          = Signal(addrBits)
        self.IF_ID_rs2          = Signal(addrBits)
        self.ID_EX_rs2          = Signal(addrBits)
        self.EX_MEM_rd          = Signal(addrBits)
        self.MEM_WB_rd          = Signal(addrBits)
        self.EX_MEM_reg_write   = Signal()
        self.MEM_WB_reg_write   = Signal()

        self.fwdAluA            = Signal(2)
        self.fwdAluB            = Signal(2)
        self.fwdRegfileAout     = Signal()
        self.fwdRegfileBout     = Signal()

    def elaborate(self, platform):
        m = Module()

        # --- Forwarding for Control Hazards ---
        # Forward conditions for output A of Regfile
        with m.If((self.IF_ID_rs1 != 0) & (self.IF_ID_rs1 == self.EX_MEM_rd) & (self.EX_MEM_reg_write)):
            m.d.comb += self.fwdRegfileAout.eq(RegfileOutForwardCtrl.EX_MEM.value)
        with m.Else():
            m.d.comb += self.fwdRegfileAout.eq(RegfileOutForwardCtrl.NO_FWD.value)

        # Forward conditions for output B of Regfile
        with m.If((self.IF_ID_rs2 != 0) & (self.IF_ID_rs2 == self.EX_MEM_rd) & (self.EX_MEM_reg_write)):
            m.d.comb += self.fwdRegfileBout.eq(RegfileOutForwardCtrl.EX_MEM.value)
        with m.Else():
            m.d.comb += self.fwdRegfileBout.eq(RegfileOutForwardCtrl.NO_FWD.value)

        # --- Forwarding for Data Hazards ---
        # Forward conditions for input A of ALU
        with m.If((self.EX_MEM_reg_write) & (self.EX_MEM_rd != 0) & (self.EX_MEM_rd == self.ID_EX_rs1)):
            m.d.comb += self.fwdAluA.eq(AluForwardCtrl.EX_MEM)
        with m.Elif((self.MEM_WB_reg_write) & (self.MEM_WB_rd != 0) &
            ~((self.EX_MEM_reg_write) & (self.EX_MEM_rd != 0) & (self.EX_MEM_rd == self.ID_EX_rs1)) &
                (self.MEM_WB_rd == self.ID_EX_rs1)):
                    m.d.comb += self.fwdAluA.eq(AluForwardCtrl.MEM_WB)
        with m.Else():
            m.d.comb += self.fwdAluA.eq(AluForwardCtrl.NO_FWD)

        # Forward conditions for input B of ALU
        with m.If((self.EX_MEM_reg_write) & (self.EX_MEM_rd != 0) & (self.EX_MEM_rd == self.ID_EX_rs2)):
            m.d.comb += self.fwdAluB.eq(AluForwardCtrl.EX_MEM)
        with m.Elif((self.MEM_WB_reg_write) & (self.MEM_WB_rd != 0) &
            ~((self.EX_MEM_reg_write) & (self.EX_MEM_rd != 0) & (self.EX_MEM_rd == self.ID_EX_rs2)) &
                (self.MEM_WB_rd == self.ID_EX_rs2)):
                    m.d.comb += self.fwdAluB.eq(AluForwardCtrl.MEM_WB)
        with m.Else():
            m.d.comb += self.fwdAluB.eq(AluForwardCtrl.NO_FWD)

        return m
