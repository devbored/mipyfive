from nmigen import *
from .utils import *
from .types import *

class ForwardingUnit(Elaboratable):
    def __init__(self, regCount):
        addrBits                    = ceilLog2(regCount)
        self.IF_ID_rs1              = Signal(addrBits)
        self.ID_EX_rs1              = Signal(addrBits)
        self.IF_ID_rs2              = Signal(addrBits)
        self.ID_EX_rs2              = Signal(addrBits)
        self.EX_MEM_rd              = Signal(addrBits)
        self.MEM_WB_rd              = Signal(addrBits)
        self.WB_BUFFER_rd           = Signal(addrBits)
        self.EX_MEM_reg_write       = Signal()
        self.MEM_WB_reg_write       = Signal()
        self.WB_BUFFER_reg_write    = Signal()

        self.fwdAluA                = Signal(ceilLog2(len(AluForwardCtrl)))
        self.fwdAluB                = Signal(ceilLog2(len(AluForwardCtrl)))

    def elaborate(self, platform):
        m = Module()

        # --- Forwarding logic for Data Hazards ---
        rs1_EX_MEM = (
            (self.EX_MEM_reg_write) & (self.EX_MEM_rd != 0) & (self.EX_MEM_rd == self.ID_EX_rs1)
        )
        rs1_MEM_WB = (
            ~rs1_EX_MEM & (self.MEM_WB_reg_write) & (self.MEM_WB_rd != 0) & (self.MEM_WB_rd == self.ID_EX_rs1)
        )
        rs1_WB_BUFFER = (
            ~rs1_EX_MEM & ~rs1_MEM_WB & (self.WB_BUFFER_reg_write) & (self.WB_BUFFER_rd != 0) &
                (self.WB_BUFFER_rd == self.ID_EX_rs1)
        )

        rs2_EX_MEM = (
            (self.EX_MEM_reg_write) & (self.EX_MEM_rd != 0) & (self.EX_MEM_rd == self.ID_EX_rs2)
        )
        rs2_MEM_WB = (
            ~rs2_EX_MEM & (self.MEM_WB_reg_write) & (self.MEM_WB_rd != 0) & (self.MEM_WB_rd == self.ID_EX_rs2)
        )
        rs2_WB_BUFFER = (
            ~rs2_EX_MEM & ~rs2_MEM_WB & (self.WB_BUFFER_reg_write) & (self.WB_BUFFER_rd != 0) &
                (self.WB_BUFFER_rd == self.ID_EX_rs2)
        )

        # ALU A
        with m.If(rs1_EX_MEM):
            m.d.comb += self.fwdAluA.eq(AluForwardCtrl.EX_MEM)
        with m.Elif(rs1_MEM_WB):
            m.d.comb += self.fwdAluA.eq(AluForwardCtrl.MEM_WB)
        with m.Elif(rs1_WB_BUFFER):
            m.d.comb += self.fwdAluA.eq(AluForwardCtrl.WB_BUFFER)
        with m.Else():
            m.d.comb += self.fwdAluA.eq(AluForwardCtrl.NO_FWD)

        # ALU B
        with m.If(rs2_EX_MEM):
            m.d.comb += self.fwdAluB.eq(AluForwardCtrl.EX_MEM)
        with m.Elif(rs2_MEM_WB):
            m.d.comb += self.fwdAluB.eq(AluForwardCtrl.MEM_WB)
        with m.Elif(rs2_WB_BUFFER):
            m.d.comb += self.fwdAluB.eq(AluForwardCtrl.WB_BUFFER)
        with m.Else():
            m.d.comb += self.fwdAluB.eq(AluForwardCtrl.NO_FWD)

        return m
