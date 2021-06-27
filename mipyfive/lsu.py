from nmigen import *
from .types import *

# Load-Store Unit - really only does sign/zero extensions at the moment
class LSU(Elaboratable):
    def __init__(self, width):
        self.lDataIn    = Signal(width)
        self.lCtrlIn    = Signal(3)
        self.sDataIn    = Signal(width)
        self.sCtrlIn    = Signal(2)
        self.sDataOut   = Signal(width)
        self.lDataOut   = Signal(width)

    def elaborate(self, platform):
        m = Module()

        # Load logic
        lb  = Cat(self.lDataIn[0:8], Repl(self.lDataIn[7], 24))
        lbu = Cat(self.lDataIn[0:8], Repl(0, 24))
        lh  = Cat(self.lDataIn[0:16], Repl(self.lDataIn[15], 16))
        lhu = Cat(self.lDataIn[0:16], Repl(0, 16))
        with m.Switch(self.lCtrlIn):
            with m.Case(LSULoadCtrl.LSU_LB.value):
                m.d.comb += self.lDataOut.eq(lb)
            with m.Case(LSULoadCtrl.LSU_LBU.value):
                m.d.comb += self.lDataOut.eq(lbu)
            with m.Case(LSULoadCtrl.LSU_LH.value):
                m.d.comb += self.lDataOut.eq(lh)
            with m.Case(LSULoadCtrl.LSU_LHU.value):
                m.d.comb += self.lDataOut.eq(lhu)
            with m.Default():
                m.d.comb += self.lDataOut.eq(self.lDataIn)

        # Store logic
        sb = Cat(self.sDataIn[0:8], Repl(0, 24))
        sh = Cat(self.sDataIn[0:16], Repl(0, 16))
        with m.Switch(self.sCtrlIn):
            with m.Case(LSUStoreCtrl.LSU_SB.value):
                m.d.comb += self.sDataOut.eq(sb)
            with m.Case(LSUStoreCtrl.LSU_SH.value):
                m.d.comb += self.sDataOut.eq(sh)
            with m.Default():
                m.d.comb += self.sDataOut.eq(self.sDataIn)

        return m
