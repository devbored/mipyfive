from nmigen import *
from .utils import *
from examples.common.ram import *

class RegFile(Elaboratable):
    def __init__(self, width, regCount, bramRegfile=False):
        self.addrBits       = ceilLog2(regCount)
        self.bramRegfile    = bramRegfile
        self.rs1Data        = Signal(width)
        self.rs2Data        = Signal(width)
        self.writeData      = Signal(width)
        self.writeEnable    = Signal()
        self.rs1Addr        = Signal(self.addrBits)
        self.rs2Addr        = Signal(self.addrBits)
        self.writeAddr      = Signal(self.addrBits)

         # TODO: Still generates using LUTs - need to look into this...
        if bramRegfile:
            self.dualPortBram   = RAM(width=width, depth=regCount, wordAligned=False, dualPort=True)
        else:
            # Implement regfile using LUTs instead
            self.regArray       = Memory(width=width, depth=regCount)

    def elaborate(self, platform):
        m = Module()

        if self.bramRegfile:
            m.submodules.dualPortBram = self.dualPortBram
            m.d.comb += [
                self.dualPortBram.readAddr.eq(self.rs1Addr),
                self.dualPortBram.readAddr2.eq(self.rs2Addr),
                self.dualPortBram.writeAddr.eq(self.writeAddr),
                self.dualPortBram.writeData.eq(self.writeData),
                self.dualPortBram.writeEnable.eq(self.writeEnable),

                self.rs1Data.eq(self.dualPortBram.readData),
                self.rs2Data.eq(self.dualPortBram.readData2),
            ]
            pass
        else:
            m.d.comb += [
                self.rs1Data.eq(self.regArray[self.rs1Addr]),
                self.rs2Data.eq(self.regArray[self.rs2Addr])
            ]
            with m.If(self.writeEnable):
                m.d.sync += self.regArray[self.writeAddr].eq(self.writeData)

        return m