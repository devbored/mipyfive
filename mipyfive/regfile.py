from nmigen import *
from .utils import *

class RegFile(Elaboratable):
    def __init__(self, width, regCount):
        self.addrBits   = ceilLog2(regCount)
        self.we         = Signal()
        self.rs1Addr    = Signal(self.addrBits)
        self.rs1Data    = Signal(width)
        self.rs2Addr    = Signal(self.addrBits)
        self.rs2Data    = Signal(width)
        self.rdAddr     = Signal(self.addrBits)
        self.rdData     = Signal(width)

        self.memory     = Memory(width=width, depth=regCount)
        self.rs1        = self.memory.read_port()
        self.rs2        = self.memory.read_port()
        self.rd         = self.memory.write_port()

    def elaborate(self, platform):
        m = Module()

        m.submodules.rs1 = self.rs1
        m.submodules.rs2 = self.rs2
        m.submodules.rd  = self.rd
        m.d.comb += [
            self.rs1.addr.eq(self.rs1Addr),
            self.rs1Data.eq(self.rs1.data),
            self.rs2.addr.eq(self.rs2Addr),
            self.rs2Data.eq(self.rs2.data),
            self.rd.addr.eq(self.rdAddr),
            self.rd.data.eq(self.rdData),
            self.rd.en.eq(self.we)
        ]

        return m