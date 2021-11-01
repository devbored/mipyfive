from nmigen import *

# A basic Seven Segment Driver HW module for a single digit - displays 0-F
#  _           _0_
# |_| --->  5 |_6_| 1
# |_|       4 |_3_| 2
#
class SSEGdriver(Elaboratable):
    def __init__(self, isCommonAnode=False, dataWidth=32):
        self.dataWidth          = dataWidth
        self.isCommonAnode      = isCommonAnode

        self.writeEnable        = Signal()
        self.dataIn             = Signal(dataWidth)
        self.dataOut            = Signal(dataWidth)
        self.valueOut           = Signal(7)

        # Segment mappings: 0-F
        self.segMap = Array([
            # 0-9
            0b0111111,
            0b0000110,
            0b1011011,
            0b1001111,
            0b1100110,
            0b1101101,
            0b1111101,
            0b0000111,
            0b1111111,
            0b1100111,
            # A-F
            0b1110111,
            0b1111100,
            0b0111001,
            0b1011110,
            0b1111001,
            0b1110001
        ])

    def elaborate(self, platform):
        m = Module()
        dataReg = Signal(self.dataWidth)

        with m.If(self.writeEnable):
            m.d.sync += dataReg.eq(self.dataIn)

        if self.isCommonAnode:
            m.d.comb += self.valueOut.eq(~self.segMap[dataReg[:4]])
        else:
            m.d.comb += self.valueOut.eq(self.segMap[dataReg[:4]])

        m.d.comb += self.dataOut.eq(dataReg)

        return m
