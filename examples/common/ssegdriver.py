from nmigen import *

# A basic Seven Segment Driver HW module for a single digit - displays 0-f
#  _           _0_
# |_| --->  5 |_6_| 1
# |_|       4 |_3_| 2
#
class SSEGdriver(Elaboratable):
    def __init__(self):
        self.inVal  = Signal(4)
        self.outVal = Signal(7)

        # Segment mappings: 0 - 9
        self.segMap = Array([
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
        ])

    def elaborate(self, platform):
        m = Module()
        m.d.comb += self.outVal.eq(self.segMap[self.inVal])
        return m
