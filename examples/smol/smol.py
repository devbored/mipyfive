from nmigen import *
from mipyfive.core import *
from mipyfive.alu import *
from mipyfive.utils import *
from examples.common.ssegdriver import *
from examples.common.debounce import *

# TODO: Just an unrelated 'Hello World' example for now...
class smol(Elaboratable):
    def __init__(self):
        self.dataWidth  = 32
        self.BTN1    = Signal()

        self.P1B1    = Signal()
        self.P1B2    = Signal()
        self.P1B3    = Signal()
        self.P1B4    = Signal()
        self.P1B7    = Signal()
        self.P1B8    = Signal()
        self.P1B9    = Signal()

        # --- Submodules ---
        self.sseg0      = SSEGdriver()
        self.debounce   = DebounceEdge(counterVal=1000000)

    def elaborate(self, platform):
        m = Module()
        m.submodules.sseg0 = self.sseg0
        m.submodules.debounce = self.debounce

        segIndex    = Signal(ceilLog2(9))
        segClk      = ClockDomain(local=True)
        segInc      = ClockSignal(domain="segClk")

        outSeg = Cat(self.P1B1, self.P1B2, self.P1B3, self.P1B4, self.P1B7, self.P1B8, self.P1B9)
        m.d.comb += [
            self.debounce.inVal.eq(self.BTN1),
            self.sseg0.inVal.eq(4),
            segInc.eq(self.debounce.outVal),
            outSeg.eq(self.sseg0.outVal),
        ]

        with m.If(segIndex == 9):
            m.d.segClk += segIndex.eq(0)
        with m.Else():
            m.d.segClk += segIndex.eq(segIndex + 1)

        return m

