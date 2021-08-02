from nmigen import *
from mipyfive.utils import *

# A basic button debounce and edge-detect circuit
class DebounceEdge(Elaboratable):
    def __init__(self, counterVal, risingEdge=True):
        self.counterBits    = ceilLog2(counterVal)
        self.risingEdge     = risingEdge
        self.counterVal     = counterVal

        self.inVal          = Signal()

        self.outVal         = Signal()

    def elaborate(self, platform):
        m = Module()

        # Use 3 D-FF's to perform edge-detection
        dFF0    = Signal()
        dFF1    = Signal()
        dFF2    = Signal()
        counter = Signal(self.counterBits)
        slowClk = ClockDomain(local=True)
        slowClkReg = ClockSignal(domain="slowClk")

        # Toggle slowClk when counter val is reached
        with m.If(counter == self.counterVal):
            m.d.sync += [
                counter.eq(0),
                slowClkReg.eq(~slowClkReg)
            ]
        with m.Else():
            m.d.sync += [
                counter.eq(counter + 1),
                slowClkReg.eq(slowClkReg)
            ]

        m.d.slowClk += [
            # Edge-detect shift reg
            dFF0.eq(self.inVal),
            dFF1.eq(dFF0),
            dFF2.eq(dFF1)
        ]

        if self.risingEdge == True:
            m.d.comb += self.outVal.eq(dFF1 & (~dFF2))
        else:
            # Falling-edge
            m.d.comb += self.outVal.eq((~dFF1) & dFF2)

        return m
