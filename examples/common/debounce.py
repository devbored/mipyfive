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
        m.domains += ClockDomain("debounce", local=True, reset_less=True)

        # Toggle slowClk when counter val is reached
        with m.If(counter == self.counterVal):
            m.d.sync += [
                counter.eq(0),
                ClockSignal("debounce").eq(~ClockSignal("debounce"))
            ]
        with m.Else():
            m.d.sync += [
                counter.eq(counter + 1),
                ClockSignal("debounce").eq(ClockSignal("debounce"))
            ]

        m.d.debounce += [
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
