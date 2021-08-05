from nmigen import *
from mipyfive.core import *
from mipyfive.alu import *
from mipyfive.utils import *
from examples.common.ssegdriver import *
from examples.common.debounce import *

# TODO: Just an unrelated 'Hello World' example for now...
class smol(Elaboratable):
    def __init__(self, needDebouncing=False, debounceDelay=1, isCommonAnode=False):
        self.dataWidth          = 32
        self.needDebouncing     = needDebouncing
        self.btn                = Signal()

        self.ssegLEDs           = Signal(7)

        # --- Submodules ---
        self.sseg0              = SSEGdriver(isCommonAnode=isCommonAnode)

        # If button input is either noisy or does not output a single pulse when pressed
        if self.needDebouncing:
            self.debounce       = DebounceEdge(counterVal=debounceDelay)

    def elaborate(self, platform):
        m = Module()
        m.submodules.sseg0 = self.sseg0
        if self.needDebouncing:
            m.submodules.debounce = self.debounce

        segIndex    = Signal(ceilLog2(9))
        m.domains   += ClockDomain("sseg", local=True, reset_less=True)
        m.d.comb += [
            self.sseg0.inVal.eq(segIndex),
            self.ssegLEDs.eq(self.sseg0.outVal),
        ]

        if self.needDebouncing:
            m.d.comb += [
                ClockSignal("sseg").eq(self.debounce.outVal),
                self.debounce.inVal.eq(self.btn)
            ]
        else:
            m.d.comb += ClockSignal("sseg").eq(self.btn)

        with m.If(segIndex == 9):
            m.d.sseg += segIndex.eq(0)
        with m.Else():
            m.d.sseg += segIndex.eq(segIndex + 1)

        return m

