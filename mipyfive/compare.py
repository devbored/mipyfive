from mipyfive.types import CompareTypes
from nmigen import *
from .utils import *
from .types import *

class CompareUnit(Elaboratable):
    def __init__(self, width):
        self.in1        = Signal(width)
        self.in2        = Signal(width)
        self.cmpType    = Signal(ceilLog2(len(CompareTypes)))

        self.isTrue     = Signal()

    def elaborate(self, platform):
        m = Module()

        with m.Switch(self.cmpType):
            with m.Case(CompareTypes.EQUAL):
                m.d.comb += self.isTrue.eq(self.in1 == self.in2)
            with m.Case(CompareTypes.NOT_EQUAL):
                m.d.comb += self.isTrue.eq(self.in1 != self.in2)
            with m.Case(CompareTypes.LESS_THAN):
                m.d.comb += self.isTrue.eq(self.in1.as_signed() < self.in2.as_signed())
            with m.Case(CompareTypes.LESS_THAN_U):
                m.d.comb += self.isTrue.eq(self.in1 < self.in2)
            with m.Case(CompareTypes.GREATER_EQUAL):
                m.d.comb += self.isTrue.eq(self.in1.as_signed() >= self.in2.as_signed())
            with m.Case(CompareTypes.GREATER_EQUAL_U):
                m.d.comb += self.isTrue.eq(self.in1 >= self.in2)

        return m
