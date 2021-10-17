from nmigen import *
from mipyfive.core import *
from mipyfive.utils import *

from examples.common.uart import *

# A small/basic SoC that at the heart contains an RV32I mipyfive core
# TODO: Implement this SoC...
class smol(Elaboratable):
    def __init__(self):
        pass

    def elaborate(self, platform):
        m = Module()
        return m

