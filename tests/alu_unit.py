import os
import sys
import unittest
import random
from nmigen import *
from nmigen.back.pysim import *

mipyfiveRootDir = os.path.abspath(os.path.join(os.path.dirname(sys.argv[0]), ".."))
mipyfiveCoreDir = os.path.join(mipyfiveRootDir, "core")
sys.path.append(mipyfiveCoreDir)
from alu import *

# TODO: Add optional VCD output?
#
# Template:
#
# Create test output folder
#if not os.path.exists(os.path.join(mipyfiveRootDir, "out", "tests")):
#    os.makedirs(os.path.join(mipyfiveRootDir, "out", "tests"))
#
#with sim.write_vcd(vcd_file=os.path.join(mipyfiveRootDir, "out", "tests", "dump.vcd")):
#    sim.run()
#
# TODO: Add optional VCD output?

# Define unit tests
def test_addition(in1, in2):
    def test(self):
        sim = Simulator(self.dut)
        def process():
            yield self.dut.in1.eq(in1)
            yield self.dut.in2.eq(in2)
            yield self.dut.aluOp.eq(0)
            yield Delay(1e-6)
            self.assertEqual((yield self.dut.out), in1 + in2)
        sim.add_process(process)
        sim.run()
    return test

def test_subtraction(in1, in2):
    def test(self):
        sim = Simulator(self.dut)
        def process():
            yield self.dut.in1.eq(in1)
            yield self.dut.in2.eq(in2)
            yield self.dut.aluOp.eq(1)
            yield Delay(1e-6)
            self.assertEqual((yield self.dut.out), (in1 - in2) & 0xffffffff)
        sim.add_process(process)
        sim.run()
    return test

# Define unit tests object for ALU
class TestAlu(unittest.TestCase):
    def setUp(self):
        self.dut = ALU(width=32)

    # Run some random-valued unit tests...
    int1 = random.randint(0, 2147483647)
    int2 = random.randint(0, 2147483647)
    test_alu_addition = test_addition(int1, int2)
    int1 = random.randint(0, 2147483647)
    int2 = random.randint(0, 2147483647)
    test_alu_subtraction = test_subtraction(int1, int2)

if __name__ == '__main__':
    unittest.main(verbosity=2)