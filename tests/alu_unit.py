import os
import sys
import random
import argparse
import unittest
from enum import Enum
from nmigen import *
from nmigen.back.pysim import *

mipyfiveRootDir = os.path.abspath(os.path.join(os.path.dirname(sys.argv[0]), ".."))
mipyfiveCoreDir = os.path.join(mipyfiveRootDir, "core")
sys.path.append(mipyfiveCoreDir)
from alu import *

useVcd = False
def test_runner(in1, in2, aluOp):
    def test(self):
        sim = Simulator(self.dut)
        def process():
            yield self.dut.in1.eq(in1)
            yield self.dut.in2.eq(in2)
            if aluOp is AluOp.ADD:
                yield self.dut.aluOp.eq(0)
                yield Delay(1e-6)
                self.assertEqual((yield self.dut.out), in1 + in2)
            if aluOp is AluOp.SUB:
                yield self.dut.aluOp.eq(1)
                yield Delay(1e-6)
                self.assertEqual((yield self.dut.out), (in1 - in2) & 0xffffffff)
            if aluOp is AluOp.AND:
                yield self.dut.aluOp.eq(2)
                yield Delay(1e-6)
                self.assertEqual((yield self.dut.out), in1 & in2)
        
        sim.add_process(process)
        global useVcd
        if useVcd:
            outputDir = os.path.join(mipyfiveRootDir, "out", "vcd")
            if not os.path.exists(outputDir):
                os.makedirs(outputDir)
            with sim.write_vcd(vcd_file=os.path.join(outputDir, f"test_{aluOp}.vcd")):
                sim.run()
        else:
            sim.run()
    return test

# Define unit tests object for ALU
class TestAlu(unittest.TestCase):
    def setUp(self):
        self.dut = ALU(width=32)

    int1 = random.randint(0, 2147483647)
    int2 = random.randint(0, 2147483647)

    # Unit tests
    test_alu_addition = test_runner(int1, int2, AluOp.ADD)
    test_alu_subtraction = test_runner(int1, int2, AluOp.SUB)
    test_alu_and = test_runner(int1, int2, AluOp.AND)
    test_alu_or = test_runner(int1, int2, AluOp.OR)
    test_alu_xor = test_runner(int1, int2, AluOp.XOR)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        useVcd = True
    
    unittest.main(verbosity=2)
