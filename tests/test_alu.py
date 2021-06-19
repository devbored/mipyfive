import os
import sys
import random
import argparse
import unittest
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from mipyfive.alu import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_runner(in1, in2, aluOp):
    def test(self):
        global createVcd
        global outputDir
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
            if aluOp is AluOp.OR:
                yield self.dut.aluOp.eq(3)
                yield Delay(1e-6)
                self.assertEqual((yield self.dut.out), in1 | in2)
            if aluOp is AluOp.XOR:
                yield self.dut.aluOp.eq(4)
                yield Delay(1e-6)
                self.assertEqual((yield self.dut.out), in1 ^ in2)
            if aluOp is AluOp.SLT:
                yield self.dut.aluOp.eq(5)
                yield Delay(1e-6)
                in1_twos = in1
                in2_twos = in2
                if (in1 & (1<<31)):
                    in1_twos = (in1 - (1 << 32))
                if (in2 & (1<<31)):
                    in2_twos = (in2 - (1 << 32))
                if in1_twos < in2_twos:
                    self.assertEqual((yield self.dut.out), 1)
                else:
                    self.assertEqual((yield self.dut.out), 0)
            if aluOp is AluOp.SLTU:
                yield self.dut.aluOp.eq(6)
                yield Delay(1e-6)
                if in1 < in2:
                    self.assertEqual((yield self.dut.out), 1)
                else:
                    self.assertEqual((yield self.dut.out), 0)
            if aluOp is AluOp.SLL:
                yield self.dut.aluOp.eq(7)
                yield Delay(1e-6)
                self.assertEqual((yield self.dut.out), (in1 << in2)%(2**32))
            if aluOp is AluOp.SRL:
                yield self.dut.aluOp.eq(8)
                yield Delay(1e-6)
                self.assertEqual((yield self.dut.out), in1 >> in2)
            if aluOp is AluOp.SRA:
                yield self.dut.aluOp.eq(9)
                yield Delay(1e-6)
                if (in1 & (1<<31)):
                    self.assertEqual((yield self.dut.out), ((in1 - (1 << 32)) >> in2) & 0xffffffff)
                else:
                    self.assertEqual((yield self.dut.out), in1 >> in2)

            # Test ALU flag output(s)
            if (yield self.dut.out) == 0:
                self.assertEqual((yield self.dut.zflag), 1)
        
        sim.add_process(process)
        if createVcd:
            if not os.path.exists(outputDir):
                os.makedirs(outputDir)
            with sim.write_vcd(vcd_file=os.path.join(outputDir, f"{self._testMethodName}.vcd")):
                sim.run()
        else:
            sim.run()
    return test

# Define unit tests
class TestAlu(unittest.TestCase):
    def setUp(self):
        self.dut = ALU(width=32)

    int1 = random.randint(0, 2147483647)
    int2 = random.randint(0, 2147483647)
    test_alu_addition       = test_runner(int1, int2, AluOp.ADD)
    test_alu_subtraction    = test_runner(int1, int2, AluOp.SUB)
    test_alu_and            = test_runner(int1, int2, AluOp.AND)
    test_alu_or             = test_runner(int1, int2, AluOp.OR)
    test_alu_xor            = test_runner(int1, int2, AluOp.XOR)
    test_alu_slt            = test_runner(int1, int2, AluOp.SLT)
    test_alu_sltu           = test_runner(int1, int2, AluOp.SLTU)

    int1 = random.randint(0, 4294967295)
    int2 = random.randint(0, 31)
    test_alu_sll            = test_runner(int1, int2, AluOp.SLL)
    test_alu_srl            = test_runner(int1, int2, AluOp.SRL)
    test_alu_sra            = test_runner(int1, int2, AluOp.SRA)

parser = argparse.ArgumentParser()
parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
args, argv = parser.parse_known_args()
sys.argv[1:] = argv
if args.vcd is True:
    print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
    createVcd = True

unittest.main(verbosity=2)
