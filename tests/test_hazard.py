import os
import sys
import random
import argparse
import unittest
from enum import Enum
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from mipyfive.types import *
from mipyfive.hazard import *

def expectedHazardResolution(IF_ID_rs1, IF_ID_rs2, ID_EX_memRead, ID_EX_rd):
    PCwrite     = 1
    IF_ID_write = 1
    ctrlBubble  = 0

    if ID_EX_memRead and ((ID_EX_rd == IF_ID_rs1) or (ID_EX_rd == IF_ID_rs2)):
        PCwrite     = 0
        IF_ID_write = 0
        ctrlBubble  = 1
    
    return PCwrite, IF_ID_write, ctrlBubble

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_hazard(IF_ID_rs1, IF_ID_rs2, ID_EX_memRead, ID_EX_rd):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.ID_EX_memRead.eq(ID_EX_memRead)
            yield self.dut.ID_EX_rd.eq(ID_EX_rd)
            yield self.dut.IF_ID_rs1.eq(IF_ID_rs1)
            yield self.dut.IF_ID_rs2.eq(IF_ID_rs2)
            yield Delay(1e-6)

            PCwrite, IF_ID_write, ctrlBubble = expectedHazardResolution(
                IF_ID_rs1, IF_ID_rs2, ID_EX_memRead, ID_EX_rd
            )
            self.assertEqual((yield self.dut.PCwrite), PCwrite)
            self.assertEqual((yield self.dut.IF_ID_write), IF_ID_write)
            self.assertEqual((yield self.dut.ctrlBubble), ctrlBubble)
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
class TestForward(unittest.TestCase):
    def setUp(self):
        self.dut = HazardUnit(regCount=32)

    IF_ID_rs1       = random.randint(0, 31)
    IF_ID_rs2       = random.randint(0, 31)
    ID_EX_rd        = random.randint(0, 31)
    ID_EX_memRead   = random.randint(0, 1)
    test_hazard_random = test_hazard(IF_ID_rs1, IF_ID_rs2, ID_EX_memRead, ID_EX_rd)
    
    test_non_hazard = test_hazard(IF_ID_rs1=4, IF_ID_rs2=3, ID_EX_memRead=1, ID_EX_rd=12)
    test_hazard = test_hazard(IF_ID_rs1=4, IF_ID_rs2=5, ID_EX_memRead=1, ID_EX_rd=5)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True
    
    unittest.main(verbosity=2)
