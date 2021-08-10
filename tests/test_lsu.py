import os
import sys
import argparse
import unittest
from enum import Enum
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from mipyfive.types import *
from mipyfive.lsu import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_load(lDin, lCtrl, lDoutExpected):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.lDataIn.eq(lDin)
            yield self.dut.lCtrlIn.eq(lCtrl)
            yield self.dut.sDataIn.eq(0)
            yield self.dut.sCtrlIn.eq(0)
            yield Delay(1e-6)

            self.assertEqual((yield self.dut.lDataOut), lDoutExpected)
        sim.add_process(process)
        if createVcd:
            if not os.path.exists(outputDir):
                os.makedirs(outputDir)
            with sim.write_vcd(vcd_file=os.path.join(outputDir, f"{self._testMethodName}.vcd")):
                sim.run()
        else:
            sim.run()
    return test

def test_store(sDin, sCtrl, sDoutExpected):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.lDataIn.eq(0)
            yield self.dut.lCtrlIn.eq(0)
            yield self.dut.sDataIn.eq(sDin)
            yield self.dut.sCtrlIn.eq(sCtrl)
            yield Delay(1e-6)

            self.assertEqual((yield self.dut.sDataOut), sDoutExpected)
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
class TestLSU(unittest.TestCase):
    def setUp(self):
        self.dut = LSU(width=32)

    test_lb  = test_load(0x1234abcd, LSULoadCtrl.LSU_LB.value,  0xffffffcd)
    test_lh  = test_load(0x1234abcd, LSULoadCtrl.LSU_LH.value,  0xffffabcd)
    test_lw  = test_load(0x1234abcd, LSULoadCtrl.LSU_LW.value,  0x1234abcd)
    test_lbu = test_load(0x1234abcd, LSULoadCtrl.LSU_LBU.value, 0x000000cd)
    test_lhu = test_load(0x1234abcd, LSULoadCtrl.LSU_LHU.value, 0x0000abcd)

    test_sb = test_store(0x1234abcd, LSUStoreCtrl.LSU_SB.value, 0x000000cd)
    test_sh = test_store(0x1234abcd, LSUStoreCtrl.LSU_SH.value, 0x0000abcd)
    test_sw = test_store(0x1234abcd, LSUStoreCtrl.LSU_SW.value, 0x1234abcd)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[mipyfive - Info]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True

    unittest.main(verbosity=2)
