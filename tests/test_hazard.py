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

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_hazard(
    Jump, Branch, BranchMispredict, IF_valid, MEM_valid, IF_ID_rs1, IF_ID_rs2, ID_EX_memRead, ID_EX_rd,
        IF_stall, IF_ID_stall, ID_EX_stall, EX_MEM_stall, MEM_WB_stall, IF_ID_flush, ID_EX_flush, EX_MEM_flush
    ):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.Jump.eq(Jump)
            yield self.dut.Branch.eq(Branch)
            yield self.dut.BranchMispredict.eq(BranchMispredict)
            yield self.dut.IF_valid.eq(IF_valid)
            yield self.dut.MEM_valid.eq(MEM_valid)
            yield self.dut.IF_ID_rs1.eq(IF_ID_rs1)
            yield self.dut.IF_ID_rs2.eq(IF_ID_rs2)
            yield self.dut.ID_EX_memRead.eq(ID_EX_memRead)
            yield self.dut.ID_EX_rd.eq(ID_EX_rd)
            yield Delay(1e-6)

            self.assertEqual((yield self.dut.IF_stall), IF_stall)
            self.assertEqual((yield self.dut.IF_ID_stall), IF_ID_stall)
            self.assertEqual((yield self.dut.ID_EX_stall), ID_EX_stall)
            self.assertEqual((yield self.dut.EX_MEM_stall), EX_MEM_stall)
            self.assertEqual((yield self.dut.MEM_WB_stall), MEM_WB_stall)
            self.assertEqual((yield self.dut.IF_ID_flush), IF_ID_flush)
            self.assertEqual((yield self.dut.ID_EX_flush), ID_EX_flush)
            self.assertEqual((yield self.dut.EX_MEM_flush), EX_MEM_flush)
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
class TestHazard(unittest.TestCase):
    def setUp(self):
        self.dut = HazardUnit(regCount=32)

    test_branch_flush_hazard = test_hazard(
        Jump=0, Branch=1, BranchMispredict=1, IF_valid=1, MEM_valid=1, IF_ID_rs1=0, IF_ID_rs2=0, ID_EX_memRead=0,
            ID_EX_rd=0, IF_stall=0, IF_ID_stall=0, ID_EX_stall=0, EX_MEM_stall=0, MEM_WB_stall=0, IF_ID_flush=1,
                ID_EX_flush=1, EX_MEM_flush=1
    )
    test_jump_flush_hazard = test_hazard(
        Jump=1, Branch=0, BranchMispredict=0, IF_valid=1, MEM_valid=1, IF_ID_rs1=0, IF_ID_rs2=0, ID_EX_memRead=0,
            ID_EX_rd=0, IF_stall=0, IF_ID_stall=0, ID_EX_stall=0, EX_MEM_stall=0, MEM_WB_stall=0, IF_ID_flush=1,
                ID_EX_flush=1, EX_MEM_flush=1
    )
    test_load_stall_hazard = test_hazard(
        Jump=0, Branch=0, BranchMispredict=0, IF_valid=1, MEM_valid=1, IF_ID_rs1=5, IF_ID_rs2=0, ID_EX_memRead=1,
            ID_EX_rd=5, IF_stall=1, IF_ID_stall=1, ID_EX_stall=0, EX_MEM_stall=0, MEM_WB_stall=0, IF_ID_flush=0,
                ID_EX_flush=1, EX_MEM_flush=0
    )
    test_ifetch_stall_hazard = test_hazard(
        Jump=0, Branch=0, BranchMispredict=0, IF_valid=0, MEM_valid=1, IF_ID_rs1=0, IF_ID_rs2=0, ID_EX_memRead=0,
            ID_EX_rd=0, IF_stall=1, IF_ID_stall=1, ID_EX_stall=0, EX_MEM_stall=0, MEM_WB_stall=0, IF_ID_flush=0,
                ID_EX_flush=0, EX_MEM_flush=0
    )
    test_dfetch_stall_hazard = test_hazard(
        Jump=0, Branch=0, BranchMispredict=0, IF_valid=1, MEM_valid=0, IF_ID_rs1=0, IF_ID_rs2=0, ID_EX_memRead=0,
            ID_EX_rd=0, IF_stall=1, IF_ID_stall=1, ID_EX_stall=1, EX_MEM_stall=1, MEM_WB_stall=1, IF_ID_flush=0,
                ID_EX_flush=0, EX_MEM_flush=0
    )
    test_non_hazard = test_hazard(
        Jump=0, Branch=0, BranchMispredict=0, IF_valid=1, MEM_valid=1, IF_ID_rs1=0, IF_ID_rs2=0, ID_EX_memRead=0,
            ID_EX_rd=0, IF_stall=0, IF_ID_stall=0, ID_EX_stall=0, EX_MEM_stall=0, MEM_WB_stall=0, IF_ID_flush=0,
                ID_EX_flush=0, EX_MEM_flush=0
    )

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    parser.add_argument("-v", dest="verbosity", type=int, default=2, help="Verbosity level.")
    args, unknown = parser.parse_known_args()
    if unknown:
        print(f"Ignoring unknown args:\n{unknown}\n")
    sys.argv[1:] = [x for x in unknown if x not in sys.argv]
    if args.vcd is True:
        print(f"[mipyfive - Info]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True

    unittest.main(verbosity=args.verbosity)
