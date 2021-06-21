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
from mipyfive.forward import *

def expectedHazardResolution(rs1, rs2, EX_MEM_rd, MEM_WB_rd, EX_MEM_reg_write, MEM_WB_reg_write):
    fwdAluA = AluForwardCtrl.NO_FWD
    fwdAluB = AluForwardCtrl.NO_FWD

    # Forward conditions for input A of ALU
    if((EX_MEM_reg_write) & (EX_MEM_rd != 0) & (EX_MEM_rd == rs1)):
        fwdAluA = AluForwardCtrl.EX_MEM
    elif((MEM_WB_reg_write) & (MEM_WB_rd != 0) &
        ~((EX_MEM_reg_write) & (EX_MEM_rd != 0) & (EX_MEM_rd == rs1)) &
            (MEM_WB_rd == rs1)):
                fwdAluA = AluForwardCtrl.MEM_WB
    else:
        fwdAluA = AluForwardCtrl.NO_FWD

    # Forward conditions for input B of ALU
    if((EX_MEM_reg_write) & (EX_MEM_rd != 0) & (EX_MEM_rd == rs2)):
        fwdAluB = AluForwardCtrl.EX_MEM
    elif((MEM_WB_reg_write) & (MEM_WB_rd != 0) &
        ~((EX_MEM_reg_write) & (EX_MEM_rd != 0) & (EX_MEM_rd == rs2)) &
            (MEM_WB_rd == rs1)):
                fwdAluB = AluForwardCtrl.MEM_WB
    else:
            fwdAluB = AluForwardCtrl.NO_FWD
    
    return fwdAluA.value, fwdAluB.value

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_forward(rs1, rs2, EX_MEM_rd, MEM_WB_rd, EX_MEM_reg_write, MEM_WB_reg_write):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.ID_EX_rs1.eq(rs1)
            yield self.dut.ID_EX_rs2.eq(rs2)
            yield self.dut.EX_MEM_rd.eq(EX_MEM_rd)
            yield self.dut.MEM_WB_rd.eq(MEM_WB_rd)
            yield self.dut.EX_MEM_reg_write.eq(EX_MEM_reg_write)
            yield self.dut.MEM_WB_reg_write.eq(MEM_WB_reg_write)
            yield Delay(1e-6)

            expectedAluACtrl, expectedAluBCtrl = expectedHazardResolution(
                rs1, rs2, EX_MEM_rd, MEM_WB_rd, EX_MEM_reg_write, MEM_WB_reg_write
            )
            self.assertEqual((yield self.dut.fwdAluA), expectedAluACtrl)
            self.assertEqual((yield self.dut.fwdAluB), expectedAluBCtrl)
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
        self.dut = ForwardingUnit(width=32, regCount=32)

    # Start with a random test
    test_fwd_random = test_forward(
        rs1=random.randint(1,32),
        rs2=random.randint(1,32),
        EX_MEM_rd=random.randint(1,32),
        MEM_WB_rd=random.randint(1,32),
        EX_MEM_reg_write=1,
        MEM_WB_reg_write=1
    )

    # Test EX/MEM Data Hazards
    test_fwd_EX_MEM_hazard_rs1 = test_forward(
        rs1=2, rs2=0, EX_MEM_rd=2, MEM_WB_rd=4, EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )
    test_fwd_EX_MEM_hazard_rs2 = test_forward(
        rs1=1, rs2=6, EX_MEM_rd=6, MEM_WB_rd=6, EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )
    test_fwd_EX_MEM_hazard_both = test_forward(
        rs1=7, rs2=7, EX_MEM_rd=7, MEM_WB_rd=6, EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )

    # Test MEM/WB Data Hazards
    test_fwd_MEM_WB_hazard_rs1 = test_forward(
        rs1=10, rs2=2, EX_MEM_rd=3, MEM_WB_rd=10, EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )
    test_fwd_MEM_WB_hazard_rs2 = test_forward(
        rs1=1, rs2=6, EX_MEM_rd=3, MEM_WB_rd=6, EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )
    test_fwd_MEM_WB_hazard_both = test_forward(
        rs1=8, rs2=8, EX_MEM_rd=17, MEM_WB_rd=8, EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )

    # Check non-data-hazard cases
    test_fwd_no_hazard = test_forward(rs1=3, rs2=4, EX_MEM_rd=6, MEM_WB_rd=10, EX_MEM_reg_write=1, MEM_WB_reg_write=1)
    test_fwd_no_write_no_hazard = test_forward(
        rs1=11, rs2=11, EX_MEM_rd=11, MEM_WB_rd=11, EX_MEM_reg_write=0, MEM_WB_reg_write=0
    )

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True
    
    unittest.main(verbosity=2)
