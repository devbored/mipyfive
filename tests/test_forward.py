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

def expectedHazardResolution(IF_ID_rs1, ID_EX_rs1, IF_ID_rs2, ID_EX_rs2, EX_MEM_rd, MEM_WB_rd,
    EX_MEM_reg_write, MEM_WB_reg_write):

    fwdAluA = AluForwardCtrl.NO_FWD
    fwdAluB = AluForwardCtrl.NO_FWD
    fwdRegfileAout = RegfileOutForwardCtrl.NO_FWD
    fwdRegfileBout = RegfileOutForwardCtrl.NO_FWD

    # Forward conditions for output A of Regfile
    if((IF_ID_rs1 != 0) & (IF_ID_rs1 == EX_MEM_rd) & (EX_MEM_reg_write)):
        fwdRegfileAout = RegfileOutForwardCtrl.EX_MEM
    else:
        fwdRegfileAout = RegfileOutForwardCtrl.NO_FWD

    # Forward conditions for output B of Regfile
    if((IF_ID_rs2 != 0) & (IF_ID_rs2 == EX_MEM_rd) & (EX_MEM_reg_write)):
        fwdRegfileBout = RegfileOutForwardCtrl.EX_MEM
    else:
        fwdRegfileBout = RegfileOutForwardCtrl.NO_FWD

    # Forward conditions for input A of ALU
    if((EX_MEM_reg_write) & (EX_MEM_rd != 0) & (EX_MEM_rd == ID_EX_rs1)):
        fwdAluA = AluForwardCtrl.EX_MEM
    elif((MEM_WB_reg_write) & (MEM_WB_rd != 0) &
        ~((EX_MEM_reg_write) & (EX_MEM_rd != 0) & (EX_MEM_rd == ID_EX_rs1)) &
            (MEM_WB_rd == ID_EX_rs1)):
                fwdAluA = AluForwardCtrl.MEM_WB
    else:
        fwdAluA = AluForwardCtrl.NO_FWD

    # Forward conditions for input B of ALU
    if((EX_MEM_reg_write) & (EX_MEM_rd != 0) & (EX_MEM_rd == ID_EX_rs2)):
        fwdAluB = AluForwardCtrl.EX_MEM
    elif((MEM_WB_reg_write) & (MEM_WB_rd != 0) &
        ~((EX_MEM_reg_write) & (EX_MEM_rd != 0) & (EX_MEM_rd == ID_EX_rs2)) &
            (MEM_WB_rd == ID_EX_rs2)):
                fwdAluB = AluForwardCtrl.MEM_WB
    else:
            fwdAluB = AluForwardCtrl.NO_FWD

    return fwdAluA.value, fwdAluB.value, fwdRegfileAout.value, fwdRegfileBout.value

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_forward(IF_ID_rs1, ID_EX_rs1, IF_ID_rs2, ID_EX_rs2, EX_MEM_rd, MEM_WB_rd,
    EX_MEM_reg_write, MEM_WB_reg_write):

    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.IF_ID_rs1.eq(IF_ID_rs1)
            yield self.dut.ID_EX_rs1.eq(ID_EX_rs1)
            yield self.dut.IF_ID_rs2.eq(IF_ID_rs2)
            yield self.dut.ID_EX_rs2.eq(ID_EX_rs2)
            yield self.dut.EX_MEM_rd.eq(EX_MEM_rd)
            yield self.dut.MEM_WB_rd.eq(MEM_WB_rd)
            yield self.dut.EX_MEM_reg_write.eq(EX_MEM_reg_write)
            yield self.dut.MEM_WB_reg_write.eq(MEM_WB_reg_write)
            yield Delay(1e-6)

            expectedAluACtrl, expectedAluBCtrl, expectedRegfileAout, expectedRegfileBout = expectedHazardResolution(
                IF_ID_rs1, ID_EX_rs1, IF_ID_rs2, ID_EX_rs2, EX_MEM_rd, MEM_WB_rd, EX_MEM_reg_write, MEM_WB_reg_write
            )
            self.assertEqual((yield self.dut.fwdAluA), expectedAluACtrl)
            self.assertEqual((yield self.dut.fwdAluB), expectedAluBCtrl)
            self.assertEqual((yield self.dut.fwdRegfileAout), expectedRegfileAout)
            self.assertEqual((yield self.dut.fwdRegfileBout), expectedRegfileBout)
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
        self.dut = ForwardingUnit(regCount=32)

    # Start with a random test
    test_fwd_random = test_forward(
        IF_ID_rs1=random.randint(1,32),
        ID_EX_rs1=random.randint(1,32),
        IF_ID_rs2=random.randint(1,32),
        ID_EX_rs2=random.randint(1,32),
        EX_MEM_rd=random.randint(1,32),
        MEM_WB_rd=random.randint(1,32),
        EX_MEM_reg_write=1,
        MEM_WB_reg_write=1
    )

    # Test Control Hazards
    test_fwd_ctrl_hazard_rs1 = test_forward(
        IF_ID_rs1=13, ID_EX_rs1=0, IF_ID_rs2=0, ID_EX_rs2=0, EX_MEM_rd=13, MEM_WB_rd=4,
            EX_MEM_reg_write=1, MEM_WB_reg_write=0
    )
    test_fwd_ctrl_hazard_rs2 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=0, IF_ID_rs2=16, ID_EX_rs2=0, EX_MEM_rd=16, MEM_WB_rd=4,
            EX_MEM_reg_write=1, MEM_WB_reg_write=0
    )

    # Test EX/MEM Data Hazards
    test_fwd_EX_MEM_hazard_rs1 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=2, IF_ID_rs2=0, ID_EX_rs2=2, EX_MEM_rd=2, MEM_WB_rd=4,
            EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )
    test_fwd_EX_MEM_hazard_rs2 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=1, IF_ID_rs2=0, ID_EX_rs2=6, EX_MEM_rd=6, MEM_WB_rd=6,
            EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )
    test_fwd_EX_MEM_hazard_both = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=7, IF_ID_rs2=0, ID_EX_rs2=7, EX_MEM_rd=7, MEM_WB_rd=6,
            EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )

    # Test MEM/WB Data Hazards
    test_fwd_MEM_WB_hazard_rs1 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=10, IF_ID_rs2=0, ID_EX_rs2=2, EX_MEM_rd=3, MEM_WB_rd=10,
            EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )
    test_fwd_MEM_WB_hazard_rs2 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=1, IF_ID_rs2=0, ID_EX_rs2=6, EX_MEM_rd=3, MEM_WB_rd=6,
            EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )
    test_fwd_MEM_WB_hazard_both = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=8, IF_ID_rs2=0, ID_EX_rs2=8, EX_MEM_rd=17, MEM_WB_rd=8,
            EX_MEM_reg_write=1, MEM_WB_reg_write=1
    )

    # Check non-data-hazard cases
    test_fwd_no_hazard = test_forward(IF_ID_rs1=1, ID_EX_rs1=2, IF_ID_rs2=3, ID_EX_rs2=4, EX_MEM_rd=6,
        MEM_WB_rd=10, EX_MEM_reg_write=1, MEM_WB_reg_write=1)
    test_fwd_no_write_no_hazard = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=0, IF_ID_rs2=0, ID_EX_rs2=0, EX_MEM_rd=11, MEM_WB_rd=11,
            EX_MEM_reg_write=0, MEM_WB_reg_write=0
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
