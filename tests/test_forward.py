import os
import sys
import random
import argparse
import unittest
from amaranth import *
from amaranth.sim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from mipyfive.types import *
from mipyfive.forward import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "build", "vcd"))
def test_forward(
    IF_ID_rs1, ID_EX_rs1, IF_ID_rs2, ID_EX_rs2, EX_MEM_rd, MEM_WB_rd, WB_BUFFER_rd,
        EX_MEM_reg_write, MEM_WB_reg_write, WB_BUFFER_reg_write, expectedAluACtrl, expectedAluBCtrl
    ):
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
            yield self.dut.WB_BUFFER_rd.eq(WB_BUFFER_rd)
            yield self.dut.EX_MEM_reg_write.eq(EX_MEM_reg_write)
            yield self.dut.MEM_WB_reg_write.eq(MEM_WB_reg_write)
            yield self.dut.WB_BUFFER_reg_write.eq(WB_BUFFER_reg_write)
            yield Delay(1e-6)

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
        self.dut = ForwardingUnit(regCount=32)

    # Test EX/MEM Data Hazards
    test_fwd_EX_MEM_hazard_rs1 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=2, IF_ID_rs2=0, ID_EX_rs2=5, EX_MEM_rd=2, MEM_WB_rd=4, WB_BUFFER_rd=0,
            WB_BUFFER_reg_write=1, EX_MEM_reg_write=1, MEM_WB_reg_write=1,
                expectedAluACtrl=AluForwardCtrl.EX_MEM.value, expectedAluBCtrl=AluForwardCtrl.NO_FWD.value
    )
    test_fwd_EX_MEM_hazard_rs2 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=1, IF_ID_rs2=0, ID_EX_rs2=6, EX_MEM_rd=6, MEM_WB_rd=6, WB_BUFFER_rd=0,
            WB_BUFFER_reg_write=1, EX_MEM_reg_write=1, MEM_WB_reg_write=1,
                expectedAluACtrl=AluForwardCtrl.NO_FWD.value, expectedAluBCtrl=AluForwardCtrl.EX_MEM.value
    )
    test_fwd_EX_MEM_hazard_both = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=7, IF_ID_rs2=0, ID_EX_rs2=7, EX_MEM_rd=7, MEM_WB_rd=6, WB_BUFFER_rd=0,
            WB_BUFFER_reg_write=1, EX_MEM_reg_write=1, MEM_WB_reg_write=1,
                expectedAluACtrl=AluForwardCtrl.EX_MEM.value, expectedAluBCtrl=AluForwardCtrl.EX_MEM.value
    )

    # Test MEM/WB Data Hazards
    test_fwd_MEM_WB_hazard_rs1 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=10, IF_ID_rs2=0, ID_EX_rs2=2, EX_MEM_rd=3, MEM_WB_rd=10, WB_BUFFER_rd=0,
            WB_BUFFER_reg_write=1, EX_MEM_reg_write=1, MEM_WB_reg_write=1,
                expectedAluACtrl=AluForwardCtrl.MEM_WB.value, expectedAluBCtrl=AluForwardCtrl.NO_FWD.value
    )
    test_fwd_MEM_WB_hazard_rs2 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=1, IF_ID_rs2=0, ID_EX_rs2=6, EX_MEM_rd=3, MEM_WB_rd=6, WB_BUFFER_rd=0,
            WB_BUFFER_reg_write=1, EX_MEM_reg_write=1, MEM_WB_reg_write=1,
                expectedAluACtrl=AluForwardCtrl.NO_FWD.value, expectedAluBCtrl=AluForwardCtrl.MEM_WB.value
    )
    test_fwd_MEM_WB_hazard_both = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=8, IF_ID_rs2=0, ID_EX_rs2=8, EX_MEM_rd=17, MEM_WB_rd=8, WB_BUFFER_rd=0,
            WB_BUFFER_reg_write=1, EX_MEM_reg_write=1, MEM_WB_reg_write=1,
                expectedAluACtrl=AluForwardCtrl.MEM_WB.value, expectedAluBCtrl=AluForwardCtrl.MEM_WB.value
    )

    # Test WB buffer Data Hazards
    test_fwd_WB_BUFFER_hazard_rs1 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=10, IF_ID_rs2=0, ID_EX_rs2=2, EX_MEM_rd=3, MEM_WB_rd=1, WB_BUFFER_rd=10,
            WB_BUFFER_reg_write=1, EX_MEM_reg_write=1, MEM_WB_reg_write=1,
                expectedAluACtrl=AluForwardCtrl.WB_BUFFER.value, expectedAluBCtrl=AluForwardCtrl.NO_FWD.value
    )
    test_fwd_WB_BUFFER_hazard_rs2 = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=1, IF_ID_rs2=0, ID_EX_rs2=6, EX_MEM_rd=3, MEM_WB_rd=16, WB_BUFFER_rd=6,
            WB_BUFFER_reg_write=1, EX_MEM_reg_write=1, MEM_WB_reg_write=1,
                expectedAluACtrl=AluForwardCtrl.NO_FWD.value, expectedAluBCtrl=AluForwardCtrl.WB_BUFFER.value
    )
    test_fwd_WB_BUFFER_hazard_both = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=8, IF_ID_rs2=0, ID_EX_rs2=8, EX_MEM_rd=17, MEM_WB_rd=18, WB_BUFFER_rd=8,
            WB_BUFFER_reg_write=1, EX_MEM_reg_write=1, MEM_WB_reg_write=1,
                expectedAluACtrl=AluForwardCtrl.WB_BUFFER.value, expectedAluBCtrl=AluForwardCtrl.WB_BUFFER.value
    )

    # Check non-data-hazard cases
    test_fwd_no_hazard = test_forward(IF_ID_rs1=1, ID_EX_rs1=2, IF_ID_rs2=3, ID_EX_rs2=4, EX_MEM_rd=6, WB_BUFFER_rd=0,
        WB_BUFFER_reg_write=1, MEM_WB_rd=10, EX_MEM_reg_write=1, MEM_WB_reg_write=1,
            expectedAluACtrl=AluForwardCtrl.NO_FWD.value, expectedAluBCtrl=AluForwardCtrl.NO_FWD.value
    )
    test_fwd_no_write_no_hazard = test_forward(
        IF_ID_rs1=0, ID_EX_rs1=0, IF_ID_rs2=0, ID_EX_rs2=0, EX_MEM_rd=11, MEM_WB_rd=11, WB_BUFFER_rd=0,
            WB_BUFFER_reg_write=1, EX_MEM_reg_write=0, MEM_WB_reg_write=0,
                expectedAluACtrl=AluForwardCtrl.NO_FWD.value, expectedAluBCtrl=AluForwardCtrl.NO_FWD.value
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
