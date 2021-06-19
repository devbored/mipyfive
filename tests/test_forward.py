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

def expectedHazardResolution(dutInputs):
    fwdAluA = AluForwardCtrl.NO_FWD
    fwdAluB = AluForwardCtrl.NO_FWD
    
    # MEM/WB Hazard detected
    if (dutInputs['MEM_WB_reg_write'] is 1 and (dutInputs['EX_MEM_rd'] != dutInputs['EX_MEM_rd']) 
        and dutInputs['MEM_WB_rd'] is not 0):
            if dutInputs['MEM_WB_rd'] == dutInputs['ID_EX_rs1']:
                fwdAluA = AluForwardCtrl.MEM_WB
            if dutInputs['MEM_WB_rd'] == dutInputs['ID_EX_rs2']:
                fwdAluB = AluForwardCtrl.MEM_WB
            return fwdAluA.value, fwdAluB.value

    # EX/MEM Hazard detected
    if dutInputs['EX_MEM_reg_write'] is 1 and dutInputs['MEM_WB_rd'] is not 0:
            if dutInputs['MEM_WB_rd'] == dutInputs['ID_EX_rs1']:
                fwdAluA = AluForwardCtrl.EX_MEM
            if dutInputs['MEM_WB_rd'] == dutInputs['ID_EX_rs2']:
                fwdAluB = AluForwardCtrl.EX_MEM
            return fwdAluA.value, fwdAluB.value
    
    return fwdAluA.value, fwdAluB.value

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_forward(dutInputs):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.ID_EX_rs1.eq(dutInputs['ID_EX_rs1'])
            yield self.dut.ID_EX_rs2.eq(dutInputs['ID_EX_rs2'])
            yield self.dut.EX_MEM_rd.eq(dutInputs['EX_MEM_rd'])
            yield self.dut.MEM_WB_rd.eq(dutInputs['MEM_WB_rd'])
            yield self.dut.EX_MEM_reg_write.eq(dutInputs['EX_MEM_reg_write'])
            yield self.dut.MEM_WB_reg_write.eq(dutInputs['MEM_WB_reg_write'])
            yield Delay(1e-6)

            expectedAluACtrl, expectedAluBCtrl = expectedHazardResolution(dutInputs)
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
class TestImmgen(unittest.TestCase):
    def setUp(self):
        self.dut = ForwardingUnit(width=32, regCount=32)

    # Start with a random test
    dutInputs = {
        'ID_EX_rs1'         : random.randint(1, 32),
        'ID_EX_rs2'         : random.randint(1, 32),
        'EX_MEM_rd'         : random.randint(1, 32),
        'MEM_WB_rd'         : random.randint(1, 32),
        'EX_MEM_reg_write'  : random.randint(0, 1),
        'MEM_WB_reg_write'  : random.randint(0, 1)
    }
    test_fwd_random = test_forward(dutInputs)

    # EX/MEM Hazard tests
    dutInputs['ID_EX_rs1'] = 5
    dutInputs['EX_MEM_rd'] = 5
    dutInputs['EX_MEM_reg_write'] = 1
    dutInputs['MEM_WB_reg_write'] = 1
    test_fwd_EX_MEM_rd_hazard_rs1 = test_forward(dutInputs)
    dutInputs['ID_EX_rs1'] = 3
    dutInputs['ID_EX_rs2'] = 5
    test_fwd_EX_MEM_rd_hazard_rs2 = test_forward(dutInputs)
    dutInputs['ID_EX_rs1'] = 5
    test_fwd_EX_MEM_rd_hazard_both = test_forward(dutInputs)

    # MEM/WB Hazard tests
    dutInputs['MEM_WB_rd'] = 5
    dutInputs['EX_MEM_rd'] = 4
    dutInputs['ID_EX_rs1'] = 5
    test_fwd_MEM_WB_rd_hazard_rs1 = test_forward(dutInputs)
    dutInputs['ID_EX_rs1'] = 4
    dutInputs['ID_EX_rs2'] = 5
    test_fwd_MEM_WB_rd_hazard_rs2 = test_forward(dutInputs)
    dutInputs['ID_EX_rs1'] = 5
    test_fwd_MEM_WB_rd_hazard_both = test_forward(dutInputs)

    # Test when reg write lines are disabled
    dutInputs['EX_MEM_reg_write'] = 0
    test_fwd_EX_MEM_reg_write_off = test_forward(dutInputs)
    dutInputs['EX_MEM_reg_write'] = 1
    dutInputs['MEM_WB_reg_write'] = 0
    test_fwd_MEM_WB_reg_write_off = test_forward(dutInputs)
    dutInputs['EX_MEM_reg_write'] = 0
    test_fwd_both_reg_write_off = test_forward(dutInputs)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True
    
    unittest.main(verbosity=2)
