import os
import sys
import random
import argparse
import unittest
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from tests.utils import *
from mipyfive.utils import *
from mipyfive.controller import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_controller(
    instruction, aluOp, regWrite, memWrite, memRead, mem2Reg, aluAsrc,
        aluBsrc, branch, lsuLoadCtrl, lsuStoreCtrl, jump, jumpR
    ):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.instruction.eq(instruction)
            yield Delay(1e-6)

            self.assertEqual((yield self.dut.aluOp), aluOp)
            self.assertEqual((yield self.dut.regWrite), regWrite)
            self.assertEqual((yield self.dut.memWrite), memWrite)
            self.assertEqual((yield self.dut.memRead), memRead)
            self.assertEqual((yield self.dut.mem2Reg), mem2Reg)
            self.assertEqual((yield self.dut.aluAsrc), aluAsrc)
            self.assertEqual((yield self.dut.aluBsrc), aluBsrc)
            self.assertEqual((yield self.dut.lsuLoadCtrl), lsuLoadCtrl)
            self.assertEqual((yield self.dut.lsuStoreCtrl), lsuStoreCtrl)
            self.assertEqual((yield self.dut.branch), branch)
            self.assertEqual((yield self.dut.jump), jump)
            self.assertEqual((yield self.dut.jumpR), jumpR)
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
class TestController(unittest.TestCase):
    def setUp(self):
        self.dut = Controller()

    # TODO: Make reg values also randomized
    randImm12 = random.randint(-2048, 2047)
    randImm20 = random.randint(-524288, 524287)

    ## Upper-immediate instruction tests
    test_ctrl_lui = test_controller(asm2binU("lui", "x6", str(randImm20)),
        AluOp.PASS_B.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_PC.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_auipc = test_controller(asm2binU("auipc", "x1", str(randImm20)),
        AluOp.ADD.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_PC.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)

    ## Jump instruction tests
    test_ctrl_jal = test_controller(asm2binJ("jal", "x4", str(randImm20)),
        AluOp.ADD4_A.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_PC.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 1, 0)
    test_ctrl_jalr = test_controller(asm2binI("jalr", "x4", "x10", str(randImm12)),
        AluOp.ADD4_A.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_PC.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 1)

    ## Branch instruction tests
    test_ctrl_beq = test_controller(asm2binB("beq", "x23", str(randImm12), "x11"),
        AluOp.EQUAL.value, 0, 0, 0, Mem2RegCtrl.FROM_ALU.value,
            AluASrcCtrl.FROM_RS1.value, AluBSrcCtrl.FROM_RS2.value, 1,
                LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_bne = test_controller(asm2binB("bne", "x12", str(randImm12), "x1"),
        AluOp.NEQUAL.value, 0, 0, 0, Mem2RegCtrl.FROM_ALU.value,
            AluASrcCtrl.FROM_RS1.value, AluBSrcCtrl.FROM_RS2.value, 1,
                LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_blt = test_controller(asm2binB("blt", "x14", str(randImm12), "x21"),
        AluOp.SLT.value, 0, 0, 0, Mem2RegCtrl.FROM_ALU.value,
            AluASrcCtrl.FROM_RS1.value, AluBSrcCtrl.FROM_RS2.value, 1,
                LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_bge = test_controller(asm2binB("bge", "x3", str(randImm12), "x30"),
        AluOp.SGTE.value, 0, 0, 0, Mem2RegCtrl.FROM_ALU.value,
            AluASrcCtrl.FROM_RS1.value, AluBSrcCtrl.FROM_RS2.value, 1,
                LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_bltu = test_controller(asm2binB("bltu", "x9", str(randImm12), "x4"),
        AluOp.SLTU.value, 0, 0, 0, Mem2RegCtrl.FROM_ALU.value,
            AluASrcCtrl.FROM_RS1.value, AluBSrcCtrl.FROM_RS2.value, 1,
                LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_bgeu = test_controller(asm2binB("bgeu", "x0", str(randImm12), "x0"),
        AluOp.SGTEU.value, 0, 0, 0, Mem2RegCtrl.FROM_ALU.value,
            AluASrcCtrl.FROM_RS1.value, AluBSrcCtrl.FROM_RS2.value, 1,
                LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)

    ## Load instruction tests
    test_ctrl_lb = test_controller(asm2binI("lb", "x2", "x22", str(randImm12)),
        AluOp.ADD.value, 1, 0, 1, Mem2RegCtrl.FROM_MEM.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LB.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_lh = test_controller(asm2binI("lh", "x1", "x30", str(randImm12)),
        AluOp.ADD.value, 1, 0, 1, Mem2RegCtrl.FROM_MEM.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LH.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_lw = test_controller(asm2binI("lw", "x3", "x24", str(randImm12)),
        AluOp.ADD.value, 1, 0, 1, Mem2RegCtrl.FROM_MEM.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_lbu = test_controller(asm2binI("lbu", "x14", "x2", str(randImm12)),
        AluOp.ADD.value, 1, 0, 1, Mem2RegCtrl.FROM_MEM.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LBU.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_lhu = test_controller(asm2binI("lhu", "x2", "x0", str(randImm12)),
        AluOp.ADD.value, 1, 0, 1, Mem2RegCtrl.FROM_MEM.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LHU.value, LSUStoreCtrl.LSU_SW.value, 0, 0)

    ## Store instruction tests
    test_ctrl_sb = test_controller(asm2binS("sb", "x2", str(randImm12), "x0"),
        AluOp.ADD.value, 0, 1, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SB.value, 0, 0)
    test_ctrl_sh = test_controller(asm2binS("sh", "x1", str(randImm12), "x1"),
        AluOp.ADD.value, 0, 1, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SH.value, 0, 0)
    test_ctrl_sw = test_controller(asm2binS("sw", "x0", str(randImm12), "x2"),
        AluOp.ADD.value, 0, 1, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)

    ## Immediate instruction tests
    test_ctrl_addi = test_controller(asm2binI("addi", "x4", "x4", str(randImm12)),
        AluOp.ADD.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_slti = test_controller(asm2binI("slti", "x2", "x14", str(randImm12)),
        AluOp.SLT.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value,
            AluASrcCtrl.FROM_RS1.value, AluBSrcCtrl.FROM_IMM.value, 0,
                LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_sltiu = test_controller(asm2binI("sltiu", "x1", "x29", str(randImm12)),
        AluOp.SLTU.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value,
            AluASrcCtrl.FROM_RS1.value, AluBSrcCtrl.FROM_IMM.value, 0,
                LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_xori = test_controller(asm2binI("xori", "x1", "x6", str(randImm12)),
        AluOp.XOR.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_ori = test_controller(asm2binI("ori", "x2", "x6", str(randImm12)),
        AluOp.OR.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_andi = test_controller(asm2binI("andi", "x12", "x26", str(randImm12)),
        AluOp.AND.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_slli = test_controller(asm2binI("slli", "x2", "x6", str(randImm12)),
        AluOp.SLL.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_srli = test_controller(asm2binI("slri", "x22", "x7", str(randImm12)),
        AluOp.SRL.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_srai = test_controller(asm2binI("srai", "x20", "x17", "6"),
        AluOp.SRA.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_IMM.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)

    ## Register instruction tests
    test_ctrl_add = test_controller(asm2binR("add", "x20", "x8", "x17"),
        AluOp.ADD.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_RS2.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_sub = test_controller(asm2binR("sub", "x2", "x7", "x27"),
        AluOp.SUB.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_RS2.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_sll = test_controller(asm2binR("sll", "x1", "x6", "x0"),
        AluOp.SLL.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_RS2.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_slt = test_controller(asm2binR("slt", "x0", "x8", "x1"),
        AluOp.SLT.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_RS2.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_sltu = test_controller(asm2binR("sltu", "x9", "x28", "x4"),
        AluOp.SLTU.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_RS2.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_xor = test_controller(asm2binR("xor", "x22", "x18", "x25"),
        AluOp.XOR.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_RS2.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_srl = test_controller(asm2binR("srl", "x23", "x12", "x12"),
        AluOp.SRL.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_RS2.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_sra = test_controller(asm2binR("sra", "x30", "x6", "x1"),
        AluOp.SRA.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_RS2.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_or = test_controller(asm2binR("or", "x27", "x5", "x11"),
        AluOp.OR.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_RS2.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)
    test_ctrl_and = test_controller(asm2binR("and", "x0", "x5", "x17"),
        AluOp.AND.value, 1, 0, 0, Mem2RegCtrl.FROM_ALU.value, AluASrcCtrl.FROM_RS1.value,
            AluBSrcCtrl.FROM_RS2.value, 0, LSULoadCtrl.LSU_LW.value, LSUStoreCtrl.LSU_SW.value, 0, 0)

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
