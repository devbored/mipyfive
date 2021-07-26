from nmigen import *
from .types import *
from .utils import *

# TODO: Look into a ROM-based microcoded controller unit
#       (Compare with this hard-wired combinational circuit)
class Controller(Elaboratable):
    def __init__(self):
        self.instruction    = Signal(32)
        self.aluOp          = Signal(ceilLog2(len(AluOp)))
        self.cmpType        = Signal(ceilLog2(len(CompareTypes)))
        self.lsuLoadCtrl    = Signal(ceilLog2(len(LSULoadCtrl)))
        self.lsuStoreCtrl   = Signal(ceilLog2(len(LSUStoreCtrl)))
        self.regWrite       = Signal()
        self.memWrite       = Signal()
        self.memRead        = Signal()
        self.mem2Reg        = Signal()
        self.aluAsrc        = Signal(2)
        self.aluBsrc        = Signal(2)
        self.branch         = Signal()
        self.jump           = Signal()
        self.jumpR          = Signal()

    def elaborate(self, platform):
        m = Module()
        opcode = self.instruction[0:7]
        funct3 = self.instruction[12:15]
        funct7 = self.instruction[25:32]

        # Get instruction type via opcode
        with m.Switch(opcode):
            # --- R-Type ---
            with m.Case(Rv32iTypes.R.value):
                m.d.comb += [
                    self.regWrite.eq(1),
                    self.memWrite.eq(0),
                    self.memRead.eq(0),
                    self.cmpType.eq(CompareTypes.EQUAL.value),
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_RS1.value),
                    self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LW.value),
                    self.lsuStoreCtrl.eq(LSUStoreCtrl.LSU_SW.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_RS2.value),
                    self.branch.eq(0),
                    self.jump.eq(0),
                    self.jumpR.eq(0)
                ]
                with m.Switch(Cat(opcode, funct3, funct7)):
                    with m.Case(Rv32iInstructions.ADD.value):
                        m.d.comb += self.aluOp.eq(AluOp.ADD.value)
                    with m.Case(Rv32iInstructions.SUB.value):
                        m.d.comb += self.aluOp.eq(AluOp.SUB.value)
                    with m.Case(Rv32iInstructions.SLL.value):
                        m.d.comb += self.aluOp.eq(AluOp.SLL.value)
                    with m.Case(Rv32iInstructions.XOR.value):
                        m.d.comb += self.aluOp.eq(AluOp.XOR.value)
                    with m.Case(Rv32iInstructions.SRL.value):
                        m.d.comb += self.aluOp.eq(AluOp.SRL.value)
                    with m.Case(Rv32iInstructions.SRA.value):
                        m.d.comb += self.aluOp.eq(AluOp.SRA.value)
                    with m.Case(Rv32iInstructions.OR.value):
                        m.d.comb += self.aluOp.eq(AluOp.OR.value)
                    with m.Case(Rv32iInstructions.AND.value):
                        m.d.comb += self.aluOp.eq(AluOp.AND.value)
                    with m.Case(Rv32iInstructions.SLT.value):
                        m.d.comb += self.aluOp.eq(AluOp.SLT.value)
                    with m.Case(Rv32iInstructions.SLTU.value):
                        m.d.comb += self.aluOp.eq(AluOp.SLTU.value)
                    with m.Default():
                        pass # TODO: Handle invalid instruction here later...

            # --- I-Type ---
            with m.Case(Rv32iTypes.I_Arith.value, Rv32iTypes.I_Jump.value, Rv32iTypes.I_Load.value):
                m.d.comb += [
                    self.branch.eq(0),
                    self.jump.eq(0),
                    self.jumpR.eq(0),
                    self.regWrite.eq(1),
                    self.memWrite.eq(0),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_RS1.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_IMM.value)
                ]
                with m.If(opcode == Rv32iTypes.I_Load.value):
                    m.d.comb += [
                        self.memRead.eq(1),
                        self.aluOp.eq(AluOp.ADD.value),
                        self.lsuStoreCtrl.eq(LSUStoreCtrl.LSU_SB.value),
                        self.mem2Reg.eq(Mem2RegCtrl.FROM_MEM.value)
                    ]
                    with m.Switch(Cat(opcode, funct3)):
                        with m.Case(Rv32iInstructions.LB.value):
                            m.d.comb += self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LB.value)
                        with m.Case(Rv32iInstructions.LH.value):
                            m.d.comb += self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LH.value)
                        with m.Case(Rv32iInstructions.LBU.value):
                            m.d.comb += self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LBU.value)
                        with m.Case(Rv32iInstructions.LHU.value):
                            m.d.comb += self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LHU.value)
                        with m.Default():
                            m.d.comb += self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LW.value)
                with m.Else():
                    m.d.comb += [
                        self.memRead.eq(0),
                        self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LW.value),
                        self.lsuStoreCtrl.eq(LSUStoreCtrl.LSU_SB.value),
                    ]
                    with m.Switch(Cat(opcode, funct3, Repl(C(0), len(funct7)))):
                        with m.Case(Rv32iInstructions.JALR.value):
                            m.d.comb += [
                                self.jumpR.eq(1),
                                self.aluAsrc.eq(AluASrcCtrl.FROM_PC.value),
                                self.aluBsrc.eq(AluBSrcCtrl.FROM_ZERO.value),
                                self.aluOp.eq(AluOp.ADD.value),
                                self.cmpType.eq(CompareTypes.EQUAL.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.ADDI.value):
                            m.d.comb += [
                                self.aluOp.eq(AluOp.ADD.value),
                                self.cmpType.eq(CompareTypes.EQUAL.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.SLTI.value):
                            m.d.comb += [
                                self.aluOp.eq(AluOp.SLT.value),
                                self.cmpType.eq(CompareTypes.LESS_THAN.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.SLTIU.value):
                            m.d.comb += [
                                self.aluOp.eq(AluOp.SLTU.value),
                                self.cmpType.eq(CompareTypes.LESS_THAN_U.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.XORI.value):
                            m.d.comb += [
                                self.cmpType.eq(CompareTypes.EQUAL.value),
                                self.aluOp.eq(AluOp.XOR.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.ORI.value):
                            m.d.comb += [
                                self.cmpType.eq(CompareTypes.EQUAL.value),
                                self.aluOp.eq(AluOp.OR.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.ANDI.value):
                            m.d.comb += [
                                self.cmpType.eq(CompareTypes.EQUAL.value),
                                self.aluOp.eq(AluOp.AND.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.SLLI.value, Rv32iInstructions.SRLI.value,
                            Rv32iInstructions.SRAI.value):
                                m.d.comb += self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                                with m.If(funct3 == 0b001):
                                    m.d.comb += [
                                        self.aluOp.eq(AluOp.SLL.value),
                                        self.cmpType.eq(CompareTypes.EQUAL.value),
                                        self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                                    ]
                                with m.Elif((funct3 == 0b101) & (funct7 == 0b0100000)):
                                    m.d.comb += [
                                        self.aluOp.eq(AluOp.SRA.value),
                                        self.cmpType.eq(CompareTypes.EQUAL.value),
                                        self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                                    ]
                                with m.Else():
                                    m.d.comb += [
                                        self.aluOp.eq(AluOp.SRL.value),
                                        self.cmpType.eq(CompareTypes.EQUAL.value),
                                        self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                                    ]
                        with m.Default():
                            pass # TODO: Handle invalid instruction here later...

            # --- I-Type (System and Sync stuff) ---
            with m.Case(Rv32iTypes.I_Sync.value, Rv32iTypes.I_Sys.value):
                # TODO: Essentially a nop for now - revisit later...
                m.d.comb += [
                    self.branch.eq(0),
                    self.jump.eq(0),
                    self.jumpR.eq(0),
                    self.regWrite.eq(0),
                    self.memWrite.eq(0),
                    self.memRead.eq(0),
                    self.aluOp.eq(AluOp.ADD.value),
                    self.cmpType.eq(CompareTypes.EQUAL.value),
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LW.value),
                    self.lsuStoreCtrl.eq(LSUStoreCtrl.LSU_SW.value),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_RS1.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_IMM.value)
                ]

            # --- S-Type ---
            with m.Case(Rv32iTypes.S):
                m.d.comb += [
                    self.branch.eq(0),
                    self.jump.eq(0),
                    self.jumpR.eq(0),
                    self.regWrite.eq(0),
                    self.memWrite.eq(1),
                    self.memRead.eq(0),
                    self.aluOp.eq(AluOp.ADD.value),
                    self.cmpType.eq(CompareTypes.EQUAL.value),
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LW.value),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_RS1.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_IMM.value)
                ]
                with m.Switch(Cat(opcode, funct3)):
                    with m.Case(Rv32iInstructions.SB.value):
                        m.d.comb += self.lsuStoreCtrl.eq(LSUStoreCtrl.LSU_SB.value)
                    with m.Case(Rv32iInstructions.SH.value):
                        m.d.comb += self.lsuStoreCtrl.eq(LSUStoreCtrl.LSU_SH.value)
                    with m.Default():
                        m.d.comb += self.lsuStoreCtrl.eq(LSUStoreCtrl.LSU_SW.value)

            # --- B-Type ---
            with m.Case(Rv32iTypes.B):
                m.d.comb += [
                    self.branch.eq(1),
                    self.jump.eq(0),
                    self.jumpR.eq(0),
                    self.regWrite.eq(0),
                    self.memWrite.eq(0),
                    self.memRead.eq(0),
                    self.aluOp.eq(AluOp.ADD.value),
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LW.value),
                    self.lsuStoreCtrl.eq(LSUStoreCtrl.LSU_SW.value),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_RS1.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_RS2.value)
                ]
                with m.Switch(Cat(opcode, funct3)):
                    with m.Case(Rv32iInstructions.BEQ.value):
                        m.d.comb += self.cmpType.eq(CompareTypes.EQUAL.value)
                    with m.Case(Rv32iInstructions.BNE.value):
                        m.d.comb += self.cmpType.eq(CompareTypes.NOT_EQUAL.value)
                    with m.Case(Rv32iInstructions.BLT.value):
                        m.d.comb += self.cmpType.eq(CompareTypes.LESS_THAN.value)
                    with m.Case(Rv32iInstructions.BLTU.value):
                        m.d.comb += self.cmpType.eq(CompareTypes.LESS_THAN_U.value)
                    with m.Case(Rv32iInstructions.BGE.value):
                        m.d.comb += self.cmpType.eq(CompareTypes.GREATER_EQUAL.value)
                    with m.Case(Rv32iInstructions.BGEU.value):
                        m.d.comb += self.cmpType.eq(CompareTypes.GREATER_EQUAL_U.value)
                    with m.Default():
                        pass # TODO: Handle invalid instruction here later...

            # -- U-Type ---
            with m.Case(Rv32iTypes.U_Add.value, Rv32iTypes.U_Load.value):
                m.d.comb += [
                    self.branch.eq(0),
                    self.jump.eq(0),
                    self.jumpR.eq(0),
                    self.regWrite.eq(1),
                    self.memWrite.eq(0),
                    self.memRead.eq(0),
                    self.aluOp.eq(AluOp.ADD.value),
                    self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LW.value),
                    self.lsuStoreCtrl.eq(LSUStoreCtrl.LSU_SW.value),
                    self.cmpType.eq(CompareTypes.EQUAL.value),
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_IMM.value)
                ]
                with m.Switch(opcode):
                    with m.Case(Rv32iInstructions.LUI.value):
                        m.d.comb += self.aluAsrc.eq(AluASrcCtrl.FROM_ZERO)
                    with m.Case(Rv32iInstructions.AUIPC.value):
                        m.d.comb += self.aluAsrc.eq(AluASrcCtrl.FROM_PC)
                    with m.Default():
                        pass # TODO: Handle invalid instruction here later...

            # -- J-Type --
            with m.Case(Rv32iTypes.J):
                m.d.comb += [
                    self.branch.eq(0),
                    self.jump.eq(1),
                    self.jumpR.eq(0),
                    self.regWrite.eq(1),
                    self.memWrite.eq(0),
                    self.memRead.eq(0),
                    self.aluOp.eq(AluOp.ADD.value),
                    self.cmpType.eq(CompareTypes.EQUAL.value),
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.lsuLoadCtrl.eq(LSULoadCtrl.LSU_LW.value),
                    self.lsuStoreCtrl.eq(LSUStoreCtrl.LSU_SW.value),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_PC.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_ZERO.value)
                ]

            # -- Unknown instruction --
            with m.Default():
                pass # TODO: Handle invalid instruction here later...

        return m
