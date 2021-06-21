from nmigen import *
from .types import *
from .utils import *

# TODO: Look into a ROM-based microcoded controller unit
#       (Compare with this hard-wired combinational circuit)
class Controller(Elaboratable):
    def __init__(self):
        self.instruction    = Signal(32)
        self.aluOp          = Signal(ceilLog2(len(AluOp)))
        self.regWrite       = Signal()
        self.memWrite       = Signal()
        self.mem2Reg        = Signal()
        self.aluAsrc        = Signal(2)
        self.aluBsrc        = Signal()
        self.branch         = Signal()

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
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_RS1.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_RS2.value),
                    #self.aluOp.eq(AluOp.XOR.value),
                    self.branch.eq(0)
                ]

                # Decode instruction
                with m.Switch(Cat(opcode, funct3, funct7)):
                    with m.Case(Rv32iInstructions.ADD.value):
                        m.d.comb += self.aluOp.eq(AluOp.ADD.value)
                    with m.Case(Rv32iInstructions.SUB.value):
                        m.d.comb += self.aluOp.eq(AluOp.SUB.value)
                    with m.Case(Rv32iInstructions.SLL.value):
                        m.d.comb += self.aluOp.eq(AluOp.SLL.value)
                    with m.Case(Rv32iInstructions.SLT.value):
                        m.d.comb += self.aluOp.eq(AluOp.SLT.value)
                    with m.Case(Rv32iInstructions.SLTU.value):
                        m.d.comb += self.aluOp.eq(AluOp.SLTU.value)
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
                    with m.Default():
                        pass # TODO: Handle invalid instruction here later...

            # --- I-Type --- 
            with m.Case(Rv32iTypes.I_Arith.value, Rv32iTypes.I_Jump.value, Rv32iTypes.I_Load.value):
                m.d.comb += [
                    self.branch.eq(0),
                    self.regWrite.eq(1),
                    self.memWrite.eq(0),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_RS1.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_IMM.value)
                ]

                with m.If(opcode == Rv32iTypes.I_Load.value):
                    m.d.comb += [
                        self.aluOp.eq(AluOp.ADD.value),
                        self.mem2Reg.eq(Mem2RegCtrl.FROM_MEM.value)
                    ]
                with m.Else():
                    with m.Switch(Cat(opcode, funct3, Repl(C(0), len(funct7)))):
                        with m.Case(Rv32iInstructions.JALR.value, Rv32iInstructions.ADDI.value):
                            m.d.comb += [
                                self.aluOp.eq(AluOp.ADD.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.SLTI.value):
                            m.d.comb += [
                                self.aluOp.eq(AluOp.SLT.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.SLTIU.value):
                            m.d.comb += [
                                self.aluOp.eq(AluOp.SLTU.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.XORI.value):
                            m.d.comb += [
                                self.aluOp.eq(AluOp.XOR.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.ORI.value):
                            m.d.comb += [
                                self.aluOp.eq(AluOp.OR.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.ANDI.value):
                            m.d.comb += [
                                self.aluOp.eq(AluOp.AND.value),
                                self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                            ]
                        with m.Case(Rv32iInstructions.SLLI.value, Rv32iInstructions.SRLI.value,
                            Rv32iInstructions.SRAI.value):
                                m.d.comb += self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value)
                                with m.If(funct3 == 0b001):
                                    m.d.comb += self.aluOp.eq(AluOp.SLL.value)
                                with m.Elif((funct3 == 0b101) & (funct7 == 0b0100000)):
                                    m.d.comb += self.aluOp.eq(AluOp.SRA.value)
                                with m.Else():
                                    m.d.comb += self.aluOp.eq(AluOp.SRL.value)
                        with m.Default():
                            pass # TODO: Handle invalid instruction here later...

            # --- I-Type (System and Sync stuff) ---
            with m.Case(Rv32iTypes.I_Sync.value, Rv32iTypes.I_Sys.value):
                # TODO: Essentially a nop for now - revisit later...
                m.d.comb += [
                    self.branch.eq(0),
                    self.regWrite.eq(0),
                    self.memWrite.eq(0),
                    self.aluOp.eq(AluOp.ADD.value),
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_RS1.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_IMM.value)
                ]

            # --- S-Type --- 
            with m.Case(Rv32iTypes.S):
                m.d.comb += [
                    self.branch.eq(0),
                    self.regWrite.eq(0),
                    self.memWrite.eq(1),
                    self.aluOp.eq(AluOp.ADD.value),
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_RS1.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_IMM.value)
                ]

            # --- B-Type --- 
            with m.Case(Rv32iTypes.B):
                m.d.comb += [
                    self.branch.eq(1),
                    self.regWrite.eq(0),
                    self.memWrite.eq(0),
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_RS1.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_RS2.value)
                ]

                with m.Switch(Cat(opcode, funct3)):
                    with m.Case(Rv32iInstructions.BEQ.value, Rv32iInstructions.BNE.value):
                        m.d.comb += self.aluOp.eq(AluOp.XOR.value)
                    with m.Case(Rv32iInstructions.BLT.value, Rv32iInstructions.BGE.value):
                        m.d.comb += self.aluOp.eq(AluOp.SLT.value)
                    with m.Case(Rv32iInstructions.BLTU.value, Rv32iInstructions.BGEU.value):
                        m.d.comb += self.aluOp.eq(AluOp.SLTU.value)
                    with m.Default():
                        pass # TODO: Handle invalid instruction here later...

            # -- U-Type ---
            with m.Case(Rv32iTypes.U_Add.value, Rv32iTypes.U_Load.value):
                m.d.comb += [
                    self.branch.eq(0),
                    self.regWrite.eq(1),
                    self.memWrite.eq(0),
                    self.aluOp.eq(AluOp.ADD.value),
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
                    self.regWrite.eq(1),
                    self.memWrite.eq(0),
                    self.aluOp.eq(AluOp.ADD.value),
                    self.mem2Reg.eq(Mem2RegCtrl.FROM_ALU.value),
                    self.aluAsrc.eq(AluASrcCtrl.FROM_ZERO.value),
                    self.aluBsrc.eq(AluBSrcCtrl.FROM_IMM.value)
                ]
        
            # -- Unknown instruction --
            with m.Default():
                pass # TODO: Handle invalid instruction here later...

        return m
