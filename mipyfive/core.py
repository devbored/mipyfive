from nmigen import *
from .alu import *
from .lsu import *
from .utils import *
from .types import *
from .immgen import *
from .hazard import *
from .forward import *
from .pipereg import *
from .regfile import *
from .controller import *

from dataclasses import dataclass

@dataclass
class MipyfiveConfig:
    # Soft-core configs
    core_isa                : CoreISAconfigs = None
    core_data_width         : int = None
    core_reg_count          : int = None
    core_pc_start           : int = None

class MipyfiveCore(Elaboratable):
    # TODO: Starting boot addr, extensions, etc. can be configured here
    def __init__(self, config:MipyfiveConfig=None):
        if type(config) is not MipyfiveConfig:
            raise ValueError(
                "[mipyfive - smol.py]: " +
                f"Incorrect config object type [ { type(config)} ] for Smol - expected [ {type(MipyfiveConfig)} ]"
            )
        self.config = config

        self.dataWidth      = config.core_data_width
        self.pcStart        = config.core_pc_start
        self.addrBits       = ceilLog2(config.core_reg_count)
        self.ISA            = config.core_isa # TODO: Use later when extensions are added/supported

        self.instruction    = Signal(32)
        self.IF_valid       = Signal()
        self.MEM_valid      = Signal()
        self.DataIn         = Signal(config.core_data_width)

        self.PCout          = Signal(32)
        self.DataAddr       = Signal(32)
        self.DataOut        = Signal(config.core_data_width)
        self.DataWE         = Signal()

        # --- Core Submodules ---
        self.alu        = ALU(config.core_data_width)
        self.lsu        = LSU(config.core_data_width)
        self.immgen     = ImmGen() # TODO: Allow for arbitrary width?
        self.hazard     = HazardUnit(config.core_reg_count)
        self.forward    = ForwardingUnit(config.core_reg_count)
        self.regfile    = RegFile(config.core_data_width, config.core_reg_count)
        self.control    = Controller()

        # Configure and create pipeline registers
        # --- IF_ID Pipeline reg ---
        IF_ID_config = {
            # Name          # Number of bits (i.e. bit-vector)
            "pc"            : self.dataWidth,
            "instr"         : self.dataWidth
        }

        self.IF_ID          = PipeReg(**IF_ID_config)
        self.IF_ID_pc       = self.IF_ID.doutSlice("pc")
        self.IF_ID_instr    = self.IF_ID.doutSlice("instr")

        # --- ID_EX Pipeline reg ---
        ID_EX_config = {
            # Name          # Number of bits (i.e. bit-vector)
            "aluOp"         : ceilLog2(len(AluOp)),
            "lsuLoadCtrl"   : ceilLog2(len(LSULoadCtrl)),
            "lsuStoreCtrl"  : ceilLog2(len(LSUStoreCtrl)),
            "regWrite"      : 1,
            "memWrite"      : 1,
            "memRead"       : 1,
            "mem2Reg"       : 1,
            "jump"          : 1,
            "jumpR"         : 1,
            "branch"        : 1,
            "aluAsrc"       : ceilLog2(len(AluASrcCtrl)),
            "aluBsrc"       : ceilLog2(len(AluBSrcCtrl)),
            "rs1Addr"       : self.regfile.addrBits,
            "rs2Addr"       : self.regfile.addrBits,
            "rdAddr"        : self.regfile.addrBits,
            "imm"           : self.dataWidth,
            "pc"            : self.dataWidth
        }

        self.ID_EX                  = PipeReg(**ID_EX_config)
        self.ID_EX_aluOp            = self.ID_EX.doutSlice("aluOp")
        self.ID_EX_lsuLoadCtrl      = self.ID_EX.doutSlice("lsuLoadCtrl")
        self.ID_EX_lsuStoreCtrl     = self.ID_EX.doutSlice("lsuStoreCtrl")
        self.ID_EX_regWrite         = self.ID_EX.doutSlice("regWrite")
        self.ID_EX_memWrite         = self.ID_EX.doutSlice("memWrite")
        self.ID_EX_memRead          = self.ID_EX.doutSlice("memRead")
        self.ID_EX_mem2Reg          = self.ID_EX.doutSlice("mem2Reg")
        self.ID_EX_jump             = self.ID_EX.doutSlice("jump")
        self.ID_EX_jumpR            = self.ID_EX.doutSlice("jumpR")
        self.ID_EX_branch           = self.ID_EX.doutSlice("branch")
        self.ID_EX_aluAsrc          = self.ID_EX.doutSlice("aluAsrc")
        self.ID_EX_aluBsrc          = self.ID_EX.doutSlice("aluBsrc")
        self.ID_EX_rs1              = self.regfile.rs1Data
        self.ID_EX_rs2              = self.regfile.rs2Data
        self.ID_EX_rs1Addr          = self.ID_EX.doutSlice("rs1Addr")
        self.ID_EX_rs2Addr          = self.ID_EX.doutSlice("rs2Addr")
        self.ID_EX_rdAddr           = self.ID_EX.doutSlice("rdAddr")
        self.ID_EX_imm              = self.ID_EX.doutSlice("imm")
        self.ID_EX_pc               = self.ID_EX.doutSlice("pc")

        # --- EX_MEM Pipeline reg ---
        EX_MEM_config = {
            # Name          # Number of bits (i.e. bit-vector)
            "lsuLoadCtrl"   : ceilLog2(len(LSULoadCtrl)),
            "lsuStoreCtrl"  : ceilLog2(len(LSUStoreCtrl)),
            "regWrite"      : 1,
            "mem2Reg"       : 1,
            "memWrite"      : 1,
            "jump"          : 1,
            "jumpR"         : 1,
            "branch"        : 1,
            "aluOut"        : self.dataWidth,
            "writeData"     : self.dataWidth,
            "rdAddr"        : self.regfile.addrBits
        }
        self.EX_MEM                     = PipeReg(**EX_MEM_config)
        self.EX_MEM_lsuLoadCtrl         = self.EX_MEM.doutSlice("lsuLoadCtrl")
        self.EX_MEM_lsuStoreCtrl        = self.EX_MEM.doutSlice("lsuStoreCtrl")
        self.EX_MEM_regWrite            = self.EX_MEM.doutSlice("regWrite")
        self.EX_MEM_mem2Reg             = self.EX_MEM.doutSlice("mem2Reg")
        self.EX_MEM_memWrite            = self.EX_MEM.doutSlice("memWrite")
        self.EX_MEM_jump                = self.EX_MEM.doutSlice("jump")
        self.EX_MEM_jumpR               = self.EX_MEM.doutSlice("jumpR")
        self.EX_MEM_branch              = self.EX_MEM.doutSlice("branch")
        self.EX_MEM_aluOut              = self.EX_MEM.doutSlice("aluOut")
        self.EX_MEM_writeData           = self.EX_MEM.doutSlice("writeData")
        self.EX_MEM_rdAddr              = self.EX_MEM.doutSlice("rdAddr")

        # --- MEM_WB Pipeline reg ---
        MEM_WB_config = {
            # Name          # Number of bits (i.e. bit-vector)
            "lsuLoadCtrl"   : ceilLog2(len(LSULoadCtrl)),
            "regWrite"      : 1,
            "mem2Reg"       : 1,
            "aluOut"        : self.dataWidth,
            "rdAddr"        : self.regfile.addrBits,
            "dataIn"        : self.dataWidth
        }
        self.MEM_WB             = PipeReg(**MEM_WB_config)
        self.MEM_WB_lsuLoadCtrl = self.MEM_WB.doutSlice("lsuLoadCtrl")
        self.MEM_WB_regWrite    = self.MEM_WB.doutSlice("regWrite")
        self.MEM_WB_mem2Reg     = self.MEM_WB.doutSlice("mem2Reg")
        self.MEM_WB_aluOut      = self.MEM_WB.doutSlice("aluOut")
        self.MEM_WB_rdAddr      = self.MEM_WB.doutSlice("rdAddr")
        self.MEM_WB_dataIn      = self.MEM_WB.doutSlice("dataIn")

    def elaborate(self, platform):
        m = Module()

        # Instantiate Submodules
        m.submodules.alu        = self.alu
        m.submodules.lsu        = self.lsu
        m.submodules.immgen     = self.immgen
        m.submodules.hazard     = self.hazard
        m.submodules.forward    = self.forward
        m.submodules.regfile    = self.regfile
        m.submodules.control    = self.control
        m.submodules.IF_ID      = self.IF_ID
        m.submodules.ID_EX      = self.ID_EX
        m.submodules.EX_MEM     = self.EX_MEM
        m.submodules.MEM_WB     = self.MEM_WB

        WB_Buffer           = Signal(self.dataWidth)
        WB_Buffer_rd        = Signal(self.addrBits)
        WB_BUFFER_reg_write = Signal()
        mem2RegWire         = Signal(self.dataWidth)
        aluAin              = Signal(self.dataWidth)
        fwdAluAin           = Signal(self.dataWidth)
        aluBin              = Signal(self.dataWidth)
        fwdAluBin           = Signal(self.dataWidth)

        # Hazard and Forwarding setup/logic
        m.d.comb += [
            # Hazard
            self.hazard.Jump.eq(self.EX_MEM_jump | self.EX_MEM_jumpR),
            self.hazard.Branch.eq(self.EX_MEM_branch),
            self.hazard.BranchMispredict.eq(self.EX_MEM_aluOut[0]),
            self.hazard.IF_valid.eq(self.IF_valid),
            self.hazard.MEM_valid.eq(self.MEM_valid),
            self.hazard.ID_EX_memRead.eq(self.ID_EX_memRead),
            self.hazard.IF_ID_rs1.eq(self.IF_ID_instr[15:20]),
            self.hazard.IF_ID_rs2.eq(self.IF_ID_instr[20:25]),
            self.hazard.ID_EX_rd.eq(self.ID_EX_rdAddr),
            # Forward
            self.forward.IF_ID_rs1.eq(self.IF_ID_instr[15:20]),
            self.forward.ID_EX_rs1.eq(self.ID_EX_rs1Addr),
            self.forward.IF_ID_rs2.eq(self.IF_ID_instr[20:25]),
            self.forward.ID_EX_rs2.eq(self.ID_EX_rs2Addr),
            self.forward.EX_MEM_rd.eq(self.EX_MEM_rdAddr),
            self.forward.MEM_WB_rd.eq(self.MEM_WB_rdAddr),
            self.forward.WB_BUFFER_rd.eq(WB_Buffer_rd),
            self.forward.EX_MEM_reg_write.eq(self.EX_MEM_regWrite),
            self.forward.MEM_WB_reg_write.eq(self.MEM_WB_regWrite),
            self.forward.WB_BUFFER_reg_write.eq(WB_BUFFER_reg_write),
        ]

        # --- Fetch ---------------------------------------------------------------------------------------------------

        PC                  = Signal(32, reset=self.pcStart)
        PC_last             = Signal(32, reset=self.pcStart)
        jumpAddrAdderOut    = Signal(32)
        lastValid           = Signal()

        takeBranch          = self.ID_EX_branch & self.alu.out[0]
        invalidFetch        = ~self.IF_valid & ~lastValid
        flush_IF_ID         = self.hazard.IF_ID_flush | ~lastValid | invalidFetch
        IF_ID_din           = Cat(
            PC_last,
            self.instruction
        )

        m.d.comb += [
            # Pipereg
            self.IF_ID.rst.eq(flush_IF_ID),
            self.IF_ID.en.eq(~self.hazard.IF_ID_stall),
            self.IF_ID.din.eq(IF_ID_din),
            # Jump Address calculation
            jumpAddrAdderOut.eq(
                Mux(self.ID_EX_jumpR, self.ID_EX_imm, (self.ID_EX_imm << 1))
                +
                Mux(self.ID_EX_jumpR, fwdAluAin, self.ID_EX_pc)
            ),
            self.PCout.eq(PC)
        ]

        m.d.sync += [
            lastValid.eq(self.IF_valid),
            PC_last.eq(PC)
        ]
        with m.If((takeBranch | self.ID_EX_jump | self.ID_EX_jumpR) & ~flush_IF_ID):
            m.d.sync += PC.eq(jumpAddrAdderOut)
        with m.Elif(invalidFetch):
            m.d.sync += PC.eq(PC_last)
        with m.Else():
            m.d.sync += PC.eq(PC + C(4, self.dataWidth))

        # --- Decode --------------------------------------------------------------------------------------------------

        rs1Addr     = self.IF_ID_instr[15:20]
        rs2Addr     = self.IF_ID_instr[20:25]
        rdAddr      = self.IF_ID_instr[7:12]
        ID_EX_din   = Cat(
            self.control.aluOp,
            self.control.lsuLoadCtrl,
            self.control.lsuStoreCtrl,
            self.control.regWrite,
            self.control.memWrite,
            self.control.memRead,
            self.control.mem2Reg,
            self.control.jump,
            self.control.jumpR,
            self.control.branch,
            self.control.aluAsrc,
            self.control.aluBsrc,
            rs1Addr,
            rs2Addr,
            rdAddr,
            self.immgen.imm,
            self.IF_ID_pc
        )

        m.d.comb += [
            # Pipereg
            self.ID_EX.rst.eq(self.hazard.ID_EX_flush),
            self.ID_EX.en.eq(~self.hazard.ID_EX_stall),
            self.ID_EX.din.eq(ID_EX_din),
            # Immgen
            self.immgen.instruction.eq(self.IF_ID_instr),
            # Control
            self.control.instruction.eq(self.IF_ID_instr),
            # Regfile
            self.regfile.rs1Addr.eq(self.IF_ID_instr[15:20]),
            self.regfile.rs2Addr.eq(self.IF_ID_instr[20:25]),
            self.regfile.rdData.eq(self.lsu.lDataOut),
            self.regfile.rdAddr.eq(self.MEM_WB_rdAddr),
        ]
        with m.If(self.MEM_WB_rdAddr != C(0, self.regfile.addrBits)):
            m.d.comb += self.regfile.we.eq(self.MEM_WB_regWrite)
        with m.Else():
            m.d.comb += self.regfile.we.eq(0)


        # --- Execute -------------------------------------------------------------------------------------------------

        EX_MEM_din = Cat(
            self.ID_EX_lsuLoadCtrl,
            self.ID_EX_lsuStoreCtrl,
            self.ID_EX_regWrite,
            self.ID_EX_mem2Reg,
            self.ID_EX_memWrite,
            self.ID_EX_jump,
            self.ID_EX_jumpR,
            self.ID_EX_branch,
            self.alu.out,
            fwdAluBin,
            self.ID_EX_rdAddr
        )

        m.d.comb += [
            # Pipereg
            self.EX_MEM.rst.eq(self.hazard.EX_MEM_flush),
            self.EX_MEM.en.eq(~self.hazard.EX_MEM_stall),
            self.EX_MEM.din.eq(EX_MEM_din),
            # ALU
            self.alu.in1.eq(aluAin),
            self.alu.in2.eq(aluBin),
            self.alu.aluOp.eq(self.ID_EX_aluOp),
        ]
        # Fwd ALU A
        with m.Switch(self.forward.fwdAluA):
            with m.Case(AluForwardCtrl.NO_FWD):
                m.d.comb += fwdAluAin.eq(self.ID_EX_rs1)
            with m.Case(AluForwardCtrl.MEM_WB):
                m.d.comb += fwdAluAin.eq(self.MEM_WB_aluOut)
            with m.Case(AluForwardCtrl.EX_MEM):
                m.d.comb += fwdAluAin.eq(self.EX_MEM_aluOut)
            with m.Case(AluForwardCtrl.WB_BUFFER):
                m.d.comb += fwdAluAin.eq(WB_Buffer)
            with m.Default():
                m.d.comb += fwdAluAin.eq(self.ID_EX_rs1)
        # Fwd ALU B
        with m.Switch(self.forward.fwdAluB):
            with m.Case(AluForwardCtrl.NO_FWD):
                m.d.comb += fwdAluBin.eq(self.ID_EX_rs2)
            with m.Case(AluForwardCtrl.MEM_WB):
                m.d.comb += fwdAluBin.eq(self.MEM_WB_aluOut)
            with m.Case(AluForwardCtrl.EX_MEM):
                m.d.comb += fwdAluBin.eq(self.EX_MEM_aluOut)
            with m.Case(AluForwardCtrl.WB_BUFFER):
                m.d.comb += fwdAluBin.eq(WB_Buffer)
            with m.Default():
                m.d.comb += fwdAluBin.eq(self.ID_EX_rs2)
        # ALU A Src
        with m.Switch(self.ID_EX_aluAsrc):
            with m.Case(AluASrcCtrl.FROM_RS1):
                m.d.comb += aluAin.eq(fwdAluAin)
            with m.Case(AluASrcCtrl.FROM_PC):
                m.d.comb += aluAin.eq(self.ID_EX_pc)
            with m.Default():
                m.d.comb += aluAin.eq(fwdAluAin)
        # ALU B Src
        with m.Switch(self.ID_EX_aluBsrc):
            with m.Case(AluBSrcCtrl.FROM_RS2):
                m.d.comb += aluBin.eq(fwdAluBin)
            with m.Case(AluBSrcCtrl.FROM_IMM):
                m.d.comb += aluBin.eq(self.ID_EX_imm)
            with m.Default():
                m.d.comb += aluBin.eq(fwdAluBin)

        # --- Memory --------------------------------------------------------------------------------------------------

        MEM_WB_din = Cat(
            self.EX_MEM_lsuLoadCtrl,
            self.EX_MEM_regWrite,
            self.EX_MEM_mem2Reg,
            self.EX_MEM_aluOut,
            self.EX_MEM_rdAddr,
            self.DataIn
        )

        m.d.comb += [
            # Pipereg
            self.MEM_WB.rst.eq(0),
            self.MEM_WB.en.eq(~self.hazard.MEM_WB_stall),
            self.MEM_WB.din.eq(MEM_WB_din),
            # LSU
            self.lsu.lDataIn.eq(mem2RegWire),
            self.lsu.lCtrlIn.eq(self.MEM_WB_lsuLoadCtrl),
            self.lsu.sDataIn.eq(self.EX_MEM_writeData),
            self.lsu.sCtrlIn.eq(self.EX_MEM_lsuStoreCtrl),
            # DataAddr
            self.DataAddr.eq(self.EX_MEM_aluOut),
            # DataOut
            self.DataOut.eq(self.lsu.sDataOut),
            # DataWE
            self.DataWE.eq(self.EX_MEM_memWrite)
        ]

        # --- Writeback -----------------------------------------------------------------------------------------------

        m.d.comb += [
            # Mem2Reg
            mem2RegWire.eq(Mux(self.MEM_WB_mem2Reg, self.MEM_WB_aluOut, self.MEM_WB_dataIn)),
        ]

        m.d.sync += [
            # WB save buffer
            WB_Buffer.eq(self.lsu.lDataOut),
            WB_Buffer_rd.eq(self.MEM_WB_rdAddr),
            WB_BUFFER_reg_write.eq(self.MEM_WB_regWrite),
        ]

        return m
