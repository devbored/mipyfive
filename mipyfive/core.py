from nmigen import *
from .alu import *
from .lsu import *
from .utils import *
from .types import *
from .immgen import *
from .hazard import *
from .compare import *
from .forward import *
from .pipereg import *
from .regfile import *
from .controller import *

class MipyfiveCore(Elaboratable):
    # TODO: Starting boot addr, extensions, etc. can be configured here
    def __init__(self, dataWidth, regCount):
        self.dataWidth      = dataWidth
        self.instruction    = Signal(32)
        self.DataIn         = Signal(dataWidth)
        
        self.PCout          = Signal(32)
        self.DataAddr       = Signal(32)
        self.DataOut        = Signal(dataWidth)

        # --- Core Submodules ---
        self.alu        = ALU(dataWidth)
        self.lsu        = LSU(dataWidth)
        self.immgen     = ImmGen() # TODO: Allow for arbitrary width?
        self.hazard     = HazardUnit(regCount)
        self.compare    = CompareUnit(dataWidth)
        self.forward    = ForwardingUnit(regCount)
        self.regfile    = RegFile(dataWidth, regCount)
        self.control    = Controller()

        # Create pipeline registers
        self.IF_ID = PipeReg(pc=self.dataWidth)
        self.IF_ID_pc = self.IF_ID.doutSlice("pc")

        self.ID_EX = PipeReg(
            aluOp=ceilLog2(len(AluOp)),
            lsuLoadCtrl=ceilLog2(len(LSULoadCtrl)),
            lsuStoreCtrl=ceilLog2(len(LSUStoreCtrl)),
            regWrite=1,
            memWrite=1,
            memRead=1,
            mem2Reg=1,
            aluAsrc=2,
            aluBsrc=1,
            rs1=self.dataWidth,
            rs2=self.dataWidth,
            rs1Addr=self.regfile.addrBits,
            rs2Addr=self.regfile.addrBits,
            rdAddr=self.regfile.addrBits,
            imm=self.dataWidth,
            pc=self.dataWidth
        )
        self.ID_EX_aluOp          = self.ID_EX.doutSlice("aluOp")
        self.ID_EX_lsuLoadCtrl    = self.ID_EX.doutSlice("lsuLoadCtrl")
        self.ID_EX_lsuStoreCtrl   = self.ID_EX.doutSlice("lsuStoreCtrl")
        self.ID_EX_regWrite       = self.ID_EX.doutSlice("regWrite")
        self.ID_EX_memWrite       = self.ID_EX.doutSlice("memWrite")
        self.ID_EX_memRead        = self.ID_EX.doutSlice("memRead")
        self.ID_EX_mem2Reg        = self.ID_EX.doutSlice("mem2Reg")
        self.ID_EX_aluAsrc        = self.ID_EX.doutSlice("aluAsrc")
        self.ID_EX_aluBsrc        = self.ID_EX.doutSlice("aluBsrc")
        self.ID_EX_rs1            = self.ID_EX.doutSlice("rs1")
        self.ID_EX_rs2            = self.ID_EX.doutSlice("rs2")
        self.ID_EX_rs1Addr        = self.ID_EX.doutSlice("rs1Addr")
        self.ID_EX_rs2Addr        = self.ID_EX.doutSlice("rs2Addr")
        self.ID_EX_rdAddr         = self.ID_EX.doutSlice("rdAddr")
        self.ID_EX_imm            = self.ID_EX.doutSlice("imm")
        self.ID_EX_pc             = self.ID_EX.doutSlice("pc")

        self.EX_MEM = PipeReg(
            lsuLoadCtrl=ceilLog2(len(LSULoadCtrl)),
            lsuStoreCtrl=ceilLog2(len(LSUStoreCtrl)),
            regWrite=1,
            mem2Reg=1,
            memWrite=1,
            aluOut=self.dataWidth,
            writeData=self.dataWidth,
            rdAddr=self.regfile.addrBits
        )
        self.EX_MEM_lsuLoadCtrl    = self.EX_MEM.doutSlice("lsuLoadCtrl")
        self.EX_MEM_lsuStoreCtrl   = self.EX_MEM.doutSlice("lsuStoreCtrl")
        self.EX_MEM_regWrite       = self.EX_MEM.doutSlice("regWrite")
        self.EX_MEM_mem2Reg        = self.EX_MEM.doutSlice("mem2Reg")
        self.EX_MEM_memWrite       = self.EX_MEM.doutSlice("memWrite")
        self.EX_MEM_aluOut         = self.EX_MEM.doutSlice("aluOut")
        self.EX_MEM_writeData      = self.EX_MEM.doutSlice("writeData")
        self.EX_MEM_rdAddr         = self.EX_MEM.doutSlice("rdAddr")

        self.MEM_WB = PipeReg(
            lsuLoadCtrl=ceilLog2(len(LSULoadCtrl)),
            regWrite=1,
            mem2Reg=1,
            aluOut=self.dataWidth,
            rdAddr=self.regfile.addrBits
        )
        self.MEM_WB_lsuLoadCtrl = self.MEM_WB.doutSlice("lsuLoadCtrl")
        self.MEM_WB_regWrite    = self.MEM_WB.doutSlice("regWrite")
        self.MEM_WB_mem2Reg     = self.MEM_WB.doutSlice("mem2Reg")
        self.MEM_WB_aluOut      = self.MEM_WB.doutSlice("aluOut")
        self.MEM_WB_rdAddr      = self.MEM_WB.doutSlice("rdAddr")

    def elaborate(self, platform):
        m = Module()

        PC          = Signal(32, reset=0)
        mem2RegWire = Signal(self.dataWidth)
        aluAin      = Signal(self.dataWidth)
        fwdAluAin   = Signal(self.dataWidth)
        aluBin      = Signal(self.dataWidth)
        fwdAluBin   = Signal(self.dataWidth)

        # Instantiate Submodules
        m.submodules.alu        = self.alu
        m.submodules.lsu        = self.lsu
        m.submodules.immgen     = self.immgen
        m.submodules.hazard     = self.hazard
        m.submodules.compare    = self.compare
        m.submodules.forward    = self.forward
        m.submodules.regfile    = self.regfile
        m.submodules.control    = self.control
        m.submodules.IF_ID      = self.IF_ID
        m.submodules.ID_EX      = self.ID_EX
        m.submodules.EX_MEM     = self.EX_MEM
        m.submodules.MEM_WB     = self.MEM_WB

        # Internal logic
        takeBranch = self.control.branch & self.compare.isTrue

        # Hazard and Forwarding setup/logic
        m.d.comb += [
            # Hazard
            self.hazard.ID_EX_memRead.eq(self.ID_EX_memRead),
            self.hazard.Branch.eq(self.control.branch),
            self.hazard.EX_MEM_memToReg.eq(self.EX_MEM_mem2Reg),
            self.hazard.ID_EX_regWrite.eq(self.ID_EX_regWrite),
            self.hazard.ID_EX_rd.eq(self.ID_EX_rdAddr),
            self.hazard.EX_MEM_rd.eq(self.EX_MEM_rdAddr),
            self.hazard.IF_ID_rs1.eq(self.instruction[15:20]),
            self.hazard.IF_ID_rs2.eq(self.instruction[20:25]),
            # Forward
            self.forward.IF_ID_rs1.eq(self.instruction[15:20]),
            self.forward.ID_EX_rs1.eq(self.ID_EX_rs1Addr),
            self.forward.IF_ID_rs2.eq(self.instruction[20:25]),
            self.forward.ID_EX_rs2.eq(self.ID_EX_rs2Addr),
            self.forward.EX_MEM_rd.eq(self.EX_MEM_rdAddr),
            self.forward.MEM_WB_rd.eq(self.MEM_WB_rdAddr),
            self.forward.EX_MEM_reg_write.eq(self.EX_MEM_regWrite),
            self.forward.MEM_WB_reg_write.eq(self.MEM_WB_regWrite)
        ]

        # -------------
        # --- Fetch ---
        # -------------
        m.d.comb += [
            # Pipereg
            self.IF_ID.rst.eq(takeBranch),
            self.IF_ID.en.eq(~self.hazard.IF_ID_stall),
            self.IF_ID.din.eq(PC),
            # PCout
            self.PCout.eq(PC)
        ]
        with m.If(takeBranch):
            m.d.sync += PC.eq(self.IF_ID_pc + (self.immgen.imm << 1))
        with m.Elif(self.hazard.IF_stall):
            m.d.sync += PC.eq(PC)
        with m.Else():
            m.d.sync += PC.eq(PC + 4)

        # --------------
        # --- Decode ---
        # --------------
        rs1Data = Mux(self.forward.fwdRegfileAout, self.EX_MEM_aluOut, self.regfile.rs1Data)
        rs2Data = Mux(self.forward.fwdRegfileBout, self.EX_MEM_aluOut, self.regfile.rs2Data)

        rs1Addr = self.instruction[15:20]
        rs2Addr = self.instruction[20:25]
        rdAddr  = self.instruction[7:12]

        m.d.comb += [
            # Pipereg
            self.ID_EX.rst.eq(self.hazard.ID_EX_flush),
            self.ID_EX.en.eq(1),
            self.ID_EX.din.eq(
                Cat(
                    self.control.aluOp,
                    self.control.lsuLoadCtrl,
                    self.control.lsuStoreCtrl,
                    self.control.regWrite,
                    self.control.memWrite,
                    self.control.memRead,
                    self.control.mem2Reg,
                    self.control.aluAsrc,
                    self.control.aluBsrc,
                    rs1Data,
                    rs2Data,
                    rs1Addr,
                    rs2Addr,
                    rdAddr,
                    self.immgen.imm,
                    self.IF_ID_pc
                )
            ),
            # Immgen
            self.immgen.instruction.eq(self.instruction),
            # Compare
            self.compare.in1.eq(rs1Data),
            self.compare.in2.eq(rs2Data),
            self.compare.cmpType.eq(self.control.cmpType),
            # Control
            self.control.instruction.eq(self.instruction),
            # Regfile
            self.regfile.rs1Addr.eq(self.instruction[15:20]),
            self.regfile.rs2Addr.eq(self.instruction[20:25]),
            self.regfile.writeData.eq(self.lsu.lDataOut),
            self.regfile.writeEnable.eq(self.MEM_WB_regWrite),
            self.regfile.writeAddr.eq(self.MEM_WB_rdAddr)
        ]

        # ---------------
        # --- Execute ---
        # ---------------
        m.d.comb += [
            # Pipereg
            self.EX_MEM.rst.eq(0),
            self.EX_MEM.en.eq(1),
            self.EX_MEM.din.eq(
                Cat(
                    self.ID_EX_lsuLoadCtrl,
                    self.ID_EX_lsuStoreCtrl,
                    self.ID_EX_regWrite,
                    self.ID_EX_mem2Reg,
                    self.ID_EX_memWrite,
                    self.alu.out,
                    aluBin,
                    self.ID_EX_rdAddr
                )
            ),
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
        # Fwd ALU B
        with m.Switch(self.forward.fwdAluB):
            with m.Case(AluForwardCtrl.NO_FWD):
                m.d.comb += fwdAluBin.eq(self.ID_EX_rs1)
            with m.Case(AluForwardCtrl.MEM_WB):
                m.d.comb += fwdAluBin.eq(self.MEM_WB_aluOut)
            with m.Case(AluForwardCtrl.EX_MEM):
                m.d.comb += fwdAluBin.eq(self.EX_MEM_aluOut)
        # ALU A Src
        with m.Switch(self.ID_EX_aluAsrc):
            with m.Case(AluASrcCtrl.FROM_RS1):
                m.d.comb += aluAin.eq(fwdAluAin)
            with m.Case(AluASrcCtrl.FROM_ZERO):
                m.d.comb += aluAin.eq(0)
            with m.Case(AluASrcCtrl.FROM_PC):
                m.d.comb += aluAin.eq(self.ID_EX_pc)
        # ALU B Src
        m.d.comb += aluBin.eq(Mux(self.ID_EX_aluBsrc, self.ID_EX_imm, fwdAluBin))

        # --------------
        # --- Memory ---
        # --------------
        m.d.comb += [
            # Pipereg
            self.MEM_WB.rst.eq(0),
            self.MEM_WB.en.eq(1),
            self.MEM_WB.din.eq(
                Cat(
                    self.EX_MEM_lsuLoadCtrl,
                    self.EX_MEM_regWrite,
                    self.EX_MEM_mem2Reg,
                    self.EX_MEM_aluOut,
                    self.EX_MEM_rdAddr
                )
            ),
            # LSU
            self.lsu.lDataIn.eq(mem2RegWire),
            self.lsu.lCtrlIn.eq(self.MEM_WB_lsuLoadCtrl),
            self.lsu.sDataIn.eq(self.EX_MEM_writeData),
            self.lsu.sCtrlIn.eq(self.EX_MEM_lsuStoreCtrl),
            # DataAddr
            self.DataAddr.eq(self.EX_MEM_aluOut),
            # DataOut
            self.DataOut.eq(self.lsu.sDataOut)
        ]

        # -----------------
        # --- Writeback ---
        # -----------------
        m.d.comb += [
            # Mem2Reg
            mem2RegWire.eq(Mux(self.MEM_WB_mem2Reg, self.MEM_WB_aluOut, self.DataIn))
        ]

        return m
