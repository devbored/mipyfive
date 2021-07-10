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
        self.immgen     = ImmGen() # TODO: Allow for arbitrary width
        self.hazard     = HazardUnit(regCount)
        self.compare    = CompareUnit(dataWidth)
        self.forward    = ForwardingUnit(regCount)
        self.regfile    = RegFile(dataWidth, regCount)
        self.control    = Controller()

        # Create pipeline registers
        self.IF_ID = PipeReg(
            pc=self.dataWidth,
            pc_4=self.dataWidth
        )
        self.IF_ID_pc     = self.IF_ID.doutSlice("pc")
        self.IF_ID_pc_4   = self.IF_ID.doutSlice("pc_4")

        self.ID_EX = PipeReg(
            aluOp=len(AluOp),
            lsuLoadCtrl=len(LSULoadCtrl),
            lsuStoreCtrl=len(LSUStoreCtrl),
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
            imm=self.dataWidth
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

        self.EX_MEM = PipeReg(
            lsuLoadCtrl=len(LSULoadCtrl),
            lsuStoreCtrl=len(LSUStoreCtrl),
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
            lsuLoadCtrl=len(LSULoadCtrl),
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

        # -----------------------

    def elaborate(self, platform):
        m = Module()
        PC = Signal(32, reset=0)

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

        # --- Fetch ---
        m.d.comb += [
         self.IF_ID.rst.eq(takeBranch),
         self.IF_ID.en.eq(self.hazard.IF_ID_stall),
         self.IF_ID.din.eq(Cat(PC, (PC + 4)))
        ]
        with m.If(takeBranch):
            m.d.sync += PC.eq(Cat(PC, (PC + 4)) + (self.immgen.imm << 2))
        with m.Elif(self.hazard.IF_stall):
            m.d.sync += PC.eq(PC)
        with m.Else():
            m.d.sync += PC.eq(PC + 4)

        # --- Decode ---
        # ...

        # --- Execute ---
        # ...

        # --- Memory ---
        # ...

        # --- Writeback ---
        # ...

        return m
