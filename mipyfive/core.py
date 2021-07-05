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

        # Calculate pipeline register widths and use for submodule init
        IF_ID_width = len(self.instruction) + (len(self.PCout) * 2)
        ID_EX_width = (
            len(self.control.aluOp) + len(self.control.regWrite) + len(self.control.memWrite) + 
                len(self.control.memRead) + len(self.control.mem2Reg) + len(self.control.aluAsrc) +
                    len(self.control.aluBsrc) + len(self.immgen.imm) + len(self.regfile.rs1Data) +
                        len(self.regfile.rs2Data) + (ceilLog2(regCount) * 3) + len(self.PCout)
        )
        EX_MEM_width = (
            len(self.control.regWrite) + len(self.control.memWrite) + len(self.control.mem2Reg) +
                len(self.alu.out) + dataWidth
        )
        MEM_WB_width = (
            len(self.control.regWrite) + len(self.control.mem2Reg) + len(self.alu.out)
                + dataWidth + ceilLog2(regCount)
        )
        
        self.IF_ID      = PipeReg(IF_ID_width)
        self.ID_EX      = PipeReg(ID_EX_width)
        self.EX_MEM     = PipeReg(EX_MEM_width)
        self.MEM_WB     = PipeReg(MEM_WB_width)

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

        # Internal wires/buses
        takeBranch = self.control.branch & self.compare.isTrue

        # --- Fetch ---
        # ...

        # --- Decode ---
        # ...

        # --- Execute ---
        # ...

        # --- Memory ---
        # ...

        # --- Writeback ---
        # ...

        return m
