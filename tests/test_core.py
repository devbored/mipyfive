import os
import sys
import argparse
import unittest
from amaranth import *
from amaranth.sim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from tests.utils import *
from tests.programs import *
from mipyfive.utils import *
from mipyfive.core import *
from mipyfive.types import *

# A basic BRAM module
class RAM(Elaboratable):
    def __init__(self, width, depth, init=None, dualRead=False, name=None):
        addrBits            = ceilLog2(depth)
        self.dualRead       = dualRead
        self.writeEnable    = Signal()
        self.readData       = Signal(width)
        if dualRead:
            self.readData2  = Signal(width)
        self.writeData      = Signal(width)
        self.readAddr       = Signal(addrBits)
        if dualRead:
            self.readAddr2  = Signal(addrBits)
        self.writeAddr      = Signal(addrBits)
        self.memory         = Memory(width=width, depth=depth, init=init, name=name)

        self.readPort       = self.memory.read_port()
        if dualRead:
            self.readPort2  = self.memory.read_port()
        self.writePort      = self.memory.write_port()

    def elaborate(self, platform):
        m = Module()

        m.submodules.readPort       = self.readPort
        if self.dualRead:
            m.submodules.readPort2  = self.readPort2
        m.submodules.writePort      = self.writePort

        m.d.comb += [
            self.writePort.addr.eq(self.writeAddr),
            self.writePort.data.eq(self.writeData),
            self.writePort.en.eq(self.writeEnable)
        ]
        if self.dualRead:
            m.d.comb += [
                self.readPort.addr.eq(self.readAddr),
                self.readPort2.addr.eq(self.readAddr2),
                self.readData.eq(self.readPort.data),
                self.readData2.eq(self.readPort2.data),
            ]
        else:
            m.d.comb += [
                self.readPort.addr.eq(self.readAddr),
                self.readData.eq(self.readPort.data),
            ]

        return m

createVcd      = False
verboseProgram = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "build", "vcd"))
def test_core(program, initRegs=[], expectedRegs=[]):
    def test(self):
        global createVcd
        global outputDir
        global verboseProgram

        # Assemble
        if verboseProgram is True:
            print(f"\n----------------------------------------------------------------------")
        programBinary = asm2Bin(program, verbose=verboseProgram)
        if programBinary is None:
            raise ValueError('[mipyfive - test_core]: asm2Bin failed assembling.')

        sim = Simulator(self.dut)
        def process():
            # Init imem
            for i in range(len(programBinary)):
                yield self.dut.submodules.imem.memory[i].eq(programBinary[i])

            # Init regfile
            for i in range(len(initRegs)):
                if type(initRegs[i]) is not tuple:
                    raise ValueError(f'[mipyfive - test_core]: "{initRegs[i]}" at index "{i}" needs to be a tuple.')
                if len(initRegs[i]) < 2:
                    raise ValueError('[mipyfive - test_core]: initReg tuple needs 2 elements (regIndex, value).')
                yield self.dut.submodules.core.regfile.memory[initRegs[i][0]].eq(initRegs[i][1])

            # Let the clock run for a bit
            for i in range(len(programBinary) * 2):
                if i < 1:
                    yield self.dut.submodules.core.IF_valid.eq(0)
                else:
                    yield self.dut.submodules.core.IF_valid.eq(1)

                yield Tick()

            # Evaluate from expectedRegs
            for i in range(len(expectedRegs)):
                if type(expectedRegs[i]) is not tuple:
                    raise ValueError(f'[mipyfive - test_core]: "{expectedRegs[i]}" at index "{i}" needs to be a tuple.')
                if len(expectedRegs[i]) < 2:
                    raise ValueError('[mipyfive - test_core]: expectedRegs tuple needs 2 elements (regIndex, value).')
                self.assertEqual(
                    (yield self.dut.submodules.core.regfile.memory[expectedRegs[i][0]]),
                        expectedRegs[i][1]
                )

        sim.add_clock(1e-6)
        sim.add_sync_process(process)
        if createVcd:
            if not os.path.exists(outputDir):
                os.makedirs(outputDir)
            with sim.write_vcd(vcd_file=os.path.join(outputDir, f"{self._testMethodName}.vcd")):
                sim.run()
        else:
            sim.run()
    return test

# Define unit tests
class TestCore(unittest.TestCase):
    def setUp(self):
        self.dut  = Module()
        config = MipyfiveConfig(
            core_isa        = CoreISAconfigs.RV32I,
            core_data_width = 32,
            core_reg_count  = 32,
            core_pc_start   = 0
        )
        self.dut.submodules.core = MipyfiveCore(config=config)
        self.dut.submodules.imem = RAM(width=32, depth=512)
        self.dut.submodules.dmem = RAM(width=32, depth=512)

        self.dut.d.comb += [
            # imem connections
            self.dut.submodules.imem.writeEnable.eq(0),
            self.dut.submodules.imem.writeData.eq(0),
            self.dut.submodules.imem.readAddr.eq(self.dut.submodules.core.PCout[2:]),
            self.dut.submodules.imem.writeAddr.eq(0),
            # dmem connections
            self.dut.submodules.dmem.writeEnable.eq(self.dut.submodules.core.DataWE),
            self.dut.submodules.dmem.writeData.eq(self.dut.submodules.core.DataOut),
            self.dut.submodules.dmem.readAddr.eq(self.dut.submodules.core.DataAddr),
            self.dut.submodules.dmem.writeAddr.eq(self.dut.submodules.core.DataAddr),
            # core connections
            self.dut.submodules.core.instruction.eq(self.dut.submodules.imem.readData),
            self.dut.submodules.core.DataIn.eq(self.dut.submodules.dmem.readData),
            self.dut.submodules.core.MEM_valid.eq(1)
        ]

    # --- Core tests - programs in "tests/programs.py" ---
    arithTestInitRegfile = [
        (1,arithRs1), (2,arithRs2), (3,arithRs3), (4,arithRs4), (5,arithRs5), (6,arithRs6), (7,arithRs7),
            (8,arithRs8), (9,arithRs9), (10,arithRs10), (11,arithRs11), (12,arithRs12)
    ]
    arithTestExpectedRegfile = [(31, 0)]
    test_core_arith = test_core(arithTestProgram, arithTestInitRegfile, arithTestExpectedRegfile)

    logicTestInitRegfile = [
        (1,logicRs1), (2,logicRs2), (3, logicRs3), (4, logicRs4), (5, logicRs5)
    ]
    logicTestExpectedRegfile = [(31, 0)]
    test_core_logic = test_core(logicTestProgram, logicTestInitRegfile, logicTestExpectedRegfile)

    jumpTestInitRegfile = []
    logicTestExpectedRegfile = [(31, 0)]
    test_core_jump = test_core(jumpTestProgram, jumpTestInitRegfile, logicTestExpectedRegfile)

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
    if args.verbosity >= 3:
        verboseProgram = True

    unittest.main(verbosity=args.verbosity)
