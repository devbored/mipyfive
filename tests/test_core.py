import os
import sys
import argparse
import unittest
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from tests.utils import *
from tests.programs import *
from mipyfive.utils import *
from mipyfive.core import *
from mipyfive.types import *
from examples.common.ram import *

createVcd = False
verboseProgram = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
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
                yield self.dut.submodules.core.regfile.regArray[initRegs[i][0]].eq(initRegs[i][1])

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
                self.assertEqual((yield self.dut.submodules.core.regfile.regArray[expectedRegs[i][0]]),
                    expectedRegs[i][1])

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
        self.dut.submodules.core = MipyfiveCore(dataWidth=32, regCount=32, pcStart=0, ISA=CoreISAconfigs.RV32I.value)
        self.dut.submodules.imem = RAM(width=32, depth=512, wordAligned=True)
        self.dut.submodules.dmem = RAM(width=32, depth=512)

        self.dut.d.comb += [
            # imem connections
            self.dut.submodules.imem.writeEnable.eq(0),
            self.dut.submodules.imem.writeData.eq(0),
            self.dut.submodules.imem.readAddr.eq(self.dut.submodules.core.PCout),
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

    # Core tests - programs in "tests/programs.py"
    arithTestInitRegfile = [
        (1,arithRs1), (2,arithRs2), (3,arithRs3), (4,arithRs4), (5,arithRs5), (6,arithRs6), (7,arithRs7),
            (8,arithRs8), (9,arithRs9), (10,arithRs10), (11,arithRs11), (12,arithRs12)
    ]
    arithTestExpectedRegfile = [(31, 0)]
    test_core_arith = test_core(arithTestProgram, arithTestInitRegfile, arithTestExpectedRegfile)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    parser.add_argument("--verbose", action="store_true", help="Print out RISC-V asm and machine code.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[mipyfive - Info]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True
    if args.verbose is True:
        verboseProgram = True

    unittest.main(verbosity=2)
