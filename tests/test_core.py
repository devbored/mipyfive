import os
import sys
import argparse
import unittest
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from tests.utils import *
from mipyfive.utils import *
from mipyfive.core import *
from mipyfive.types import *
from examples.common.ram import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_core(program):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            # NOTE/TODO: No assertions for now - add later
            # (i.e. test(s) will always pass - currently just using this for VCD dumping)
            for i in range(len(program)):
                yield self.dut.submodules.imem.memory[i].eq(program[i])

            for i in range(len(program) + 5):
                yield Tick()

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
        self.dut.submodules.core = MipyfiveCore(dataWidth=32, regCount=32, pcStart=-8, ISA=CoreISAconfigs.RV32I.value)
        self.dut.submodules.imem = RAM(width=32, depth=256, wordAligned=True)
        self.dut.submodules.dmem = RAM(width=32, depth=256)

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
            self.dut.submodules.core.DataIn.eq(self.dut.submodules.dmem.readData)
        ]

    # Test each instruction
    program = '''
        addi   x1, x0, 4
        slti   x2, x1, -6
        sltiu  x3, x1, 6
        xori   x4, x1, 255
        ori    x5, x1, 3
        andi   x6, x1, 15
        slli   x7, x1, 29
        slri   x8, x1, 1
        srai   x9, x7, 3
        add    x10, x1, x3
        sub    x11, x1, x3
        sll    x12, x1, x11
        slt    x13, x1, x9
        sltu   x14, x1, x9
        xor    x15, x1, x8
        srl    x16, x1, x3
        sra    x17, x1, x3
        or     x18, x1, x3
        and    x19, x4, x5
        lui    x20, 5
        auipc  x21, 4
        sb     x4, 16, x0
        sh     x4, 20, x0
        sw     x4, 24, x0
        lb     x22, x0, 24
        lh     x23, x0, 24
        lw     x24, x0, 24
        lbu    x25, x0, 24
        lhu    x26, x0, 24
        add    x0, x0, x0
        add    x0, x0, x0
        jalr   x22, x0, 68
        add    x0, x0, x0
        add    x0, x0, x0
        jal    x23, 74
        add    x0, x0, x0
        add    x0, x0, x0
        beq    x1, 6, x0
        add    x0, x0, x0
        add    x0, x0, x0
        bne    x1, 4, x0
        add    x0, x0, x0
        add    x0, x0, x0
        blt    x1, 4, x0
        add    x0, x0, x0
        add    x0, x0, x0
        bge    x1, 4, x0
        add    x0, x0, x0
        add    x0, x0, x0
        bltu   x1, 4, x0
        add    x0, x0, x0
        add    x0, x0, x0
        bgeu   x1, 4, x0
        add    x0, x0, x0
        add    x0, x0, x0
    '''
    programBinary = asm2Bin(program, debugPrint=True)
    programBinary[8] |= 0x40000000  # Bug in generated srai instruction generated - this is a quick workaround
    test_core = test_core(programBinary)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True

    unittest.main(verbosity=2)
