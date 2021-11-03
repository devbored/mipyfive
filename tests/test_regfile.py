import os
import sys
import random
import argparse
import unittest
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from mipyfive.regfile import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "vcd"))
def test_regfile_read():
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        testList = []
        def process():
            yield self.dut.rdData.eq(0)
            yield self.dut.we.eq(0)

            for i in range(self.dut.memory.depth):
                randVal = random.randint(1, 4294967295)
                yield self.dut.memory[i].eq(randVal)
                testList.append(randVal)
            yield Tick()
            for i in range(self.dut.memory.depth):
                yield self.dut.rs1Addr.eq(i)
                yield self.dut.rs2Addr.eq(i)
                for j in range(2):
                    yield Tick()
                self.assertEqual((yield self.dut.rs1Data), testList[i])
                self.assertEqual((yield self.dut.rs2Data), testList[i])
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

def test_regfile_write(rdData):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.rdData.eq(rdData)

            for i in range(self.dut.memory.depth):
                yield self.dut.memory[i].eq(random.randint(1, 4294967295))
            yield Tick()
            # Test that the write works for all addresses
            for i in range(self.dut.memory.depth):
                yield self.dut.rdAddr.eq(i)
                yield self.dut.we.eq(1)
                for j in range(2):
                    yield Tick()
                yield self.dut.rs1Addr.eq(i)
                yield self.dut.rs2Addr.eq(i)
                for j in range(1):
                    yield Tick()
                self.assertEqual((yield self.dut.rs1Data), rdData)
                self.assertEqual((yield self.dut.rs2Data), rdData)
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
class TestRegfile(unittest.TestCase):
    def setUp(self):
        self.dut = RegFile(width=32, regCount=32)

    test_regfile_write  = test_regfile_write(rdData=0xdeadbeef)
    test_regfile_read   = test_regfile_read()

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
