import os
import sys
import random
import argparse
import unittest
from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from mipyfive.peripherals.ram import *

createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "out", "ram_vcd"))
def test_ram_read():
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        testList = []
        def process():
            yield self.dut.writeData.eq(0)
            yield self.dut.writeEnable.eq(0)
            for i in range(self.dut.memory.depth):
                randVal = random.randint(1, 4294967295)
                yield self.dut.memory[i].eq(randVal)
                testList.append(randVal)
            yield Tick()

            # Test that reading from 
            for i in range(self.dut.memory.depth):
                yield self.dut.readAddr.eq(i)
                for j in range(2):
                    yield Tick()
                self.assertEqual((yield self.dut.readData), testList[i])
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

def test_ram_write(writeData):
    def test(self):
        global createVcd
        global outputDir
        sim = Simulator(self.dut)
        def process():
            yield self.dut.writeData.eq(writeData)
            for i in range(self.dut.memory.depth):
                yield self.dut.memory[i].eq(random.randint(1, 4294967295))
            yield Tick()

            # Test that the write works for all addresses
            for i in range(self.dut.memory.depth):
                yield self.dut.writeAddr.eq(i)
                yield self.dut.writeEnable.eq(1)
                for j in range(2):
                    yield Tick()
                yield self.dut.readAddr.eq(i)
                yield Tick()
                self.assertEqual((yield self.dut.readData), writeData)
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
        self.dut = RAM(width=32, depth=256)

    test_write  = test_ram_write(writeData=0xdeadbeef)
    test_read   = test_ram_read()

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[INFO]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True
    
    unittest.main(verbosity=2)
