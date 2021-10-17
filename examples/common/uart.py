import sys
import string
import random
import argparse
import unittest

from nmigen import *
from nmigen.cli import main
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from mipyfive.pipereg import *
from mipyfive.utils import *

# A basic UART transceiver module (start--data_bits--stop) - no parity bits
class UART(Elaboratable):
    def __init__(self, clkFreq=1e6, targetBaudrate=9600, dataBits=8):
        self.clkFreq = clkFreq
        self.targetBaudrate = targetBaudrate
        self.dataBits = dataBits

        assert targetBaudrate > 0 and clkFreq > 0 and dataBits > 0
        self.baudSampleRate = int((clkFreq // ( 16 * targetBaudrate)))
        if self.baudSampleRate == 0:
            print(
                f"[UART - Warning]: Sampling rate of ({clkFreq / ( 16 * targetBaudrate)}) will be rounded to 1, " +
                f"this might not be enough resolution for {clkFreq} Hz and {targetBaudrate} Baud."
            )
            self.baudSampleRate = 1

        self.tx_reg_i       = Signal(self.dataBits)
        self.tx_reg_wr_i    = Signal()
        self.tx_start_i     = Signal()
        self.tx_o           = Signal()
        self.tx_valid_o     = Signal()

        self.rx_i           = Signal()
        self.rx_ready_o     = Signal()
        self.rx_reg_o       = Signal(self.dataBits)

    def elaborate(self, platform):
        m = Module()

        # Define shift regs
        shiftSize = (1 + self.dataBits + 1)
        self.rxShift = Signal(shiftSize)
        self.txShift = Signal(shiftSize)

        # Create a modulus counter for clk dividing, a sample counter, and a counter for bit progress
        rxSampleTickCounter = Signal(ceilLog2(self.baudSampleRate))
        txSampleTickCounter = Signal(ceilLog2(self.baudSampleRate))
        rxSampleCounter     = Signal(ceilLog2(15))
        txSampleCounter     = Signal(ceilLog2(15))
        rxBitCounter        = Signal(ceilLog2(shiftSize))
        txBitCounter        = Signal(ceilLog2(shiftSize))

        txReg = Signal(self.dataBits)
        m.d.comb += self.tx_o.eq(self.txShift[0])
        with m.If(self.tx_reg_wr_i):
            m.d.sync += txReg.eq(self.tx_reg_i)
        with m.Else():
            m.d.sync += txReg.eq(txReg)


        # ===================================================================================
        # RX FSM
        # ===================================================================================
        with m.FSM():
            with m.State("RX_IDLE"):
                with m.If(~self.rx_i):
                    m.next = "RX_START"
                    m.d.sync += [
                        rxSampleTickCounter.eq(self.baudSampleRate),
                        rxSampleCounter.eq(7)
                    ]
                m.d.sync += [self.rx_ready_o.eq(0), self.rx_reg_o.eq(self.rxShift[1:shiftSize-1])]

            with m.State("RX_START"):
                with m.If(rxSampleTickCounter == 0):
                    m.d.sync += rxSampleTickCounter.eq(self.baudSampleRate)
                    with m.If(rxSampleCounter == 0):
                        m.next = "RX_DATA"
                        m.d.sync += [
                            rxSampleCounter.eq(15),
                            rxBitCounter.eq(self.dataBits),
                            self.rxShift.eq(Cat(self.rxShift[1:], self.rx_i)),
                            rxSampleTickCounter.eq(self.baudSampleRate),
                        ]
                    with m.Else():
                        m.d.sync += rxSampleCounter.eq(rxSampleCounter - 1)
                with m.Else():
                    m.d.sync += rxSampleTickCounter.eq(rxSampleTickCounter - 1)

            with m.State("RX_DATA"):
                with m.If(rxSampleTickCounter == 0):
                    m.d.sync += rxSampleTickCounter.eq(self.baudSampleRate)
                    with m.If(rxSampleCounter == 0):
                        with m.If(rxBitCounter == 0):
                            m.next = "RX_STOP"
                            m.d.sync += [
                                rxSampleCounter.eq(15),
                                rxSampleTickCounter.eq(self.baudSampleRate)
                            ]
                        with m.Else():
                            m.d.sync += [
                                rxBitCounter.eq(rxBitCounter - 1),
                                self.rxShift.eq(Cat(self.rxShift[1:], self.rx_i)),
                                rxSampleCounter.eq(15)
                            ]
                    with m.Else():
                        m.d.sync += rxSampleCounter.eq(rxSampleCounter - 1)
                with m.Else():
                    m.d.sync += rxSampleTickCounter.eq(rxSampleTickCounter - 1)

            with m.State("RX_STOP"):
                with m.If(rxSampleTickCounter == 0):
                    m.d.sync += rxSampleTickCounter.eq(self.baudSampleRate)
                    with m.If(rxSampleCounter == 0):
                        m.next = "RX_IDLE"
                        m.d.sync += [
                            self.rx_ready_o.eq(1),
                            self.rxShift.eq(Cat(self.rxShift[1:], self.rx_i)),
                            rxSampleTickCounter.eq(self.baudSampleRate),
                        ]
                    with m.Else():
                        m.d.sync += rxSampleCounter.eq(rxSampleCounter - 1)
                with m.Else():
                    m.d.sync += rxSampleTickCounter.eq(rxSampleTickCounter - 1)

        # ===================================================================================
        # TX FSM
        # ===================================================================================
        with m.FSM():
            with m.State("TX_IDLE"):
                with m.If(self.tx_start_i):
                    m.next = "TX_START"
                    m.d.sync += [
                        self.tx_valid_o.eq(1),
                        txSampleCounter.eq(7),
                        self.txShift.eq(Cat(0, txReg, 1)),
                        txSampleTickCounter.eq(self.baudSampleRate)
                    ]

            with m.State("TX_START"):
                with m.If(txSampleTickCounter == 0):
                    m.d.sync += txSampleTickCounter.eq(self.baudSampleRate)
                    with m.If(txSampleCounter == 0):
                        m.next = "TX_DATA"
                        m.d.sync += [
                            txSampleCounter.eq(15),
                            txBitCounter.eq(self.dataBits),
                            self.txShift.eq(Cat(self.txShift[1:], 1)),
                            txSampleTickCounter.eq(self.baudSampleRate),
                        ]
                    with m.Else():
                        m.d.sync += txSampleCounter.eq(txSampleCounter - 1)
                with m.Else():
                    m.d.sync += txSampleTickCounter.eq(txSampleTickCounter - 1)

            with m.State("TX_DATA"):
                with m.If(txSampleTickCounter == 0):
                    m.d.sync += txSampleTickCounter.eq(self.baudSampleRate)
                    with m.If(txSampleCounter == 0):
                        m.d.sync += [
                            self.txShift.eq(Cat(self.txShift[1:], 1)),
                            txSampleCounter.eq(15)
                        ]
                        with m.If(txBitCounter == 0):
                            m.next = "TX_STOP"
                            m.d.sync += [
                                txSampleCounter.eq(15),
                                txSampleTickCounter.eq(self.baudSampleRate)
                            ]
                        with m.Else():
                            m.d.sync += txBitCounter.eq(txBitCounter - 1)
                    with m.Else():
                        m.d.sync += txSampleCounter.eq(txSampleCounter - 1)
                with m.Else():
                    m.d.sync += txSampleTickCounter.eq(txSampleTickCounter - 1)

            with m.State("TX_STOP"):
                with m.If(txSampleTickCounter == 0):
                    m.d.sync += txSampleTickCounter.eq(self.baudSampleRate)
                    with m.If(txSampleCounter == 0):
                        m.next = "TX_IDLE"
                        m.d.sync += [
                            self.tx_valid_o.eq(0),
                            self.txShift.eq(Cat(self.txShift[1:], 1)),
                            txSampleTickCounter.eq(self.baudSampleRate)
                        ]
                    with m.Else():
                        m.d.sync += txSampleCounter.eq(txSampleCounter - 1)
                with m.Else():
                    m.d.sync += txSampleTickCounter.eq(txSampleTickCounter - 1)

        return m

# --- Simulation ------------------------------------------------------------------------------------------------------
# UART unit test code
createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "out", "vcd"))
def test_tx_uart(packet):
    def test(self):
        print(f"Sending packet: [ {chr(packet)} ]")
        sim = Simulator(self.dut)
        def process():
            # Idle
            yield self.dut.rx_i.eq(1)
            yield self.dut.tx_start_i.eq(0)
            yield self.dut.tx_reg_i.eq(packet)
            yield self.dut.tx_reg_wr_i.eq(1)
            yield Delay(5e-6)
            # Start bit
            yield self.dut.tx_start_i.eq(1)
            yield self.dut.tx_reg_wr_i.eq(0)
            for j in range((self.dut.baudSampleRate+1) * 8):
                yield Tick()
            # Data bits
            yield self.dut.tx_start_i.eq(0)
            for i in range(self.dut.dataBits):
                for j in range((self.dut.baudSampleRate+1) * 8):
                    yield Tick()
                print(f"Actual: {(yield self.dut.tx_o)}, Expected: {(packet & (1 << i)) >> i}")
                self.assertEqual((yield self.dut.tx_o), (packet & (1 << i)) >> i)
                for j in range((self.dut.baudSampleRate+1) * 8):
                    yield Tick()
            for i in range((self.dut.baudSampleRate+1) * 8):
                yield Tick()
            # Stop bit
            yield self.dut.rx_i.eq(1)
            for j in range((self.dut.baudSampleRate+1) * 64):
                yield Tick()

        sim.add_clock(1e-6)
        sim.add_process(process)
        if createVcd:
            if not os.path.exists(outputDir):
                os.makedirs(outputDir)
            with sim.write_vcd(vcd_file=os.path.join(outputDir, f"{self._testMethodName}.vcd")):
                sim.run()
        else:
            sim.run()
    return test

def test_rx_uart(packet):
    def test(self):
        print(f"Receiving packet: [ {chr(packet)} ]")
        sim = Simulator(self.dut)
        def process():
            # Idle
            yield self.dut.rx_i.eq(1)
            yield self.dut.tx_start_i.eq(0)
            yield Delay(5e-6)
            # Start bit
            yield self.dut.rx_i.eq(0)
            for j in range((self.dut.baudSampleRate+1) * 8):
                yield Tick()
            # Data bits
            for i in range(self.dut.dataBits):
                for j in range((self.dut.baudSampleRate+1) * 8):
                    yield Tick()
                yield self.dut.rx_i.eq((packet & (1 << i)) >> i)
                for j in range((self.dut.baudSampleRate+1) * 8):
                    yield Tick()
            for i in range((self.dut.baudSampleRate+1) * 8):
                yield Tick()
            # Stop bit
            yield self.dut.rx_i.eq(1)
            for j in range((self.dut.baudSampleRate+1) * 64):
                yield Tick()

            # Check if packet matches
            print(f"Actual: {(yield self.dut.rx_reg_o)}, Expected: {packet}")
            self.assertEqual((yield self.dut.rx_reg_o), packet)

        sim.add_clock(1e-6)
        sim.add_process(process)
        if createVcd:
            if not os.path.exists(outputDir):
                os.makedirs(outputDir)
            with sim.write_vcd(vcd_file=os.path.join(outputDir, f"{self._testMethodName}.vcd")):
                sim.run()
        else:
            sim.run()
    return test

# Define unit tests
class TestUart(unittest.TestCase):
    def setUp(self):
        self.dut = UART(clkFreq=10e6, targetBaudrate=115200, dataBits=8)

    # Unit tests
    test_rx_uart = test_rx_uart(ord(random.choice(string.ascii_letters)))
    test_tx_uart = test_tx_uart(ord(random.choice(string.ascii_letters)))

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vcd", action="store_true", help="Emit VCD files.")
    parser.add_argument("-v", dest="verbosity", type=int, default=2, help="Verbosity level.")
    args, argv = parser.parse_known_args()
    sys.argv[1:] = argv
    if args.vcd is True:
        print(f"[mipyfive - Info]: Emitting VCD files to --> {outputDir}\n")
        createVcd = True
    unittest.main(verbosity=args.verbosity)
