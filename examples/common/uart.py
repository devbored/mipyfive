import os
import sys
import string
import random
import argparse
import unittest

from nmigen import *
from nmigen.back.pysim import *

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from mipyfive.pipereg import *
from mipyfive.utils import *

from enum import Enum
from dataclasses import dataclass

@dataclass
class UartConfig:
    clk_frequency           : float = 1e6
    target_baudrate         : int   = 9600
    data_bits               : int   = 8

class UartControllerBits(Enum):
    # Status bits
    TX_BUSY     = 0
    RX_READY    = 1
    # Control (ctrl) bits
    TX_START    = 0
    TX_REG_WE   = 1

# A basic UART transceiver module (start -- data_bits -- stop) - no parity bits
class UART(Elaboratable):
    def __init__(self, config:UartConfig):
        if type(config) is not UartConfig:
            raise ValueError(
                "[mipyfive - uart.py]: " +
                f"Incorrect config object type [ { type(config)} ] for Smol - expected [ {type(UartConfig)} ]"
            )
        self.config         = config

        assert config.target_baudrate > 0 and config.clk_frequency > 0 and config.data_bits > 0
        self.baudSampleRate = int((config.clk_frequency // ( 16 * config.target_baudrate)))
        if self.baudSampleRate == 0:
            print(
                f"[UART - Warning]: Sampling rate of ({config.clk_frequency / ( 16 * config.target_baudrate)}) " +
                 "will be rounded to 1, " +
                f"this might not be enough resolution for {config.clk_frequency} Hz and {config.target_baudrate} Baud."
            )
            self.baudSampleRate = 1

        self.tx_reg_i       = Signal(self.config.data_bits)
        self.tx_reg_we_i    = Signal()
        self.tx_start_i     = Signal()
        self.tx_o           = Signal()
        self.tx_busy_o      = Signal()

        self.rx_i           = Signal()
        self.rx_ready_o     = Signal()
        self.rx_reg_o       = Signal(self.config.data_bits)

    def elaborate(self, platform):
        m = Module()

        # Define shift regs
        shiftSize = (1 + self.config.data_bits + 1)
        self.rxShift = Signal(shiftSize)
        self.txShift = Signal(shiftSize)

        # Create a modulus counter for clk dividing, a sample counter, and a counter for bit progress
        rxSampleTickCounter = Signal(ceilLog2(self.baudSampleRate))
        txSampleTickCounter = Signal(ceilLog2(self.baudSampleRate))
        rxSampleCounter     = Signal(ceilLog2(15))
        txSampleCounter     = Signal(ceilLog2(15))
        rxBitCounter        = Signal(ceilLog2(shiftSize))
        txBitCounter        = Signal(ceilLog2(shiftSize))

        txReg = Signal(self.config.data_bits)
        m.d.comb += self.tx_o.eq(self.txShift[0])
        with m.If(self.tx_reg_we_i):
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
                            rxBitCounter.eq(self.config.data_bits),
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
                        self.tx_busy_o.eq(1),
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
                            txBitCounter.eq(self.config.data_bits),
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
                            self.tx_busy_o.eq(0),
                            self.txShift.eq(Cat(self.txShift[1:], 1)),
                            txSampleTickCounter.eq(self.baudSampleRate)
                        ]
                    with m.Else():
                        m.d.sync += txSampleCounter.eq(txSampleCounter - 1)
                with m.Else():
                    m.d.sync += txSampleTickCounter.eq(txSampleTickCounter - 1)

        return m

# Module that wraps around the UART module - adds memory-mapped regs for interaction
# TODO: Test this in simulation later
class UARTcontroller(Elaboratable):
    def __init__(self, addrWidth=32, dataWidth=32, config:UartConfig=None):
        if type(config) is not UartConfig:
            raise ValueError(
                "[mipyfive - uart.py]: " +
                f"Incorrect config object type [ { type(config)} ] for Smol - expected [ {type(UartConfig)} ]"
            )
        self.config         = config
        self.dataWidth      = dataWidth
        self.addrWidth      = addrWidth

        self.writeEnable    = Signal()
        self.addrIn         = Signal(addrWidth)
        self.dataIn         = Signal(dataWidth)
        self.dataOut        = Signal(dataWidth)
        self.tx             = Signal()
        self.rx             = Signal()

        # UART submodule
        self.uart           = UART(config=config)

    def elaborate(self, platform):
        m = Module()

        # Peripheral regs
        rxReg       = Signal(self.dataWidth)    # READ-ONLY reg
        txReg       = Signal(self.dataWidth)
        statusReg   = Signal(self.dataWidth)    # READ-ONLY reg
        ctrlReg     = Signal(self.dataWidth)

        m.submodules.uart = self.uart
        m.d.comb += [
            self.uart.rx_i.eq(self.rx),
            self.uart.tx_start_i.eq(ctrlReg[UartControllerBits.TX_START.value]),
            self.uart.tx_reg_we_i.eq(ctrlReg[UartControllerBits.TX_REG_WE.value]),
            self.uart.tx_reg_i.eq(txReg)
        ]
        # UART output assignments
        m.d.sync += [
            statusReg.eq(
                Cat(
                    self.uart.tx_busy_o, self.uart.rx_ready_o,
                    Repl(C(0), (self.dataWidth-2))
                )
            )
        ]

        m.d.comb += self.tx.eq(self.uart.tx_o)

        # Address decode logic
        rxSel       = ~self.addrIn[4] & ~self.addrIn[3]
        txSel       = ~self.addrIn[4] &  self.addrIn[3]
        statusSel   =  self.addrIn[4] & ~self.addrIn[3]
        ctrlSel     =  self.addrIn[4] &  self.addrIn[3]

        # Read-logic
        with m.If(rxSel):
            m.d.comb += self.dataOut.eq(rxReg)
        with m.Elif(txSel):
            m.d.comb += self.dataOut.eq(txReg)
        with m.Elif(statusSel):
            m.d.comb += self.dataOut.eq(statusReg)
        with m.Elif(ctrlSel):
            m.d.comb += self.dataOut.eq(ctrlReg)
        with m.Else():
            m.d.comb += self.dataOut.eq(rxReg)

        # Write-logic
        with m.If(self.writeEnable):
            with m.If(txSel):
                m.d.sync += txReg.eq(self.dataIn)
            with m.Elif(ctrlSel):
                m.d.sync += ctrlReg.eq(self.dataIn)
            with m.Else():
                m.d.sync += [
                    txReg.eq(txReg),
                    ctrlReg.eq(ctrlReg),
                ]
        with m.Else():
            # Current control reg bits are one-shot, so clear them if they are not being written to
            m.d.sync += ctrlReg.eq(0)

        # Update the RX reg if its ready
        with m.If(statusReg[UartControllerBits.RX_READY.value]):
            m.d.sync += rxReg.eq(self.uart.rx_reg_o)

        return m

# --- Simulation ------------------------------------------------------------------------------------------------------
# Unit test main UART module
createVcd = False
outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "build", "vcd"))
def test_tx_uart(packet):
    def test(self):
        print(f"Sending packet: [ {chr(packet)} ]")
        sim = Simulator(self.dut)
        def process():
            # Idle
            yield self.dut.rx_i.eq(1)
            yield self.dut.tx_start_i.eq(0)
            yield self.dut.tx_reg_i.eq(packet)
            yield self.dut.tx_reg_we_i.eq(1)
            yield Delay(5e-6)
            # Start bit
            yield self.dut.tx_start_i.eq(1)
            yield self.dut.tx_reg_we_i.eq(0)
            for j in range((self.dut.baudSampleRate+1) * 8):
                yield Tick()
            # Data bits
            yield self.dut.tx_start_i.eq(0)
            for i in range(self.dut.config.data_bits):
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
            for i in range(self.dut.config.data_bits):
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
        config = UartConfig(
            clk_frequency=10e6,
            target_baudrate=115200,
            data_bits=8
        )
        self.dut = UART(config=config)

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
