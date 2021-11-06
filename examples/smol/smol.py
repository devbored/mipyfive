from nmigen import *
from mipyfive.core import *
from mipyfive.utils import *

# Peripherals
from examples.common.uart import UartConfig
from examples.common.uart import UARTcontroller
from examples.common.ssegdriver import *

from dataclasses import dataclass

# Memory/IO-bus controller for Smol
class MIOC(Elaboratable):
    def __init__(self, dataWidth):
        self.dataWidth      = dataWidth

        self.dataWeIn       = Signal()
        self.dataAddrIn     = Signal(dataWidth)
        self.dataImemIn     = Signal(dataWidth)
        self.dataDmemIn     = Signal(dataWidth)
        self.dataUartIn     = Signal(dataWidth)
        self.dataSseg0In    = Signal(dataWidth)
        self.dataSseg1In    = Signal(dataWidth)

        self.dataWeRAMOut   = Signal()
        self.dataWeUartOut  = Signal()
        self.dataWeSseg0Out = Signal()
        self.dataWeSseg1Out = Signal()
        self.dataOut        = Signal(dataWidth)

    def elaborate(self, platform):
        m = Module()

        # Address decoding logic
        # ----------------------------------------------------------------------
        # | Address Range             | Description                             |
        # | ------------------------- | --------------------------------------- |
        # | 0x00000000 ... 0x000001FF | Internal IMEM (BRAM) - 2KB (read-only)  |
        # | 0x00000200 ... 0x000003FF | Internal DMEM (BRAM) - 2KB              |
        # | 0x00003000 ... 0x00003003 | UART RX register                        |
        # | 0x00003004 ... 0x00003007 | UART TX register                        |
        # | 0x00003008 ... 0x0000300B | UART status register                    |
        # | 0x0000300C ... 0x0000300F | UART control register                   |
        # | 0x00003010 ... 0x00003013 | Seven-segment display digit 0 value     |
        # | 0x00003020 ... 0x00003023 | Seven-segment display digit 1 value     |
        # ----------------------------------------------------------------------
        Imem    = ~self.dataAddrIn[12] & ~self.dataAddrIn[9]
        Dmem    = ~self.dataAddrIn[12] &  self.dataAddrIn[9]
        Uart    =  self.dataAddrIn[12] &  self.dataAddrIn[9] & ~self.dataAddrIn[5] & ~self.dataAddrIn[4]
        Sseg0   =  self.dataAddrIn[12] &  self.dataAddrIn[9] & ~self.dataAddrIn[5] &  self.dataAddrIn[4]
        Sseg1   =  self.dataAddrIn[12] &  self.dataAddrIn[9] &  self.dataAddrIn[5] & ~self.dataAddrIn[4]

        # Concatenated output to shorten the control code logic below
        outCat  = Signal(4)

        # Store logic
        with m.If(self.dataWeIn):
            with m.If(Imem):
                m.d.comb += outCat.eq(Cat(0,0,0,0))
            with m.Elif(Dmem):
                m.d.comb += outCat.eq(Cat(1,0,0,0))
            with m.Elif(Uart):
                m.d.comb += outCat.eq(Cat(0,1,0,0))
            with m.Elif(Sseg0):
                m.d.comb += outCat.eq(Cat(0,0,1,0))
            with m.Elif(Sseg1):
                m.d.comb += outCat.eq(Cat(0,0,0,1))
            with m.Else():
                m.d.comb += outCat.eq(Cat(0,0,0,0))
        # Expand
        m.d.comb += [
            self.dataWeRAMOut.eq(outCat[0]),
            self.dataWeUartOut.eq(outCat[1]),
            self.dataWeSseg0Out.eq(outCat[2]),
            self.dataWeSseg1Out.eq(outCat[3])
        ]

        # Load logic
        with m.If(Imem):
            m.d.comb += self.dataOut.eq(self.dataImemIn)
        with m.Elif(Dmem):
            m.d.comb += self.dataOut.eq(self.dataDmemIn)
        with m.Elif(Uart):
            m.d.comb += self.dataOut.eq(self.dataUartIn)
        with m.Elif(Sseg0):
            m.d.comb += self.dataOut.eq(self.dataSseg0In)
        with m.Elif(Sseg1):
            m.d.comb += self.dataOut.eq(self.dataSseg1In)

        return m

@dataclass
class SmolConfig:
    # Soft-core configs
    core_config             : MipyfiveConfig = None
    # UART controller
    uart_addr_width         : int = None
    uart_data_width         : int = None
    uart_config             : UartConfig = None
    # Seven-segment0 driver
    sseg0_is_common_anode   : bool = False
    sseg0_data_width        : int = None
    # Seven-segment1 driver
    sseg1_is_common_anode   : bool = False
    sseg1_data_width        : int = None

# A small example SoC with an mipyfive (RV32I) soft-core.
class Smol(Elaboratable):
    def __init__(self, mp5Config:MipyfiveConfig=None, config:SmolConfig=None):
        if type(config) is not SmolConfig:
            raise ValueError(
                "[mipyfive - smol.py]: " +
                f"Incorrect config object type [ { type(config)} ] for Smol - expected [ {type(SmolConfig)} ]"
            )
        self.config = config

        # Smol module I/O
        self.uart_tx    = Signal()
        self.uart_rx    = Signal()
        self.sseg0_out  = Signal(7)
        self.sseg1_out  = Signal(7)

        # Smol submodules
        self.core = MipyfiveCore(config=mp5Config)
        self.mioc = MIOC(
            dataWidth   = 32
        )
        self.RAM = RAM(
            width       = 32,
            depth       = 0x400, # 4KB (1024 x 32bit)
            dualRead    = True,
            name        = "SMOL_RAM"
        )
        self.uart = UARTcontroller(
            addrWidth=self.config.uart_addr_width,
            dataWidth=self.config.uart_data_width,
            config=self.config.uart_config
        )
        self.sseg0 = SSEGdriver(
            self.config.sseg0_is_common_anode,
            self.config.sseg0_data_width
        )
        self.sseg1 = SSEGdriver(
            self.config.sseg1_is_common_anode,
            self.config.sseg1_data_width
        )

    def elaborate(self, platform):
        m = Module()

        # Instantiate Submodules
        m.submodules.core        = self.core
        m.submodules.mioc        = self.mioc
        m.submodules.RAM         = self.RAM
        m.submodules.uart        = self.uart
        m.submodules.sseg0       = self.sseg0
        m.submodules.sseg1       = self.sseg1

        m.d.comb += [
            # Soft core
            self.core.instruction.eq(self.RAM.readData),
            self.core.IF_valid.eq(1),
            self.core.MEM_valid.eq(1),
            self.core.DataIn.eq(self.mioc.dataOut),
            # MIOC
            self.mioc.dataWeIn.eq(self.core.DataWE),
            self.mioc.dataAddrIn.eq(self.core.DataAddr),
            self.mioc.dataImemIn.eq(self.RAM.readData),
            self.mioc.dataDmemIn.eq(self.RAM.readData2),
            self.mioc.dataUartIn.eq(self.uart.dataOut),
            self.mioc.dataSseg0In.eq(self.sseg0.dataOut),
            self.mioc.dataSseg1In.eq(self.sseg1.dataOut),
            # RAM
            self.RAM.writeEnable.eq(self.mioc.dataWeRAMOut),
            self.RAM.readAddr.eq(self.core.PCout),
            self.RAM.readAddr2.eq(self.core.DataAddr),
            self.RAM.writeAddr.eq(self.core.DataAddr),
            self.RAM.writeData.eq(self.core.DataOut),
            # UART
            self.uart.writeEnable.eq(self.mioc.dataWeUartOut),
            self.uart.addrIn.eq(self.core.DataAddr),
            self.uart.dataIn.eq(self.core.DataOut),
            self.uart.rx.eq(self.uart_rx),
            # SSEG0
            self.sseg0.writeEnable.eq(self.mioc.dataWeSseg0Out),
            self.sseg0.dataIn.eq(self.core.DataOut),
            # SSEG1
            self.sseg1.writeEnable.eq(self.mioc.dataWeSseg1Out),
            self.sseg1.dataIn.eq(self.core.DataOut)
        ]

        # Peripheral outputs
        m.d.comb += [
            self.uart_tx.eq(self.uart.tx),
            self.sseg0_out.eq(self.sseg0.valueOut),
            self.sseg1_out.eq(self.sseg1.valueOut),
        ]

        return m
