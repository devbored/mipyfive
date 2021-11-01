from nmigen import *
from mipyfive.core import *
from mipyfive.utils import *

# Peripherals
from examples.common.uart import *
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

        self.dataWeImemOut  = Signal()
        self.dataWeDmemOut  = Signal()
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
        # | 0x00000000 ... 0x00000FFF | Internal IMEM (BRAM) - 4KB (read-only)  |
        # | 0x00001000 ... 0x000017FF | Internal DMEM (BRAM) - 2KB              |
        # | 0x00003000 ... 0x0000300B | UART transceiver                        |
        # | 0x00003010 ... 0x00003013 | Seven-segment display digit 0           |
        # | 0x00003020 ... 0x00003023 | Seven-segment display digit 1           |
        # ----------------------------------------------------------------------
        Imem    = ~self.dataAddrIn[13] & ~self.dataAddrIn[12]
        Dmem    = ~self.dataAddrIn[13] &  self.dataAddrIn[12]
        Uart    =  self.dataAddrIn[13] &  self.dataAddrIn[12] & ~self.dataAddrIn[5] & ~self.dataAddrIn[4]
        Sseg0   =  self.dataAddrIn[13] &  self.dataAddrIn[12] & ~self.dataAddrIn[5] &  self.dataAddrIn[4]
        Sseg1   =  self.dataAddrIn[13] &  self.dataAddrIn[12] &  self.dataAddrIn[5] & ~self.dataAddrIn[4]

        # Concatenated output to shorten the control code logic below
        outCat  = Signal(5)

        # Store logic
        with m.If(self.dataWeIn):
            with m.If(Imem):
                m.d.comb += outCat.eq(Cat(1,0,0,0,0))
            with m.Elif(Dmem):
                m.d.comb += outCat.eq(Cat(0,1,0,0,0))
            with m.Elif(Uart):
                m.d.comb += outCat.eq(Cat(0,0,1,0,0))
            with m.Elif(Sseg0):
                m.d.comb += outCat.eq(Cat(0,0,0,1,0))
            with m.Elif(Sseg1):
                m.d.comb += outCat.eq(Cat(0,0,0,0,1))
            with m.Else():
                m.d.comb += outCat.eq(Cat(0,0,0,0,0))
        # Expand
        m.d.comb += [
            self.dataWeImemOut.eq(outCat[0]),
            self.dataWeDmemOut.eq(outCat[1]),
            self.dataWeUartOut.eq(outCat[2]),
            self.dataWeSseg0Out.eq(outCat[3]),
            self.dataWeSseg1Out.eq(outCat[4])
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
    core_isa                : str = None
    core_data_width         : int = None
    core_reg_count          : int = None
    core_pc_start           : int = None
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
    def __init__(self, config:SmolConfig=None):
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
        self.core = MipyfiveCore(
            self.config.core_data_width,
            self.config.core_reg_count,
            self.config.core_pc_start,
            self.config.core_isa
        )
        self.mioc = MIOC(
            dataWidth   = 32
        )
        self.imem = RAM(
            width       = 32,
            depth       = 0x1000,
            init        = None,
            dualRead    = True
        )
        self.dmem = RAM(
            width       = 32,
            depth       = 0x800,
            init        = None,
            dualRead    = False
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
        m.submodules.imem        = self.imem
        m.submodules.dmem        = self.dmem
        m.submodules.uart        = self.uart
        m.submodules.sseg0       = self.sseg0
        m.submodules.sseg1       = self.sseg1

        m.d.comb += [
            # Soft core
            self.core.instruction.eq(self.imem.readData),
            self.core.IF_valid.eq(1),
            self.core.MEM_valid.eq(1),
            self.core.DataIn.eq(self.mioc.dataOut),
            # MIOC
            self.mioc.dataWeIn.eq(self.core.DataWE),
            self.mioc.dataAddrIn.eq(self.core.DataAddr),
            self.mioc.dataImemIn.eq(self.imem.readData2),
            self.mioc.dataDmemIn.eq(self.dmem.readData),
            self.mioc.dataUartIn.eq(self.uart.dataOut),
            self.mioc.dataSseg0In.eq(self.sseg0.dataOut),
            self.mioc.dataSseg1In.eq(self.sseg1.dataOut),
            # IMEM
            self.imem.writeEnable.eq(0),
            self.imem.readAddr.eq(self.core.PCout),
            self.imem.readAddr2.eq(self.core.DataAddr),
            self.imem.writeAddr.eq(0),
            # DMEM
            self.dmem.writeEnable.eq(self.mioc.dataWeDmemOut),
            self.dmem.readAddr.eq(self.core.DataAddr),
            self.dmem.writeAddr.eq(self.core.DataOut),
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
