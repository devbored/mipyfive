# smol
A small example SoC with an mipyfive (RV32I) soft-core.

🚧 *Work in Progress* 🚧
- Still need to simulate/test for correct functionality

## Features
- Embedded mipyfive (RV32I) soft-core
- Harvard architecture
    - Separate instruction (IMEM) and data (DMEM) memories
    - IMEM is dual-ported read so that instruction fetch and data read from IMEM can happen simultaneously
- MMIO, DMEM, and IMEM are connected to a Memory/IO-bus and controller (MIOC)
- Peripherals:
    - A UART transceiver (RX/TX)
        - Adjustable baud rate and data packet width (adjusted at build-time, **not** run-time adjustable)
        - No parity bits
    - A basic 2-digit seven-segment driver module

## Memory map 🗺️
| Address Range             | Description                             |
| ------------------------- | --------------------------------------- |
| 0x00000000 ... 0x000003FF | Internal IMEM (BRAM) - 1KB (read-only)  |
| 0x00000400 ... 0x000007FF | Internal DMEM (BRAM) - 1KB              |
| 0x00003000 ... 0x00003003 | UART RX register                        |
| 0x00003004 ... 0x00003007 | UART TX register                        |
| 0x00003008 ... 0x0000300B | UART status register                    |
| 0x0000300C ... 0x0000300F | UART control register                   |
| 0x00003010 ... 0x00003013 | Seven-segment display digit 0 value     |
| 0x00003020 ... 0x00003023 | Seven-segment display digit 1 value     |
