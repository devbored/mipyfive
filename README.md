# mipyfive
Another RISC-V core - implemented via [nMigen](https://github.com/m-labs/nmigen)

🚧 *Work in Progress* 🚧
- See related main checklist items below

## Design
- Implements RV32I
- 5 stage pipelined processor
- Static branch prediction (assume not-taken)

## Main Checklist Items:
✅ Design the main mipyfive RV32I Core

✅ Design an example SoC that utilizes a mipyfive core (w/ some random peripherals)

✅ Unit Testing (via nMigen)

❌ Verify mipyfive core using example SoC on actual FPGA hardware

❌ Validate the main RISC-V RV32I Core (RISC-V Conformance Testing)

❌ Create/Finish initial documentation

## Extra Checklist Items
❌ Explore/Add RISC-V Extensions in the future (mainly multiply/divide and fp32)

## Main dependencies
- Python 3.7 (or newer)
- Yosys 0.9 (or newer)
    - Windows and Ubuntu 👉 http://www.clifford.at/yosys/download.html
    - macOS 👉 https://formulae.brew.sh/formula/yosys
- nmigen

## Setup
```Bash
python3 ./setup.py install
```

## Building/Generating the Mipyfive core 🛠️
To generate just the RV32I mipyfive core:
```Bash
python3 ./cli.py --buildCore
```
There is also a `-il` option that outputs as Yosys RTLIL instead.

## Smol 🤏
A small example SoC with an mipyfive (RV32I) soft-core.

🚧 *Work in Progress* 🚧
- Still need to simulate/test for correct functionality

## Features
- Embedded mipyfive (RV32I) soft-core
- Harvard architecture
    - Separate instruction (IMEM) and data (DMEM) memory buses
- MMIO and RAM are connected to a Memory/IO-bus and controller (MIOC)
- Peripherals:
    - A UART transceiver (RX/TX)
        - Adjustable baud rate and data packet width (adjusted at build-time, **not** run-time adjustable)
        - No parity bits
    - A basic 2-digit seven-segment driver module

## Memory map 🗺️
| Address Range             | Description                             |
| ------------------------- | --------------------------------------- |
| 0x00000000 ... 0x000001FF | Internal IMEM (BRAM) - 2KB (read-only)  |
| 0x00000200 ... 0x000003FF | Internal DMEM (BRAM) - 2KB              |
| 0x00003000 ... 0x00003003 | UART RX register                        |
| 0x00003004 ... 0x00003007 | UART TX register                        |
| 0x00003008 ... 0x0000300B | UART status register                    |
| 0x0000300C ... 0x0000300F | UART control register                   |
| 0x00003010 ... 0x00003013 | Seven-segment display digit 0 value     |
| 0x00003020 ... 0x00003023 | Seven-segment display digit 1 value     |

## Smol firmware dependencies
- CMake (v3.10 or higher)
- A RISC-V compiler/cross-compiler C toolchain (e.g. riscv64-unknown-elf-*)

## Building/Generating the smol SoC and firmware 🛠️
To generate the example smol SoC, make sure CMake is in your PATH then run:
```Bash
python3 ./cli.py --buildSmol
```
To use a different CMake genrator (i.e. Make, Ninja, etc.), you can specify the `CMAKE_GENERATOR` env variable.

Example using GNU Make:
```Bash
CMAKE_GENERATOR="Unix Makefiles" python3 ./cli.py -bs
```

## Testing 🧪
Make sure you have the `riscv-assembler` python package installed prior to running unit tests.

**NOTE**: You will likely need to use my fork of the `riscv-assembler` package rather than what's in PyPI:
```Bash
pip3 install git+https://github.com/devbored/riscv-assembler.git
```

To run all the mipyfive unit tests:
```Bash
python3 ./cli.py -t
```

Individual unit tests can be ran by passing the test name(s) via the `-t` nargs option.

Example - Running test_alu and test_core:
```Bash
python3 ./cli.py -t alu core
```

To dump the VCD file(s) of the test(s) add the `-vcd` option. You can also change the unittest verbosity
level by using the `-tv <int>` flag.

Any generated VCD file is located in `build/vcd`.
