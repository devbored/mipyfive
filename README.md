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

## Dependencies
- Python 3.7 (or newer)
- Yosys 0.9 (or newer)
    - Windows and Ubuntu 👉 http://www.clifford.at/yosys/download.html
    - macOS 👉 https://formulae.brew.sh/formula/yosys
- nmigen
    - Installation 👉 [Link](https://github.com/m-labs/nmigen#installation)

## Building/Generating the hardware 🛠️
The main utility that drives almost everything is the `cli.py` script.
`cli.py` uses the argparse module for options, so you can `-h` for help and more info.

To generate just the RV32I mipyfive core:
```Bash
python3 ./cli.py --buildCore
```

To generate the example `smol` SoC:
```Bash
python3 ./cli.py --buildSmol
```
Readme for smol is 👉 [here](./examples/smol)

Both cases generate Verilog code of the designs; there is also a `-il` option that
outputs as Yosys `RTLIL` instead.

Generated files are located in the `out/` folder.

## Testing 🧪
Make sure you have the `riscv-assembler` python package installed prior to running unit tests.

**NOTE**: You will likely need to use my fork of the `riscv-assembler` package rather than what's in Pip:
```Bash
git clone https://github.com/devbored/riscv-assembler.git
cd ./riscv-assembler
python3 ./setup.py install
```

To run all the mipyfive unit tests:
```Bash
python3 ./cli.py -t
```

Individual unit tests can be ran by running from the `tests/` folder. Example:
```Bash
python3 ./tests/test_alu.py
```
You can specify the help option `-h` for each sub-test to see what options are available
(mainly just dumping the VCD file and changing test verbosity).
