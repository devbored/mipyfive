# mipyfive
Another RISC-V core - implemented via [nMigen](https://github.com/m-labs/nmigen)

🚧 *Work in Progress* 🚧
Repo is subject to minor and major changes (See related main checklist items below).

## Design
- Implements RV32I
- 5 stage pipelined processor
- Static branch prediction (assume not-taken)

## Main Checklist Items:
✅ Design the main mipyfive RV32I Core

✅ Unit Testing (via nMigen)

❌ Validate the main RISC-V RV32I Core (RISC-V Conformance Testing)

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

## Building 🛠️
To generate just the RV32I mipyfive core:
```Bash
python3 ./cli.py --buildCore
```
There is also a `-il` option that outputs as Yosys RTLIL instead.

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
