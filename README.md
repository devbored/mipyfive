# mipyfive
A RISC-V core - implemented via [nMigen](https://github.com/m-labs/nmigen)

## Design
- Implements RV32I
- 5 stage pipelined processor
- Static branch prediction (assume not-taken)

## Main dependencies
- Python 3.7 (or newer)
- Yosys 0.9 (or newer)
- nmigen

## Setup
```Bash
python3 ./setup.py install
```

## Building 🛠️
To generate just the RV32I mipyfive core:
```Bash
python3 ./cli.py --build
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
