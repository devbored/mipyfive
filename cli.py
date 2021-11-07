import os
import re
import sys
import glob
import argparse
import subprocess

from nmigen import *
from nmigen.cli import main
from mipyfive.core import *
from mipyfive.types import *
from examples.smol.smol import *

outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "build"))

def runTests(args):
    ''' Helper function to run the unit tests '''
    testDir     = os.path.abspath(os.path.join(os.path.dirname(__file__), "tests"))
    tests       = glob.glob(f"{testDir}/test_*.py")
    testNames   = [os.path.splitext(os.path.basename(x))[0].split("_")[1] for x in tests]
    testDict    = {testNames[x]: tests[x] for x in range(len(testNames))}
    testCount   = 0
    failedTests = []
    # If no tests are specified, then run all of them
    if args.runTests is not None and not args.runTests:
        args.runTests = testNames

    # Run
    print("[mipyfive - Info]: Running unit tests...")
    for test in args.runTests:
        if test not in testDict:
            print(f"[mipyfive - Error]: Unknown test name: [ {test} ].")
            print(f"\nList of available unit tests: {testNames}")
            exit(1)
        testFile = testDict[test]
        print(f"\n[mipyfive - Running test_{test}]:")
        testCmd = f"{sys.executable} {testFile} -v {args.testVerbosity}"
        if args.vcd:
            testCmd += " --vcd"
        subproc = subprocess.run(testCmd)
        if subproc.returncode != 0:
            failedTests.append(test)
        testCount += 1

    # Results
    print()
    print("=" * 70)
    print(f"Summary:")
    print("=" * 70)
    print(f"PASSED: {testCount - len(failedTests)}")
    print(f"FAILED: {len(failedTests)}")
    if len(failedTests) > 0:
        print(f"\nFailed tests:\n{failedTests}")
    print("=" * 70)
    print()

def buildCore(args):
    ''' Helper function to build the Mipyfive core '''
    isaConfig       = ISAtable["RV32I"] # TODO: Once more ISA types are available, pull from "args" instead
    pcStart         = int(args.pcStart, 16) if args.pcStart[:2] == "0x" else int(args.pcStart)
    generateType    = "il" if args.il is True else "v" # Verilog vs. RTLIL

    # Ensure output dir exists
    if not os.path.exists(outputDir):
        os.makedirs(outputDir)
    rtlFile = os.path.join(outputDir, f"mipyfive_core.{generateType}")
    print("=" * 70)
    print(f"[mipyfive - Info]: Generating mipyfive core to: {rtlFile}")

    # Config
    config = MipyfiveConfig(
            core_isa        = isaConfig,
            core_data_width = 32,
            core_reg_count  = 32,
            core_pc_start   = pcStart
    )
    print(f"\nMipyfive core configuration:")
    print("=" * 70)
    for field in config.__dataclass_fields__:
        value = getattr(config, field)
        print(f"{field}: {value}")
    print("=" * 70)
    print()
    m = MipyfiveCore(config=config)

    # Override sys.argv for nMigen main and run
    nmigenMainArgs = [
        "generate",
        "-t", generateType,
        rtlFile
    ]
    sys.argv[1:] = nmigenMainArgs
    main(m, ports=[
        m.instruction,
        m.DataIn,
        m.IF_valid,
        m.MEM_valid,
        m.PCout,
        m.DataAddr,
        m.DataOut,
        m.DataWE
    ])
    print("\n[mipyfive - Info]: Done.")
    print("=" * 70)
    print()

def buildSmol(args):
    ''' Helper function to build the example Smol SoC w/ Mipyfive soft-core'''
    isaConfig       = ISAtable["RV32I"] # TODO: Once more ISA types are available, pull from "args" instead
    pcStart         = int(args.pcStart, 16) if args.pcStart[:2] == "0x" else int(args.pcStart)
    generateType    = "il" if args.il is True else "v" # Verilog vs. RTLIL

    # Ensure output dir exists
    if not os.path.exists(outputDir):
        os.makedirs(outputDir)
    rtlFile = os.path.join(outputDir, f"smol.{generateType}")
    print("=" * 70)
    print(f"[mipyfive - Info]: Generating smol SoC to: {rtlFile}")

    # Config
    mp5Config = MipyfiveConfig(
        core_isa        = isaConfig,
        core_data_width = 32,
        core_reg_count  = 32,
        core_pc_start   = pcStart
    )
    smolConfig = SmolConfig(
        # Core
        core_config             = mp5Config,
        # UART controller
        uart_addr_width         = 32,
        uart_data_width         = 32,
        uart_config             = UartConfig(
            clk_frequency           = 10e6,
            target_baudrate         = 115200,
            data_bits               = 8,
        ),
        # SSEG Digit-0
        sseg0_is_common_anode   = False,
        sseg0_data_width        = 32,
        # SSEG Digit-1
        sseg1_is_common_anode   = False,
        sseg1_data_width        = 32
    )
    print(f"\nSmol configuration:")
    print("=" * 70)
    for field in smolConfig.__dataclass_fields__:
        value = getattr(smolConfig, field)
        print(f"{field}: {value}")
    print("=" * 70)
    print()
    m = Smol(mp5Config=mp5Config, config=smolConfig)

    # Override sys.argv for nMigen main and run
    nmigenMainArgs = [
        "generate",
        "-t", generateType,
        rtlFile
    ]
    sys.argv[1:] = nmigenMainArgs
    main(m, ports=[
        # Input(s)
        m.uart_tx,
        # Output(s)
        m.uart_rx,
        m.sseg0_out,
        m.sseg1_out
    ])

    # Gross ... but can't seem to elicit "$readmemh(...)" in nmigen
    if args.il is False:
        with open(rtlFile, 'r') as file:
            rtlFileStr = file.read()
            # Clear-out the assignments
            rtlFileStr = re.sub(
                r'\s*SMOL_RAM\[\d*\].*',
                '',
                rtlFileStr
            )
            # Add-in the $readmemh(...)
            rtlFileStr = re.sub(
                r'\s*initial begin\n\s*end',
                '\n  initial begin\n    $readmemh("smol_ram_init.hex", SMOL_RAM);\n  end',
                rtlFileStr
            )
        with open(rtlFile, 'w') as file:
            file.write(rtlFileStr)

    # Compile firmware and convert to a readmemh-accepted hex file
    print("[mipyfive - Info]: Compiling smol_firmware.c")
    subprocess.run("cmake . -Bbuild")       # Configure
    subprocess.run("cmake --build build")   # Build

    print("\n[mipyfive - Info]: Done.")
    print("=" * 70)
    print()

if __name__ == "__main__":
    # Define args/opts
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument("-il", action="store_true", help="Emit design in RTLIL (Yosys) instead of Verilog.")
    parser.add_argument("--pcStart", "-pc", dest="pcStart", default="0",
        help="PC start/reset value (Prefix value with '0x' for hex).")
    parser.add_argument("--buildCore", "-bc", action="store_true", help="Build and output the main core")
    parser.add_argument("--buildSmol", "-bs", action="store_true", help="Build and output the smol SoC example")
    parser.add_argument("--runTests", "-t", nargs="*", dest="runTests", help="Run the unit test(s) and exit")
    parser.add_argument("--testVerbosity", "-tv", type=int, dest="testVerbosity", default=2,
        help="Unittest verbosity [0 - 3]")
    parser.add_argument("--vcd", action="store_true", help="Dump VCD file of test(s)")
    args, unknown = parser.parse_known_args()

    if not len(sys.argv) > 1:
        parser.print_help()
        exit(0)

    if len(unknown) != 0:
        print(f"[mipyfive - Error]: Unknown argument(s)/option(s):\n{unknown}\n")
        parser.print_help()
        exit(0)

    # A convienence test-runner wrapper
    if args.runTests is not None:
        runTests(args)

    # Generate core RTL
    if args.buildCore:
        buildCore(args)

    # Example SoC
    if args.buildSmol:
        buildSmol(args)
