#!/usr/bin/env python3

import os
import re
import sys
import glob
import argparse
import subprocess

from amaranth import *
from amaranth.cli import main
from mipyfive.core import *
from mipyfive.types import *

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
    print("=" * 70)
    for test in args.runTests:
        if test not in testDict:
            print(f"[mipyfive - Error]: Unknown test name: [ {test} ].")
            print(f"\nList of available unit tests: {testNames}")
            exit(1)
        testFile = testDict[test]
        if args.testVerbosity > 0:
            print(f"\n[mipyfive - Running test_{test}]:\n")
        testCmd = f"{sys.executable} {testFile} -v {args.testVerbosity}"
        if args.vcd:
            testCmd += " --vcd"
        subproc = subprocess.run(testCmd.split())
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
    rtlFile = os.path.join(outputDir, f"top.{generateType}")
    print(f"[mipyfive - Info]: Generating mipyfive core to: [ {rtlFile} ]")

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
        if field == "core_isa":
            value = str(getattr(config, field)).split('.')[1]
        else:
            value = getattr(config, field)
        print(f"{field:<20}: {value}")
    print("=" * 70)
    print()
    m = MipyfiveCore(config=config)

    # Override sys.argv for amaranth main and run
    amaranthMainArgs = [
        "generate",
        "-t", generateType,
        rtlFile
    ]
    sys.argv[1:] = amaranthMainArgs
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
    print("[mipyfive - Info]: Done.")

if __name__ == "__main__":
    # Define args/opts
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument("-il", action="store_true", help="Emit design in RTLIL (Yosys) instead of Verilog.")
    parser.add_argument("-pc", dest="pcStart", default="0",
        help="PC start/reset value (Prefix value with '0x' for hex).")
    parser.add_argument("-b", "--build", action="store_true", help="Build and output the mipyfive core")
    parser.add_argument("-t", nargs="*", dest="runTests", help="Run the unit test(s) and exit")
    parser.add_argument("-tv", type=int, dest="testVerbosity", default=2,
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
    if args.build:
        buildCore(args)
