import os
import sys
import glob
import argparse

from nmigen import *
from nmigen.cli import main
from mipyfive.core import *
from mipyfive.types import *
from examples.smol.smol import *

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

    # Unit test vars
    testDir     = os.path.abspath(os.path.join(os.path.dirname(__file__), "tests"))
    tests       = glob.glob(f"{testDir}/test_*.py")
    testNames   = [os.path.splitext(os.path.basename(x))[0].split("_")[1] for x in tests]
    testDict    = {testNames[x]: tests[x] for x in range(len(testNames))}
    # If no tests are specified, then run all of them
    if args.runTests is not None and not args.runTests:
        args.runTests = testNames

    isaString = "RV32I"
    isas = {
        "RV32I"     : CoreISAconfigs.RV32I,
        "RV32IF"    : CoreISAconfigs.RV32IM,
        "RV32IM"    : CoreISAconfigs.RV32IF,
        "RV32IFM"   : CoreISAconfigs.RV32IMF
    }
    isaConfig = isas[isaString]

    pcStart     = 0
    if args.pcStart[:2] == "0x":
        pcStart = int(args.pcStart, 16)
    else:
        pcStart = int(args.pcStart)

    # Default generate type is Verilog - can be overriden to RTLIL (il)
    generateType = "v"
    if args.il is True:
        generateType = "il"

    # A convienence test-runner wrapper
    if args.runTests:
        testCount = 0
        failedTests = []
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
            ret = os.system(testCmd)
            if ret != 0:
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

    # Generate core RTL
    if args.buildCore:
        # Ensure output dir exists
        outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "out"))
        if not os.path.exists(outputDir):
            os.makedirs(outputDir)

        rtlFile = os.path.join(outputDir, f"mipyfive_core.{generateType}")
        print(f"[mipyfive - Info]: Generating mipyfive core RTL to --> {rtlFile}")

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
        # Override sys.argv for nMigen main
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
        print("[mipyfive - Info]: Done.")

    # Example SoC
    if args.buildSmol:
        # Ensure output dir exists
        outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "out"))
        if not os.path.exists(outputDir):
            os.makedirs(outputDir)

        rtlFile = os.path.join(outputDir, f"smol.{generateType}")
        print(f"[mipyfive - Info]: Generating smol SoC RTL to --> {rtlFile}")

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

        # Override sys.argv for nMigen main
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
        print("[mipyfive - Info]: Done.")
