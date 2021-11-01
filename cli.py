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
    parser.add_argument("--runTests", "-t", action="store_true", help="Run the unit tests and exit")
    args, unknown = parser.parse_known_args()

    if not len(sys.argv) > 1:
        parser.print_help()
        exit(0)

    if len(unknown) != 0:
        print(f"[mipyfive - Error]: Unknown argument(s)/option(s):\n{unknown}\n")
        parser.print_help()
        exit(0)

    isaString = "RV32I"
    isas = {
        "RV32I"     : CoreISAconfigs.RV32I.value,
        "RV32IF"    : CoreISAconfigs.RV32IM.value,
        "RV32IM"    : CoreISAconfigs.RV32IF.value,
        "RV32IFM"   : CoreISAconfigs.RV32IMF.value
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

    # A convienence test-runner to go over all tests
    if args.runTests:
        testDir     = os.path.abspath(os.path.join(os.path.dirname(__file__), "tests"))
        tests       = glob.glob(f"{testDir}/test_*.py")
        testNames   = [os.path.splitext(os.path.basename(x))[0] for x in tests]
        testDict    = {testNames[x]: tests[x] for x in range(len(testNames))}

        testCount = 0
        failedTests = []
        print("[mipyfive - Info]: Running unit tests...")
        for testName, testPath in testDict.items():
            print(f"\n[ {testName} ]")
            ret = os.system(f"{sys.executable} {testPath} -v 0")
            if ret != 0:
                failedTests.append(testName)
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
        m = MipyfiveCore(
            dataWidth       = 32,
            regCount        = 32,
            pcStart         = pcStart,
            ISA             = isaConfig
        )
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

        config = SmolConfig(
            # Core
            core_isa                = isaConfig,
            core_data_width         = 32,
            core_reg_count          = 32,
            core_pc_start           = pcStart,
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
        print(f"ISA IDs: {isas}")
        print(f"\nConfiguration:\n{config}\n")
        m = Smol(config=config)

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
