import os
import sys
import argparse

from nmigen import *
from nmigen.cli import main
from mipyfive.core import *
from mipyfive.types import *

if __name__ == "__main__":
    # Define args/opts
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument("-il", action="store_true", help="Emit design in RTLIL (Yosys) instead of Verilog.")
    parser.add_argument("--pcStart", dest="pcStart", default="0",
        help="PC start/reset value (Prefix value with '0x' for hex).")
    parser.add_argument("--buildCore", action="store_true", help="Build and output the main core")
    # TODO: Uncomment when extensions are available
    #parser.add_argument("--enableM", action="store_true", help="Enable the Multiply/Divide Extension")
    #parser.add_argument("--enableF", action="store_true", help="Enable the Single-Precision Floating Point Extension")
    args, unknown = parser.parse_known_args()

    if len(unknown) is not 0:
        print(f"[mipyfive - Error]: Unknown argument(s)/option(s):\n{unknown}\n")
        parser.print_help()

    isaString = "RV32I"
    isas = {
        "RV32I"     : CoreISAconfigs.RV32I.value,
        "RV32IF"    : CoreISAconfigs.RV32IM.value,
        "RV32IM"    : CoreISAconfigs.RV32IF.value,
        "RV32IFM"   : CoreISAconfigs.RV32IMF.value
    }
    isaConfig = isas[isaString]

    pcStart = 0
    if args.pcStart[:2] == "0x":
        pcStart = int(args.pcStart, 16)
    else:
        pcStart = int(args.pcStart)

    # Default generate type is Verilog - can be overriden to RTLIL (il)
    generateType = "v"
    if args.il is True:
        generateType = "il"

    # Ensure output dir exists
    outputDir = os.path.abspath(os.path.join(os.path.dirname(__file__), "out"))
    if not os.path.exists(outputDir):
        os.makedirs(outputDir)
    rtlFile = os.path.join(outputDir, f"top.{generateType}")

    # Override sys.argv for nMigen main
    nmigenMainArgs = [
        "generate",
        "-t", generateType,
        rtlFile
    ]
    sys.argv[1:] = nmigenMainArgs

    if not any([args.buildCore]):
        print("[mipyfive - Info]: No build target given - defaulting to [--buildCore].")
        args.buildCore = True

    # Generate core RTL
    if args.buildCore:
        print(f"[mipyfive - Info]: Generating RTL to --> {rtlFile}")
        m = MipyfiveCore(dataWidth=32, regCount=32, pcStart=pcStart, ISA=isaConfig)
        main(m, ports=[m.instruction, m.DataIn, m.PCout, m.DataAddr, m.DataOut, m.DataWE])
        print("[mipyfive - Info]: Done.")
