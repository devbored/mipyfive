import os
import sys
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__))))
import argparse

from nmigen import *
from nmigen.cli import main

from mipyfive.alu import *
from mipyfive.lsu import *
from mipyfive.utils import *
from mipyfive.types import *
from mipyfive.immgen import *
from mipyfive.hazard import *
from mipyfive.compare import *
from mipyfive.forward import *
from mipyfive.pipereg import *
from mipyfive.regfile import *
from mipyfive.controller import *
from mipyfive.core import *

if __name__ == "__main__":
    # Define args
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument("-il", action="store_true", help="Emit design in RTLIL (Yosys) instead of Verilog.")
    args, unknown = parser.parse_known_args()
    
    if len(unknown) is not 0:
        print(f"Error. Unknown argument(s)/option(s):\n{unknown}\n")
        parser.print_help()
    
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

    # Generate RTL
    print(f"[mipyfive]: Generating RTL to --> {rtlFile}")
    m = MipyfiveCore(dataWidth=32, regCount=32)
    main(m, ports=[m.instruction, m.DataIn, m.PCout, m.DataAddr, m.DataOut])
    print("[mipyfive]: Done.")
