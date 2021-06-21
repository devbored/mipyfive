from nmigen import *
from .types import *

class ImmGen(Elaboratable):
    def __init__(self):
        self.instruction        = Signal(32)
        self.imm                = Signal(32)

    def elaborate(self, platform):
        m = Module()

        # NOTE: Python slice notation for [a:b] == [a:(b-1)]
        # --- I-types (that need sign-extension) ---
        with m.Switch(self.instruction[0:7]):
            with m.Case(Rv32iTypes.I_Jump.value, Rv32iTypes.I_Load.value, Rv32iTypes.I_Arith):
                m.d.comb += self.imm.eq(
                    Cat(
                        self.instruction[20:31],
                        Repl(self.instruction[31], 21)
                    )
                )

            # --- S-type ---
            with m.Case(Rv32iTypes.S.value):
                m.d.comb += self.imm.eq( 
                    Cat(
                        self.instruction[7:12],
                        self.instruction[25:31],
                        Repl(self.instruction[31], 21)
                    )
                )

            # --- B-type ---
            with m.Case(Rv32iTypes.B.value):
                m.d.comb += self.imm.eq(
                    Cat(
                        C(0),
                        self.instruction[8:12],
                        self.instruction[25:31],
                        self.instruction[7],
                        Repl(self.instruction[31], 20)
                    )
                )

            # --- U-type ---
            with m.Case(Rv32iTypes.U_Add.value, Rv32iTypes.U_Load.value):
                m.d.comb += self.imm.eq(
                    Cat(
                        Repl(C(0), 12),
                        self.instruction[12:32]
                    )
                )

            # --- J-type ---
            with m.Case(Rv32iTypes.J.value):
                m.d.comb += self.imm.eq(
                    Cat(
                        C(0),
                        self.instruction[21:25],
                        self.instruction[25:31],
                        self.instruction[20],
                        self.instruction[12:20],
                        Repl(self.instruction[31], 12)
                    )
                )

        return m