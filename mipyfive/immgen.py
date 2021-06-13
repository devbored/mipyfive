from nmigen import *

class ImmGen(Elaboratable):
    def __init__(self):
        self.instruction        = Signal(32)
        self.imm                = Signal(32)
        self.opcodeTypeTable    = Array()

    def elaborate(self, platform):
        m = Module()

        # NOTE: Python slice notation for --> [a:b] == a to (b-1)
        # --- I-type ---
        with m.Switch(self.instruction[0:7]):
            with m.Case(0b0000011):
                m.d.comb += self.imm.eq(
                    Cat(
                        self.instruction[20:31],
                        Repl(self.instruction[31], 21)
                    )
                )
            with m.Case(0b0001111):
                m.d.comb += self.imm.eq(
                    Cat(
                        self.instruction[20:31],
                        Repl(self.instruction[31], 21)
                    )
                )
            with m.Case(0b0010011):
                m.d.comb += self.imm.eq(
                    Cat(
                        self.instruction[20:31],
                        Repl(self.instruction[31], 21)
                    )
                )
            with m.Case(0b1110011):
                m.d.comb += self.imm.eq(
                    Cat(
                        self.instruction[20:31],
                        Repl(self.instruction[31], 21)
                    )
                )
            
            # --- S-type ---
            with m.Case(0b0100011):
                m.d.comb += self.imm.eq( 
                    Cat(
                        self.instruction[7:12],
                        self.instruction[25:31],
                        Repl(self.instruction[31], 21)
                    )
                )
            
            # --- B-type ---
            with m.Case(0b1100011):
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
            with m.Case(0b0010111):
                m.d.comb += self.imm.eq(
                    Cat(
                        Repl(C(0), 12),
                        self.instruction[12:32]
                    )
                )
            with m.Case(0b0110111):
                m.d.comb += self.imm.eq(
                    Cat(
                        Repl(C(0), 12),
                        self.instruction[12:32]
                    )
                )

            # --- J-type ---
            with m.Case(0b1100111):
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
            with m.Case(0b1101111):
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