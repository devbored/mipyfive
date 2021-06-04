# TODO: Implement this...

# Formal assertion for adding
#with m.If(alu.aluOp == 0):
#    m.d.comb += Assert(alu.out == (alu.in1 + alu.in2)[:aluWidth])
#    m.d.comb += Cover((alu.out == 0x00) & (alu.in1 == 0xfe))

# Formal assertion for subtracting
#with m.If(alu.aluOp == 1):
#    m.d.comb += Assert(alu.out == (alu.in1 - alu.in2)[:aluWidth])
#    m.d.comb += Cover((alu.out == 0x00) & (alu.in1 == 0xfe))