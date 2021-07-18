import math

def ceilLog2(value):
    ''' Return ceiling of Log2\n\n NOTE: "value" is always treated as --> abs(value) '''
    if abs(value) == 1:
        return 1
    return math.ceil(math.log(abs(value), 2))

def mipysoc(configFile):
    ''' An SoC generation tool specific to mipyfive to quickly create simple ready-to-use designs '''
    # TODO: Implement this later...
    pass
