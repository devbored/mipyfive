import math

def ceilLog2(value):
    ''' Return ceiling of Log2\n\n NOTE: "value" is always treated as --> abs(value) '''
    if abs(value) == 1:
        return 1
    return math.ceil(math.log(abs(value), 2))
