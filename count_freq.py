import fileinput
from collections import Counter

def read_lengths():
    for i, line in enumerate(fileinput.input()):
        if (i+1) % 4 != 2:
            continue
        yield len(line)-1

c = Counter(read_lengths())
for k,v in c.items():
    print(v, k)
