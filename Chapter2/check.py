import sys

if len(sys.argv) < 2:
    raise ValueError

with open(sys.argv[1], 'r') as f:
    count = 0
    for num, line in enumerate(f.readlines()):
        print(num, count, line)

        for ch in line:
            if ch == '(':
                count += 1
            elif ch ==')':
                count -= 1
    print(count)
