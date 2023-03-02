maxSimpleCounter = 0
n = 0
a = 0
b = 0
res = 0

for a in range(-1000, 1000):
    for b in range(-1000, 1000):
        n = 0
        simpleCounter = 0
        notSimple = False

        while not notSimple:
            value = n ** 2 + a * n + b

            if value <= 1:
                break

            if value == 2:
                simpleCounter += 1
                n += 1
                continue

            for i in range(2, value):
                if not (value % i):
                    notSimple = True
                    break

                if i + 1 == value:
                    simpleCounter += 1
                    n += 1
                    continue

        if simpleCounter > maxSimpleCounter:
            maxSimpleCounter = simpleCounter
            res = a * b
print(res)

