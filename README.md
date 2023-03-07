## Лабораторная №1 

### Вариант 3, 27

<b>Выполнил:</b> Адмакин Максим Александрович \
<b>Группа:</b> P34102 \
<b>Преподаватель:</b> Пенской Александр Владимирович

### Описание лабораторной работы: 
Решить 2 задачи [Проекта Эйлер](https://projecteuler.net), используя приёмы функционального программирования. 

### Задача 3: Largest Prime Factor

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

[Оригинал](https://projecteuler.net/problem=3)

<b>Ответ:</b> 6857

### Решения: 

<b>Python:</b> \
Проверяем все цифры до $\sqrt{value}$ и возвращаем наибольший делитель.
```python
while i * i < value or res == 0:
	if not(value % i):
		for z in range(2, i):
			if not(i % z): break
			if z + 1 == i: res = i
	i += 2
```
<b>Хвостовая рекурсия:</b> \
Примерный аналог Python-решения.
```f#
let rec findMaxPrimeFactorTail (value: uint64) (i: uint64) (d: uint64) (res: uint64) = 
    match value > (i * i) with
    | false -> res
    | true when i <> d && (i % d) = 0UL -> findMaxPrimeFactorTail value (i + 1UL) 2UL res
    | true when i <> d && (i % d) > 0UL -> findMaxPrimeFactorTail value i (d + 1UL) res
    | true when i = d && (value % i) = 0UL -> findMaxPrimeFactorTail value (i + 1UL) 2UL i
    | _ -> findMaxPrimeFactorTail value (i + 1UL) 2UL res
```
<b>Seq.initInfinite, Seq.filter, Seq.fold:</b>\
Создаем бесконечный список из нечетных чисел, заменяя четные на ноль, после чего убираем все нули. 
Вырезаем из бесконечной последовательности часть, в которой все простые числа меньше $\sqrt{value}$, 
после чего находим в новой последовательности простые числа, являющиеся делителями $value$. 
В конце при помощи <b>Seq.fold</b> меняем элементы местами и возвращаем первый элемент. 
```f#
let findMaxPrimeFactorSeq (value: uint64) = 
    let isSimple n =
        let rec check i =
            i > n/2 || (n % i <> 0 && check (i + 1))
        check 2

    let numbers = Seq.initInfinite (fun index -> if index % 2 <> 0 then index else 0) 
                |> Seq.filter (fun index -> index <> 0) 
                |> Seq.filter (fun index -> isSimple(index))
                |> Seq.takeWhile (fun index -> (System.Convert.ToUInt64(index) * System.Convert.ToUInt64(index)) < value) 
                |> Seq.filter (fun index -> value % System.Convert.ToUInt64(index) = 0UL)
                |> Seq.fold (fun s e -> e::s) []
                |> Seq.head
    
    numbers
```
### Задача 27: Quadratic primes

Euler discovered the remarkable quadratic formula:

```math 
n^2 + a * n + b
```

It turns out that the formula will produce 40 primes for the consecutive integer values $0 \leq n \leq 39$ . However, when $n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41$ is divisible by 41, and certainly when $n = 41, 41^2 + 41 + 41$ is clearly divisible by 41. 

The incredible formula $n^2 - 79n + 1601$ was discovered, which produces 80 primes for the consecutive values $0 \leq n \leq 79$. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form: 

$n^2 + an + b$, where $| a | < 1000$ and $| b | <= 1000$

where $| n |$ is the modulus/absolute value of $n$
e.g. $| 11 | = 11$ and $| -4 | = 4$

Find the product of the coefficients, $a$ and $b$, for the quadric expression that produces the maximum number of primes for consecutive values of $n$, starting with $n = 0$.

[Оригинал](https://projecteuler.net/problem=27)

<b>Ответ:</b> -59231
### Решения: 

<b>Python:</b>\
Решение при помощи прохода и вычисления уравнения для всех значений, с итерированием $n$ до нахождения первого составного числа. 
```python
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
```
<b>Хвостовая рекурсия:</b>\
Примерный аналог Python-решения. 
```f#
let rec findMaxSequenceTail n a b d maxN res =
    let value = (n * n + a * n + b)
    match b >= 1000 with
        | false when value > 1 && value <> d && (value % d) = 0 -> findMaxSequenceTail 0 a (b + 1) 2 maxN res
        | false when value > 1 && value <> d && (value % d) > 0 -> findMaxSequenceTail n a b (d + 1) maxN res
        | false when value > 1 && value = d && maxN < n -> findMaxSequenceTail (n + 1) a b 2 n (a * b)
        | false when value > 1 && value = d && (maxN > n || maxN = n) -> findMaxSequenceTail (n + 1) a b 2 maxN res
        | false -> findMaxSequenceTail 0 a (b + 1) 2 maxN res
        | true when a < 999 -> findMaxSequenceTail 0 (a + 1) -1000 2 maxN res
        | _ -> res
```

<b>Хвостовая рекурсия, Seq.initInfinite, Seq.map:</b>\
Проходим по всем возможным комбинациям $a$ и $b$. Длина последовательности из n находится путем создания бесконечного списка, содержащего в себе все значения выражения при итерировании $n$, после чего все составные значения меняются на ноль, для того, чтобы в последствии отделять часть последовательности до первого нуля и вернуть количество элементов последовательности. 
```f#
let rec findMaxSequenceInfSeq a b maxN res= 
    let isSimple n =
        let rec check i =
            n > 0 && (i > n/2 || (n % i <> 0 && check (i + 1)))
        check 2

    let howManyPrimes aSeq bSeq= Seq.initInfinite (fun n -> n * n + aSeq * n + bSeq) 
                                |> Seq.map (fun index -> if isSimple(index) then index else 0)
                                |> Seq.takeWhile (fun index -> index <> 0)
                                |> Seq.length
   

    match b >= 1000 with
        | false when howManyPrimes a b > maxN -> findMaxSequenceInfSeq a (b + 1) (howManyPrimes a b) (a * b)
        | false -> findMaxSequenceInfSeq a (b + 1) maxN res 
        | true when a < 999 -> findMaxSequenceInfSeq (a + 1) -1000 maxN res
        | _ -> res
```
### Заключение: 
Выполняя лабораторную работу на практике ознакомился с подходом к решению задач при помощи рекурсии (<b>rec</b>), последовательностей (<b>seq</b>, <b>Seq.initInfinite</b>) и операций над ними (<b>Seq.map</b>, <b>Seq.fold</b>, <b>Seq.filter</b> и т.д.). Синтаксис языка оказался простым и к нему быстро привыкаешь, однако с ходу начать писать код на F# может быть затруднительно (<b>|></b>, <b>| _ -></b> и т.д.). Также повышал продуктивность FSI, позволяющий сразу же проводить тестирование кода после его написания. Впервые попробовал GitHub Actions и написал CI проекта. 
