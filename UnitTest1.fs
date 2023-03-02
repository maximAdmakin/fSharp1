module FuctionalLab1
open NUnit.Framework
open Functions

[<SetUp>]
let Setup () = ()

let res3 = 6857
let res27 = -59231

[<Test>]
let Test1 () =
    assert (findMaxPrimeFactorTail 600851475143UL 2UL 2UL 0UL = System.Convert.ToUInt64(res3))

[<Test>]
let Test2 () =
    assert (findMaxPrimeFactorSeq 600851475143UL = res3)

[<Test>]
let Test3 () =
    assert (findMaxSequenceInfSeq -999 -1000 0 0  = res27)