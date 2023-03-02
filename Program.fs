module Program

open Functions

[<EntryPoint>]
let main argv =
    let resFMPFT = findMaxPrimeFactorTail 600851475143UL 2UL 2UL 0UL
    printf "Problem 3 - Tail Recursion: %d\n" resFMPFT
    
    let resFMPFS = findMaxPrimeFactorSeq 600851475143UL
    printf "Problem 3 - Seq.initInfinite, Seq.fold: %d\n" resFMPFS

    let resFMSIS = findMaxSequenceInfSeq -999 -1000 0 0 
    printf "Problem 27 - Tail Recursion, Seq.initInfinite, Seq.map: %d\n" resFMSIS

    0