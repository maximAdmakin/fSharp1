module Functions

open System

let rec findMaxPrimeFactorTail (value: uint64) (i: uint64) (d: uint64) (res: uint64) = 
    match value > (i * i) with
    | false -> res
    | _ -> match i = d with
            | false -> match i % d with 
                        | 0UL -> findMaxPrimeFactorTail value (i + 1UL) 2UL res
                        | _ -> findMaxPrimeFactorTail value i (d + 1UL) res
            | _ -> match value % i with 
                        | 0UL -> findMaxPrimeFactorTail value (i + 1UL) 2UL i
                        | _ -> findMaxPrimeFactorTail value (i + 1UL) 2UL res

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

let rec findMaxSequenceTail n a b d maxN res =
    match b >= 1000 with
        | false -> match (n * n + a * n + b) > 1 with 
                    | false -> findMaxSequenceTail 0 a (b + 1) 2 maxN res
                    | true -> match (n * n + a * n + b) = d with 
                                | false -> match (n * n + a * n + b) % d with
                                            | 0 -> findMaxSequenceTail 0 a (b + 1) 2 maxN res
                                            | _ -> findMaxSequenceTail n a b (d + 1) maxN res
                                | true -> match (maxN > n) with
                                            | false -> findMaxSequenceTail (n + 1) a b 2 n (a * b) 
                                            | true -> findMaxSequenceTail (n + 1) a b 2 maxN res
        | true -> match (a >= 999) with
                    | false -> findMaxSequenceTail 0 (a + 1) -1000 2 maxN res
                    | true -> res
 
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
        | false -> match howManyPrimes a b > maxN with  
                    | false -> findMaxSequenceInfSeq a (b + 1) maxN res  
                    | true -> findMaxSequenceInfSeq a (b + 1) (howManyPrimes a b) (a * b)
        | true -> match (a >= 999) with
                    | false -> findMaxSequenceInfSeq (a + 1) -1000 maxN res
                    | true -> res 




                                                        



    