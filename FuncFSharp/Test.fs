
namespace FuncFSharp
open System
open NUnit.Framework

type 'a RNG = RNG of (unit -> ('a * RNG<'a>))
// type 'a Rand = RNG -> ('a * RNG)
module Test = 
    let rec simple (seed: int64) : RNG<int> =
            let seed2 = (seed*0x5DEECE66DL + 0xBL) &&& ((1L <<< 48) - 1L)
            let toRet = fun () ->
                        ((seed2 >>> 16) |> int, simple(seed2))
            RNG(toRet)
 
    let randInt (rng: RNG<int>) =
        match rng with
        | RNG nextItem -> nextItem()

    let positiveInt (rng: RNG<int>) =
        let (value, nextRng) = randInt rng
        let positiveValue = 
            match value with 
            | System.Int32.MinValue -> System.Int32.MaxValue
            | _ -> value
        (positiveValue, nextRng)

    let randDouble (rng:RNG<int>) =
        let (value, nextRng) = randInt rng
        let dbl = double(value) / double(System.Int32.MaxValue)
        dbl, nextRng

    let intDouble(rng: RNG<int>) =
        let (intValue, nextRng) = randInt rng
        let (doubleValue, nextRng) = randDouble(nextRng)
        (intValue, doubleValue), nextRng
        
    let doubleInt(rng: RNG<int>) =
        let (intValue, nextRng) = randInt rng
        let (doubleValue, nextRng) = randDouble(nextRng)
        (doubleValue, intValue), nextRng

    let double3(rng: RNG<int>) =
        let (double1, rng) = randDouble(rng)
        let (double2, rng) = randDouble(rng)
        let (double3, rng) = randDouble(rng)
        (double1, double2, double3), rng

    let rec ints(count: int)(rng: RNG<int>) : (List<int> * RNG<int>) =
        match count with
        | 0 -> (List.empty, rng)
        | _ -> 
            let (values, nextRng) =
                ints(count - 1)(rng)
            let (myValue, nextRng) = randInt(nextRng)
            (myValue :: values, nextRng)

    [<Test>] 
    let TestCase  () =
        let rng = simple(DateTime.Now.Ticks)
        let (first, nextRng) = 
            match rng with
            | RNG nextItem -> nextItem()
        let (second, _) = 
            match nextRng with
            | RNG nextItem -> nextItem()
        Assert.AreNotEqual(first,second)
        ()

    [<Test>] 
    let PositiveCase  () =
        let rng = simple(DateTime.Now.Ticks)
        let (first, nextRng) = positiveInt(rng)
        let (second, _) = positiveInt(nextRng)
        Assert.AreNotEqual(first,second)
        ()

    [<Test>] 
    let tripleDouble  () =
        let rng = simple(DateTime.Now.Ticks)
        let ((d1, d2, d3), _) = double3(rng)
        printfn "%f, %f, %f" d1 d2 d3
        ()

    [<Test>] 
    let aList  () =
        let rng = simple(DateTime.Now.Ticks)
        let blah = ints 10
        let blah2,_ = blah rng
        for value in blah2 do
            printfn "%d" value
        ()
