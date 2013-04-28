
namespace FuncFSharp
open System
open NUnit.Framework

type RNG = RNG of (unit -> (int * RNG))
type 'a Rand = RNG -> ('a * RNG)
module Test = 
    let unit (a: 'a) : Rand<'a> =
        fun rng -> (a, rng)

    let map (s: Rand<'a>) (f: 'a -> 'b) : Rand<'b> =
        fun rng ->
            let (a, rng2) = s(rng)
            (f(a), rng2)

    let map2(ra: Rand<'a>, rb: Rand<'b>)(f: ('a * 'b) -> 'c) : Rand<'c> =
        fun rng ->
            let (a, rng2) = ra(rng)
            let (b, rng3) = rb(rng2)
            (f(a,b), rng3)

    let simpleGen (rng:RNG) : (int * RNG) =
        let (value, nextRng) = 
            match rng with
            | RNG nextItem -> nextItem()
        unit(value)(nextRng)

    let rec simple (seed: int64) : RNG =
            let seed2 = (seed*0x5DEECE66DL + 0xBL) &&& ((1L <<< 48) - 1L)
            let toRet = fun () ->
                        ((seed2 >>> 16) |> int, simple(seed2))
            RNG(toRet)
 
    let randInt (rng: RNG) =
        match rng with
        | RNG nextItem -> nextItem()

    let positiveValue value = 
            match value with 
            | System.Int32.MinValue -> System.Int32.MaxValue
            | _ -> value

    let positiveInt (rng: RNG) =
        let (value, nextRng) = randInt rng
        (positiveValue(value), nextRng)

    let randDouble =
        map simpleGen (fun value -> double(value) / double(System.Int32.MaxValue))

    let intDouble(rng: RNG) =
        let (intValue, nextRng) = randInt rng
        let (doubleValue, nextRng) = randDouble(nextRng)
        (intValue, doubleValue), nextRng
        
    let doubleInt(rng: RNG) =
        let (intValue, nextRng) = randInt rng
        let (doubleValue, nextRng) = randDouble(nextRng)
        (doubleValue, intValue), nextRng

    let double3(rng: RNG) =
        let (double1, rng) = randDouble(rng)
        let (double2, rng) = randDouble(rng)
        let (double3, rng) = randDouble(rng)
        (double1, double2, double3), rng

    let rec ints(count: int)(rng: RNG) : (List<int> * RNG) =
        match count with
        | 0 -> (List.empty, rng)
        | _ -> 
            let (values, nextRng) =
                ints(count - 1)(rng)
            let (myValue, nextRng) = randInt(nextRng)
            (myValue :: values, nextRng)


    let positiveMax (n:int) : Rand<int> =
        let getPosMax x =
            let positive = decimal(positiveValue x)
            int(Math.Floor(positive/decimal(System.Int32.MaxValue) * decimal(n)))
        map simpleGen getPosMax


    [<Test>]
    let positiveMaxCase () =
        let d2 = positiveMax(10)
        let (value, _) = d2(simple(DateTime.Now.Ticks))
        Assert.LessOrEqual(value, 10)

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
