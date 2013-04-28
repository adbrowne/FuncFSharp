
namespace FuncFSharp
open System
open NUnit.Framework

type RNG = RNG of (unit -> (int * RNG))
type State<'s,'a> = 's -> ('a * 's)
type 'a Rand = State<RNG,'a>

module State = 
    let unit (a: 'a) : State<'s,'a> =
        fun s -> (a, s)

    let flatMap(f: State<'s,'a>)(g: 'a -> State<'s,'b>) : State<'s,'b> =
        fun s ->
            let (a, rng2) = f(s)
            g(a)(rng2)

    let map (s) (f: 'a -> 'b)  =
        flatMap(s) (fun a -> unit(f(a)))

    let map2(ra, rb)(f: ('a * 'b) -> 'c) =
        flatMap(ra) (fun (a) -> flatMap(rb)(fun(b) -> unit(f(a,b))))

    let rec sequence fs = 
        match fs with
        | [] -> unit([])
        | x::xs -> map2 (x, sequence(xs)) (fun (a, b) -> a :: b)
    
module RandTest = 
    open State
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

    let rec positiveInt (rng: RNG) =
        let (value, nextRng) = randInt rng
        match value with
            | System.Int32.MinValue -> positiveInt nextRng
            | _ -> (Math.Abs(value), nextRng)

    let randDouble =
        map simpleGen (fun value -> double(value) / double(System.Int32.MaxValue))

    let intDouble =
        map2 (simpleGen, randDouble) id
        
    let doubleInt(rng: RNG) =
        map2 (randDouble, simpleGen) id

    let double3(rng: RNG) =
        let (double1, rng) = randDouble(rng)
        let (double2, rng) = randDouble(rng)
        let (double3, rng) = randDouble(rng)
        (double1, double2, double3), rng

    let rec ints(count: int) : Rand<List<int>> =
        sequence(List.replicate count randInt)

    let positiveMax (n:int) : Rand<int> =
        let getPosMax x =
            int(Math.Floor(decimal(x)/decimal(System.Int32.MaxValue) * decimal(n)))
        map positiveInt getPosMax

    [<Test>]
    let positiveMaxCase () =
        let d2 = positiveMax(10)
        let (value, _) = d2(simple(DateTime.Now.Ticks))
        Assert.LessOrEqual(value, 10)

    [<Test>]
    let intDoubleCase () =
        let d2 = intDouble
        let ((intValue,doubleValue), _) = d2(simple(DateTime.Now.Ticks))
        printfn "(%d,%f)" intValue doubleValue

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