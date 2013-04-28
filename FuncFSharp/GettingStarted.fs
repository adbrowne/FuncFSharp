module GettingStarted
    open NUnit.Framework

    let rec isSorted (list: list<'a>) (gt: ('a*'a) -> bool) : bool =   
        match list with
            | [] -> true
            | x::[] -> true
            | x1::x2::xs -> 
                match gt(x1,x2) with
                | true -> false
                | false -> isSorted (x2::xs) gt
    [<Test>]
    let ``Empty returns true`` () : unit =
        Assert.IsTrue(isSorted [] (fun (a,b) -> a > b))

    [<Test>]
    let ``Single item returns true`` () : unit =
        Assert.IsTrue(isSorted [1] (fun (a,b) -> a > b))

    [<Test>]
    let ``Unorderd list returns false`` () : unit =
        Assert.IsFalse(isSorted [2;1] (fun (a,b) -> a > b))

    [<Test>]
    let ``Orderd list returns true`` () : unit =
        Assert.IsTrue(isSorted [1;2] (fun (a,b) -> a > b))

    let partial1 a f : 'b -> 'c =
        fun b -> f(a,b)     

    [<Test>]
    let ``Partial application`` () : unit =
        let firstPart = partial1 1 (fun (x,y) -> x + y) 
        let result = firstPart 2
        Assert.AreEqual(3, result)

    let curry (f:('a*'b) -> 'c) : 'a -> ('b -> 'c) =
        fun a ->
            fun b ->
                f(a, b)

    let uncurry (f:'a -> 'b -> 'c) : ('a*'b) -> 'c =
        fun (a, b) ->
            f(a)(b)

    let compose (f: 'b -> 'c) (g:'a -> 'b) : 'a -> 'c =
        fun x ->
            f(g(x))