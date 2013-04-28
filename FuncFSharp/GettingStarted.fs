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