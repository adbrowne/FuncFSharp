module FunctionalDataStructures
open NUnit.Framework
type MyList<'a> = 
    | Nil
    | Cons of ('a * MyList<'a>)

module MyList =
    let rec sum (ints: MyList<int>) : int =
        match ints with
        | Nil -> 0
        | Cons(x, xs) -> x + sum (xs)

    let rec product (ds: MyList<double>) : double = 
        match ds with
        | Nil -> 1.0
        | Cons(0.0, _) -> 0.0
        | Cons(x,xs) -> x * product(xs)

    let rec buildList (input: seq<'a>) : MyList<'a> =
        if (input |> Seq.isEmpty) then
            Nil
        else
            Cons(input |> Seq.head, buildList(input |> Seq.skip(1)))

    let tail (myList: MyList<'a>) : MyList<'a> =
        match myList with
        | Nil -> Nil
        | Cons(x, xs) -> xs

    let rec drop n (myList: MyList<'a>) : MyList<'a> =
        match (n, myList) with
        | (_, Nil) -> Nil
        | (0, _) -> myList
        | (n, Cons(x,xs)) -> drop (n - 1) xs

    let rec dropWhile (f : 'a -> bool) (myList: MyList<'a>) : MyList<'a> =
        match myList with
        | Nil -> Nil
        | Cons(x, xs) when f(x) -> dropWhile f xs
        | _ -> myList        

    let setHead (myList: MyList<'a>) (newHead:'a) : MyList<'a> =
        match myList with
        | Nil -> Cons(newHead, Nil)
        | Cons(x,xs) -> Cons(newHead, xs)

    let rec init (myList: MyList<'a>) : MyList<'a> =
        match myList with
        | Nil -> Nil
        | Cons(x,Nil) -> Nil
        | Cons(x,xs) -> Cons(x, init(xs))

    let rec foldRight (l: MyList<'a>) (z:'b) (f: ('a*'b) -> 'b) : 'b =
        match l with
        | Nil -> z
        | Cons(x, xs) -> f (x,(foldRight xs z f))

    let rec foldLeft (l: MyList<'a>) (z:'b) (f: ('b*'a) -> 'b) : 'b =
        match l with
        | Nil -> z
        | Cons(x, xs) -> foldLeft xs (f (z, x)) f

    let foldLeftByRight (l: MyList<'a>) (z:'b) (f: ('b*'a) -> 'b) : 'b =
        foldRight l z (fun (b,a) -> f(a,b))

    let foldRightByLeft (l: MyList<'a>) (z:'b) (f: ('a*'b) -> 'b) : 'b =
        foldLeft l z (fun (b,a) -> f(a,b))

    let tupleUp (f: 'a -> 'b -> 'b) =
        fun (a,b) ->
            f(a)(b)

    let sum2 l =
        foldRight l 0.0 (tupleUp (+))

    let product2 l =
        foldRight l 1.0 (tupleUp (*))

    let example = Cons(1, Cons(2, Cons(3, Nil)))
    let floatExample = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))
    let example2 = buildList [1;2;3;]

    [<Test>]
    let ``Simple list tests`` () : unit =
        let total = sum(example)
        Assert.AreEqual(6, total)

    [<Test>]
    let ``Sum with fold`` () : unit =
        let result = sum2 floatExample
        Assert.AreEqual(6, result)

    [<Test>]
    let ``Product with fold`` () : unit =
        let result = product2 floatExample
        Assert.AreEqual(6, result)

    [<Test>]
    let ``Data constructor with fold`` () : unit =
        let input = Cons(1,Cons(2,Cons(3,Nil)))
        let result = foldRight input Nil (Cons)
        Assert.AreEqual(input, result)

    let length l =
        foldRight l 0 (fun (_, currentLength) -> currentLength + 1)

    [<Test>]
    let ``Length Test`` () : unit =
        let input = Cons(1,Cons(2,Cons(3,Nil)))
        let result = length input
        Assert.AreEqual(3, result)

    let sumLeft l =
        foldLeft l 0.0 (tupleUp (+))

    [<Test>]
    let ``Won't Stackoverflow`` () : unit =
        let rec createList count list =
            match count with
            | 0 -> list
            | _ -> createList (count - 1) (Cons(float(count),list))

        let input = createList 10000000 Nil

        // sum2 input |> ignore // will overflow
        sumLeft input |> ignore

    let reverse1 l = 
        let rec intReverse i o =
            match i with
            | Nil -> o
            | Cons(x,xs) -> intReverse xs (Cons(x,o))
        intReverse l Nil

    let reverseFold l = 
        foldLeft l Nil (fun (a,b) -> Cons(b,a))

    [<Test>]
    let ``Reverse`` () : unit =
        let input = Cons(1,Cons(2,Cons(3,Nil)))
        let result = reverseFold input
        let expectedResult = Cons(3,Cons(2,Cons(1,Nil)))
        Assert.AreEqual(expectedResult, result)

    let append (l1,l2) =
        foldRight l1 l2 (fun (x,y) -> Cons(x,y))


    [<Test>]
    let ``AppendTest`` () : unit =
        let input = Cons(1,Cons(2,Nil))
        let result = append (input, input)
        let expectedResult = Cons(1,Cons(2,Cons(3,Cons(1,Cons(2,Cons(3,Nil))))))
        Assert.AreEqual(expectedResult, result)

    let concat (l:MyList<MyList<'a>>) =
        foldRight l Nil append

    [<Test>]
    let ``ConcatTest`` () : unit =
        let sublist1 = Cons(1,Cons(2,Nil))
        let sublist2 = Cons(2,Nil)
        let sublist3 = Cons(3,Cons(2,Cons(1,Nil)))
        let input = Cons(sublist1, (Cons(sublist2, Cons(sublist3, Nil))))
        let result = concat input
        let expectedResult = Cons(1,Cons(2,Cons(2,Cons(3,Cons(2,Cons(1,Nil))))))
        Assert.AreEqual(expectedResult, result)