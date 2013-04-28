module FunctionalDataStructures
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

    let example = Cons(1, Cons(2, Cons(3, Nil)))
    let example2 = buildList [1;2;3;]
    let total = sum(example)