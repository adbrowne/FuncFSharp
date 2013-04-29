module FunctionalDataStructures
open NUnit.Framework
type MyList<'a> = 
    | Nil
    | Cons of ('a * MyList<'a>)

type MyTree<'a> =
    | Leaf of 'a
    | Branch of (MyTree<'a>*MyTree<'a>)

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
        let input = Cons(1,Cons(2,Cons(3,Nil)))
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

    let map l f =
        reverseFold (foldLeft l Nil (fun (x,y) -> Cons(f y, x)))

    [<Test>]
    let ``Map Test`` () : unit = 
        let result = map (Cons(1, Nil)) (fun x -> x + 1)
        Assert.AreEqual(Cons(2,Nil), result)
        
    let filter l f =
        let accFilter (x,y) =
            match y with
            | _ when f y -> Cons(y,x)
            | _ -> x

        reverseFold (foldLeft l Nil accFilter)

    let flatMap l f =
        foldRight l Nil (fun (x,y) -> append((f x), y))

    let flatFilter (l:MyList<'a>) (f: 'a -> bool) =
        flatMap l (fun x -> 
                       match x with
                       | _ when f x -> Cons(x,Nil)
                       | _ -> Nil)

    [<Test>]
    let ``Filter Test`` () : unit = 
        let result = flatFilter (Cons(1, Cons(2, Nil))) (fun x -> x % 2 = 0)
        Assert.AreEqual(Cons(2,Nil), result)

    [<Test>]
    let ``FlatMap Test`` () : unit = 
        let result = flatMap (Cons(1,Cons(2,Cons(3,Nil)))) (fun x -> Cons(x,Cons(x,Nil)))
        Assert.AreEqual((Cons(1,Cons(1,Cons(2,Cons(2,Cons(3,Cons(3,Nil))))))), result)

    let rec combineLists l1 l2 =
        match (l1,l2) with
        | (Cons(x,xs), Cons(y,ys)) -> Cons(x + y, combineLists xs ys)
        | _ -> Nil

    let rec flexCombineLists l1 l2 f =
        match (l1,l2) with
        | (Cons(x,xs), Cons(y,ys)) -> Cons(f x y, combineLists xs ys)
        | _ -> Nil

    [<Test>]
    let ``Add lists test`` () : unit =
        let result = flexCombineLists (Cons(1,Cons(2,Cons(3,Nil)))) (Cons(4,Cons(5,Cons(6,Nil)))) (+)
        Assert.AreEqual((Cons(5,Cons(7,Cons(9,Nil)))), result)

    let rec beginsWith l sub =
        match (l,sub) with
        | (_, Nil) -> true
        | (Nil, _) -> false
        | (Cons(x,xs), Cons(y,ys)) -> 
            if x = y then beginsWith xs ys
            else false

    let rec hasSubsequence l sub =
        if beginsWith l sub then true
        else
            match l with
            | Nil -> false
            | Cons(x,xs) -> hasSubsequence xs sub

    [<Test>]
    let ``Has Subsequence Test`` () : unit =
        let myList = (Cons(1,Cons(2,Cons(3,Cons(4, Nil)))))
        Assert.True (hasSubsequence myList (Cons(1,Cons(2,Nil))))
        Assert.True (hasSubsequence myList (Cons(2,Cons(3,Nil))))
        Assert.True (hasSubsequence myList (Cons(4,Nil)))
        Assert.False (hasSubsequence myList (Cons(5,Nil)))
        Assert.False (hasSubsequence myList (Cons(3,Cons(2,Nil))))

    let rec treeFold (t:MyTree<'a>) (fl:'a -> 'b) (fb: 'b -> 'b -> 'b) : 'b =
        match t with
        | Leaf x -> fl x
        | Branch(left, right) -> fb (treeFold left fl fb) (treeFold right fl fb)

    let treeSize t =
        treeFold t (fun l -> 1) (fun x y -> x + y)

    let rec treeSize' t =
        match t with
        | Leaf _ -> 1
        | Branch(left, right) -> treeSize' left + treeSize' right

    [<Test>]
    let ``Tree Size`` () : unit =
        let myTree = Branch (Leaf(1), Branch(Leaf(2),Leaf(3)))
        let result = treeSize myTree
        Assert.AreEqual(3, result)

    let treeMax (t:MyTree<int>) =
        treeFold t id max

    let rec treeMax' (t:MyTree<int>) =
        match t with
        | Leaf x -> x
        | Branch(left, right) -> max (treeMax' left) (treeMax' right)

    [<Test>]
    let ``Tree Maximum`` () : unit =
        let myTree = Branch (Leaf(1), Branch(Leaf(2),Leaf(3)))
        let result = treeMax myTree
        Assert.AreEqual(3, result)

    let treeDepth t =
        treeFold t (fun _ -> 1) max

    let rec treeDepth' t =
        match t with
        | Leaf _ -> 1
        | Branch(left,right) -> 1 + (max (treeDepth' left) (treeDepth' right))

    [<Test>]
    let ``Tree Depth`` () : unit =
        let myTree = Branch (Leaf(1), Branch(Leaf(2),Leaf(3)))
        let result = treeDepth myTree
        Assert.AreEqual(3, result)

    let treeMap t f =
        treeFold t (fun x -> Leaf (f x)) (fun x y -> Branch(x,y))

    let rec treeMap' t f =
        match t with
        | Leaf x -> Leaf (f x)
        | Branch(left,right) -> Branch((treeMap' left f),(treeMap' right f))

    [<Test>]
    let ``Add one to tree items`` () : unit =
        let myTree = Branch (Leaf(1), Branch(Leaf(2),Leaf(3)))
        let result = treeMap myTree (fun x -> x + 1)
        let expected = Branch (Leaf(2), Branch(Leaf(3),Leaf(4)))
        Assert.AreEqual(expected, result)