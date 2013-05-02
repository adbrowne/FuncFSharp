module HandlingErrors
open NUnit.Framework
type MyOption<'a> = 
    | None
    | Some of 'a

module MyOption = 
    let mean (xs:seq<double>) = 
        if Seq.isEmpty xs then
            None
        else
            Some((Seq.sum xs) / double(Seq.length xs))

//    def map[B](f: A => B): Option[B]
//    def flatMap[B](f: A => Option[B]): Option[B]
//    def getOrElse[B >: A](default: => B): B
//    def orElse[B >: A](ob: => Option[B]): Option[B]
//    def filter(f: A => Boolean): Option[A]

    let map f value =
        match value with
        | None -> None
        | Some(x) -> Some(f(x))

    let flatMap (f: 'a -> MyOption<'b>) (value:MyOption<'a>) : MyOption<'b> =
        match value with
        | None -> None
        | Some(x) -> f(x)

    let getOrElse (d:(unit -> 'a)) (value:MyOption<'a>) : 'a =
        match value with
        | None -> d()
        | Some(x) -> x

    let orElse (ob:(unit -> MyOption<'a>)) value =
        match value with
        | None -> ob()
        | _ -> value

    let filter (f:('a -> bool)) value =
        value |> flatMap (fun x -> 
                            if f(x) then value
                            else None)

    let variance xs =
        let m = mean xs
        let varCalc meanValue = mean(xs |> Seq.map (fun (x) -> System.Math.Pow(x - meanValue, 2.0)))
        flatMap varCalc m

    let map2 a b f =
        match (a,b) with
        | (None,_) -> None
        | (_,None) -> None
        | (Some x, Some y) -> Some (f x y)

    let traverse (a:list<'a>) (f: 'a -> MyOption<'b>) : MyOption<list<'b>> = 
        let rec travAcc acc input = 
            match input with
            | [] -> Some (List.rev acc)
            | x :: xs -> match f(x) with
                         | None -> None
                         | Some y -> travAcc (y::acc) xs
        travAcc [] a

    let sequence (a:list<MyOption<'a>>) : MyOption<list<'a>> =
        traverse a id

    [<Test>]
    let ``Sequence with no nones`` () : unit =
        let input = [Some 2; Some 3; Some 4]
        let result = sequence input
        Assert.AreEqual(Some [2;3;4], result)        

    [<Test>]
    let ``Sequence with a none`` () : unit =
        let input = [Some 2; None; Some 4]
        let result = sequence input
        match result with
            | None -> ()
            | Some _ -> Assert.Fail()