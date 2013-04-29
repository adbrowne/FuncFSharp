namespace FuncFSharp

type IMonoid<'T> =
  abstract member mempty  : unit -> 'T
  abstract member mappend : 'T * 'T -> 'T

type intAdditionMonoid =
    interface IMonoid<int> with
        member this.mempty() = 0
        member this.mappend(a, b) = a + b

type intMultiplicationMonoid =
    interface IMonoid<int> with
        member this.mempty() = 1
        member this.mappend(a, b) = a * b

type booleanOr =
    interface IMonoid<bool> with
        member this.mempty() = false
        member this.mappend(a, b) = a || b

type booleanAnd =
    interface IMonoid<bool> with
        member this.mempty() = true
        member this.mappend(a, b) = a && b

type optionMonoid<'a> =
    interface IMonoid<option<'a>> with
        member this.mempty() = None
        member this.mappend(a, b) =
            match a with 
            | None -> b
            | _ -> a

module MonoidTests =
    open NUnit.Framework
    [<Test>] 
    let ``Some Test`` () : unit = 
            let b = Some "Andrew" : option<string>
            Assert.True(true)
