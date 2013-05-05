module Play1
    open NUnit.Framework

    [<Test>] 
    let ``seq play`` () : unit =
        let input = [1;2]
        let blah = 
            seq {
                for n in input do
                for m in input do
                yield n + m
            } |> Seq.toList

        Assert.AreEqual([2;3;3;4], blah)

    [<Test>] 
    let ``opt play`` () : unit =
        let input = Some 3
        let blah =
            FSharpx.Option.maybe {
                let! n = input
                return n + 1
            }

        Assert.AreEqual(Some 4, blah)

    [<Test>] 
    let ``nested expression`` () : unit =
        let input = []
        let blah = 
            FSharpx.Option.maybe {
                let! n = input |> Seq.tryFind (fun x -> true)
                return seq {
                    yield! input
                    yield! input
                } |> List.ofSeq
            } 

        Assert.AreEqual(None, blah)

    type Logging<'a> =
        | Log of 'a * list<string>

    let printLog (a:Logging<'a>) = 
        match a with
        | Log(_, items) -> for i in items do
                            System.Console.WriteLine(i)

    type LoggingBuilder() =
        member this.Bind(Log(value, logs1), f) = 
            let (Log(newValue, logs2)) = f(value)
            Log(newValue, logs1 @ logs2)

        member this.Return(value) =
            Log(value, [])

        member this.Zero() = 
            Log((), [])
    let log = new LoggingBuilder()
    let logMessage(s) =
        Log((), [s])

    let isAndrew value = 
       match value with
       | "Andrew" -> Log(true, ["Tried Andrew"])
       | "andrew" -> Log(true, ["Failed first match";"Matched Lower case"])
       | _ -> Log(false, ["Didn't match anything"]) 

    [<Test>] 
    let ``logging text`` () : unit =
        let result = log {
            let! n = isAndrew("nothing")
            let! b = isAndrew("andrew")
            do! logMessage("last item")
            return n
        }
        printLog result
        Assert.True(true)

