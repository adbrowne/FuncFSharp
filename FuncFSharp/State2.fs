namespace FuncFSharp.State2

open NUnit.Framework

type State<'a,'s> = State of ('s -> 'a * 's)

module State =
    let runState (State s) initialState = s initialState
    let getState = State (fun s -> (s,s))
    let putState s = State (fun _ -> ((),s))
    let evalState m s = runState m s |> fst
    let execState m s = runState m s |> snd
    let retrunF a = State (fun s -> (a,s))
    let bind m k =
        State (fun s -> let (a,s') = runState m s in runState (k a) s')

    type StateBuilder() =
      member this.Return a = State (fun s -> (a, s))
      member this.Bind(m, k) = 
        State (fun s -> let (a,s') = runState m s in runState (k a) s')
    let state = new StateBuilder()

    [<Test>] 
    let testCase1 () =
        let tick = state {
            let! n = getState
            do! putState (n + 1)
            return n
        }
        let result = execState tick 3

        Assert.AreEqual(4, result)
        ()

    [<Test>] 
    let testCase1ByHand () =
        let tick = 
            state.Bind(getState, fun n ->
                state.Bind(putState(n + 1), fun () ->
                    state.Return(n)
                )
            )

        let result = runState tick 3 |> snd

        Assert.AreEqual(4, result)
        ()