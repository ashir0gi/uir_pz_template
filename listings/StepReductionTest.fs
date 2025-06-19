module Mephi.Cybernetics.Mace.AbstractMachines.Tests.BetaReduction.Reduction

open NUnit.Framework
open Mephi.Cybernetics.Mace.AbstractMachines.BetaReduction.Machine
open Mephi.Cybernetics.Mace.Core
open Mephi.Cybernetics.Race.Core.Host.Computing
open Mephi.Cybernetics.Race.Core.Host.Definitions
open Mephi.Cybernetics.Race.Core.Host.Definitions.Tff
open Mephi.Cybernetics.Mace.AbstractMachines.BetaReduction.Atomic

let substitution = DefaultSubstitution :> ISubstitution
let applicationTff = MultiApplicationTff :> IApplicationTff
let simpleLambdaTff = DefaultSimpleLambdaTff() :> ITff

let cfg : Config = {
    applicationTff = applicationTff
    substitution  = substitution
}

let reductor : IAbstractMachine = BetaReductor(cfg)
let term2Code (t:ITerm) : IMachineCode = Code(t)

let reduceStepByStep (machine: IAbstractMachine) (code: IMachineCode) : string list =
    let rec loop (state: IMachineState) (acc: string list) =
        let regs = state.RegisterStrings |> Array.toList |> List.map (fun s -> s.Trim())
        let newAcc = acc @ [ String.concat " | " regs ]
        if state.IsFinal then newAcc
        else 
            let next = machine.EvaluateState(state, true)
            loop next newAcc
    loop (machine.CreateDefaultState code) []

let assertStepsEqual (got: string[]) (expected: ITerm list) =
    let expArr = expected |> List.map (fun t -> t.ToString()) |> List.toArray
    Assert.That(got, Is.EqualTo<string[]>(expArr))

[<TestFixture>]
type BetaReductionTests() =

    [<Test>]
    member _.``Debug one step``() =
        let term = applicationTff.CreateTerm([
            simpleLambdaTff.CreateTerm([ DefaultVariable "x"; DefaultVariable "x" ])
            DefaultVariable "x"
        ])
        let st0 = reductor.CreateDefaultState(term2Code term)
        printfn "BEFORE: %A, IsFinal=%b" st0.RegisterStrings st0.IsFinal

        let st1 = reductor.EvaluateState(st0, true)
        printfn "AFTER:  %A, IsFinal=%b" st1.RegisterStrings st1.IsFinal

        Assert.That(st1.RegisterStrings, Is.EqualTo<string[]>([| "x" |]))
        Assert.That(st1.IsFinal, Is.True)

    [<Test>]
    member _.``Debug full seq``() =
        let term = applicationTff.CreateTerm([
            simpleLambdaTff.CreateTerm([ DefaultVariable "x"; DefaultVariable "x" ])
            DefaultVariable "x"
        ])
        let seq = reduceStepByStep reductor (term2Code term)
        printfn "STEP SEQUENCE:"
        seq |> List.iteri (fun i s -> printfn "Step %d: %s" i s)
        Assert.That(seq |> List.last, Is.EqualTo("x"))

    [<Test>]
    member _.``singleIdLambda``() =
        let term = applicationTff.CreateTerm([
            simpleLambdaTff.CreateTerm([ DefaultVariable "x"; DefaultVariable "x" ])
            DefaultVariable "x"
        ])
        let got = reduceStepByStep reductor (term2Code term) |> List.toArray  // <- тут
        let expected = [ DefaultVariable "x" :> ITerm ]
        assertStepsEqual got expected

    [<Test>]
    member _.``doubleIdLambda``() =
        let inner = applicationTff.CreateTerm([
            simpleLambdaTff.CreateTerm([ DefaultVariable "x"; DefaultVariable "x" ])
            DefaultVariable "x"
        ])
        let term = applicationTff.CreateTerm([
            simpleLambdaTff.CreateTerm([ DefaultVariable "x"; DefaultVariable "x" ])
            inner
        ])
        let got = reduceStepByStep reductor (term2Code term) |> List.toArray
        let expected = [ inner :> ITerm; DefaultVariable "x" :> ITerm ]
        assertStepsEqual got expected

    [<Test>]
    member _.``step by step plus``() =
        // (+ 1 2) -> 3
        let plus = Data.BinOp("+", fun a b -> 
            match a, b with
            | Data.Int x, Data.Int y -> Data.Int (x + y)
            | _ -> failwith "invalid"
        )
        let term = applicationTff.CreateTerm([
            plus :> ITerm
            Data.Int 1
            Data.Int 2
        ])
        let seq = reduceStepByStep reductor (term2Code term)
        printfn "STEP SEQUENCE (+ 1 2):"
        seq |> List.iteri (fun i s -> printfn "Step %d: %s" i s)
        Assert.That(seq |> List.last, Is.EqualTo("Int 3"))

    [<Test>]
    member _.``step by step lambda application``() =
        // (\x -> x) 5 -> 5
        let lam = simpleLambdaTff.CreateTerm([ DefaultVariable "x"; DefaultVariable "x" ])
        let term = applicationTff.CreateTerm([ lam; Data.Int 5 ])
        let seq = reduceStepByStep reductor (term2Code term)
        printfn "STEP SEQUENCE ((\\x->x) 5):"
        seq |> List.iteri (fun i s -> printfn "Step %d: %s" i s)
        Assert.That(seq |> List.last, Is.EqualTo("Int 5"))
