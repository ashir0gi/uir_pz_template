module Mephi.Cybernetics.Mace.AbstractMachines.BetaReduction.Machine

open System
open Mephi.Cybernetics.Mace.Core
open Mephi.Cybernetics.Race.Core.Host.Definitions
open Mephi.Cybernetics.Mace.AbstractMachines.BetaReduction.Environment
open Mephi.Cybernetics.Race.Core.Host.Definitions.Tff
open Mephi.Cybernetics.Mace.AbstractMachines.BetaReduction.Atomic
let internal RTermName = "Term"
let internal MachineName = "Beta-Reductor"
let internal InstructionBetaReductionName = "beta-reduction"

type Config = {
    applicationTff : IApplicationTff
    substitution : ISubstitution
}
type AtomicTerm(data: Data) =
    interface ITerm
    member _.Value = data

let inline atomic (d: Data) : ITerm = AtomicTerm(d) :> ITerm
type Code(term : ITerm) =
    member internal this.Term = term
    interface IMachineCode with
        member _.InstructionCount = 1
        member _.InternalData = term :> obj

type Register =
    | Term
    interface IAbstractMachineRegister with
        member _.Index = 0
        member _.Name = RTermName

type State(code : Code, stepNum : int, isFinal : bool) =
    member internal this.Code = code
    member internal this.StepNum = stepNum
    member internal this.IsFinalFlag = isFinal
    interface IMachineState with
        member _.GetRegisterRawValue(name: string) =
            if name = RTermName then code.Term :> obj
            else invalidArg "registerName" "Invalid register name"
        member _.GetRegisterRawValue(index: int) =
            if index = 0 then code.Term :> obj
            else invalidArg "registerIndex" "Invalid register index"
        member this.GetRegisterStringValue(name: string) =
            (this :> IMachineState).GetRegisterRawValue(name).ToString()
        member this.GetRegisterStringValue(index: int) =
            (this :> IMachineState).GetRegisterRawValue(index).ToString()
        member _.CurrentInstructionName = InstructionBetaReductionName
        member _.IsFinal = isFinal
        member _.Number = int64 stepNum
        member _.RegisterStrings = [| code.Term.ToString() |]

type Info() =
    interface IAbstractMachineInfo with
        member _.ApplicableOptions = Seq.empty
        member _.EffectiveOptions   = Seq.empty
        member _.Name               = MachineName

type BetaReductor(cfg : Config) =

    let rec evaluateOneStep (term : ITerm) : ITerm =
        let tff = cfg.applicationTff
        match term with
        | :? IApplicationTerm as app ->
            let f = app.Function
            let args = app.Arguments |> Seq.toList

            match f with
            | :? SimpleLambda as lam when args.Length >= 1 ->
                let x = args.[0]
                let body = lam.Body
                cfg.substitution.Substitute(body, Seq.singleton (lam.Variable, x))

            | :? MultiLambda as lam ->
                let vs = lam.Variables |> Seq.toList
                let n  = vs.Length
                if args.Length >= n then
                    let xs   = args |> List.take n
                    let body = (lam :> IFunctionalAbstraction).GetBody
                    let pairs = Seq.zip vs xs
                    cfg.substitution.Substitute(body, pairs)
                else
                    term

            | :? IFunction as fn when fn.IsReducibleWith(args) ->
                fn.ReduceWithArguments(args, tff) :?> ITerm

            | _ ->
                let f' = evaluateOneStep f
                if not (obj.ReferenceEquals(f, f')) then
                    tff.CreateTerm(seq { yield f'; yield! args }) :> ITerm
                else
                    let args' = args |> List.map evaluateOneStep
                    if List.exists2 (fun a b -> not (obj.ReferenceEquals(a,b))) args args' then
                        tff.CreateTerm(seq { yield f; yield! args' }) :> ITerm
                    else
                        term

        | :? IFunctionalAbstraction as lam ->
            let body  = lam.GetBody
            let body' = evaluateOneStep body
            if obj.ReferenceEquals(body, body') then term
            else
                match lam with
                | :? SimpleLambda as s -> SimpleLambda(s.Variable, body', cfg.substitution) :> ITerm
                | :? MultiLambda  as m -> MultiLambda(m.Variables, body', cfg.substitution) :> ITerm
                | _ -> term

        | _ -> term

    let rec evaluateFully (term : ITerm) : ITerm =
        let term' = evaluateOneStep term
        if obj.ReferenceEquals(term, term') then term
        else evaluateFully term'

    interface IAbstractMachine with
        member _.CreateDefaultState(code) =
            let c = code :?> Code
            State(c, 0, false) :> IMachineState

        member _.Evaluate(state) =
            match state with
            | :? State as s ->
                let curr = (s :> IMachineState).GetRegisterRawValue(0) :?> ITerm
                let res  = evaluateFully curr
                State(Code(res), int s.StepNum + 1, true) :> IMachineState
            | _ -> invalidArg "state" "Invalid state type"

        member _.EvaluateCode(code, singleStep) =
            let c = code :?> Code
            if singleStep then
                let r1   = evaluateOneStep c.Term
                let r2   = evaluateOneStep r1
                let fin  = obj.ReferenceEquals(r1, r2)
                State(Code(r1), 0, fin) :> IMachineState
            else
                let res = evaluateFully c.Term
                State(Code(res), 0, true) :> IMachineState

        member _.EvaluateState(state, singleStep) =
            match state with
            | :? State as s ->
                let curr = s.Code.Term
                if singleStep then
                    let r1  = evaluateOneStep curr
                    let r2  = evaluateOneStep r1
                    let fin = obj.ReferenceEquals(r1, r2)
                    State(Code(r1), int s.StepNum + 1, fin) :> IMachineState
                else
                    let res = evaluateFully curr
                    State(Code(res), int s.StepNum + 1, true) :> IMachineState
            | _ -> invalidArg "state" "Invalid state type"

        member _.AbstractMachineInfo : IAbstractMachineInfo = Info() :> IAbstractMachineInfo
        member _.Registers                       = seq { Register.Term }

type Compiler() =
    interface ILanguageCompiler with
        member _.Compile(term) = Code(term)
        member _.SupportedAbstractMachine = typeof<BetaReductor>
        member _.SupportedEnvironment = Environment.Environment()
