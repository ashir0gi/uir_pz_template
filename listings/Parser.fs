module Mephi.Cybernetics.Mace.AbstractMachines.BetaReduction.Parser

open System
open Mephi.Cybernetics.Mace.AbstractMachines.BetaReduction.Atomic
open Mephi.Cybernetics.Mace.Core
open Mephi.Cybernetics.Race.Core.Host
open Mephi.Cybernetics.Mace.AbstractMachines.Parser
open Mephi.Cybernetics.Race.Core.Host.Computing
open Mephi.Cybernetics.Race.Core.Host.Definitions

exception BRParsingException of Exception

let internal brUnOps = [
    UnOp {| keyword = "-"; term = atomicUnOpMinus; priority = 1 |}
]

let internal brBinOps = [
    BinOp {| keyword = "not"; term = atomicUnOpNot; priority = 3; associativity = Associativity.Left |}
    BinOp {| keyword = "*"; term = atomicBinOpMult; priority = 2; associativity = Associativity.Left |}
    BinOp {| keyword = "/"; term = atomicBinOpMult; priority = 2; associativity = Associativity.Left |}
    BinOp {| keyword = "+"; term = atomicBinOpPlus; priority = 1; associativity = Associativity.Left |}
    BinOp {| keyword = "-"; term = atomicBinOpMinus; priority = 1; associativity = Associativity.Left |}
    BinOp {| keyword = "="; term = atomicBinOpEquals; priority = 0; associativity = Associativity.Left |}
    BinOp {| keyword = "<"; term = atomicBinOpLess; priority = 0; associativity = Associativity.Left |}
    BinOp {| keyword = "<="; term = atomicBinOpLessEq; priority = 0; associativity = Associativity.Left |}
    BinOp {| keyword = ">"; term = atomicBinOpMore; priority = 0; associativity = Associativity.Left |}
    BinOp {| keyword = ">="; term = atomicBinOpMoreEq; priority = 0; associativity = Associativity.Left |}
    BinOp {| keyword = "and"; term = atomicBinOpAnd; priority = -1; associativity = Associativity.Left |}
    BinOp {| keyword = "or"; term = atomicBinOpOr; priority = -2; associativity = Associativity.Left |}
]

let internal brVarCallback(name : string) : ITerm =
    DefaultVariable(name) :> ITerm

let internal brStrLiteralCallback(str : string) : ITerm =
    Data.Str str

let internal brNumbersCallback(n : Number) : ITerm =
    match n with
    | Int n -> Data.Int n
    | Float n -> Data.Float n

let internal brKeywords : Keyword list =
    ["true"; "false"; "Y"; "NULL"]

let internal brKeywordCallback(kw : string) : ITerm =
    match kw with
    | "true" -> Data.Bool true    
    | "false" -> Data.Bool false
    | "if" -> Data.IfComb
    | "Y" -> Data.YComb
    | "NULL" -> Data.Null
    | _ -> raise (BRParsingException(ParseException($"invalid keyword: {kw}")))


let internal getLambdaParserCfg(subs : ISubstitution, appTff : IApplicationTff) : Config =
    {
        applicationTff = appTff
        simpleLambdaTff = Tff.SimpleLambdaTff(subs)
        multiLambdaTff = Some(Tff.MultiLambdaTff(subs))
        operators = brUnOps @ brBinOps
        keywords = brKeywords
        parseNumericConsts = true
        ifStatements = false
    }

type BRParser() =
    let mutable substitution: Definitions.ISubstitution = Environment.defaultBRSubstitution
    let mutable applicationTff: Definitions.IApplicationTff = Environment.defaultBRApplicationTff

    member this.Substitution
        with get() = substitution
        and set(s : ISubstitution) = substitution <- s

    member this.ApplicationTff
        with get() = applicationTff
        and set(a : IApplicationTff) = applicationTff <- a

    member internal this.LambdaParser =
        let lp = Parser(getLambdaParserCfg(this.Substitution, this.ApplicationTff))
        lp.pVariableCallback <- brVarCallback
        lp.pNumericCallback <- brNumbersCallback
        lp.pKeywordCallback <- brKeywordCallback
        lp

    interface ILanguageParser with
        member this.Parameters = failwith "todo"
        member this.SupportedParameters = failwith "todo"
        member this.TargetEnvironment = Environment.Environment()
        member this.Parse(input) =
            match this.LambdaParser.Parse input with
            | Ok term -> term
            | Error e -> raise (BRParsingException e)