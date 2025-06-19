module Mephi.Cybernetics.Mace.AbstractMachines.Tests.Common.Parser

open Mephi.Cybernetics.Mace.Core
open Mephi.Cybernetics.Race.Core.Host.Definitions
open Mephi.Cybernetics.Race.Core.Host.Definitions.Tff
open NUnit.Framework
open Mephi.Cybernetics.Mace.AbstractMachines.Parser
open Mephi.Cybernetics.Race.Core.Host.Computing

type unOp =
    | Minus
    interface IAtomic with
        member this.Arity with get() = failwith "stub"
        member this.IsReducibleWith(_) = failwith "stub"
        member this.ReduceWithArguments(_, _) = failwith "stub"
and binOp =
    | Plus
    | Minus
    | Mult
    interface IAtomic with
        member this.Arity with get() = failwith "stub"
        member this.IsReducibleWith(_) = failwith "stub"
        member this.ReduceWithArguments(_, _) = failwith "stub"
    
let unOps = [
    UnOp {| keyword = "-"; term = unOp.Minus; priority = 1 |}
]

let binOps = [
    BinOp {| keyword = "plus"; term = binOp.Plus; priority = 1; associativity = Associativity.Left|}
    BinOp {| keyword = "-"; term = binOp.Minus; priority = 1; associativity = Associativity.Left|}
    BinOp {| keyword = "*"; term = binOp.Mult; priority = 1; associativity = Associativity.Left|}
]

let substitution = DefaultSubstitution :> ISubstitution
let applicationTff = MultiApplicationTff :> IApplicationTff

let parserCfg : Config = {
    applicationTff = applicationTff
    simpleLambdaTff = SimpleLambdaTff(substitution)
    multiLambdaTff = Some(MultiLambdaTff(substitution))
    operators = unOps @ binOps
    keywords = ["magicWord0"; "magicWord1"]
    parseNumericConsts = true
    ifStatements = false
}

let parser = Parser(parserCfg)

let callParse2String (parser: Parser, input : string) =
    let res = parser.Parse input
    match res with
    | Ok(term) -> term.ToString()
    | Error(e) -> e.ToString()

[<TestFixture>]
type ParseTest() =
    
    [<Test>]
    member this.identity() =
        let input = "\\ x -> x"
        let expected = SimpleLambda(DefaultVariable "x", DefaultVariable "x", substitution)
        
        let got = callParse2String (parser, input)
        Assert.That(got.ToString(), Is.EqualTo (expected.ToString()))
    
    [<Test>] 
    member this.unaryMinus() =
        let input = "\\ x -> -x"
        let expected = SimpleLambda(DefaultVariable "x", applicationTff.CreateTerm [unOp.Minus; DefaultVariable "x"], substitution)
        
        let got = callParse2String (parser, input)
        Assert.That(got.ToString(), Is.EqualTo (expected.ToString()))
        
    [<Test>] 
    member this.nestedAbstractions() =
        let input = "\\ x -> \\ y -> \\ z -> y"
        let expected = SimpleLambda(
            DefaultVariable "x",
            SimpleLambda(
                DefaultVariable "y",
                SimpleLambda(
                    DefaultVariable "z",
                    DefaultVariable "y",
                    substitution),
                substitution),
            substitution)
        
        let got = callParse2String (parser, input)
        Assert.That(got.ToString(), Is.EqualTo (expected.ToString()))
        
    [<Test>] 
    member this.Plus() =
        let input = "\\ x -> x plus x "
        let expected = SimpleLambda(DefaultVariable "x", applicationTff.CreateTerm [binOp.Plus; DefaultVariable "x"; DefaultVariable "x"], substitution)
        
        let got = callParse2String (parser, input)
        Assert.That(got.ToString(), Is.EqualTo (expected.ToString()))
        
    [<Test>] 
    member this.PlusX3() =
        let input = "\\ x -> x plus x plus x plus x "
        let expected = SimpleLambda(
            DefaultVariable "x",
            applicationTff.CreateTerm [
                binOp.Plus
                applicationTff.CreateTerm [
                    binOp.Plus
                    applicationTff.CreateTerm [
                        binOp.Plus
                        DefaultVariable "x"
                        DefaultVariable "x"                        
                    ]
                    DefaultVariable "x"
                ]
                DefaultVariable "x"
            ],
            substitution)
        
        let got = callParse2String (parser, input)
        Assert.That(got.ToString(), Is.EqualTo (expected.ToString()))
        
    [<Test>] 
    member this.PlusWithUnMinus() =
        let input = "\\ x -> x plus - x"
        let expected = SimpleLambda(DefaultVariable "x", applicationTff.CreateTerm [binOp.Plus; DefaultVariable "x"; applicationTff.CreateTerm [unOp.Minus; DefaultVariable "x"]], substitution)
        
        let got = callParse2String (parser, input)
        Assert.That(got.ToString(), Is.EqualTo (expected.ToString()))
        
    [<Test>]
    member this.Parentheses() =
        let inputNoParentheses = "\\ x -> x plus x - x"
        let expectedNoParentheses = SimpleLambda(
            DefaultVariable "x",
            applicationTff.CreateTerm [
                binOp.Minus
                applicationTff.CreateTerm [
                    binOp.Plus
                    DefaultVariable "x"
                    DefaultVariable "x"
                ]
                DefaultVariable "x"
            ], substitution)
        
        let inputWithParentheses = "\\ x -> x plus (x - x )"
        let expecedtWithParentheses = SimpleLambda(
            DefaultVariable "x",
            applicationTff.CreateTerm [
                binOp.Plus
                DefaultVariable "x"
                applicationTff.CreateTerm [
                    binOp.Minus
                    DefaultVariable "x"
                    DefaultVariable "x"
                ]
            ], substitution)
        
        let got = callParse2String (parser, inputNoParentheses)
        Assert.That(
            got.ToString(),
            Is.EqualTo (expectedNoParentheses.ToString()),
            "incorrect parsing no parentheses")
        
        let got = callParse2String (parser, inputWithParentheses)
        Assert.That(
            got.ToString(),
            Is.EqualTo (expecedtWithParentheses.ToString()),
            "incorrect parsing with parentheses")
        
    [<Test>]
    member this.ApplicationOfLambdaToArg() =
        let input = "(\\ x -> x plus x) 5"
    
        let x = DefaultVariable "x" :> IVariable
        let arg = DefaultVariable "5" :> ITerm

        let body = applicationTff.CreateTerm [binOp.Plus; x :> ITerm; x :> ITerm]
        let lambda = SimpleLambda(x, body, substitution)
        let expected = applicationTff.CreateTerm [lambda :> ITerm; arg]

        let got = callParse2String (parser, input)
        Assert.That(got.ToString(), Is.EqualTo(expected.ToString()))

    [<Test>]
    member this.ApplicationOfTwoLambdas() =
        let input = "(\\ x -> x) (\\ y -> y plus y)"

        let x = DefaultVariable "x" :> IVariable
        let y = DefaultVariable "y" :> IVariable

        let bodyInner = applicationTff.CreateTerm [binOp.Plus; y :> ITerm; y :> ITerm]
        let lambdaInner = SimpleLambda(y, bodyInner, substitution)

        let lambdaOuter = SimpleLambda(x, x :> ITerm, substitution)

        let expected = applicationTff.CreateTerm [lambdaOuter :> ITerm; lambdaInner :> ITerm]
        let got = callParse2String (parser, input)

        Assert.That(got.ToString(), Is.EqualTo(expected.ToString()))
    [<Test>]
    member this.ApplicationToLiteral() =
        let input = "(\\ x -> x plus 1) 2"

        let x = DefaultVariable "x" :> IVariable
        let one = DefaultVariable "1" :> ITerm
        let two = DefaultVariable "2" :> ITerm

        let body = applicationTff.CreateTerm [binOp.Plus; x :> ITerm; one]
        let lambda = SimpleLambda(x, body, substitution)

        let expected = applicationTff.CreateTerm [lambda :> ITerm; two]
        let got = callParse2String (parser, input)

        Assert.That(got.ToString(), Is.EqualTo(expected.ToString()))
    
    [<Test>]
    member this.NestedApplicationInLambda() =
        let input = "\\ x -> x y z"

        let x = DefaultVariable "x" :> IVariable
        let y = DefaultVariable "y" :> ITerm
        let z = DefaultVariable "z" :> ITerm

        let inner = applicationTff.CreateTerm [applicationTff.CreateTerm [x :> ITerm; y]; z]
        let lambda = SimpleLambda(x, inner, substitution)

        let expected = lambda :> ITerm
        let got = callParse2String (parser, input)

        Assert.That(got.ToString(), Is.EqualTo(expected.ToString()))

    [<Test>]
    member this.MultiLambda() =
        let input = "\\ x y -> y x 4"
        
        let x = DefaultVariable "x" :> IVariable
        let y = DefaultVariable "y" :> IVariable
        let four = DefaultVariable "4" :> ITerm
        
        let body = applicationTff.CreateTerm [
            applicationTff.CreateTerm [y; x]
            four
        ]
        let multiLambda = MultiLambda([x; y], body, substitution)
        
        let expected = multiLambda :> ITerm
        let got = callParse2String (parser, input)
        
        Assert.That(got.ToString(), Is.EqualTo(expected.ToString()))
        
    [<Test>]
    member this.MultiApplication() =
        let input = "(\\ x y -> y x 4) 3 2"
        
        let x = DefaultVariable "x" :> IVariable
        let y = DefaultVariable "y" :> IVariable
        let four = DefaultVariable "4" :> ITerm
        let three = DefaultVariable "3" :> ITerm
        let two = DefaultVariable "2" :> ITerm
        
        let body = applicationTff.CreateTerm [
            applicationTff.CreateTerm [y; x]
            four
        ]
        let multiLambda = MultiLambda([x; y], body, substitution)
        
        let mApplication = applicationTff.CreateTerm [
            multiLambda
            three
            two
        ]
        
        let expected = mApplication :> ITerm
        let got = callParse2String (parser, input)
        
        Assert.That(got.ToString(), Is.EqualTo(expected.ToString()))
        
    [<Test>]
    member this.MultiApplicationUnderflowArgs() =
        let input = "(\\ x y -> y x 4) 3"
        
        let x = DefaultVariable "x" :> IVariable
        let y = DefaultVariable "y" :> IVariable
        let four = DefaultVariable "4" :> ITerm
        let three = DefaultVariable "3" :> ITerm
        
        let body = applicationTff.CreateTerm [
            applicationTff.CreateTerm [y; x]
            four
        ]
        let multiLambda = MultiLambda([x; y], body, substitution)
        
        let mApplication = applicationTff.CreateTerm [
            multiLambda
            three
        ]
        
        let expected = mApplication :> ITerm
        let got = callParse2String (parser, input)
        
        Assert.That(got.ToString(), Is.EqualTo(expected.ToString()))

    [<Test>]
    member this.MultiApplicationOverflowArgs() =
        let input = "(\\ x y -> y x 4) 3 2 2 3 4"
        
        let x = DefaultVariable "x" :> IVariable
        let y = DefaultVariable "y" :> IVariable
        let four = DefaultVariable "4" :> ITerm
        let three = DefaultVariable "3" :> ITerm
        let two = DefaultVariable "2" :> ITerm

        let body = applicationTff.CreateTerm [
            applicationTff.CreateTerm [y; x]
            four
        ]
        let multiLambda = MultiLambda([x; y], body, substitution)
        
        let mApplication = applicationTff.CreateTerm [
            multiLambda
            three
            two
            two
            three
            four
        ]
        
        let expected = mApplication :> ITerm
        let got = callParse2String (parser, input)
        
        Assert.That(got.ToString(), Is.EqualTo(expected.ToString()))

    [<Test>]
    member this.MultiApplicationWithKeywords() =
        let input = "(\\ x y -> magicWord1 y x ) magicWord0 "
        
        let x = DefaultVariable "x" :> IVariable
        let y = DefaultVariable "y" :> IVariable
        let mw0 = DefaultVariable "magicWord0" :> ITerm
        let mw1 = DefaultVariable "magicWord1" :> ITerm
        
        let body = applicationTff.CreateTerm [
            applicationTff.CreateTerm [mw1; y]
            x
        ]
        let lambda = MultiLambda([x; y], body, substitution)
        let mAppl = applicationTff.CreateTerm [lambda; mw0]
        
        let expected = mAppl :> ITerm
        let got = callParse2String (parser, input)
        
        Assert.That(got.ToString(), Is.EqualTo(expected.ToString()))