module Mephi.Cybernetics.Mace.AbstractMachines.Parser


open System
open Mephi.Cybernetics.Race.Core.Host.Computing
open Mephi.Cybernetics.Race.Core.Host.Definitions
open Mephi.Cybernetics.Race.Core.Host.Definitions.Tff
open Mephi.Cybernetics.Mace.Core

open FParsec
open Microsoft.FSharp.Core

exception UnsupportedArityException of string
exception ParseException of string

let RunParser(parser : Parser<ITerm, unit>, input : string) : ITerm =
    match run (spaces >>. parser .>> eof) input with
    | CharParsers.Success(result, _, _) -> result
    | CharParsers.Failure(errorMsg, _, pos) ->
        raise (ParseException($"Parse error at %A{pos}: %s{errorMsg}"))

type Operator =
    | UnOp of struct {| keyword : Keyword; term : IAtomic; priority : int |}
    | BinOp of struct {| keyword : Keyword; term : IAtomic; priority : int; associativity : Associativity |}
    member internal this.Arity with get() : int =
        match this with
        | UnOp _ -> 1
        | BinOp _ -> 2
    member internal this.Priority with get() =
        match this with
        | UnOp op -> op.priority
        | BinOp op -> op.priority
    member internal this.FParsecAssociativity with get() =
        match this with
        | UnOp _ -> FParsec.Associativity.None
        | BinOp op -> op.associativity.ToFParsec()
    member internal this.Term with get() =
        match this with
        | UnOp op -> op.term
        | BinOp op -> op.term
    member internal this.Keyword with get() =
        match this with
        | UnOp op -> op.keyword
        | BinOp op -> op.keyword
and Operators = List<Operator> 
and Config = {
    applicationTff : IApplicationTff
    simpleLambdaTff : ITff
    multiLambdaTff : Option<ITff>
    operators : Operators
    keywords : Keyword list
    parseNumericConsts : bool
    ifStatements : bool
}

and Keyword = string
and Associativity =
    | None
    | Left
    | Right
    member internal this.ToFParsec() : FParsec.Associativity =
        match this with
        | None -> FParsec.Associativity.None
        | Left -> FParsec.Associativity.Left
        | Right -> FParsec.Associativity.Right

let internal Operator2ITerm op : ITerm =
    match op with
    | UnOp op -> op.term 
    | BinOp op -> op.term
    
let internal addOps ( opParser : OperatorPrecedenceParser<ITerm,unit,unit>, ops : Operators, applicationTff : IApplicationTff ) : OperatorPrecedenceParser<ITerm,unit,unit> =
    let newOpP (op : Operator) : Operator<ITerm, unit, unit> =
        match op.Arity with
        | 1 -> PrefixOperator (op.Keyword, spaces, op.Priority, true, (fun arg -> applicationTff.CreateTerm [op.Term; arg]))
        | 2 -> InfixOperator (op.Keyword, spaces, op.Priority, op.FParsecAssociativity, (fun left right -> applicationTff.CreateTerm [op.Term; left; right]))
        | _ -> raise (UnsupportedArityException("unsupported operator arity"))
    
    ops |> List.iter (fun operator -> opParser.AddOperator(newOpP operator))
    opParser
    

type Number =
    | Int of int
    | Float of float
    override this.ToString (): string =
        match this with
        | Int(n) -> n |> string
        | Float(n) -> n |> string

let internal isAsciiIdStart c = isAsciiLetter c || c = '_'
let internal isAsciiIdContinue c = isAsciiLetter c || isDigit c || c = '_' || c = '''
let internal pID =
    identifier (
        IdentifierOptions(
            isAsciiIdStart = isAsciiIdStart,
            isAsciiIdContinue = isAsciiIdContinue
        )
    )
    
let internal pStringLiteral : Parser<string, unit> =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))
            
let internal float2Number (n : float) : Number =
    if n = floor n then Number.Int(n |> int)
    else Number.Float(n)

let internal defaultCallback(name : string) : ITerm = DefaultVariable(name) :> ITerm
let internal defaultNumericCallback (n : Number) : ITerm = DefaultVariable(n |> string) :> ITerm
let internal defaultVarCallback = defaultCallback
let internal defaultStringLiteralCallback= defaultCallback
let internal defaultSimpleLambdaCallback = defaultCallback
let internal defaultMultiLambdaCallback= defaultCallback
let internal defaultKeywordCallback = defaultCallback

let internal getDefaultIfCallback (appTff : IApplicationTff, cond, ifTrue, ifFalse) : ITerm =
    appTff.CreateTerm [ CombinatorIf.IF; cond; ifTrue; ifFalse ]

type LambdaSyntax = string * string
let internal defaultLamdaSyntax = LambdaSyntax("\\", "->")

type IfSyntax = string * string * string
let internal defaultIfSyntax = IfSyntax("if", "then", "else")

type Parser( cfg: Config ) =
    let applicationTff = cfg.applicationTff
    let simpleLambdaTff = cfg.simpleLambdaTff
    
    let mutable variableCallback = defaultVarCallback
    let mutable numericCallback = defaultNumericCallback
    let mutable stringLiteralCallback = defaultStringLiteralCallback
    let mutable keywordCallback = defaultKeywordCallback
    let mutable ifCallback = fun a b c -> getDefaultIfCallback(applicationTff, a, b, c)
    
    let mutable _lambdaSyntax = defaultLamdaSyntax
    let mutable _ifSyntax = defaultIfSyntax
    
    member this.pVariableCallback
        with get() = variableCallback
        and set( pc : PCallbackString ) = variableCallback <- pc
    member this.pNumericCallback
        with get() = numericCallback
        and set( pc : PCallback<Number> ) = numericCallback <- pc
    member this.pStringLiteralCallback
        with get() = stringLiteralCallback
        and set( pc : PCallbackString ) = stringLiteralCallback <- pc
    member this.pKeywordCallback
        with get() = keywordCallback
        and set( pc : PCallbackString ) = keywordCallback <- pc
    member this.pIfCallback
        with get() = ifCallback
        and set( pc : ITerm -> ITerm -> ITerm -> ITerm ) = ifCallback <- pc
    member this.lamdbaSyntax
        with get() = _lambdaSyntax
        and set( lS : LambdaSyntax ) = _lambdaSyntax <- lS
    member this.ifSyntax
        with get() = _ifSyntax
        and set( ifS : IfSyntax ) = _ifSyntax <- ifS
        
    member this.Build : Parser<ITerm, unit> =
        let pExpr, pExprRef = createParserForwardedToRef<ITerm, unit>()

        let pKeyword: Parser<ITerm, unit> =
            cfg.keywords |> List.map (fun kw -> pstring kw |>> this.pKeywordCallback) |> choice
        
        let pVariable: Parser<ITerm, unit> =
            pID |>> (fun name -> this.pVariableCallback name)
            
        let pStrLiteral: Parser<ITerm, unit> =
            pStringLiteral |>> this.pStringLiteralCallback
            
        let lambdaSymbol1, lambdaSymbol2 = this.lamdbaSyntax
        let pSingleAbstraction: Parser<ITerm, unit> =
            pipe2
                (pstring lambdaSymbol1 >>. spaces >>. pVariable .>> spaces .>> pstring lambdaSymbol2 .>> spaces)
                pExpr
                (fun param body -> simpleLambdaTff.CreateTerm [param; body] :> ITerm)

        let enableMultiLambda = cfg.multiLambdaTff.IsSome
        let pMultiAbstraction: Option<Parser<ITerm, unit>> =
            match cfg.multiLambdaTff with
            | Some(mLTff) -> 
                let p =
                    pipe2
                        (pstring lambdaSymbol1 >>. spaces >>. many1 (pVariable .>> spaces) .>> pstring lambdaSymbol2 .>> spaces)
                        pExpr
                        (fun args body -> mLTff.CreateTerm (args @ [body]) :> ITerm)
                Some(p)
            | _ -> Option.None
                
        let pAbstraction =
            if enableMultiLambda then
                pMultiAbstraction.Value <|> pSingleAbstraction
            else
                pSingleAbstraction

        let pParenthizedExpr: Parser<ITerm, unit> =
            between (pstring "(" .>> spaces) (pstring ")" .>> spaces) pExpr
        
        let pNumber: Parser<ITerm, unit> = choice [
            pfloat .>> spaces |>> (fun num -> this.pNumericCallback (float2Number num))
        ]
        let pNumber = if cfg.parseNumericConsts then [pNumber] else []
        
        let pIF =
            match cfg.ifStatements with
            | false -> []
            | true ->
                let ifKW, thenKW, elseKW = this.ifSyntax
                let ifClause =
                    pipe3
                        (pstring ifKW   >>. spaces >>. pExpr .>> spaces)
                        (pstring thenKW >>. spaces >>. pExpr .>> spaces)
                        (pstring elseKW >>. spaces >>. pExpr .>> spaces)
                        this.pIfCallback
                [ ifClause ]
                    
        
        let pAtom: Parser<ITerm, unit> =
            choice ([
                pParenthizedExpr
                pAbstraction
                pKeyword
                pVariable
                pStrLiteral
            ] @ pIF @ pNumber) .>> spaces
        
        let opParser = OperatorPrecedenceParser<ITerm, unit, unit>()
        let opParser = addOps(opParser, cfg.operators, applicationTff)
        opParser.TermParser <- pAtom

        do pExprRef.Value <-
            many1 (opParser.ExpressionParser .>> spaces)
            |>> fun terms ->
                match terms with
                | func :: args ->
                    if not enableMultiLambda || List.length args < 2 then
                        List.fold (fun acc arg -> applicationTff.CreateTerm [acc; arg] :> ITerm) func args
                    else
                        applicationTff.CreateTerm ([func] @ args)
                | [] -> failwith "Empty application chain"
                
        pExpr
        
    member this.Parse(input: string) : Result<ITerm, Exception> =
        let parser = this.Build
        try Result.Ok(RunParser(parser, input)) with
        | :? ParseException as e -> Result.Error(e)

and PCallback<'T> = 'T -> ITerm
and PCallbackString = PCallback<string>
