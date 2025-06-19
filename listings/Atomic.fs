module Mephi.Cybernetics.Mace.AbstractMachines.BetaReduction.Atomic

open Mephi.Cybernetics.Race.Core.Host.Definitions

[<CustomEquality;NoComparison>]
type Data =
    | Str of string
    | Int of int
    | Float of double
    | Bool of bool
    | Null
    | BinOp of string * (Data -> Data -> Data)
    | UnOp of string * (Data -> Data)
    | IfComb
    | YComb // Y := \f -> (\x -> f (x x)) (\x -> f (x x))
    with
        override this.Equals(obj) =
            match obj with
            | :? Data as d ->
                match this, d with
                | Str a, Str b -> a = b
                | Int a, Int b -> a = b
                | Float a, Float b -> a = b
                | Bool a, Bool b -> a = b
                | Null, Null -> true
                | BinOp (s1, _), BinOp (s2, _) -> s1 = s2
                | UnOp (s1, _), UnOp (s2, _) -> s1 = s2
                | IfComb, IfComb -> true
                | YComb, YComb -> true
                | _ -> false
            | _ -> false

        interface IAtomic with
            member x.Arity =
                match x with
                | BinOp _ -> 2
                | UnOp _ -> 1
                | IfComb -> 3
                | YComb -> 1
                | _ -> 0

            member x.IsReducibleWith args =
                (x :> IAtomic).Arity = Seq.length args

            member x.ReduceWithArguments(args, _) =
                let argList = Seq.toList args |> List.map (fun a -> a :?> Data)
                match x, argList with
                | BinOp (_, f), [a; b] -> f a b
                | UnOp (_, f), [a] -> f a
                | IfComb, _ ->
                    match argList with
                    | cond :: ifTrue :: ifFalse :: tail ->
                        match cond with
                        | Bool true -> ifTrue
                        | Bool false -> ifFalse
                        | _ -> failwith "IfComb condition must be Bool"
                    | _ -> failwith "IfComb requires exactly 3 arguments"
                | YComb, _ -> failwith "TODO: Здесь нужен Term для YComb"
                | _ -> failwith $"Cannot reduce term: {x}; with arguments: {argList}"
        override this.ToString() =
            match this with
            | Str s -> s
            | Int i -> $"Int {i}"
            | Float f -> $"Float {f}"
            | Bool b -> $"Bool {b}"
            | Null -> "Null"
            | BinOp (name, _) -> $"BinOp ({name})"
            | UnOp (name, _) -> $"UnOp ({name})"
            | IfComb -> "IfComb"
            | YComb -> "YComb"
let internal unOpMinusName = "-"
let internal binOpPlusName = "+"
let internal binOpMinusName = "-"
let internal binOpMultName = "*"
let internal binOpEqualsName = "="
let internal unOpNotName = "not"

let areEqualData a b =
    match a, b with
    | Str x, Str y -> x = y
    | Int x, Int y -> x = y
    | Float x, Float y -> x = y
    | Bool x, Bool y -> x = y
    | Null, Null -> true
    | YComb, YComb -> true
    | BinOp (n1, _), BinOp (n2, _) -> n1 = n2
    | UnOp (n1, _), UnOp (n2, _) -> n1 = n2
    | _ -> false

let internal atomicUnOpMinus = UnOp(unOpMinusName, function
    | Int x -> Int(-x)
    | Float x -> Float(-x)
    | _ -> failwith "Invalid - operand")

let internal atomicUnOpNot = UnOp(unOpNotName, function
    | Bool x -> Bool(not x)
    | _ -> failwith "Invalid not operand")

let public atomicBinOpPlus = BinOp(binOpPlusName, fun a b ->
    match a, b with
    | Int x, Int y -> Int(x + y)
    | Float x, Float y -> Float(x + y)
    | _ -> failwith "Invalid + operands")

let internal atomicBinOpMinus = BinOp(binOpMinusName, fun a b ->
    match a, b with
    | Int x, Int y -> Int(x - y)
    | Float x, Float y -> Float(x - y)
    | _ -> failwith "Invalid - operands")

let internal atomicBinOpMult = BinOp(binOpMultName, fun a b ->
    match a, b with
    | Int x, Int y -> Int(x * y)
    | Float x, Float y -> Float(x * y)
    | _ -> failwith "Invalid * operands")

let internal atomicBinOpEquals = BinOp(binOpEqualsName, fun a b ->
    Bool (a.Equals b))

let internal atomicBinOpLess = BinOp("<", fun a b ->
    match a, b with
    | Int x, Int y -> Bool (x < y)
    | Float x, Float y -> Bool (x < y)
    | _ -> failwith "Invalid < operands")

let internal atomicBinOpMore = BinOp(">", fun a b ->
    match a, b with
    | Int x, Int y -> Bool (x > y)
    | Float x, Float y -> Bool (x > y)
    | _ -> failwith "Invalid > operands")

let internal atomicBinOpLessEq = BinOp("<=", fun a b ->
     match a, b with
     | Int x, Int y -> Bool (x <= y)
     | Float x, Float y -> Bool (x <= y)
     | _ -> failwith "Invalid <= operands")

let internal atomicBinOpMoreEq = BinOp(">=", fun a b ->
     match a, b with
     | Int x, Int y -> Bool (x >= y)
     | Float x, Float y -> Bool (x >= y)
     | _ -> failwith "Invalid >= operands")
let internal atomicBinOpAnd = BinOp("and", fun a b ->
     match a, b with
     | Bool x, Bool y -> Bool (x && y)
     | _ -> failwith "Invalid and operands")

let internal atomicBinOpOr = BinOp("or", fun a b ->
     match a, b with
     | Bool x, Bool y -> Bool (x || y)
     | _ -> failwith "Invalid or operands")

let brOperators : Data list = [
    atomicBinOpPlus
    atomicBinOpMinus
    atomicBinOpMult
    atomicBinOpEquals
    atomicBinOpLess
    atomicBinOpMore
    atomicBinOpLessEq
    atomicBinOpMoreEq
    atomicBinOpAnd
    atomicBinOpOr
    atomicUnOpNot
    atomicUnOpMinus
]

let brAtomics : IAtomic list =
    brOperators @ [
        YComb
        Str "sample"
        Int 0
        Float 0.0
        Bool true
        Bool false
        Null
    ] |> List.map (fun x -> x :> IAtomic)