module Mephi.Cybernetics.Mace.AbstractMachines.BetaReduction.Environment

open Mephi.Cybernetics.Race.Core.Host
open Mephi.Cybernetics.Race.Core.Host.Computing
open Mephi.Cybernetics.Race.Core.Host.Definitions

let internal defaultBRSubstitution : ISubstitution = DefaultSubstitution :> ISubstitution
let internal defaultBRApplicationTff : IApplicationTff = MultiApplicationTff :> IApplicationTff
let internal defaultBRSimpleLambdaTff = Tff.SimpleLambdaTff(defaultBRSubstitution)
let internal defaultBRMultiLambdaTff = Tff.MultiLambdaTff(defaultBRSubstitution)

let internal brTffs : ITff list = [
    defaultBRApplicationTff :> ITff
    defaultBRSimpleLambdaTff :> ITff
    defaultBRMultiLambdaTff :> ITff
]

let internal brAtomics : IAtomic seq = Atomic.brAtomics

type Environment() =
    inherit Definitions.Environment(brTffs, brAtomics)

    