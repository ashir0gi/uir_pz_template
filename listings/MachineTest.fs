module Mephi.Cybernetics.Mace.AbstractMachines.Tests.BetaReduction.MachineTest

open Mephi.Cybernetics.Mace.AbstractMachines.BetaReduction.Machine
open Mephi.Cybernetics.Mace.Core
open Mephi.Cybernetics.Race.Core.Host.Computing
open Mephi.Cybernetics.Race.Core.Host.Definitions
open Mephi.Cybernetics.Race.Core.Host.Definitions.Tff
open NUnit.Framework

[<TestFixture>]
type MachineTest() =

    [<Test>]
    member this.ParseInput() =
        Assert.That(true)