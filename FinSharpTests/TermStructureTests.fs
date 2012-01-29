#light

module TermStructureTests

open NUnit.Framework
open FsUnit
open TermStructure

[<TestFixture>]
type ``Bootstrap yield curve to spot curve`` ()=
    [<Test>] member x.
        ``A yield curve with a single point one year in the future is the same as spot`` ()=
        bootstrap_spot_rates [0.09] [] |> should equal [0.09]

    [<Test>] member x.
        ``A flat yield curve with multiple points is the same as spot`` ()=
        bootstrap_spot_rates [0.09; 0.09] [] |> should equal [0.09; 0.09]

    [<Test>] member x.
        ``Arbitrary upward sloping yield curve produces steeper spot with correct values`` ()=
        bootstrap_spot_rates [0.06; 0.07; 0.20] [] |> should equal [0.06; 0.0703534769675696; 0.235201475103649]

    [<Test>] member x.
        ``Arbitrary downward sloping yield curve produces steeper downward sloping spot with correct values`` ()=
        bootstrap_spot_rates [0.06; 0.05; 0.02] [] |> should equal [0.06; 0.04975244606517; 0.019359076919939]

[<TestFixture>]
type ``Spot to forward rate curve`` ()=
    [<Test>] member x.
        ``A flate spot curve produces a flat forward curve`` ()=
        spot_to_forward_rates [0.04; 0.04] |> should equal (Seq.ofList [0.04])

    [<Test>] member x.
        ``An arbitrary upward sloping spot curve produces a steeper forward curve with correct values`` ()=
        spot_to_forward_rates [0.05; 0.06; 0.09] |> should equal (Seq.ofList [0.07009523809524; 0.15257119971520])

    [<Test>] member x.
        ``An arbitrary downward sloping spot curve produces a steeper downward sloping forward curve with correct values`` ()=
        spot_to_forward_rates [0.06; 0.05; 0.045] |> should equal (Seq.ofList [0.04009433962264; 0.03507131519274])
