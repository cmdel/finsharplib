module QuasiRandomTests

open NUnit.Framework
open FsUnit
open QuasiRandom

[<TestFixture>]
type ``Quasi-Random Sequences`` ()=

    [<Test>] member x.
        ``Binary Van Der Corput i=1 should be 1/2`` ()=
        van_der_corput_sequence 1 2 |> should equal 0.5

    [<Test>] member x.
        ``Binary Van Der Corput i=2 should be 1/4`` ()=
        van_der_corput_sequence 2 2 |> should equal 0.25

    [<Test>] member x.
        ``Binary Van Der Corput i=3 should be 3/4`` ()=
        van_der_corput_sequence 3 2 |> should equal 0.75

    [<Test>] member x.
        ``Binary Van Der Corput i=11 should be 13/16`` ()=
        van_der_corput_sequence 11 2 |> should equal 0.8125

    [<Test>] member x.
        ``Ternary Van Der Corput i=3 should be 2/3`` ()=
        van_der_corput_sequence 2 3 |> should equal (2.0 / 3.0)

    [<Test>] member x.
        ``Halton Sequence is the multidimensional generalization of the Van Der Corput sequence`` ()=
        halton_sequence 1 [2; 3] |> Seq.toList |> should equal [0.5; 0.33333333333333]

    [<Test>] member x.``Gray code of 0 is 0`` ()= gray_code 0 |> should equal 0
    [<Test>] member x.``Gray code of 1 is 1`` ()= gray_code 1 |> should equal 1
    [<Test>] member x.``Gray code of 2 is 3`` ()= gray_code 2 |> should equal 3
    [<Test>] member x.``Gray code of 3 is 2`` ()= gray_code 3 |> should equal 2
    [<Test>] member x.``Gray code of 4 is 6`` ()= gray_code 4 |> should equal 6
    [<Test>] member x.``Gray code of 5 is 7`` ()= gray_code 5 |> should equal 7
    [<Test>] member x.``Gray code of 6 is 5`` ()= gray_code 6 |> should equal 5
    [<Test>] member x.``Gray code of 7 is 5`` ()= gray_code 7 |> should equal 4

    [<Test>] member x.
        ``One-Dimensional Sobol Sequence produces the correct first ten values`` ()=
        Seq.toList(Seq.map Seq.toList (sobol_sequence 10 1)) |> should equal [[0.0; 0.5; 0.75; 0.25; 0.375; 0.875; 0.625; 0.125; 0.1875; 0.6875]]

    [<Test>] member x.
        ``Five-Dimensional Sobol Sequence produces the correct first fifty values`` ()=
        Seq.toList(Seq.map Seq.toList (sobol_sequence 10 5)) |> should equal [[0.0;0.5;0.75;0.25;0.375;0.875;0.625;0.125;0.1875;0.6875];
                                                                            [0.0;0.5;0.25;0.75;0.375;0.875;0.125;0.625;0.3125;0.8125];
                                                                            [0.0;0.5;0.25;0.75;0.625;0.125;0.875;0.375;0.9375;0.4375];
                                                                            [0.0;0.5;0.25;0.75;0.875;0.375;0.625;0.125;0.4375;0.9375];
                                                                            [0.0;0.5;0.75;0.25;0.375;0.875;0.625;0.125;0.5625;0.0625]]
