module CommonTests

open NUnit.Framework
open FsUnit
open Common

[<TestFixture>]
type ``Tests of common functions`` ()=
    [<Test>] member x.
        ``Transposing a two by two sequence of sequences inverts rows and columns`` ()=
        [|[|1;2;3|];[|4;5;6|]|] |> transpose |> Seq.map Seq.toArray |> Seq.toArray |> should equal [|[|1;4|];[|2;5|];[|3;6|]|]

