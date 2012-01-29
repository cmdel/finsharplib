#light

module InitialTests

open NUnit.Framework
open FsUnit

type LightBulb(state) =
   member x.On = state
   override x.ToString() =
       match x.On with
       | true  -> "On"
       | false -> "Off"

[<TestFixture>] 
type ``Given a LightBulb that has had its state set to true`` ()=
   let lightBulb = new LightBulb(true)

   [<Test>] member x.
    ``when I ask whether it is On it answers true.`` ()=
           lightBulb.On |> should be True

