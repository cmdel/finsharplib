[<AutoOpen()>]
module Common

// Short-hand aliases and helper functions

let DebugString x = printfn "%A\n" x |> ignore

let DebugFloat x = printfn "%f" x |> ignore

let DebugSeq s =
    printfn "Seq ["
    Seq.iter (fun x -> DebugFloat x) s
    printfn "]"

let DebugSeqSeq ss = Seq.iter (fun s -> DebugSeq s) ss

let exp = System.Math.Exp

let ln = System.Math.Log

let cos = System.Math.Cos

let sin = System.Math.Sin

let pi = System.Math.PI

let sum = Seq.sum<float>

let map = Seq.map

let product x = Seq.reduce<float> (fun acc xi -> acc * xi) x

let multiply (x:float) (y:float) = x * y

let subtract (x:float) (y:float) = x - y

let length x = double (Seq.length x)

let floor (x : float) = System.Math.Floor x

let inverse x = 1.0 / x

let negative x = (-1.0) * x

let square (x : float) = x * x

let cube x = x ** 3.0

let fourth x = x ** 4.0

let ceil (x : float) = System.Math.Ceiling(x)

// Transposes a rectangular seq<seq> by inverting rows and columns. caller must be sure inner sequences have same length
let transpose ss =
    let array = ss |> Seq.map Seq.toArray |> Seq.toArray
    seq { for inner in 0 .. array.[0].Length - 1 do yield seq { for outer in 0 .. array.Length - 1 do yield array.[outer].[inner] } }