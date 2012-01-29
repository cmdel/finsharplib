module DistributionRandomTests

open NUnit.Framework
open FsUnit
open DistributionFunctions

[<TestFixture>]
type ``Normal Distribution Functions`` ()=
    [<Test>] member x.
        ``The Box Muller Transform on two uniform random values is a known normally distributed value`` ()=
        let (y1, y2) = box_muller_transform 0.38 0.41 in
        let first_value_correct = y1 |> should (equal |> within 1E-8) -1.17454726 in
        let second_value_correct = y2 |> should (equal |> within 1E-8) 0.7453903574 in ()

    [<Test>] member x.
        ``Transforming an arbitrary sampled value 10 by stretching it by 2 and shifting it by -1 results in 19`` ()=
        transform 10.0 2.0 -1.0 |> should equal 19.0

    [<Test>] member x.
        ``The Box-Muller filter rejects pairs of values outside the unit circle before accepting values within the circle`` ()=
        let i = ref -1 in
        let samples = [1.2; 1.4; 0.2; 0.4] in
        let (s, u, v) = marsaglia_polar_filter (fun () -> i := !i + 1; samples.[!i]) in
        let verify_s = s |> should (equal |> within 1E-8) 0.4 in
        let verify_u = u |> should (equal |> within 1E-8) -0.6 in
        let verify_v = v |> should (equal |> within 1E-8) -0.2 in ()
    
    [<Test>] member x.
        ``Box-Muller Polar Transform takes two uniform random values and transforms them to the expected normally distributed values`` ()=
        let i = ref -1 in
        let samples = [0.2; 0.4] in 
        let (a, b) = marsaglia_polar_transform (fun () -> i := !i + 1; samples.[!i]) in
        let verify_a = a |> should (equal |> within 1E-8) -1.284259833 in
        let verify_b = b |> should (equal |> within 1E-8) -0.428086611 in
        ()
        
    [<Test>] member x.
        ``Generalized rejection sampling accepts a sample if the pdf is greater than the drawn y value`` ()=
        generalized_rejection_sample (fun () -> 0.4) (fun x -> 0.81) 0.0 100.0 0.8 |> should equal 40.0

    [<Test>] member x.
        ``Generalized rejection sampling rejects a sample if the pdf is less than the drawn y value`` ()=
        let i = ref -1 in
        let samples = [0.2; 0.9; 0.3; 0.6] in 
        // First try: x1 = 0.2 * 100, y1 = 0.9 * 0.1.  pdf(20) = 0.01 < 0.09. Reject
        // Second try: x2= 0.3 * 100, y2 = 0.6 * 0.1.  pdf(30) = 0.99 > 0.06. Accept
        generalized_rejection_sample (fun () -> i := !i + 1; samples.[!i])
            (fun x -> if x = 20.0 then 0.01 elif x = 30.0 then 0.99 else failwith("unexpected")) 0.0 100.0 0.8
            |> should equal 30.0

//    [<Test>] member x.
//        ``The Central Limit Transform generator