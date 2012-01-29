module PseudoRandomTests

open NUnit.Framework
open FsUnit
open PseudoRandom

let between_exclusive low high x = x > low && x < high

let between_inclusive low high x = x > low && x < high

let get_bucket width x = floor (x / width)

let histogram width data = Seq.groupBy (get_bucket width) data |> Seq.map (fun (xval, items) -> Seq.length items)

[<TestFixture>]
type ``Generate Pseudo-Random Numbers using well-known PRNG algorithms`` ()=

    [<Test>] member x.
        ``Calling a predefined-sequence random number generator with a period of 2 in functional style generates expected values`` ()=
        let initial_state = new_predefined_sequence_state [0.25; 0.75] in
        let (state1, value1) = next_predefined_sequence_value initial_state in
        let (state2, value2) = next_predefined_sequence_value state1 in
        (value1, value2) |> should equal (0.25, 0.75)

    [<Test>] member x.
        ``Calling a predefined-sequence random number generator using lazy evaluation generate expected values`` ()=
        Seq.toList (random_sequence next_predefined_sequence_value (new_predefined_sequence_state [0.25; 0.75]) 3) |> should equal [0.25; 0.75; 0.25]

    [<Test>] member x.
        ``Mersenne Twister random number generator produces expected values`` ()=
        Seq.toList (random_sequence next_mersenne_value (new_mersenne_state 0) 3) |> should equal [0.111172186469466; 0.853855715564884; 0.519077533278399]

    member x.MersenneValues = Seq.toList (random_sequence next_mersenne_value (new_mersenne_state 0) 10000)
    
    [<Test>] member x.
        ``Mersenne Twister random values should range between 0.0 and 1.0 exclusive`` ()=
        List.forall (between_exclusive 0.0 1.0) x.MersenneValues |> should be True

    [<Test>] member x.
        ``Mersenne Twister covers uniform range 0 to 1`` ()=
        histogram 0.01 x.MersenneValues |> ranges (5, 200)

    [<Test>] member x.
        ``Blum-Blum-Shub generator produces expected values`` ()=
        Seq.toList (random_sequence next_blum_blum_shub_value (new_blum_blum_shub_state (11u, 19u, 3u)) 3) |> should equal [double 10 / double 211; double 82 / double 211; double 83 / double 211]

    member x.BlumBlumShub = Seq.toList (random_sequence  next_blum_blum_shub_value (new_blum_blum_shub_state (11u, 19u, 3u)) 10)

    [<Test>] member x.
        ``Blum-Blum-Shub random values should range between 0.0 and 1.0 exclusive`` ()=
        List.forall (between_exclusive 0.0 1.0) x.BlumBlumShub |> should be True

    [<Test>] member x.
        ``Blum-Blum-Shub covers uniform range 0 to 1`` ()=
        histogram 0.2 x.BlumBlumShub |> ranges (1, 3)

    [<Test>] member x.
        ``Generalized Lagged Fibonacci Generator combines historical values and produces shifted new state`` ()=
        let ((ancient, recent), x) = next_generalized_lagged_fibonacci_generator (+) ([1u;2u],[10u;20u]) in
        x |> should equal (double 12 / ((double System.UInt32.MaxValue)+1.0));
        ancient |> should equal [2u;10u];
        recent |> should equal [20u;11u]

    [<Test>] member x.
        ``Marsaglia 3 Shift Register generator produces expected initial sequence`` ()=
        let (s1, v1) = next_marsaglia_three_shift_register_value 3u in
        let (s2, v2) = next_marsaglia_three_shift_register_value s1 in
        s1 |> should equal 12977747u;
        v1 |> should equal 0.003021617188;
        s2 |> should equal 2154614579u;
