module DistributionsTests

open NUnit.Framework
open FsUnit
open Distributions

[<TestFixture>]
type ``Calculate descriptive statistics for a sequence of values`` ()=
    [<Test>] member x.
        ``The arithmetic mean of a single value is the value itself``()=
        arithmetic_mean [0.9] |> should equal 0.9

    [<Test>] member x.
        ``The arithmetic mean of an arbitrary sequence of numbers is the known sum of those values divided by their count`` ()=
        arithmetic_mean [1.0; 2.0; 4.5] |> should equal 2.5

    [<Test>] member x.
        ``The geometric mean of a single value is the value itself`` ()=
         geometric_mean [0.8] |> should equal 0.8
    
    [<Test>] member x.
        ``The geometric mean of an arbitrary sequence of numbers is the known nth root of their product`` ()=
        geometric_mean [2.0; 2.0; 16.0] |> should (equal |> within 1E-8) 4.0

    [<Test>] member x.
        ``The harmonic mean of a single value is the value itself`` ()=
        harmonic_mean [0.2] |> should equal 0.2

    [<Test>] member x.
        ``The harmonic mean of an arbitrary sequence of number is a known good value`` ()=
        harmonic_mean [0.5; 2.0; 6.0] |> should equal 1.125

    [<Test>] member x.
        ``The standard deviation of a sequence of number is the square root of the sum of the squared deviations over n minus one`` ()=
        sample_standard_deviation [0.5; 2.0; 6.0] |> should (equal |> within 1E-8) 2.8431203515386

    [<Test>] member x.
        ``The skewness of a sequence of numbers is the standardized third moment of deviations from the mean`` ()=
        sample_skewness [0.5; 2.0; 6.0; 8.0; 9.0] |> should (equal |> within 1E-8) -0.3469734812

    [<Test>] member x.
        ``The kurtosis of a sequence of numbers is the standardized fourth moment of deviations from the mean`` ()=
        sample_kurtosis [0.5; 2.0; 6.0; 8.0; 9.0] |> should (equal |> within 1E-8) 0.532096723

[<TestFixture>]
type ``Calculate descriptive statistics for a paired sequence of values`` ()=
    
    member x.X1 = [1.0; 2.0; 4.0; 5.0]
    member x.X2 = [3.0; 2.0; 6.0; 10.0]

    [<Test>] member x.
        ``The population covariance of two sequences is the sum product of respective deviation from the mean divided by the count`` ()=
        //[(-2)(-2.25) + (-1)(-3.25) + (1)(0.75) + (2)(4.75)] / (n)
        population_covariance x.X1 x.X2  |> should equal 4.5

    [<Test>] member x.
        ``The sample covariance of two sequences is the sum product of respective deviation from the mean divided by the count minus one`` ()=
        sample_covariance x.X1 x.X2 |> should equal 6.0

    [<Test>] member x.
        ``The correlation coefficient of two sequences is their covariance divided by the product of their respective standard deviations`` ()=
        correlation x.X1 x.X2 |> should (equal |> within 1E-5 ) 0.914400914

    member x.a1 = [86.0; 97.0; 99.0; 100.0; 101.0; 103.0; 106.0; 110.0; 112.0; 113.0]
    member x.a2 = [0.0; 20.0; 28.0; 27.0; 50.0; 29.0; 7.0; 17.0; 6.0; 12.0]
    
    [<Test>] member x.
        ``The Spearman rank correlation of two variables is the Pearson correlation of their ranks`` ()=
        spearman_rank_correlation x.a1 x.a2 |> should (equal |> within 1E-5) -0.175757575 // 1 - 6 * 194 / (1000 - 10)

[<TestFixture>]
type ``Generate probability distributions out of sequences of values`` ()=
    [<Test>] member x.
        ``Bucketing values within a range produces a discretized pdf with the right probabilities`` ()=
        let v = discretize_distribution [0.5; 21.0; 22.0; 99.0] 0.0 100.0 5 in
        v |> should equal 
            (Seq.ofList [{Value=10.0; Probability=0.25};{Value=30.0; Probability=0.5};{Value=90.0; Probability=0.25}])

[<TestFixture>]
type ``Calculate descriptive statistics of discretized probability distributions`` ()=
    member x.X1 = [{Value=1.0; Probability=0.5};{Value=10.0; Probability=0.4};{Value=50.0; Probability=0.1};]

    [<Test>] member x.
        ``The expected value of a probability distribution is the known sumproduct of each value and its probability`` ()=
        expected_value x.X1 |> should equal 9.5

    [<Test>] member x.
        ``The variance of a probability distribution is expected x-squared minutes expected-x squared`` ()=
        variance x.X1 |> should (equal |> within 1E-8) 200.25

    [<Test>] member x.
        ``The standard deviation of a probability distribution is the square root of the variance`` ()=
        standard_deviation x.X1 |> should (equal |> within 1E-8) 14.1509717

