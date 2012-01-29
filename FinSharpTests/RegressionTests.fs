module RegressionTests

open NUnit.Framework
open FsUnit
open Regression

[<TestFixture>]
type ``Least Squares Linear Regression`` ()=
    [<Test>] member x.
        ``Least Squares Linear Regression fits two points exactly`` ()=
        let (slope, intercept) = least_squares_linear_regression [1.1; 3.1] [0.0; 1.0] in
        let t1 = slope |> should equal 2.0 in
        let t2 = intercept |> should equal 1.1 in ()

    [<Test>] member x.
        ``Least Squares Linear Regression fits five noisy flat points with zero slope and nonzero intercept`` ()=
        let (slope, intercept) = least_squares_linear_regression [1.0; 2.0; 1.0; 2.0; 1.0] [0.0; 1.0; 2.0; 3.0; 4.0] in
        let t1 = slope |> should equal 0.0 in
        let t2 = intercept |> should equal 1.4 in ()

    [<Test>] member x.
        ``Least Squares Linear Regression fits arbitrary points with known slope and intercept`` ()=
        let (slope, intercept) = least_squares_linear_regression [12.0; 22.0; 11.0; 25.0; 11.0] [0.0; 1.0; 2.0; 3.0; 4.0] in
        let t1 = slope |> should equal 0.1 in
        let t2 = intercept |> should equal 16.0 in ()

