module MultivariateTests

open NUnit.Framework
open FsUnit
open Multivariate

[<TestFixture>]
type ``Cholesky Decomposition`` ()=
    [<Test>] member x.
        ``The Cholesky Decomposition of a one by one matrix is the matrix itself`` ()=
        let m = Array2D.create 1 1 3.2 in
        let c = cholesky_decomposition m in
        let length = Array2D.length1 c |> should equal 1 in
        let width = Array2D.length2 c |> should equal 1 in
        c.[0,0] |> should equal (sqrt 3.2)

    [<Test>] member x.
        ``The Cholesky Decomposition of a two by two covariance matrix produces the expected matrix root`` ()=
        let m = Array2D.create 2 2 0.0 in
        let x1 = m.[0,0] <- 1.0 in
        let x2 = m.[0,1] <- 0.6 in
        let x3 = m.[1,0] <- 0.6 in
        let x4 = m.[1,1] <- 1.0 in
        let c = cholesky_decomposition m in
        let length = Array2D.length1 c |> should equal 2 in
        let width = Array2D.length2 c |> should equal 2 in
        let c1 = c.[0,0] |> should equal 1.0 in
        let c2 = c.[0,1] |> should equal 0.0 in
        let c3 = c.[1,0] |> should equal 0.6 in
        let c4 = c.[1,1] |> should equal 0.8 in ()

    [<Test>] member x.
        ``The covariance matrix of three time series is a three by three lower symmetric matrix with the corresponding covariances`` ()=
        let cm = covariance_matrix [[1.0; 2.0; 3.0];[2.0; 3.0; 7.0];[5.0; 3.0; 12.0]] in
        let length = Array2D.length1 cm |> should equal 3 in
        let width = Array2D.length2 cm |> should equal 3 in
        let d1 = cm.[0,0] |> should equal (2.0/3.0) in
        let d2 = cm.[1,1] |> should equal (14.0/3.0) in
        let d3 = cm.[2,2] |> should equal (134.0/9.0) in
        let c1 = cm.[1,0] |> should equal (5.0/3.0) in
        let c1 = cm.[2,0] |> should equal (7.0/3.0) in
        let c1 = cm.[2,1] |> should equal (23.0/3.0) in ()

    [<Test>] member x.
        ``The correlation matrix of three time series is a three by three lower symmetric matrix with the corresponding correlations`` ()=
        let cm = correlation_matrix [[1.0; 2.0; 3.0];[2.0; 3.0; 7.0];[5.0; 3.0; 12.0]] in
        let length = Array2D.length1 cm |> should equal 3 in
        let width = Array2D.length2 cm |> should equal 3 in
        let d1 = cm.[0,0] |> should equal 1.0 in
        let d2 = cm.[1,1] |> should equal 1.0 in
        let d3 = cm.[2,2] |> should equal 1.0 in
        let c1 = cm.[1,0] |> should equal 0.944911183 in
        let c1 = cm.[2,0] |> should equal 0.740612897 in
        let c1 = cm.[2,1] |> should equal 0.919754765 in ()

