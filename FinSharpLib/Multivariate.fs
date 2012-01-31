/// <summary>
/// Multivariate analysis functions including correlation and covariance matrix
/// calculation, Cholesky factorization and principal components analysis.
/// </summary>
/// <DisplayName>Multivariate Analysis</DisplayName>
module Multivariate

open Distributions

///<summary>
///Computes the Cholesky Factorization of the given correlation matrix.
///</summary>
///<param name="m">An n by n symmetric positive definite matrix</param>
///<remarks>
///Cholesky Decomposition takes a symmetric positive definite matrix M and solves for a lower
///triangular matrix L such that
///<equation>$$L \cdot  L^T = M$$</equation>
///Each cell in the original matrix M is the sum-product of unknown values in L. Starting in
///the first row and first column of the matrix, we can immediately solve for one cell in L:
///<equation>$$L_{1,1} \cdot L_{1,1} = M_{1,1}$$</equation>
///With that new value, we can solve for the next row of the first column, where there is only one new unknown:
///<equation>$$L_{2,1} \cdot L_{2,1} + L_{2,2} \cdot L_{2,2} = M_{2,1}$$</equation>
///Proceding this way down the first column takes out all the unknowns from that column, leaving a recursively smaller
///triangular matrix of unknowns. The implementation works its way by column (j from 1 to n) then by row (i from j to n)
///as opposed to using recursion, but the result is the same.
///</remarks>
let cholesky_decomposition m : double[,] =
    let n = Array2D.length1 m
    let L = Array2D.zeroCreate n n
    for j in 0 .. n - 1 do
        for i in j .. n - 1 do
            let sum = m.[j,i] - Seq.sum [for k in 0 .. j - 1 -> m.[i,k] * m.[j,k]]
            if i = j then L.[j,j] <- sqrt sum else L.[i,j] <- sum / L.[j,j]
    L

///<summary>
///Builds a covariance matrix out of the given sequence of synchronized time series
///</summary>
///<param name="series">A sequence of n data-series (sequence of values) containing m data points each</param>
///<remarks>
///Given a sequence of length n containing time series of length m, this function builds an n x n
///covariance matrix where row i and column j represents the covariance between the ith
///time-series and the j-th time series. By definition, the diagonal cells row i column i contain
///the sample variance of the ith time series.
///Covariance here is computed as
///<equation>$$\sigma_{xy} =\frac{1}{n}\sum{(x_i-\bar{x})(y_i-\bar{y})}, {\rm \ \ where \ \ }\bar{x}=\frac{1}{n}\sum_{i=1}^{n}x_i{\rm \ \ and\ \ }\bar{y}=\frac{1}{n}\sum_{i=1}^{n}y_i$$</equation>
///</remarks>
let covariance_matrix series =
   // Could skip out on calculating the upper triangular portion by returning (if j > i then 0 else compute)
   Array2D.init (Seq.length series) (Seq.length series) (fun i j -> population_covariance (Seq.nth i series) (Seq.nth j series))

///<summary>
///Builds a correlation matrix out of the given sequence of synchronized time series
///</summary>
///<param name="series">A sequence of n time-series (sequence of values) containing m data points each</param>
///<remarks>
///Given a sequence of length n containing time series of length m, this function builds an n x n
///correlation matrix where row i and column j represents the correlation between the ith
///time-series and the j-th time series. By definition, the diagonal cells row i column i will be 1
///and the matrix will be symmetric. Correlation here is computed as:
///<equation>$$\rho_{xy} = \frac{\sigma_{xy}}{\sigma_x\cdot\sigma_y}$$</equation>
///</remarks>
let correlation_matrix series =
   // Could skip out on calculating the upper triangular portion by returning (case j > i => 0, case j = i => 1, default => compute)
   Array2D.init (Seq.length series) (Seq.length series) (fun i j -> correlation (Seq.nth i series) (Seq.nth j series))


let apply_characteristic (data : float[,]) =
    data.Length

///<summary>
///Calculates the dominant eigen-vector and associated eigen-value using the Power Method
///</summary>
///<param name="series">A sequence of n data-series (sequence of values) containing m data points each</param>
///<remarks>
///Given a dataset D of length n with m dimensions, this function starts with a random guess q for an eigen-vector,
///then repeatedly refines that guess using the following iterative step:
///<equation>$$q_{i+1}=\frac{D\times q_i}{\left \| D\times q_i \right \|}$$</equation>
///If the initial guess has a component in the direction of the dominant eigen-vector, and its associated eigen-value
///is strictly greater than the second eigen-value, then the power method is expected to converge to the correct answer.
///</remarks>
let dominant_eigenvector (data : float[,]) = 0

///<summary>
///Performs a Principal Components Analysis on a multi-dimensional dataset using Singular Value Decomposition
///</summary>
///<param name="series">A sequence of n data-series (sequence of values) containing m data points each</param>
///<remarks>
///(Not yet implemented)
///</remarks>
let singular_value_decomposition(data : float[,]) = 0