/// <summary>
/// Linear and non-linear regression methods, curve fitting and optimization models
/// </summary>
/// <DisplayName>Regression</DisplayName>
module Regression

open Common
open Distributions

///<summary>
///Computes the linear regression coefficients that lead to the smallest square of error for one independent and one dependent variable
///</summary>
///<param name="y">A sequence of values for the dependent variable</param>
///<param name="x">A sequence of values for the independent variable</param>
///<remarks>
///The function performs a simple closed form linear regression of one dependent and one independent variable.
///Given independent variable X and dependent varaible Y representing the points
///<equation>$$(x_1, y_1)\ (x_2, y_2)\ ...\ (x_n, y_n)$$</equation>
///Simple least squares regression finds the slope (beta1) and intercept (beta0) below
///<equation>$$\hat{y_i} = \beta_0 + \beta_1\cdot x_i$$</equation>
///Such that the sum of the square of the errors are minimized:
///<equation>$$\textup{min}\ \sum_{i=1}^{n}(y_i - [\beta_0 + \beta_1\cdot x_i])^2$$</equation>
///The closed form solution for the coefficients simply resolves to
///<equation>$$\beta_1=\frac{\sigma_{xy}}{\sigma^2_x}\ \ \textup{and}\ \ \beta_0=\bar{y} - \beta_1\cdot\bar{x}$$</equation>
///</remarks>
let least_squares_linear_regression y x =   
    let cov_xy = sample_covariance x y
    let var_xx = sample_variance x
    let slope = cov_xy / var_xx
    let intercept = (arithmetic_mean y) - slope * (arithmetic_mean x)
    (slope, intercept)

// TODO: SSE, SSR, SST, MSE, MSR, F, etc

///<summary>
///Regresses a logistic function (logit) against the given data to generate a predictor function that estimates
///the probability of an event based on n independent variables
///</summary>
///<remarks>[not yet implemented]</remarks>
let logistic_regression x = 0

let generalized_curve_fitting x = 0

let anova y x = 0
