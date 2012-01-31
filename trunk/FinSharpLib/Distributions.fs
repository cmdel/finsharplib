///<summary>
///Generate random variables from well-known probability distributions,
///and calculate descriptive statistics of arbitrary probability and cumulative density functions.
///</summary>
///<DisplayName>Distribution Descriptive Statistics</DisplayName>
module Distributions

open Common

///<summary>Calculates the arithmetic mean of a sequence of values</summary>
///<param name="x">A sequence of values</param>
///<remarks>
///This function implements the definition of the arithmetic mean
///<equation>$$\bar{x}_a=\frac{1}{n}\cdot\sum_{i=0}^{n}{x_i}$$</equation>
///</remarks>
let arithmetic_mean x = sum x / length x

///<summary>Calculates the geometric mean of a sequence of values</summary>
///<param name="x">A sequence of values</param>
///<remarks>
///This function implements the definition of the geometric mean
///<equation>$$\bar{x}_g=\sqrt[n]{\prod_{i=1}^{n}{x_i}}$$</equation>
///</remarks>
let geometric_mean x = product x ** inverse (length x)

///<summary>Calculates the harmonic mean of a sequence of values</summary>
///<param name="x">A sequence of values</param>
///<remarks>
///This function implements the definition of the harmonic mean
///<equation>$$\bar{x}_h=n \left [ \sum_{i=1}^n \frac{1}{x_i} \right ]^{-1}$$</equation>
///</remarks>
let harmonic_mean x = length x * inverse (sum (map inverse x))

///<summary>Calculates the sample variance of a sequence of values</summary>
///<param name="x">A sequence of values</param>
///<remarks>
///This function implements the definition of sample variance
///<equation>$$s_x=\frac{\sum{(x_i-\bar{x})^2}}{n-1}, {\rm \ \ where \ \ }\bar{x}=\frac{1}{n}\sum_{i=1}^{n}x_i$$</equation>
///</remarks>
let sample_variance x =
    let meanx = arithmetic_mean x in
    sum(map (fun xi -> (xi - meanx) ** 2.0) x) / (length x - 1.0)

let sum_product_deviations x y =
    let meanx = arithmetic_mean x in
    let meany = arithmetic_mean y in
    sum(map (fun (xi, yi) -> (xi - meanx) * (yi - meany)) (Seq.zip x y))

///<summary>
///Calculates the population covariance of two sequences of values
///</summary>
///<param name="x">The first sequence of values</param>
///<param name="y">The second sequence of values</param>
///<remarks>
///This function implements the following equation for population covariance
///<equation>$$\sigma_{xy} =\frac{1}{n}\sum{(x_i-\bar{x})(y_i-\bar{y})}, {\rm \ \ where \ \ }\bar{x}=\frac{1}{n}\sum_{i=1}^{n}x_i{\rm \ \ and\ \ }\bar{y}=\frac{1}{n}\sum_{i=1}^{n}y_i$$</equation>
///</remarks>
let population_covariance x y = (sum_product_deviations x y) / (length x)

///<summary>
///Calculates the sample covariance of two sequences of values
///</summary>
///<param name="x">The first sequence of values</param>
///<param name="y">The second sequence of values</param>
///<remarks>
///This function implements the following equation for sample covariance
///<equation>$$s_{xy} =\frac{1}{n-1}\sum{(x_i-\bar{x})(y_i-\bar{y})}, {\rm \ \ where \ \ }\bar{x}=\frac{1}{n}\sum_{i=1}^{n}x_i{\rm \ \ and\ \ }\bar{y}=\frac{1}{n}\sum_{i=1}^{n}y_i$$</equation>
///</remarks>
let sample_covariance x y = (sum_product_deviations x y) / ((length x) - 1.0)

///<summary>
///Calculates the linear correlation coefficient between two sequences
///</summary>
///<param name="x">The first sequence of values</param>
///<param name="y">The second sequence of values</param>
///<remarks>
///This function implements the following equation for linear correlation coefficient
///<equation>$$\rho_{xy} = \frac{\sigma^2_{xy}}{\sigma_x\cdot\sigma_y}$$</equation>
///</remarks>
let correlation x y = (sample_covariance x y) / sqrt((sample_variance x) * (sample_variance y))

///<summary>Calculates the sample standard deviation of a sequence of values</summary>
///<param name="x">A sequence of values</param>
///<remarks>
///This function implements the definition of sample standard deviation
///<equation>$$s_x=\sqrt{\frac{1}{n-1}\sum{(x_i-\bar{x})^2}}, {\rm \ \ where \ \ }\bar{x}=\frac{1}{n}\sum_{i=1}^{n}x_i$$</equation>
///</remarks>
let sample_standard_deviation x = sqrt (sample_variance x)

///<summary>Calculates the sample skewness of a sequence of values</summary>
///<param name="x">A sequence of values</param>
///<remarks>
///This function implements the following definition of sample skewness, which includes the sample bias adjustment in the first term:
///<equation>$$q = \frac{\sqrt{n(n-1)}}{n-2} \cdot \left(\frac{1}{n} \sum_{i=1}^n (x_i-\overline{x})^3 \right )\cdot \left(\frac{1}{n} \sum_{i=1}^n (x_i-\overline{x})^2\right)^{-3/2}$$</equation>
///</remarks>
let sample_skewness x =
    let n = length x
    let meanx = arithmetic_mean x in
    let deviations = map (fun xi -> xi - meanx) x
    let m3 = sum (map cube deviations) / n
    let m2 = sum (map square deviations) / n
    sqrt(n*(n-1.0)) * inverse(n-2.0) * m3 * (m2 ** (-1.5))

///<summary>Calculates the sample kurtosis of a sequence of values</summary>
///<param name="x">A sequence of values</param>
///<remarks>
///This function implements the following definition of sample kurtosis, which includes the sample bias adjustment:
///<equation>$$k= \frac{n-1}{(n-2)(n-3)}\cdot((n+1)k_u-3(n-1))+3,$$</equation>
///where unadjusted kurtosis is
///<equation>$$k_u=\left(\frac{1}{n} \sum_{i=1}^n (x_i-\overline{x})^4 \right )\cdot \left(\frac{1}{n} \sum_{i=1}^n (x_i-\overline{x})^2\right)^{-2}$$</equation>
///Note that the value returned is kurtosis and not excess kurtosis, so it must be reduced by 3 to reach excess kurtosis
///</remarks>
let sample_kurtosis x =
    let n = length x
    let meanx = arithmetic_mean x
    let deviations = map (fun xi -> xi - meanx) x
    let sum_squared_dev = sum (map square deviations)
    let sum_fourth_dev = sum (map fourth deviations)
    let kurt = n * sum_fourth_dev / (sum_squared_dev ** 2.0)
    let adjustment = (n-1.0)/((n-2.0)*(n-3.0))
    adjustment * ((n+1.0)*kurt - 3.0*(n-1.0)) + 3.0

let curry f a b = f (a,b)

let append_index w = Seq.zip [1..Seq.length w] w

let index x = append_index x
                |> Seq.sort 
                |> append_index 
                |> Seq.sortBy (fun (x_index, (xval, orig_index)) -> orig_index)
                |> Seq.map (fun (x_index, (xval, orig_index)) -> x_index)

///<summary>
///Calculates the Spearman Rank Correlation between the two sequences of values
///</summary>
///<param name="x">The first sequence of values</param>
///<param name="y">The second sequence of values</param>
///<remarks>
///This function calculates the non-parametric linear co-dependence between x and y as the Pearson
///correlation between the rank of x(i) and the rank of y(i). This happens to be equal to
///<equation>$$\rho_s=\frac{\sigma^2_{R(x)R(y)}}{\sigma_{R(x)}\sigma_{R(y)}},\ \ \textup{where\ }R(w_j) = \left | \{ w_j |w_j\leq w_i \} \right |,\ \ w_i, w_j \in W$$</equation>
///But we don't really have to transform the values to a rank and then evaluate their Pearson correlation. Instead,
///these two steps equivalent to calculating
///<equation>$$\rho_s = 1- {\frac {6 \cdot \sum \left (R(x_i)-R(y_i)\right )^2}{n^3 - n}}$$</equation>
///Note that in order to calculate the Spearman correlation of two variables when their ranks are available,
///it's easier to just call the Pearson correlation on the ranks.
///</remarks>
let spearman_rank_correlation x y =
    let n = length x
    let sum_dsquared = Seq.zip (index x) (index y) |> Seq.map (fun (a,b) -> (a - b) * (a - b)) |> Seq.sum
    1.0 - 6.0 * float(sum_dsquared) / (n * (n * n - 1.0))

// A probable value is a record containing a value and an associated probability for that value.
// It is a building block for named probability distributions, which are sometimes represented as lists of pairings of probability with value.
type ProbableValue = { Probability : double; Value : double; }

///<summary>
///Generates a discretized probability distribution function by aggregating
///the values from a sequence into n buckets and assigning a probability for each bucket
///</summary>
///<param name="x">A sequence of values</param>
///<param name="min">The lower bound on the range of the distribution</param>
///<param name="max">The upper bound on the range of the distribution</param>
///<param name="count">The number of discrete buckets between min and max</param>
let discretize_distribution x min max count =
    let step = (max - min) / (double count)
    let c = length x
    let buckets = Seq.countBy (fun xi -> floor((xi - min) / step)) x
    let distribution = Seq.map (fun b -> { Value = 0.5 * step + step * fst b; Probability = (double (snd b)) / c }) buckets
    List.ofSeq (Seq.sortBy (fun xi -> xi.Value) distribution)

///<summary>Calculates the expected value of a discretized probability distribution</summary>
///<param name="x">A sequence of values, each associated with a probability</param>
///<remarks>
///This function implements the following definition
///<equation>$$E[X]=\int_{-\infty}^{+\infty}x\cdot p(x) \cdot dx \approx \sum_{i=1}^{n}x_i\cdot p(x_i)$$</equation>
///</remarks>
let expected_value x = sum(map (fun pr -> pr.Probability * pr.Value) x)

///<summary>Calculates the variance of a discretized probability distribution</summary>
///<param name="x">A sequence of values, each associated with a probability</param>
///<remarks>
///This function implements the following definition
///<equation>$$\sigma^2=E[X^2]-E^2[X]$$</equation>
///</remarks>
let variance x = expected_value (map (fun xi -> {Probability=xi.Probability; Value=xi.Value*xi.Value}) x) - (expected_value x) ** 2.0

///<summary>Calculates the standard deviation of a discretized probability distribution</summary>
///<param name="x">A sequence of values, each associated with a probability</param>
///<remarks>
///This function implements the following definition
///<equation>$$\sigma=\sqrt{E[X^2]-E^2[X]}$$</equation>
///</remarks>
let standard_deviation x = sqrt (variance x)

