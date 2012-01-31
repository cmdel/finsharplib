/// <summary>
/// Generate and manipulate probability distributions. Includes common distribution generators
/// such as Box-Muller Generator, Marsaglia Polar Generator, Generalized Rejection Sampling Generator, and others.
/// </summary>
/// <DisplayName>Distribution Functions</DisplayName>
module DistributionFunctions

open PseudoRandom

///<summary>Transforms a random value by stretching then shifting its distribution</summary>
///<param name="x">A random value drawn from an arbitrary distribution</param>
///<param name="stretch">The factor by which to stretch the distribution</param>
///<param name="shift">The magnitude by which to shift the stretched distribution</param>
let transform x stretch shift = x * stretch + shift

///<summary>Transforms a random floating point value between 0.0 and 1.0 into a random integer between min and max, inclusive</summary>
///<param name="x">A random floating point value between 0.0 and 1.0</param>
///<param name="min">The left endpoint of the desired integer range, inclusive</param>
///<param name="max">The right endpoint of the desired integer range, inclusive</param>
let get_random_integer x min max = floor(x * (1.0 + max - min) + min)

///<summary>
///Uses the Box-Muller transform to generate a pair of independent, normally distributed
///values out of two uniformly distributed value in the range (0,1).
///</summary>
///<param name="x1">A random value, uniformly distributed between 0 and 1</param>
///<param name="x2">A random value, uniformly distributed between 0 and 1</param>
///<remarks>
///Given a pair of values (x1, x2) both uniformly distributed between 0 and 1, the Box-Muller transform
///is calculated as
///<equation>$$(z_1, z_2) = (R \cdot cos(\theta),\space R \cdot sin(\theta))$$</equation>
///where
///<equation>$$R = \sqrt{-2 \cdot \textup{ln}(x_1)}$$</equation>
///and
///<equation>$$\theta = 2 \pi \cdot x_2$$</equation>
///</remarks>
let box_muller_transform x1 x2 = 
    let r = sqrt((-2.0) * ln(x1))
    let t = 2.0 * pi * x2
    (r * cos t, r * sin t)

//Helper function for marsaglia_polar_transform
let rec marsaglia_polar_filter generator =
    let (u, v) = (transform (generator()) 2.0 -1.0, transform (generator()) 2.0 -1.0)
    let s = u * u + v * v
    if (s < 1.0) && (s > 0.0) then (s, u, v) else marsaglia_polar_filter generator

///<summary>
///Uses the Marsaglia polar transform to generate a pair of independent, normally distributed
///values by extending the Box-Muller to avoid the need to avoid any trig functions.
///</summary>
///<param name="generator">A function that returns a random number uniformly distributed between 0 and 1</param>
///<remarks>
///Marsaglia's polar method consists of two steps. First, repeatedly sample the two-dimensional uniform random
///space until two values u and v are found such that the point (u,v) is within the unit circle:
///<equation>$$s=u^2+v^2&lt;1$$</equation>
///Then, calculate the following two values, which will be standard-normally distributed:
///<equation>$$(u \cdot q,\ v \cdot q) \ \ where \ \ q = \sqrt{\frac{-2\cdot\textup{ln}(s)}{s}}$$</equation>
///See "A Convenient Method for Generating Normal Variables" by Marsaglia and Bray at http://www.jstor.org/pss/2027592 for details.
///</remarks>
let marsaglia_polar_transform generator =
    let (s, u, v) = marsaglia_polar_filter generator
    let q = sqrt((-2.0) * (ln s) / s)
    (u * q, v * q)

///<summary>
///Uses the Central Limit Theorem to combine a random uniform sequence of numbers into a single
///value whose distribution approaches the standard normal as the input sequence gets large
///</summary>
///<param name="runif">A sequence of random numbers uniformly distributed between 0 and 1</param>
///<remarks>
///The function implements a simple source of approximately normally distributed random numbers.
///It's not particularly efficient, but it demonstrates the use of the Central Limit Theorem for this application.
///The sum of independent random values with finite mean and variance approaches a normal distribution
///as the number of values gets large. In order to ensure that the summation will produce numbers that converge
///to the standard normal (mean=0, stdev=1), we to define a linear transform f on the uniform random values
///Z that simultaneously meets the required constraints:
///<equation>$$E(X)=0,\ \ \ \sigma_X=1,\ \ \textup{with}\ \ X = \sum_1^n{f(Z)}$$</equation>
///We first meet the expectation constraint by centering the endpoints with respect to the origin:
///<equation>$$E(X)=\int_{L}^{R}dx = R-L = 0$$</equation>
///We then define the scaling factor b = R = L, to meet the standard deviation constraint:
///<equation>$$\sigma^2_X=n \cdot \sigma^2_{f(Z)} =n \cdot \left ( E[f(Z)^2]-E[f(Z)]^2 \right ) =n \cdot E[f(Z)^2]$$</equation>
///This expectation can be integrated in closed form, and leads to the following value for b:
///<equation>$$\sigma_X^2=\frac{n}{2b}\int_{-b}^{+b}x^2dx=\left [ \frac{n}{2b} \cdot \frac{x^3}{3}\right ]_{-b}^{+b}=1,\ \ \textup{therefore}\ \ b=\sqrt{\frac{3}{n}}$$</equation>
///This explains the linear transforms in the implementation.
///</remarks>
let central_limit_normal_transform runif =
    (runif |> Seq.map (fun x -> ((x * 2.0) - 1.0) * sqrt(3.0)) |> Seq.reduce (+)) * inverse(sqrt(double(Seq.length runif)))

///<summary>
///Uses the Rejection Sampling algorithm to adapt a uniform random number generator to
///produce values distributed according to an arbitrary probability density function.
///</summary>
///<param name="generator">A function that returns random numbers uniformly distributed between 0 and 1</param>
///<param name="pdf">A function that takes in a value and returns the probabilty density of that value</param>
///<param name="xmin">A lower bound on the values to sample from. This is the minimum on a bounded distribution,
///and a desired sampling lower bound if the distribution extends to negative infinity</param>
///<param name="xmax">An upper bound on the values to sample from. This is the maximum on a bounded distribution,
///and a desired sampling upper bound if the distribution extends to infinity</param>
///<param name="pdfmax">A known upper bound on the probability density function. This should be the maximum value
///in the (xmin, xmax) range if it is known, and must be greater than or equal to the maximum if the maximum is not known</param>
///<remarks>
///This generalized implementation uses places an envelope (as defined by the pdf) on a uniform random number generator and accepts only
///random draws that fit within the envelope. In other words, we draw a tentative value x from (0,1) and stretch it to (xmin, xmax).
///Then, we draw a rejection variable y from (0,1) and stretch it to (0, pdfmax), and use it to adjust the probability of having drawn
///that tentative value: if y &lt; pdf(tentative x) then we accept that value of x, otherwise, repeat the process until an x-value is accepted.
///This algorithm works better for bounded distributions with smaller peak(s), but we can handle longer tails by breaking up the distribution
///into multiple pieces so that pdfmax is smaller when possible, thereby increasing the probability of acceptance for each iteration.
///</remarks>
let rec generalized_rejection_sample generator pdf xmin xmax pdfmax=
    let x = transform (generator()) (xmax - xmin) xmin
    let y = transform (generator()) pdfmax 0.0
    if y <= pdf(x) then x else generalized_rejection_sample generator pdf xmin xmax pdfmax

///<summary>
///Approximates the integral of the Gaussian distribution from negative infinity to the given x value using the Abramowitz-Stegun method
///</summary>
///<param name="x">The upper limit of the integral to approximate</param>
///<remarks>
///The normal cumulative distribution function is calculated using the approximation 26.2.17, p. 932 the Handbook
///of Mathematical Functions, Abramowitz and Stegun (http://www.math.hkbu.edu.hk/support/aands/page_932.htm).
///The function starts by taking the absolute value of x to take advantage of the symmetry of the normal distribution. Indeed,
///<equation>$$\phi(-x)=1-\phi(x)$$</equation>
///Then, using pre-defined constants b1, b2, b3, b4, b5, p and c, the function evalutes t, s and y as:
///<equation>$$t=\frac{1}{1+ p \cdot |x|}$$</equation>
///<equation>$$s = ((((b_5 \cdot t + b_4) \cdot t + b_3) \cdot t + b_2) \cdot t + b_1) \cdot t$$</equation>
///<equation>$$y = s \cdot e^{ -\frac{1}{2} x^2 - c}$$</equation>
///In the case where x is negative, the function returns y. Otherwise, the function returns 1 - y, by the identity above.
///</remarks>
let abramowitz_stegun_normal_cumulative_distribution_function x =
  let b1 =  0.319381530
  let b2 = -0.356563782
  let b3 =  1.781477937
  let b4 = -1.821255978
  let b5 =  1.330274429
  let p  =  0.2316419
  let c  =  0.39894228
  let ax = abs x
  let t = 1.0 / (1.0 + p * ax)
  let s = ((((b5 * t + b4) * t + b3) * t + b2) * t + b1) * t
  let y = s * exp((-0.5) * x * x - c)
  if x <= 0.0 then y else 1.0 - y
  
// short-hand
let cdf = abramowitz_stegun_normal_cumulative_distribution_function

///<summary>
///Approximates the inverse of the normal cumulative distribution function using the Beasley-Springer algorithm
///</summary>
///<param name="p">The cumulative probability to solve for</param>
///<remarks>
///The Beasley-Springer algorithm (also known as Beasley-Springer-Moro) evaluates the inverse normal cdf using one
///of two possible functions, one for central values ranging from 0.08 to 0.92, and the other for extreme values
///less than 0.08 and greater than 0.92. For details on the polynomials and constants used, please refer to the source
///code as well as The Percentage Points of the Normal Distribution, Applied Statistics 26 (1977) p. 118.
///</remarks>
let beasley_springer_inverse_normal_cumulative_distribution_function p =
    let a1, a2, a3, a4, b1, b2, b3, b4 = 2.50662823884, -18.61500062529, 41.39119773534, -25.44106049637, -8.4735109309, 23.08336743743, -21.06224101826, 3.13082909833
    let c1, c2, c3, c4, c5, c6, c7, c8, c9 = 0.337475482272615, 0.976169019091719, 0.160797971491821, 2.76438810333863E-02, 3.8405729373609E-03, 3.951896511919E-04, 3.21767881768E-05, 2.888167364E-07, 3.960315187E-07
    let y = p - 0.5
    if (abs y) < 0.42 then
        let v = y * y
        y * (((a4 * v + a3) * v + a2) * v + a1) / ((((b4 * v + b3) * v + b2) * v + b1) * v + 1.0)
    else
        let w = log ((-1.0) * (log (if y > 0.0 then 1.0 - p else p)))
        (c1 + w * (c2 + w * (c3 + w * (c4 + w * (c5 + w * (c6 + w * (c7 + w * (c8 + w * c9)))))))) * (if y >= 0.0 then (1.0) else (-1.0))

// TODO test

// short-hand
let inverse_cdf = beasley_springer_inverse_normal_cumulative_distribution_function

// TODO: distributions
let normal_random_box_muller state rng = 0
let normal_random_marsaglia_polar_transform state rng = 0
let student_t_random rng state = 0
let lognormal_random normal_rng state rng normal_transform = 0
let gamma_random rng state = 0
let beta_random rng state = 0
let chi_squared_random rng state = 0
let weibull_random rng state = 0
let bernoulli = 0
let binomial = 0
let poisson = 0