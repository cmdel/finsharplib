/// <summary>
/// Time-value of money, annuities, duration, convexity,
/// weighted average maturity, and other functions for fixed cashflows
///</summary>
/// <DisplayName>Simple Cashflows</DisplayName>
module SimpleCashflows

//A cashflow is a fixed inflow (positive) or outflow (negative) of cash at a specific time in the future.
//Tenor is the number of years from today that the cashflow occurs.
//Amount is the magnitude of the cashflow, positive for inflows, negative for outflows.
type Cashflow = { Tenor : double; Amount : double; }

///<summary>Discount a single fixed future cashflow using the given annualized rate</summary>
///<param name="cashflow">The future cashflow to discount</param>
///<param name="rate">The annualized interest rate</param>
///<remarks>
///Given a cashflow CF at time t and interest rate r, the present value of the cashflow is
///<equation>$$PV = \frac{CF}{(1+r)^t}$$</equation>
///</remarks>
let discount_cashflow cashflow rate = cashflow.Amount / ((1.0 + rate) ** cashflow.Tenor)

///<summary>Discount a list of arbitrary fixed cashflows using the given annualized rate</summary>
///<param name="list">The times and amounts of the fixed cashflows</param>
///<param name="rate">The annualized interest rate</param>
///<param name="continuation">A continuation for the running total, initially set to 0.0</param>
///<remarks>
///Given a sequence of n cashflows C at time t and constant interest rate r, the present value is
///<equation>$$PV = \sum_{i=1}^{n}\frac{C_i}{(1+r)^{t_i}}$$</equation>
///</remarks>
let rec discount_list list rate continuation =
    match list with
    | [] -> continuation
    | head :: tail -> discount_list tail rate (continuation + discount_cashflow head rate) 

///<summary>Discount a perpetual annuity at a given interest rate</summary>
///<param name="cashflow">The amount paid at the end of each year</param>
///<param name="rate">The interest rate to discount the perpetual casflows</param>
///<remarks>
///The present value of perpetual annual payments of C at interest rate r is
///<equation>$$PV = \lim_{n \to \infty }\sum_{t=1}^{n}\frac{C}{(1+r)^t}=\frac{C}{r}$$</equation>
///</remarks>
let discount_perpetuity (cashflow:double) rate = cashflow / rate

///<summary>Discount a constant growth perpetual annuity at a given interest rate</summary>
///<param name="cashflow_t1">The magnitude of the cashflow at the end of the first year</param>
///<param name="rate">The annualized interest rate</param>
///<param name="growth">The annual rate of growth of the payments</param>
///<remarks>
///The present value of a stream of annual payments C with growth rate g and interest rate r is
///<equation>$$PV = \lim_{n \to \infty }\sum_{t=1}^{n}\frac{C_t}{(1+r)^t}=
///\lim_{n \to \infty }\sum_{t=1}^{n}\frac{C_1(1+g)^{t-1}}{(1+r)^t}=\frac{C_1}{r-g}$$</equation>
///</remarks>
let discount_growing_perpetuity (cashflow_t1:double) rate growth = cashflow_t1 / (rate - growth)

///<summary>Discount a fixed time-horizon annuity at a given interest rate</summary>
///<param name="cashflow">The magnitude of the annual cashflow</param>
///<param name="rate">The interest rate to discount the cashflows</param>
///<param name="n">The number of cashflows</param>
///<remarks>
///The present value of n annual cashflows C starting at the end of the first year, with interest rate r is
///<equation>$$PV = \sum_{t=1}^{n}\frac{C_t}{(1+r)^t}$$</equation>
///But since a perpetuity can be decomposed into an annuity followed by a perpetuity, the annuity is an O(1) function of
///of the time-value of two perpetuities:
///<equation>$$PV = \lim_{n \to \infty }\sum_{t=1}^{n}\frac{C}{(1+r)^t}-\frac{\sum_{t=1}^{n}\frac{C}{(1+r)^t}}{(1+r)^n}=
///C\left[\frac{1}{r}-\frac{1}{r(1+r)^n}\right]$$
///</equation>
///</remarks>
let rec discount_annuity cashflow rate n = cashflow * ((1.0 / rate) - (1.0 / (rate * (1.0 + rate) ** n)))

///<summary>Discount a constant growth annuity with a fixed time horizon at a given interest rate</summary>
///<param name="cashflow_t1">The magnitude of the first cashflow, at the end of the first year</param>
///<param name="rate">The interest rate to discount the cashflows</param>
///<param name="n">The number of cashflows</param>
///<param name="growth">The annual growth rate of the payments</param>
///<remarks>
///The present value of n cashflows starting at the end of the first year with magnitude C1
///with constrant growth g where C(t)=(1+g)C(t-1) and interest rate r is
///<equation>$$PV = \sum_{t=1}^{n}\frac{C_1(1+g)^{t-1}}{(1+r)^t}$$</equation>
///But since the cashflows of a perpetuity can be decomposed into an annuity from year 1 to n followed by a perpetuity
///starting at year n+1, then by the law of one price
///<equation>$$\Gamma(C_1)=\sum_{t=1}^{n}\frac{C_1(1+g)^{t-1}}{(1+r)^t}+\frac{\Gamma(C_{n+1})}{(1+r)^n},\ \ \textup{where}\ \Gamma(x) = \frac{x}{r-g}$$</equation>
///We therefore calculate the price of an abritrarily long, steadily growing annuity as the difference between two partially offsetting infinite growing annuities
///with the following Order(1) operation:
///<equation>$$\sum_{t=1}^{n}\frac{C_1(1+g)^{t-1}}{(1+r)^t} = \frac{C_1}{r-g} \cdot \left[1 - \left( \frac{1+g}{1+r}\right )^n \right]$$</equation>
///</remarks>
let rec discount_growing_annuity cashflow_t1 rate n growth =
    (cashflow_t1 / (rate - growth)) * (1.0 - ((1.0 + growth) / (1.0 + rate)) ** n)

// Evaluates whether x1 and x2 are approximately equal, to a given tolerance
let near x1 x2 tolerance = abs(x1 - x2) < tolerance

// Recursive binary search for a root of continuous function f, bracketed by leftval and rightval.
// See "seek_binary" function
let rec seek_val f max_iterations left right leftval rightval tolerance =
    if max_iterations <= 0 then failwith "could not find root"
    else
        let center = (left + right) / 2.0
        let centerval = f center
        if near centerval 0.0 tolerance then center
        else if (leftval * centerval < 0.0)
            then seek_val f (max_iterations-1) left center leftval centerval tolerance
            else seek_val f (max_iterations-1) center right centerval rightval tolerance

///<summary>Finds a root of the continuous function f using binary search on a positive/negative bracket</summary>
///<param name="f">A continuous function of a single variable x</param>
///<param name="max_iterations">The maximum number of iterations to attempt before failing the search</param>
///<param name="left">A bracketing independent parameter for the function: an x-value such that left &lt; right and f(left) * f(right) &lt; 0</param>
///<param name="right">A bracketing independent parameter for the function: an x-value such that left &lt; right and f(left) * f(right) &lt; 0</param>
///<param name="tolerance">The error tolerance that defines as "close enough" to a root any value x such that abs(f(x)) &lt; tolerance</param>
let seek_binary f max_iterations left right tolerance =
    //TODO: implement root finders for other types of functions
    let leftval = f left
    let rightval = f right
    if (leftval * rightval > 0.0) then failwith "left and right do not bracket root"
    else seek_val f max_iterations left right leftval rightval tolerance

// Evaluates an approximate value for dy/dx via [f(x+h)-f(x)]/[h] with small but non-infinitesimal h
let derivative f x dx = ((f x + dx) - (f x)) / dx

///<summary>Finds a root of the function f using the Newton-Raphson method</summary>
///<param name="f">A function of a single variable</param>
///<param name="max_iterations">The maximum number of iterations to attempt before failing the search</param>
///<param name="x0">An initial guess for the root</param>
///<param name="dx">The small change in independent value to apply when approximating the derivative of function with respect to the independent</param>
///<param name="tolerance">The error tolerance that defines as "close enough" to a root any value x such that abs(f(x)) &lt; tolerance</param>
///<remarks>
///To find a root of function f, start with an initial guess and use a linear approximation to iteratively refine this guess.
///By definition, the function f and the linear approximation lambda have the same first derivative:
///<equation>$$\frac{df}{dx}=\frac{f(x+dx)-f(x)}{dx} = \frac{\lambda(x_{i+1})-\lambda(x_i)}{x_{i+1}-x_i}$$</equation>
///And one point on lambda is tangent to f, while the other is a root:
///<equation>$$\lambda(x_i)=f(x_i) \hspace{20pt} \lambda(x_{i+1})=0$$</equation>
///So given an initial guess for the root, the next best guess of a root for f is the root of the linear approximation:
///<equation>$$x_{i+1}=x_i-\frac{f(x)}{df/dx} = x_i-\frac{f(x)\cdot dx}{f(x+dx)-f(x)}$$</equation>
///This process is repeated until the linar approximation's root converges to an actual root or the maximum number of iterations is exceeded.
///</remarks>
let rec seek_newton_raphson f max_iterations x0 dx tolerance =
    if max_iterations <= 0 then failwith "could not find root"
    else
        let y0 = (f x0)
        if (near y0 0.0 tolerance) then x0
        else
            let y0d = f (x0 + dx)
            let dydx = (y0d - y0) / dx
            seek_newton_raphson f (max_iterations - 1) (x0 - y0 / dydx) dx tolerance    

///<summary>Uses binary search to find an internal rate of return for the given cashflows</summary>
///<remarks>This is just a wrapper for seek_irr. See seek_irr for details</remarks>
let irr_binary cashflows max_iterations left right tolerance =
    seek_binary (fun rate -> discount_list cashflows rate 0.0) max_iterations left right tolerance

///<summary>Uses the Newton-Raphson method to find an internal rate of return for the given cashflows</summary>
///<remarks>This is just a wrapper for seek_newton_raphson. See seek_newton_raphson for details</remarks>
let irr_newton_raphson cashflows max_iterations x0 dx tolerance =
    seek_newton_raphson (fun rate -> discount_list cashflows rate 0.0) max_iterations x0 dx tolerance

///<summary>Uses the Newton-Raphson method to find the yield to maturity of option-free cashflows</summary>
///<remarks>This is just a wrapper for seek_newton_raphson. See seek_newton_raphson for details
///Note that the price of the instrument must be included in the cashflows as an outflow at time zero.</remarks>
let yield_to_maturity = irr_binary

///<summary>Calculates the weighted average maturity of a sequence of cashflows</summary>
///<param name="cashflows">A sequence of cashflows, each with a fixed maturity and amount</param>
///<remarks>
///The weighted average maturity of n cashflows C at time t is
///<equation>$$\overline{T_w}=\frac{\sum_{i=1}^nC_it_i}{\sum_{i=1}^nC_i}$$</equation>
///Unlike Macaulay Duration, this function uses the future value of cashflows as the weight on the maturities.
///</remarks>
let weighted_average_maturity cashflows =
    let folded = Seq.fold (fun acc elem -> (fst acc + abs(elem.Amount) * elem.Tenor), snd acc + abs(elem.Amount)) (0.0, 0.0) cashflows
    fst folded / snd folded

///<summary>Calculates the Macaulay Duration of a sequence of fixed cashflows given a known internal rate of return</summary>
///<param name="cashflows">A sequence of cashflows, each with a fixed maturity and amount</param>
///<param name="rate">The annualized interest rate</param>
///<remarks>
///Macaulay Duration is a measure of weighted average maturity that uses the present value of cashflows as the weights.
///It is calculated as
///<equation>$$D_{mac}=\frac{\sum_{i=1}^nt_iC_i/(1+r)^{t_i}}{\sum_{i=1}^nC_i/(1+r)^{t_i}}$$</equation>
///Macaulay Duration is not the sensitivity of option-free fixed cashflows to changes in interest rates,
///but is an approximation for it. The difference between the two approaches zero as yield (y) decrease as annual
///compounding frequency (k) increases. Specifically,
///<equation>$$D_{mac}=D_{mod}\cdot (1+y/k)$$</equation>
///</remarks>
let macaulay_duration_from_rate cashflows (rate:double) =
    let pvcf = discount_list cashflows rate 0.0
    let maturity_weighted_pvcf = Seq.fold (fun acc elem -> acc + elem.Tenor * (discount_cashflow elem rate)) 0.0 cashflows
    maturity_weighted_pvcf / pvcf

///<summary>Calculates the Macaulay Duration of a sequence of fixed cashflows given a known value for those cashflows</summary>
///<param name="cashflows">A sequence of cashflows, each with a fixed maturity and amount</param>
///<param name="price">The present value of the cashflows</param>
///<param name="x0">An initial guess for the root</param>
///<param name="dx">The small change in independent value to apply when approximating the derivative of function with respect to the independent</param>
///<param name="tolerance">The error tolerance that defines as "close enough" to a root any value x such that abs(f(x)) &lt; tolerance</param>
///<remarks>
///This function uses the Newton-Raphson method to first infer the bond's internal rate of return, then calculates the
///Macaulay Duration as present-value of cashflow weighted maturity. See the function macaulay_duration_from_rate for details.
///</remarks>
let macaulay_duration_from_price cashflows (price:double) max_iterations x0 dx tolerance =
    let neutral_cashflows = { Tenor = 0.0; Amount = -price; } :: cashflows
    let rate = irr_newton_raphson neutral_cashflows max_iterations x0 dx tolerance
    let maturity_weighted_pvcf = Seq.fold (fun acc elem -> acc + elem.Tenor * (discount_cashflow elem rate)) 0.0 cashflows
    maturity_weighted_pvcf / price

///<summary>Calculates the approximate sensitivity of a sequence of fixed cashflows to changes in interest rates</summary>
///<param name="cashflows">A sequence of cashflows, each with a fixed maturity and amount</param>
///<param name="rate">The annualized interest rate</param>
///<param name="dr">The size of the interest rate change to apply to estimate resulting changes in value</param>
///<remarks>
///The sensitivity of the present value of cashflows is estimated by measuring changes in the value as a result of
///both increasing and decreasing the rate by a small amount:
///<equation>$$\frac{\partial V}{\partial r}=\lim_{\Delta r \to 0} \frac{V(r+\Delta r)-V(r-\Delta r)}{2\cdot V(r) \cdot \Delta r}$$</equation>
///</remarks>
let modified_duration cashflows rate dr =
    let p = discount_list cashflows (rate) 0.0
    let pp = discount_list cashflows (rate + dr) 0.0
    let pm = discount_list cashflows (rate - dr) 0.0
    (pm - pp) / (2.0 * p * dr)

///<summary>Calculates the approximate sensitivity of the duration of a sequence of fixed cashflows to changes in interest rates</summary>
///<param name="cashflows">A sequence of cashflows, each with a fixed maturity and amount</param>
///<param name="rate">The annualized interest rate</param>
///<param name="dr">The size of the interest rate change to apply to estimate resulting changes in value</param>
///<remarks>
///Convexity is the second derivative of the present value of the instrument with respect to interest rates.
///To estimate it, we calculate the value of the instrument three times, once for a neutral interest rate,
///once for a small downward change in rates and once for a small upward change in rates, and measure the sensitivity of
///the duration with respect to rates. That is,
///<equation>$$C=\frac{\partial^2V}{\partial r^2}=\lim_{\Delta r \to 0}\frac{V(r+\Delta r)+V(r-\Delta r)-2 \cdot V(r)}{2 \cdot V(r) \cdot (\Delta r)^2}$$</equation>
///</remarks>
let convexity cashflows rate (dr:float) =
    let p = discount_list cashflows (rate) 0.0
    let pp = discount_list cashflows (rate + dr) 0.0
    let pm = discount_list cashflows (rate - dr) 0.0
    (pp + pm - (2.0 * p)) / (2.0 * p * dr * dr)
