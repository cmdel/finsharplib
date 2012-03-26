/// <summary>
/// Common risk-adjusted performance ratios such as Sharpe, Sortino,
/// Modigliani-Squared, Treynor, and other ratios that generally rely on the assumptions of Modern Portfolio Theory.
/// </summary>
/// <DisplayName>Performance Metrics</DisplayName>
module PerformanceMetrics

open Distributions

// Performs a check to ensure that the given requirement is met
let require comment requirement = if requirement then () else failwith comment

// Returns the number of elements in the sequence, ensuring that that number is greater than or equal to a required minimum
let minimum_length x required = 
    let actual = Seq.length x in
    let assertion = require (sprintf "Minimum length of %i required. Found %i" required actual) (actual >= required) in
    actual

// Returns the number of elements in sequences x1 and x2, confirming they are of equal length,
// which must also be greater than or equal to the given required minimum length
let match_minimum_length x1 x2 required =
    let actual1 = Seq.length x1 in
    let actual2 = Seq.length x2 in
    let length_matches = require (sprintf "Sequence lengths do not match up. (%i, %i)" actual1 actual2) (actual1 = actual2) in
    let minimum = require (sprintf "Minimum length of %i required. Found %i" required actual1) (actual1 >= required) in
    actual1    

// Applies function f on consecutive pairs from the sequence x
let map_consecutive f x = 
    let pairs = Seq.zip (Seq.take ((Seq.length x)-1) x) (Seq.skip 1 x)
    Seq.map (fun pair -> f(fst(pair), snd(pair))) pairs

let total_return x = ((Seq.nth ((Seq.length x) - 1) x) / (Seq.nth 0 x)) - 1.0

///<summary>
///Helper function to calculates a sequence of holding period returns out of a sequence of prices
///</summary>
///<param name="x">The time series of asset values</param>
///<remarks>
///This function is heavily reused by other functions in this module. 
///Given a sequence of n asset values, the n-1 holding period returns are calculated from consecutive values as
///<equation>$$R_i = \frac{P_{i}-P_{i-1}}{P_{i-1}}$$</equation>
///</remarks>
let holding_period_return x =
    let consecutive_prices = Seq.zip (Seq.take ((Seq.length x)-1) x) (Seq.skip 1 x)
    Seq.map (fun (p0,p1) -> p1 / p0 - 1.0) consecutive_prices

///<summary>
///Helper function to calculate a period-wise sequence of holding period returns of
///asset values over an arbitrary benchmark
///</summary>
///<param name="x">The time series of asset values</param>
///<param name="b">The time series of benchmark values</param>
///<remarks>
///Given a time-series of N asset values P and a time-series of N benchmark values B,
///the excess holding period returns are the sequence of differences in holding period
///returns between the asset and the benchmark:
///<equation>$$Q_i = \frac{P_i-P_{i-1}}{P_{i-1}}-\frac{B_i-B_{i-1}}{B_{i-1}}\ \ \textup{for i=1..N}$$</equation>
///</remarks>
let excess_holding_period_return x b =
    let hpr1 = holding_period_return x
    let hpr2 = holding_period_return b
    Seq.map (fun (v1, v2) -> v1 - v2) (Seq.zip hpr1 hpr2)

let compound_holding_period_returns hprs = (Seq.fold (fun acc xi -> acc * (1.0 + xi)) 1.0 hprs) - 1.0

///<summary>
///Helper function to convert a holding period return of arbitrary timespan to an annual return
///</summary>
///<param name="hpr">The holding period return</param>
///<param name="holding_period">The time period of the given return</param>
///<param name="target_period">The desired holding period</param>
///<remarks>
///To annualize a return, we assume it is constant, divisible and repeatable and compound
///it by the ratio of the desired holding period (B) over the given holding period (A):
///<equation>$$R_{B}=(1+R_A)^{B/A}-1$$</equation>
///For example, to convert 6-month returns to 1-year returns, we use B/A = 2.
///</remarks>
let annualize_holding_period_return hpr (holding_period:double) (target_period:double) = (1.0 + hpr) ** (double target_period / double holding_period) - 1.0

///<summary>
///Helper function to convert volatility from one period to another, such as daily vol to yearly vol.
///</summary>
///<param name="vol">The volatility level to convert</param>
///<param name="steps_per_period">The ratio of the target volatility period over the given volatility period</param>
///<remarks>
///This function assumes log-normally distributed, constant-volatility returns, as in
///Geometric Brownian Motion. Since volatility is calculated as the standard deviation of the natural log of log-normally
///distributed returns, we know that the square of volatility is additive in the absence of auto-correlation, as variances are.
///<equation>$$\sigma^2_{A+B}=\sigma^2_{A}+\sigma^2_{B}$$</equation>
///<equation>$$\sigma^2_{k\cdot A}=k\cdot\sigma^2_{A}$$</equation>
///And so clearly,
///<equation>$$\sigma_{k\cdot A}=\sigma^2_{A}\cdot\sqrt{k}$$</equation>
///</remarks>
let annualize_volatility vol steps_per_period = vol * sqrt(double steps_per_period)

let scan_hprs_into_prices hpr (x0:float) = Seq.scan (fun acc xi -> acc * (1.0 + xi)) x0 hpr

///<summary>
///Calculates the annualized return (with compounding) for the given asset values time series
///</summary>
///<param name="x">The time series of asset values</param>
///<param name="steps_per_year">The number of asset values provided per year. 12 for monthly asset values, or 252 for daily</param>
///<remarks>
///The annualized return on time series of asset values is calculated as follows. Given that
///<equation>$$P_T = P_0 \cdot (1+R_T)^T$$</equation>
///where T is a real number representing the number of years in the holding period,
///it follows that the annualized return is
///<equation>$$R_T = \left(\frac{P_T}{P_0}\right)^{1/T} - 1$$</equation>
///</remarks>
let annualized_return x steps_per_year =
    let n = minimum_length x 2
    ((Seq.nth (n-1) x) / (Seq.nth 0 x)) ** ((double steps_per_year) / double (n - 1)) - 1.0

///<summary>
///Calculates the annualized volatility of the given asset values time series
///</summary>
///<param name="x">The time series of asset values</param>
///<param name="steps_per_year">The number of asset values provided per year. 12 for monthly asset values, or 252 for daily</param>
///<remarks>
///This function calculates volatility of arbitrary frequency using asset prices of arbitrary
///frequency in a generalized manner. To calculate yearly volatility out of monthly prices, use
///12 for the number of steps per period (steps per year). Volatility calculation is implemented as follows.
///Given asset prices P expressed at constant yearly frequency f,
///<equation>$$R_{i} = \textup{ln}\left(\frac{P_{i}}{P_{i-1}}\right)\ \ \textup{for\ }i=2...n$$</equation>
///For n prices, we obtain n-1 holding period return observations. We then calculate their sample standard deviation
///and annualize the frequency of observation (12 for monthly, 252 for daily)
///<equation>$$\sigma_y=\sigma_f \cdot \sqrt{f\ },\ \ \textup{where}\ \sigma^2_f=\sum_{i=2}^{n}{\frac{(R_i-\overline{R})^2}{n-2}}$$</equation>
///Note that the denominator is n-2 because we have n-1 return observations and the number of degrees of freedom is one less than that.
///</remarks>
let annualized_volatility x steps_per_year =
    let n = minimum_length x 3
    let continuous_returns = map_consecutive (fun (p0,p1) -> ln (p1 / p0)) x
    let stdev = sample_standard_deviation continuous_returns
    stdev * sqrt (double steps_per_year)

///<summary>
///Calculates the Sharpe Ratio of the given time series using the Sharpe's original (1966) reward-to-variability formula
///</summary>
///<param name="x">The asset values</param>
///<param name="rf">The values of a risk-free asset</param>
///<param name="steps_per_year">The number of asset values provided per year. 12 for monthly asset values, or 252 for daily</param>
///<remarks>
///Sharpe's original reward-to-variability ratio calculates three values separately (returns on the portfolio, returns on the risk-free asset
///and volatility of the portfolio value) and then combines them into one metric. One key assumption of the original formula is that the volatility
///of returns on the risk free asset is negligible. Thus,
///<equation>$$S_p = \frac{R_p-R_f}{\sigma_p}$$</equation>
///For details, see http://www.stanford.edu/~wfsharpe/art/sr/SR.htm
///</remarks>
let sharpe_ratio_1966 x rf steps_per_year =
    match_minimum_length x rf 3 |> ignore
    let Rp = annualized_return x steps_per_year
    let Rf = annualized_return rf steps_per_year
    let sigma = annualized_volatility x steps_per_year
    (Rp - Rf) / sigma

///<summary>
///Calculates the Sharpe Ratio of the given time series using the Sharpe's revised (1994) reward-to-variability formula
///</summary>
///<param name="x">The asset values</param>
///<param name="rf">The values of a risk-free asset</param>
///<param name="steps_per_year">The number of asset values provided per year. 12 for monthly asset values, or 252 for daily</param>
///<remarks>
///Sharpe's revised reward-to-variability ratio uses the excess returns of the portfolio over the risk free asset as a time-series
///and is calculated as the expected excess returns over the standard deviation of the excess returns:
///<equation>$$S_p = \frac{E[R_p-R_f]}{\sigma_{R_p-R_f}}$$</equation>
///</remarks>
let sharpe_ratio_1994 x rf steps_per_year =
    let n = match_minimum_length x rf 3
    // Note we're using HPRs, and not continuous rates of return in the numerator
    let excess_hpr = excess_holding_period_return x rf
    let excess_levels = Array.ofSeq (scan_hprs_into_prices excess_hpr 1.0)
    let annualized_excess_return = annualize_holding_period_return (excess_levels.[n-1]/excess_levels.[0] - 1.0) (double(n-1)) steps_per_year
    let x = sum excess_levels
    let y = sum excess_hpr
    let annualized_excess_volatility = annualized_volatility excess_levels steps_per_year
    annualized_excess_return / annualized_excess_volatility

///<summary>
///Calculates the maximum drawdown in asset value within the given time window
///</summary>
///<param name="x">The asset values</param>
///<param name="k">The maximum number of consecutive compounded returns to search through</param>
///<remarks>
///The maximum drawdown in calculated by evaluating the holding period returns in the given time series
///the searching within a sliding time window of given size (k) for the large drop in relative value.
///The candidate returns are all combinations of starting and ending asset values separated by no more
///no more than k steps. For monotonically increasing time series, this function returns 0.
///</remarks>
let maximum_drawdown x k = 
    let windows = Seq.windowed k (holding_period_return x)
    let all = Seq.map (fun window -> Seq.scan (fun c x -> (1.0 + c) * (1.0 + x) - 1.0) -0.0 window) windows
    (-1.0) * (Seq.min (Seq.map (fun window -> Seq.min window) all))

///<summary>
///Calculates Return Over Maximum Drawdown (ROMAD)
///</summary>
///<param name="x">The asset values</param>
///<param name="k">The maximum number of consecutive compounded returns to search through</param>
///<remarks>
///ROMAD is calculated as the ratio of the time series total return and the maximum drawdown.
///See documentation of Maximum Drawdown for details.
///</remarks>
let romad x k = (total_return x) / (maximum_drawdown x k)

///<summary>
///Calculates the downside-deviation of a given sequence, excluding from consideration any non-negative values
///</summary>
///<param name="x">A sequence of values</param>
///<remarks>
///This function is effectively the sample standard deviation function, but it only accumulates into the sum of squared
///deviations values that fall below the mean. Any positive values contribute 0 to the sum:
///<equation>$$\sigma_\ominus =\sqrt{\sum_{i=1}^N{\frac{\textup{[min}(R_i-\bar{R},0)]^2}{N-1}}}$$</equation>
///</remarks>
let downside_deviation x =
    let meanx = arithmetic_mean x in
    sqrt(sum(map (fun xi -> min 0.0 ((xi - meanx) ** 2.0)) x) / (length x - 1.0))

///<summary>
///Calculates the volatility of a given time series of asset values taking into consideration only
///downward deviations from average holding period returns, and annualizes it.
///</summary>
///<param name="x">A sequence of values</param>
///<param name="steps_per_year">The number of holding periods per years in the given asset values</param>
///<remarks>
///This function is effectively the annualized volatility function, but it cuts to zero any positive deviations from
///the average return. As a measure of risk, it addresses the suggestion that positive deviations should not be
///penalized as negative ones. For details on the calculation, see the downside_deviation and annualized_volatility functions.
///</remarks>
let annualized_downside_volatility x steps_per_year =
    let n = minimum_length x 3
    let continuous_returns = map_consecutive (fun (p0,p1) -> ln (p1 / p0)) x
    (downside_deviation continuous_returns) * sqrt(double steps_per_year)

///<summary>
///Calculates Roy's Safety-First Ratio, the ratio of the excess returns of an investment over an arbitrary
///threshold return, divided by the standard deviation of the returns.
///</summary>
///<param name="x">The asset values</param>
///<param name="threshold">The minimum acceptable return</param>
///<param name="steps_per_year">The number of holding periods per years in the given asset values</param>
///<remarks>
///With a threshold return of H, Roy's Safety First Ratio is calculated as:
///<equation>$$SF_p = \frac{R_p-H}{\sigma_{p}}$$</equation>
///This is very similar to the Sharpe ratio, except that the risk-free rate is replaced with an
///arbitrary threshold return.
///As with several other measures based on the Sharpe Ratio,
///since Sharpe has revised his original standard deviation of portfolio to standard deviation
///of excess returns over the risk-free rate, it makes sense to consider doing the same for
///this ratio as well. This function implements the classic definition.
///</remarks>
let roys_safety_first x threshold steps_per_year =
    let portfolio_return = annualized_return x steps_per_year
    let portfolio_volatility = annualized_volatility x steps_per_year
    (portfolio_return - threshold) / portfolio_volatility

///<summary>
///Calculates the Sortino Ratio, the ratio of excess returns of an investment over an arbitrary
///threshold return, divided by the downside-deviation of the returns.
///</summary>
///<param name="x">The asset values</param>
///<param name="threshold">The minimum acceptable return</param>
///<param name="steps_per_year">The number of holding periods per years in the given asset values</param>
///<remarks>
///Given a threshold minimum acceptable return of H, the Sortino Ratio is calculated as:
///<equation>$$SF_p = \frac{R_p-H}{\sigma_{\ominus p}}\ \ \textup{where }\sigma_\ominus   =\sqrt{\sum_{i=1}^N{\frac{\textup{[min}(R_i-\bar{R},0)]^2}{N-1}}}$$</equation>
///As with several other measures based on the Sharpe Ratio,
///since Sharpe has revised his original standard deviation of portfolio to standard deviation
///of excess returns over the risk-free rate, it makes sense to consider doing the same for
///this ratio as well. This function implements the classic definition.
///</remarks>
let sortino_ratio x threshold steps_per_year =
    let portfolio_return = annualized_return x steps_per_year
    let downside_risk = annualized_downside_volatility x steps_per_year
    (portfolio_return - threshold) / downside_risk

///<summary>
///Calculates Percent Up Ratio, the proportion of holding period returns that are greater than zero
///</summary>
///<param name="x">The asset values</param>
///<remarks>
///The percent up ratio U is calculated as the number of returns strictly greater than zero divided by the total number of returns:
///<equation>$$U=\frac{\left | \{R_i : R_i &gt; 0\}\right |}{\left | \{ R\}\right |}$$</equation>
///</remarks>
let percent_up_ratio x =
    let returns = map_consecutive (fun (p0, p1) -> p1 - p0) x
    let up = Seq.length (Seq.filter (fun xi -> xi > 0.0) x)
    let total = Seq.length x
    double up / double total

///<summary>
///Calculates Percent Down Ratio, the proportion of holding period returns that are less than zero
///</summary>
///<param name="x">The asset values</param>
///<remarks>
///The percent down ratio D is calculated as the number of returns strictly less than zero divided by the total number of returns:
///<equation>$$U=\frac{\left | \{R_i : R_i &lt; 0\}\right |}{\left | \{ R\}\right |}$$</equation>
///</remarks>
let percent_down_ratio x =
    let returns = map_consecutive (fun (p0, p1) -> p1 - p0) x
    let down = Seq.length (Seq.filter (fun xi -> xi < 0.0) x)
    let total = Seq.length x
    double down / double total

///<summary>
///Calculates the upside capture ratio, a measure of the offensive tendency of an investment with respect to the bullish periods of the market
///</summary>
///<param name="x">The value of an investment</param>
///<param name="b">The value of a benchmark</param>
///<param name="steps_per_year">The interval at which values are reported</param>
///<remarks>
///The up-capture ratio is the ratio of portfolio performance over benchmark
///returns during periods of positive benchmark returns.
///<equation>$$\textup{UC}=\frac{R_{\{i : B_i&gt;0 \}}}{B_{\{i : B_i&gt;0 \}}}$$</equation>
///A neutral value for the ratio is 1.0. Certain implementations multiply the ratio for a neutral value
///of 100 but we do not.
///To calculate the numerator, we compound the portfolio returns R for periods i where the benchmark
///returns B were strictly greater than zero, then annualize the result using the holding period h for a
///single interval, adjusted for the number of up-market holding periods:
///<equation>$$R_{\{i : B_i&gt;0 \}}=\left [ \prod \left (1+\{R_i : B_i &gt; 0\} \right )\right ]^{q}-1 \textup{  where }q = \left ( h \cdot \left | \{ i : B_i &gt; 0\} \right | \right )^{-1}$$</equation>
///To calculate the compounded, annualized benchmark returns for those periods, we do exactly the same as above,
///except we accumulate benchmark returns instead of portfolio returns:
///<equation>$$B_{\{i : B_i&gt;0 \}}=\left [ \prod \left (1+\{B_i : B_i &gt; 0\} \right )\right ]^{q}-1 \textup{  where }q = \left ( h \cdot \left | \{ i : B_i &gt; 0\} \right | \right )^{-1}$$</equation>
///</remarks>
let up_capture_ratio x b steps_per_year =
    let up_pairs = Seq.zip (holding_period_return x) (holding_period_return b) |> Seq.filter (fun p -> snd(p) > 0.0)
    let up_holding_period = (double (Seq.length up_pairs)) / steps_per_year
    let x_ret = Seq.map fst up_pairs |> compound_holding_period_returns
    let b_ret = Seq.map snd up_pairs |> compound_holding_period_returns
    (annualize_holding_period_return x_ret up_holding_period 1.0) / (annualize_holding_period_return b_ret up_holding_period 1.0)

///<summary>
///Calculates the downside capture ratio, a measure of the defensive tendency of an investment with respect to the bearish periods of the market
///</summary>
///<param name="x">The value of an investment</param>
///<param name="b">The value of a benchmark</param>
///<param name="steps_per_year">The interval at which values are reported</param>
///<remarks>
///The down-capture ratio is the ratio of portfolio performance over benchmark
///returns during periods of negative benchmark returns.
///<equation>$$\textup{UC}=\frac{R_{\{i : B_i&lt;0 \}}}{B_{\{i : B_i&lt;0 \}}}$$</equation>
///A neutral value for the ratio is 1.0. Certain implementations multiply the ratio for a neutral value
///of 100 but we do not.
///To calculate the numerator, we compound the portfolio returns R for periods i where the benchmark
///returns B were strictly less than zero, then annualize the result using the holding period h for a
///single interval, adjusted for the number of down-market holding periods:
///<equation>$$R_{\{i : B_i&lt;0 \}}=\left [ \prod \left (1+\{R_i : B_i &lt; 0\} \right )\right ]^{q}-1 \textup{  where }q = \left ( h \cdot \left | \{ i : B_i &lt; 0\} \right | \right )^{-1}$$</equation>
///To calculate the compounded, annualized benchmark returns for those periods, we do exactly the same as above,
///except we accumulate benchmark returns instead of portfolio returns:
///<equation>$$B_{\{i : B_i&lt;0 \}}=\left [ \prod \left (1+\{B_i : B_i &lt; 0\} \right )\right ]^{q}-1 \textup{  where }q = \left ( h \cdot \left | \{ i : B_i &lt; 0\} \right | \right )^{-1}$$</equation>
///</remarks>
let down_capture_ratio x b steps_per_year =
    // TODO: refactor so that up-capture and down-capture share the same implementation, except for the filter
    let up_pairs = Seq.zip (holding_period_return x) (holding_period_return b) |> Seq.filter (fun p -> snd(p) < 0.0)
    let up_holding_period = (double (Seq.length up_pairs)) / steps_per_year
    let x_ret = Seq.map fst up_pairs |> compound_holding_period_returns
    let b_ret = Seq.map snd up_pairs |> compound_holding_period_returns
    (annualize_holding_period_return x_ret up_holding_period 1.0) / (annualize_holding_period_return b_ret up_holding_period 1.0)

///<summary>
///Calculates the M-Squared (Modigliani Risk-Adjusted Performance) measure of an investment with respect to the market and the risk-free rate
///</summary>
///<param name="x">The time series of asset values</param>
///<param name="market">The time series of market values</param>
///<param name="market">The time series of risk free asset values</param>
///<param name="steps_per_year">The reporting frequency per year</param>
///<remarks>
///The Modigliani-Squared measure of performance is equal to the annualized returns of the investment, with
///the portion in excess of the risk-free rate adjusted for relative volatility compared to the market.
///As expected, returns beyond the risk free rate obtained through exposure to more volatility than the market
///risk factor is penalized by the ratio of the volatilities, and vice versa. The ratio is calculated as follows:
///<equation>$$M^2=R_f+(R_p-R_f)\frac{\sigma_m}{\sigma_p}$$</equation>
///</remarks>
let modigliani_squared x rf market steps_per_year =
    let n = match_minimum_length x rf 3
    let annualized_portfolio_return = annualize_holding_period_return ((Seq.nth (n - 1) x) / (Seq.nth 0 x) - 1.0) (double(n-1)) steps_per_year
    let annualized_risk_free_return = annualize_holding_period_return ((Seq.nth (n - 1) rf) / (Seq.nth 0 rf) - 1.0) (double(n-1)) steps_per_year
    let sigma_ptfl = sample_standard_deviation x
    let sigma_mkt = sample_standard_deviation market
    annualized_risk_free_return + (annualized_portfolio_return - annualized_risk_free_return) * sigma_mkt / sigma_ptfl

///<summary>
///Calculates the Beta of an investment with respect to the market
///</summary>
///<param name="x">The time-series of portfolio values</param>
///<param name="m">The time-series of market values</param>
///<param name="steps_per_year">The reporting frequency per year</param>
///<remarks>
///Beta is calculated as the covariance of portfolio returns with the market, divided by
///the variance of market returns:
///<equation>$$\beta_P=\frac{\textup{cov}(p,m)}{\sigma^2_m}$$</equation>
///Since the correlation between two variables is the same as their covariance
///divided by the product of their standard deviations,
///<equation>$$\rho_{xy}=\frac{\textup{cov}(x,y)}{\sigma_x\sigma_y}$$</equation>
///Beta is also the correlation between portfolio returns and market returns, adjusted
///for relative standard deviation of the market over the portfolio:
///<equation>$$\beta_p=\rho_{p,m}\frac{\sigma_p}{\sigma_m}$$</equation>
///Beta is the CAPM's measure of systematic risk.
///</remarks>
let capm_beta x m steps_per_year = 
    let ann = fun r -> annualize_holding_period_return r 1.0 steps_per_year
    let x_ret = holding_period_return x |> Seq.map ann
    let m_ret = holding_period_return m |> Seq.map ann
    (sample_covariance x_ret m_ret) / (sample_variance m_ret)

///<summary>
///Calculates the Alpha of an investment with respect to the market and the risk-free rate
///</summary>
///<param name="x">The time-series of portfolio values</param>
///<param name="m">The time-series of market values</param>
///<param name="rf">The time-series of risk-free asset values</param>
///<param name="beta">The calculated beta value for the portfolio with respect to the market</param>
///<param name="steps_per_year">The reporting frequency per year</param>
///<remarks>
///Alpha is the CAPM's measure of risk-adjusted excess returns over that which is warranted
///by the asset's systematic risk. The CAPM can be stated as:
///<equation>$$R_P = R_F + \beta_P \cdot (R_M - R_P)+\alpha_P$$</equation>
///We just rearrange this definition to calculate alpha:
///<equation>$$\alpha_P = R_P - (R_F + \beta_P \cdot (R_M - R_P))$$</equation>
///</remarks>
let capm_alpha x m beta rf steps_per_year =
    let x_ret = annualized_return x steps_per_year
    let m_ret = annualized_return m steps_per_year
    let rf_ret = annualized_return rf steps_per_year
    x_ret - (rf_ret + (m_ret - rf_ret) * beta)

///<summary>
///Calculates the Treynor Ratio for an investment with respect to the market and the risk-free rate
///</summary>
///<param name="x">The time-series of portfolio values</param>
///<param name="m">The time-series of market values</param>
///<param name="rf">The time-series of risk-free asset values</param>
///<param name="beta">The calculated beta value for the portfolio with respect to the market</param>
///<param name="steps_per_year">The reporting frequency per year</param>
///<remarks>
///The Treynor Ratio is the portfolio's excess returns over the risk-free rate, adjusted for
///for the CAPM's Beta as the measure of risk. It follows the Sharpe Ratio, but does not penalize
///an investment's idiosyncratic risk because it assumes the investment may be part of a more diversified portfolio:
///<equation>$$T=\frac{R_P-R_F}{\beta_P}$$</equation>
///See the beta function, above, for details on how the denominator is calculated.
///</remarks>
let treynor_ratio x m rf steps_per_year =
    let x_ret = annualized_return x steps_per_year
    let rf_ret = annualized_return rf steps_per_year
    let portfolio_beta = capm_beta x m steps_per_year
    (x_ret - rf_ret) / portfolio_beta

// TODO
let time_to_recover_drawdown = () // the time required to recover from the maximum drawdown
let max_uninterrupted_loss = () // distinguish between that and maximum drawdown
let romul = () // romad vs romul. return over maximum drawdown vs return over maximum uninterrupted loss
let calmar_ratio = () // ??
