module PerformanceMetricsTests

open NUnit.Framework
open FsUnit
open PerformanceMetrics

[<TestFixture>]
type ``Calculate performance metrics for an arbitrary time series of fifteen months`` ()=
    member x.Portfolio = [100.0; 94.20; 104.20; 104.23; 112.89; 133.08; 126.32; 118.85; 119.53; 132.09; 131.85; 136.32; 137.64; 135.20; 149.10]
    member x.Benchmark = [100.0; 96.20; 97.20;  102.23; 101.89; 103.08; 101.32; 100.85; 109.53; 111.09; 112.85; 114.32; 112.64; 116.20; 119.10]
    member x.RiskFree =  [100.0; 100.4; 100.70; 101.50; 102.10; 102.90; 103.20; 103.40; 103.70; 104.20; 104.80; 105.50; 106.10; 106.40; 106.90]

    [<Test>] member x.
        ``The holding period returns of a sequence of portfolio value is the percentage changes from one period to the next`` ()=
        holding_period_return (Seq.take 2 x.Portfolio) |> Seq.head |> should equal -0.058

    [<Test>] member x.
        ``The annualized return on fiften months of values takes into consideration compounding``()=
        annualized_return x.Portfolio 12. |> should equal 0.40829981 //((System.Math.Pow(149.10 / 100.0, 12.0 / 14.0)) - 1.0)

    [<Test>] member x.
        ``The annualized volatility on fiften monthly asset prices is the time horizon adjusted standard deviation of continuous returns``()=
        annualized_volatility x.Portfolio 12. |> should equal 0.24181755

    [<Test>] member x.
        ``The Sharpe ratio of a portfolio using the original formulation is its annualized return in excess of annualized risk free rate divided by annual volatility of the portfolio`` ()=
        sharpe_ratio_1966 x.Portfolio x.RiskFree 12. |> should equal 1.44506079

    [<Test>] member x.
        ``The excess holding period return of a portfolio over a benchmark compares the returns by time slice`` ()=
        List.ofSeq(excess_holding_period_return [1.0; 1.1; 1.2] [1.0; 1.01; 1.05]) |> should equal [0.09; 0.0513051305130512]

    [<Test>] member x.
        ``The Sharpe ratio of a portfolio using the 1994 revision of the formula is the expected annualized excess return over annualized volatility of excess returns`` ()=
        sharpe_ratio_1994 x.Portfolio x.RiskFree 12. |> should equal (0.3325040361 / 0.239952714)
 
    [<Test>] member x.
        ``The maximum drawdown of monotonically increasing asset values is zero`` ()=
        maximum_drawdown x.RiskFree 1 |> should equal 0.0

    [<Test>] member x.
        ``The maximum drawdown on a window of one return is the largest drop in holding period return`` ()=
        maximum_drawdown x.Portfolio 1 |> should equal 0.0591355288

    [<Test>] member x.
        ``The maximum drawdown on a window of two returns is the largest drop in compounded holding period return`` ()=
        maximum_drawdown x.Portfolio 2 |> should equal 0.1069281635

    [<Test>] member x.
        ``Return over maximum drawdown for monotonically increasing prices is infinity`` ()=
        romad x.RiskFree 1 |> should equal infinity

    [<Test>] member x.
        ``Return over maximum drawdown on a sliding window of two returns in the total return over the largest drop`` ()=
        romad x.Portfolio 2 |> should equal 4.591867885

    [<Test>] member x.
        ``Up-capture ratio is the ratio of investment performance over benchmark for positive returns of the benchmark`` ()=
        up_capture_ratio x.Portfolio x.Benchmark 12.0 |> should equal 2.246701183
        // x: product(1.106157113, 1.000287908, 1.178846665, 1.005721498, 1.105078223, 0.998183057, 1.033902162, 0.982272595, 1.102810651)
        // b: product(1.01039501, 1.051748971, 1.011679262, 1.086068418, 1.014242673, 1.01584301, 1.013026141, 1.031605114, 1.024956971)
        // ans = (x^(12/9)-1) / (b^(12/9)-1)

    [<Test>] member x.
        ``Down-capture ratio is the ratio of investment performance over benchmark for negative returns of the benchmark`` ()=
        down_capture_ratio x.Portfolio x.Benchmark 12.0 |> should equal 1.053231106

