#light

module SimpleCashflowsTests

open NUnit.Framework
open FsUnit
open SimpleCashflows

[<TestFixture>]
type ``Discount Cashflows`` ()=
    [<Test>] member x.
        ``The present value of a present cashflow is the cashflow itself`` ()=
        discount_cashflow { Tenor = 0.0; Amount = 100.0; } 0.05 |> should equal 100.0

    [<Test>] member x.
        ``The present value of a cashflow CF in one year is CF / [1 + r]`` ()=
        discount_cashflow { Tenor = 1.0; Amount = 100.0; } 0.05 |> should equal 95.2380952380952

    [<Test>] member x.
        ``The present value of a cashflow CF in two years is CF / [1 + r]^2`` ()=
        discount_cashflow { Tenor = 2.0; Amount = 100.0; } 0.05 |> should equal 90.7029478458049

[<TestFixture>]
type ``Discount List`` ()=
    [<Test>] member x.
        ``The present value of a list of cashflows is the list of present values of cashflows`` ()=
        let cashflows = [ { Tenor = 0.0; Amount = 100.0; }; { Tenor = 1.0; Amount = 100.0; }; { Tenor = 2.0; Amount = 100.0; } ]
        in discount_list cashflows 0.05 0.0 |> should (equal |> within 0.0001) 285.941043

[<TestFixture>]
type ``Present value of annuities and perpetuities`` ()=
    [<Test>] member x.
        ``The present value of a perpetuity is the ratio of the fixed cashflow and the rate of interest`` ()=
        discount_perpetuity 5000.0 0.05 |> should equal 100000.0

    [<Test>] member x.
        ``The present value of a steadily growing perpetuity adjusts the value of a perpetuity for growth`` ()=
        discount_growing_perpetuity 5000.0 0.05 0.03 |> should equal 250000.0

    [<Test>] member x.
        ``The present value of a fixed annuity is the difference between two perpetuities starting at different times`` ()=
        discount_annuity 5000.0 0.05 5.0 |> should equal 21647.383353154128

    [<Test>] member x.
        ``The present value of a growing annuity is the difference between one growing perpetuity and the present value of the perpetuity that would follow the annuity`` ()=
        discount_growing_annuity 5000.0 0.05 5.0 0.03 |> should equal 22919.607169362414