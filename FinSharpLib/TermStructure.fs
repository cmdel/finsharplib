///<summary>
///Bootstrapping procedures for yield curves, spot curves and forward curves
///</summary>
///<DisplayName>Term Structure</DisplayName>
module TermStructure

type TenorValue = { Tenor : double; Rate : double }
type Yield = TenorValue
type SpotRate = TenorValue
type ForwardRate = TenorValue

///<summary>Bootstraps a spot curve out of a yield curve</summary>
///<param name="yield_curve">A list of annual yields</param>
///<param name="spot_curve">A continuation containing the spot curve resolved so far. Pass in [] initially</param>
///<remarks>
///This function assumes the yield curve contains annual points starting at the end of the first year,
///and purposely excludes the details of semi-annual compounding in typical bonds.
///Each iteration of the algorithm uses the spot rates solved for so far and calculates the present value of
///cashflows up to T-1, and compares it to the yield at T on the yield curve to backsolve for the spot rate at T. Specifically,
///<equation>$$\sum_{t=1}^{T-1} \frac{y_T}{(1+r_t)^t} + \frac{1+y_T}{(1+r_T)^T}=1$$</equation>
///So for each time T on the curve, the spot rate is equal to
///<equation>$$r_T = \left(\frac{1+y_T}{1-\sum_{t=1}^{T-1} y_T(1+r_t)^{-t}} \right )^{1/T}-1$$</equation>
///</remarks>
let rec bootstrap_spot_rates yield_curve spot_curve =
    match yield_curve with
    | [] -> spot_curve
    | head :: tail ->
        let pvcf = Seq.fold (fun acc spot -> ((fst(acc) + head / ((1.0 + spot) ** snd(acc))), snd(acc) + 1.0)) (0.0, 1.0) spot_curve
        let last_cf = 1.0 + head
        let spot = (last_cf / (1.0-fst(pvcf))) ** (1.0 / (snd(pvcf))) - 1.0
        bootstrap_spot_rates tail (spot_curve@[spot])

///<summary>Converts a spot curve to a forward curve</summary>
///<param name="spot">A sequence of annual spot rates, starting at the rate in one year</param>
///<remarks>
///The one-year forward rate at maturity T is related to spot rates as
///<equation>$$(1+r_{s,T+1})^{T+1}=(1+r_{s,T})^{T}(1+r_{f,T})$$</equation>
///So to solve for forward rates: for each maturity T on the given spot rate, the corresponding forward rate is given by
///<equation>$$r_{f,T} = \frac{(1+r_{s,T+1})^{T+1}}{(1+r_{s,T})^{T}}-1$$</equation>
///</remarks>
let spot_to_forward_rates spot = 
    // (i+1) because mapi starts counting at 0
    let fv = Seq.mapi (fun i r -> (1.0 + r) ** (double (i + 1))) spot
    Seq.map (fun x -> snd(x) / fst(x) - 1.0) (Seq.zip fv (Seq.skip 1 fv))

///<summary>Converts a forward curve to a spot curve</summary>
///<param name="forward">The forward curve of rates starting at the forward rate in 1 year</param>
///<param name="first_spot">The spot rate in one year</param>
///<remarks>
///The one-year forward rate at maturity T is related to spot rates as
///<equation>$$(1+r_{s,T+1})^{T+1}=(1+r_{s,T})^{T}(1+r_{f,T})$$</equation>
///So the spot rate at T+1 given spot rates until T and forward rate at T is
///<equation>$$r_{s,T+1}=\sqrt[T+1]{(1+r_{s,T})^{T}(1+r_{f,T})}-1$$</equation>
///</remarks>
let forward_to_spot_rates forward first_spot=
    let fv = Seq.scan (fun acc x -> acc * (1.0 + x)) (1.0 + first_spot) forward
    Seq.mapi (fun i x -> x ** (1.0 / (double i + 1.0)) - 1.0) fv

// TODO: discounting based on a term structure, without optionality
