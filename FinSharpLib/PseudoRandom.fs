/// <summary>
/// Pseudo-random number generators, including the algorithms implemented by the random number generators
/// in Excel's RAND(), .Net's System.Random, Java's java.util.Random, and others.
/// </summary>
/// <DisplayName>Pseudo-Random Number Generators</DisplayName>
module PseudoRandom

///<summary>
///Helper function to convert a random unsigned integer on an arbitrary closed interval [0, m]
///into a uniformly distributed double on the open interval (0, 1)
///</summary>
///<param name="u">A random unsigned integer</param>
///<param name="m">The largest possible value of u</param>
///<remarks>
///This function accepts a random unsigned integer uniformly distributed between 0 and
///the given maximum value M, inclusive. We map this integer U into a double D,
///uniformly distributed between 0 and 1, exclusive, as follows:
///<equation>$$D=\frac{U+1}{\textup{M}+2}$$</equation>
///The most common usage covers all 32 or 64 bits of an unsigned integer and so is equivalent to:
///<equation>$$D=\frac{U+1}{\textup{MaxInt32}+2}$$</equation>
///</remarks>
let uint32_to_uniform_double(k : uint32, m) = ((double k) + 1.0) / ((double m) + 2.0)
let uint64_to_uniform_double(k : uint64, m) = ((double k) + 1.0) / ((double m) + 2.0)
let uint32_full_to_uniform_double(k) = uint32_to_uniform_double(k, System.UInt32.MaxValue)

///<FunctionName>predefined_sequence_generator</FunctionName>
///<summary>s
///Arbitrary random number generator that iterates through a pre-defined sequence
///</summary>
///<param name="state">A list containing the pre-defined values to iteratively draw from</param>
///<remarks>
///This random number generator has a fixed period equal to the length of the arbitrary pre-defined sequence.
///It is initialized by specifying the sequence, and every call to the generator returns the head of the list
///as the next value, and puts that value at the back of the list as the new state.
///</remarks>
let new_predefined_sequence_state list = list
let next_predefined_sequence_value state = (List.append (List.tail state) [List.head state], List.head state)

///<FunctionName>mersenne_twister_generator_MT19937</FunctionName>
///<summary>
///Generates a random number using a Mersenne Twister random number generator (MT19937)
///</summary>
///<param name="state">The current state of the Mersenne Twister, which is a pair containing a 624-array and an index to it</param>
///<remarks>
///The Mersenne Twister is a random number generator that efficiently produces high-quality random number sequences
///The algorithm was developed by Matsumoto and Nishimura in 1997 and while not cryptographically secure, it is fast to
///initialize, and exhibits excellent statistical properties for financial simulation, including an extremely long period
///(2^19937-1), uniform distribution across 623 dimensions, and passes many empirical tests for randomness while using only
///624 words. For further details, see http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/ARTICLES/mt.pdf
///</remarks>
let new_mersenne_state = MersenneTwister.new_mersenne_state
let next_mersenne_value state = let value = MersenneTwister.next_random state in (state, uint32_full_to_uniform_double(value))

///<FunctionName>blum_blum_shub_generator_1986</FunctionName>
///<summary>
///Cryptographically secure random number generator proposed by Lenore Blum, Manuel Blum and Michael Shub in 1986.
///</summary>
///<param name="m">The upper limit on the generator, calculated as the product of two large prime numbers p and q</param>
///<param name="x">The previous (or seed if this is the first call) value of the sequence</param>
///<remarks>
///This pseudo-random number generator is cryptographically secure and predicting the next value based on prior observations
///is proven equivalent to solving P = NP. It is not ideal for stochastic simulations because it is slow and has a short period.
///The upper limit m is calculated as the product of two large prime numbers p and q selected when generating the initial state,
///and the previous value x is the starting seed or the last generated value. Given the required parameters, the next value
///of a Blum-Blum-Shub sequence is calculated as
///<equation>$$x_{i} = x_{i-1}^2 \bmod m$$</equation>
///Additional constraints that should be respected when selecting initializing values for
///the generator are (1) the starting seed should not be zero or one, (2) the starting seed should not be a multiple of p nor q,
///(3) p and q should be distinct large prime numbers, and (4) p and q must be congruent to 3 (mod 4) [p % 4 = 3 and q % 4 = 3].
///</remarks>
let new_blum_blum_shub_state (p : uint32, q : uint32, s : uint32) = (p * q, s)
let next_blum_blum_shub_value (m : uint32, x : uint32) = let x1 = (x * x) % m in ((m, x1), uint32_to_uniform_double(x1, m))

///<FunctionName>linear_congruential_generator</FunctionName>
///<summary>
///Linear Congruential Random Number generator that produces a sequence based on arbitrarily selected LCG parameters
///</summary>
///<param name="a">A (constant) linear factor</param>
///<param name="x">The previously generated random value, or starting seed</param>
///<param name="b">A (constant) linear offset</param>
///<param name="m">A (constant) modulus upper value</param>
///<remarks>
///This is a generalized implementation of a linear congruential generator (LCG) that uses externally specified
///linear parameters a, b and m. LCGs are straightforward and very fast to execute.
///Given a previously generated (or starting seed) value xi, the next pseudo-random
///value is defined as
///<equation>$$x_{i+1} = (a \cdot x_i + b)\ mod\ m$$</equation>
///The quality of the random number sequence is a function of the selected parameters, and so they must be selected with care.
///A great advantage of LCGs is that they are extremely fast. However, they suffer a number of limitations, the most problematic
///being linear/planar correlations when the values are distributed onto multiple dimensions.
///</remarks>
let next_linear_congruential_value (a, x, b, m) = let x1 = (a * x + b) % m in ((a, x1, b, m), uint32_to_uniform_double(x1, m))

///<FunctionName>specialized_visual_Cpp_LCG</FunctionName>
///<summary>
///Linear Congruential Random Number Generator calibrated to the same parameters as the Visual C++ LCG (a=214013, b=2531011, m=max uint32)
///</summary>
///<param name="x">The previously generated random value, or starting seed</param>
let next_visualcpp_lcg_value x =
    let a = 214013u
    let b = 2531011u
    let m = System.UInt32.MaxValue
    let ((a1, x1, b1, m1), r) = next_linear_congruential_value (a, x, b, m) in (x1, r)

///<FunctionName>specialized_java_LCG</FunctionName>
///<summary>
///Linear Congruential Random Number Generator calibrated to the same parameters as Java's Random class (a=25214903917, b=11, m=281474976710656)
///</summary>
///<param name="x">The previously generated random value, or starting seed</param>
let java_lcg_a = System.UInt64.Parse("25214903917")
let java_lcg_b = System.UInt64.Parse("11")
let java_lcg_m = System.UInt64.Parse("281474976710656")
let next_java_lcg_value (x : uint64) = let x1 = (java_lcg_a * x + java_lcg_b) in ((java_lcg_a, x1, java_lcg_b), uint64_to_uniform_double(x1, java_lcg_m))

///<FunctionName>lehmer_generator</FunctionName>
///<summary>
///Lehmer prime modulus multiplicative congruential random number generator (with zero offset)
///</summary>
///<param name="a">A (constant) linear factor</param>
///<param name="x">The previously generated random value, or starting seed</param>
///<param name="m">A (constant) modulus upper value</param>
///<remarks>
///The Lehmer generator is a special case of the linear congruential generator where the offset b is zero, and the parameters a and m
///should follow certain properties. It is computed as
///<equation>$$x_{i+1} = (a \cdot x_i)\ mod\ m$$</equation>
///First, m should be a prime number (or at least a power of a prime number). Second, the factor a must be a
//high multiplicative order mod m. That is, the smallest value k that satisfies the following constraint must be large:
///<equation>$$a^k\ mod \ m = 1$$</equation>
///</remarks>
let next_lehmer_value (a, x, m) = let x1 = (a * x) % m in ((a, x1, m), uint32_to_uniform_double(x1, m))

///<FunctionName>park_miller_generator</FunctionName>
///<summary>
///Park-Miller (multiplicative linear congruential) random number generator with known factor and modulus.
///</summary>
///<param name="x">The previously generated random value, or starting seed</param>
///<remarks>
///The Park-Miller generator is special case of the Lehmer generator with factor a = 16807, and modulus m = 2147483647.
///Given a previously generated (or starting seed) value, the next value in the Park-Miller sequence is calculated as
///<equation>$$x_{i+1} = (16807 \cdot x_i)\ mod\ 2147483647$$</equation>
///</remarks>
let next_park_miller_value x = let ((v, x1, w), r) = next_lehmer_value (16807u, x, 2147483647u) in (x1, r)

///<FunctionName>IBM_RANDU_generator</FunctionName>
///<summary>
///IBM RANDU random number generator based on a Lehmer generator with known values for the factor and modulus
///</summary>
///<param name="x">The previously generated random value, or starting seed</param>
///<remarks>
///The IBM RANDU random number generator is a special case of the Lehmer generator with factor a = 65539, and modulus m = 2147483647.
///Given a previously generated (or starting seed) value, the next value in the IBM RANDU sequence is calculated as
///<equation>$$x_{i+1} = (65539 \cdot x_i)\ mod\ 2147483647$$</equation>
///This random number generator is known to be pretty bad, and is only included here for reference.
///</remarks>
let next_ibm_randu_value x = let ((v, x1, w), r) = next_lehmer_value (65539u, x, 2147483647u) in (x1, r) //1, 65539, 393225, 1769499

///<FunctionName>wichmann_hill_generator</FunctionName>
///<summary>
///Wichmann-Hill triple multiplicative congruential generator, implemented in Microsoft Excel's RAND() function.
///</summary>
///<param name="(a, b, c)">A triple of unsigned integers, either a random seed, or the triple produced by the last call to the generator</param>
///<remarks>
///The Wichmann-Hill algorithm combines three multiplicative LCGs as proposed in http://www2.imperial.ac.uk/~hohs/S9/2007-2008/wichmannhill.pdf.
///It is implemented in Microsoft Excel's rand() function starting in 2003 as described in the Microsoft Knowledge Base article
///(http://support.microsoft.com/kb/828795). The algorithm passes a number of statistical tests of randomness, has a reasonable period,
///but is still very quick. In summary, given a triple (a, b, c) starting seed or values produces from a previous call to the generator,
///the next state of the generator is computed as
///<equation>$$a_{i+1} = (171 \cdot a_i)\ mod\ 30269$$</equation>
///<equation>$$b_{i+1} = (172 \cdot b_i)\ mod\ 30307$$</equation>
///<equation>$$c_{i+1} = (170 \cdot c_i)\ mod\ 30323$$</equation>
///and once a, b and c are adjusted, the next value x of the sequence is normalized as
///<equation>$$x = \left( \frac{a}{30269}+\frac{b}{30307}+\frac{c}{30323}\right )\ mod\ 1$$</equation>
///</remarks>
let next_wichmann_hill_value (a, b, c) =
    let A = 30269
    let B = 30307
    let C = 30323
    let a1 = (171 * a) % A
    let b1 = (172 * b) % B
    let c1 = (170 * c) % C
    let x1 = double a1 / double A + double b1 / double B + double c1 / double C
    ((a1, b1, c1), x1 % 1.0)

///<FunctionName>generalized_lagged_fibonacci_generator</FunctionName>
///<summary>
///Generalized Lagged Fibonacci random number generator, derived from a generalization of the classical Fibonacci sequence
///</summary>
///<param name="operation">The operation to use to combine the two selected historical items</param>
///<param name="history">A pair of lists (ancient, recent) containing previously generated values, ordered from oldest to newest within each list</param>
///<remarks>
///The Generalized Lagged Fibonnaci Generator (GLFG) is a generalization of the classical Fibonacci sequence, defined as:
///<equation>$$F_{i} = F_{i-1} + F_{i-2}$$</equation>
///The sequence underlying this pseudo-random number generator generalizes the (i-1) and (i-2) lookbacks as well as the (+) operation. So,
///for any arbitrary natural numbers j and k where k > j, and arbitrary combining operation, the sequence becomes:
///<equation>$$F_{i} = F_{i-j} \star  F_{i-k}$$</equation>
///The historically generated sequence (or arbitrary seed sequence) is ordered from most ancient to most recent, and partitioned
///into two lists: (ancient_history, recent_history). This pair of lists is the state of the generator, with j equal to the length of
///recent_history and k equal to the sum of the lengths of recent_history and ancient_history. At every iteration, F[i-k] is extracted
///from the head of ancient_history and F[i-j] is extracted from the head of recent_history, F[i] is computed by combining those two values,
///and the new history is composed by transferring the head of recent_history to the tail of ancient_history, and the new value F[i] to
///the tail of recent_history, thus producing a new state and a new random value.
///</remarks>
let next_generalized_lagged_fibonacci_generator (operation : uint32 -> uint32 -> uint32) (ancient_history, recent_history) =
    // [ ancient       ] [ recent               ]  cur
    // [-10, -9, -8, -7] [-6, -5, -4, -3, -2, -1]  {0}
    let ancient_head = List.head ancient_history   // -10
    let recent_head = List.head recent_history     // -6
    let ancient_tail = List.tail ancient_history   // -9..-7
    let recent_tail = List.tail recent_history     // -5..-1
    let new_x = operation recent_head ancient_head //  0
    let new_ancient = ancient_tail @ [recent_head] // -9..-7, -6
    let new_recent = recent_tail @ [new_x]         // -5..-1, 0
    ((new_ancient, new_recent), uint32_full_to_uniform_double(new_x))

///<FunctionName>double_tap_generalized_feedback_shift_register_generator</FunctionName>
///<summary>
///Generalized Feedback Shift Register pseudo-random number generator (GFSR), with two taps (thus Fibonacci), that shifts and XOR-combines historically generated values
///</summary>
///<param name="history">A pair of lists (ancient, recent) containing previously generated values, ordered from oldest to newest within each list</param>
///<remarks>
///This generator is a special case of the Generalized Lagged Fibonacci Generator, in that it uses bitwise-XOR for it combine function,
///and is a special case of the Generalized Feedback Shift Register Generator in that it has two taps on the history. It is a fast
///and more robust alternative to linear congruential generators, with typically longer periods and higher degree of entropy. Its period can
///be made arbitrarily long by extending memory of historical values. For more details, see documentation for the GLFG.
///</remarks>
let next_double_tap_generalized_feedback_shift_register_generator history = next_generalized_lagged_fibonacci_generator (^^^) history

///<FunctionName>specialized_subtractive_lagged_fibonacci_generator</FunctionName>
///<summary>
///Subtractive Lagged Fibonacci Generator, a special case of the GLFG with a combination operation of (-), implemented in Microsoft's .Net Random class in the Base Class Library.
///</summary>
///<param name="history">A pair of lists (ancient, recent) containing previously generated values, ordered from oldest to newest within each list</param>
///<remarks>
///This is a special case of the Generalized Lagged Fibonacci Generator, and is exposed as a partial function evaluation of the general case
///because it is an important special case, and is the implementation of Random in .Net, as documented in MSDN and Donald Knuth's Art of Computer
///Programming, Vol. 2, p.27. with j and k equal to 24 and 51, which guarantees a period of 2^55 - 1.
///</remarks>
let next_subtractive_lagged_fibonacci_generator (ancient_history, recent_history) = next_generalized_lagged_fibonacci_generator (-) (ancient_history, recent_history)

///<FunctionName>marsaglia_multiple_with_carry_generator</FunctionName>
///<summary>
///Multiple-With-Carry generator proposed by Marsaglia in 1999
///</summary>
///<param name="(z, w)">A pair of values containing the continuation or seed of generator</param>
///<remarks>
///This random number generator produces two separate 16bit sequences, with
///the hi-bits extracted from z/nz and the lo-bits extracted from w/nw. The algorithm
///is based on a linear congruential generator, except it takes for its offset a variable
///value which is whatever is carried above the low 16 bits. Note that "carry" in
///Multiple-With-Carry refers to carrying the high bits back down for each iteration within
///z/nz and w/nw and not across. This generator was suggested by George Marsaglia in a post
///to Sci.Stat.Math. It's available along with a few other other methods
///at http://www.bobwheeler.com/statistics/Password/MarsagliaPost.txt.
///The sample C implementation is labelled MWC.
///</remarks>
let next_marsaglia_multiple_with_carry_value (z, w) =
    let nz = 36969u * (z &&& 65535u) + (z >>> 16)
    let nw = 18000u * (w &&& 65535u) + (w >>> 16)
    ((nz, nw), uint32_full_to_uniform_double((nz <<< 16) + (nw &&& 65535u)))

///<FunctionName>marsaglia_three_shift_register_generator</FunctionName>
///<summary>
///Three-shift register generator proposed by Marsaglia in 1999
///</summary>
///<param name="s">A value containing the continuation or seed for the generator</param>
///<remarks>
///This random number generator carries a single value for the state and shifts it
///three times to generate the next value for the state and one value for the next random number.
///It's available along with a few other methods at
///http://www.bobwheeler.com/statistics/Password/MarsagliaPost.txt.
///The sample C implementation is labelled SHR3.
///</remarks>
let next_marsaglia_three_shift_register_value (s : uint32) =
    let uno = s ^^^ (s <<< 17)
    let dos = uno ^^^ (uno >>> 13)
    let tres = dos ^^^ (dos <<< 5)
    (tres, uint32_full_to_uniform_double(tres))

let lazy_random_sequence next seed = Seq.unfold (fun s -> let (sn, v) = next s in Some(v, sn)) seed

let random_sequence next seed count = lazy_random_sequence next seed |> Seq.take count

let lazy_random_sequence2 next seed = Seq.unfold (fun s -> let (sn, v) = next s in Some((sn, v), sn)) seed

let random_sequence2 next seed count =
    let pairs = lazy_random_sequence2 next seed |> Seq.take count
    (Seq.nth (count - 1) pairs |> fst, Seq.map snd pairs)
