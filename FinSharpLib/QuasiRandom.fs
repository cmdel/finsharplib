/// <summary>
/// Low-discrepancy quasi-random number generators and related sub-random sampling methods, including Sobol Sequence and Latin Hypercube Sampling.
/// </summary>
/// <DisplayName>Quasi-Random Number Generators</DisplayName>
module QuasiRandom

open Common

let rec van_der_corput_sequence_rec i b e =
    if i = 0
    then 0.0
    else double (i % b) * (double b ** ((-1.0) * double e)) + van_der_corput_sequence_rec (i/b) b (e+1)

///<summary>
///Calculates the a value in the Van der Corput sequence of the given base, at the given position
///</summary>
///<param name="i">THe position of the element in the sequence to calculate</param>
///<param name="b">The base of the sequence</param>
///<remarks>
///Published in 1935 by mathematician J. G. van der Corput, this low-discrepancy sequence breaks the unit
///interval into increasingly granular sub-intervals by the given base. It effectively iterates through
///the decimal values in the unit interval, representing them in the desired base, then reversing the digits as
///a string. The implementation doesn't actually use any strings, but the result of the floating point operations
///is equivalent to reversing the digits. For any given depth, represented by the number of digits in the
///number being counted, the space is broken up into equally sized sub-intervals.
///For example, to generate the first eight values of the sequence in base 2 covering three digits, we
///would start by counting in decimal base 2:
///<equation>$$D_2 = (0.001,\ 0.010,\ 0.011,\ 0.100,\ 0.101,\ 0.110,\ 0.111, ...)$$</equation>
///And then reverse their binary representation to get to:
///<equation>$$C(2)_2 = (0.100,\ 0.010,\ 0.110,\ 0.001,\ 0.101,\ 0.011,\ 0.111, ...)$$</equation>
///Which is equal to:
///<equation>$$D(2)_{10} = \left (\frac{1}{2}\ ,\ \frac{1}{4}\ ,\ \frac{3}{4}\ ,\ \frac{1}{8}\ ,\ \frac{5}{8}\ ,\ \frac{3}{8}\ ,\ \frac{7}{8}\ ,\ ...\right )$$</equation>
///As another example, Van der Corput sequence base 10, expressed in decimal form is:
///<equation>$$D(10)_{10} = \left ( 0,\ 0.1,\ 0.2,\ 0.3,\ 0.4,\ 0.5,\ 0.6,\ 0.7,\ 0.8,\ 0.9...\right )$$</equation>
///But note that as we count 9, 10, 11, 12 and reverse the representation, the next elements in the sequence
///begin traversing the next level of granularity holding the most significant digits in the inner iteration
///and the least significant ones in the outer iteration:
///<equation>$$\left (...\ ,\ 0.01, 0.11, 0.21, 0.31, 0.41, ...\ , 0.81, 0.91, 0.02, 0.12, 0.22, ...\right )$$</equation>
///Note that the Halton sequence performs reasonably well for low-dimensionality, but introduces undesired
///correlation for higher dimensionality. For those cases, consider using the Sobol sequence instead.
///</remarks>
let van_der_corput_sequence i b = van_der_corput_sequence_rec i b 1

///<summary>
///Generates a multi-dimensional Halton quasi-random sequence with the given bases, one for each dimension
///</summary>
///<param name="i">The position of the next element to generate</param>
///<param name="bz">A sequence of values representing the basis for each dimension</param>
///<remarks>
///The Halton Sequence is a multi-dimensional generalization of the Van der Corput sequence, also implemented
///in this module. Whereas the Van Der Corput sequence breaks the unit interval into increasingly granular
///sub-intervals, the Halton sequence does the same for the unit square, cube or hypercube. Each dimension
///is broken up using its assigned base and without regard for the other dimensions, so the implementation
///simply relies on the Van der Corput function to generate the sequence. As an example, the two-dimensional
///Halton sequence with bases 2 and 10 starts as:
///<equation>$$H(2,10)_{10}=\left((0.5, 0.1),\ (0.25, 0.2),\ (0.75, 0.3),\ (0.125, 0.4),\ (0.625, 0.5),\ ... \right )$$</equation>
///</remarks>
let halton_sequence i bz = Seq.map (fun b -> van_der_corput_sequence i b) bz

// Calculates the number of bits needed to represent the given integer
let max_bits n = int (ceil (ln (double n) / ln 2.0))

///<summary>
///Calculates the reflected binary code (Gray code) of the given integer (used by the Sobol Sequence)
///</summary>
///<param name="n">The integer whose gray code to calculate</param>
///<remarks>
///The Gray code of an integer is a binary representation of that integer using an encoding system where
///incrementing one value to the next only requires changing a single bit. For example, unlike the standard binary encoding
///where incrementing 1 (001) to 2 (010) requires rolling back the right-most digit before incrementing the second,
///Gray encoding simply switches the second right-most digit of 1 (001) in order to get to 2 (011) without rolling the first one to zero.
///The Gray code sequence, starting at 0 is, 000, 001, 011, 010, 110, 111, 101, 100, etc.
///To calculate an integer's Gray encoding given its binary encoding, we start with the observation that for any bit B in the binary encoding,
///the bit L to its left gets incremented every time B gets reset from 1 to 0. If L is 1, then B got reset an odd number of times,
///each of which the respective Gray encoding would have lazily left B as is, causing it to be different than the binary encoding.
///If L is 0, then B got reset an even number of times, each of which the Gray encoding would have lazy left B as is, but since, the
///number of times one got flipped without the other is even, we can conclude the binary and Gray encodings for B are the same. From this,
///we calculate the gray code as:
///<equation>$$G_n = n \oplus \left(\frac{n}{2}\right)$$</equation>
///For a 32-bit integer, that is:
///<equation>$$[b_1\ .\ b_2\ .\ b_3\ .\ b_4\ ...\ b_{32}] \oplus [0\ .\ b_1\ .\ b_2\ .\ b_3\ ...\ b_{31}]$$</equation>
///where b1..b32 is the binary encoding of n. That works out to:
///<equation>$$[b_1 \oplus 0\ .\ b_2\oplus b_1\ .\ b_3\oplus b_2\ .\ b_4\oplus b_3\ ...\ b_{32}\oplus b_{31}]$$</equation>
///</remarks>
let gray_code n = n ^^^ (n / 2)

let rec right_most_zero_bit n = if (n % 2 = 0) then 0 else 1 + (right_most_zero_bit (n / 2))

let u x = uint32 x

// let (d,s,a, m) = direction_vectors.[j]
// i = current index
// j = current dimension
// http://web.maths.unsw.edu.au/~fkuo/sobol/joe-kuo-notes.pdf
let compute_direction_vector (d, s, a, (m : int32 array)) L j = 
    let V = Array.create L 0u
    if j = 0 then for i in 0 .. L - 1 do V.[i] <- 1u <<< (31 - i)
    else
        if (L <= s) then for i in 0 .. L - 1 do V.[i] <- (u m.[i]) <<< (31 - i)
        else
            for i in 0 .. s - 1 do
                V.[i] <- (u m.[i]) <<< (31 - i)
            for i in s .. L - 1 do
                V.[i] <- V.[i-s] ^^^ (V.[i-s] >>> s)
                for k in 1 .. s-1 do V.[i] <- V.[i] ^^^ ((((u a) >>> (s-1-k)) &&& 1u) * V.[i-k])
    V

let next_sobol_value i previous (direction_vector : uint32 array) = previous ^^^ direction_vector.[right_most_zero_bit (i - 1)]

let sobol_direction_vectors n dim =
    let directions = Array.create dim (Array.empty<uint32>)
    for d in 0 .. dim - 1 do
        directions.[d] <- compute_direction_vector SobolDirectionData.direction_data.[d] (max_bits n) d
    directions

let sobol_sequence_integers n d = Seq.map (fun vector -> Seq.scan (fun x xs -> next_sobol_value xs x vector) 0u [1 .. n - 1]) (sobol_direction_vectors n d)

///<summary>
///Generates the first N tuples of the Sobol Sequence for the desired number of dimensions
///</summary>
///<param name="n">The number of Sobol values to generate</param>
///<param name="d">The number of dimensions to generate the sequence for</param>
///<remarks>
///The Sobol method generates a sequence of low-discrepancy values within the unit hypercube
///as binary fractions evaluated using a set of helper direction vectors to guide its filling
///gaps in the hypercube.
///Sobol sequences were designed in an attempt to numerically integrate a function over its input parameters with a selection
///of parameter values such that the approximation converges to the correct integral as fast as possible, as the number
///of observations on the function increases.
///The direction vectors used are from Stephen Joe and Frances Kuo,
///available online at http://web.maths.unsw.edu.au/~fkuo/sobol/, which can cover up to 21,000 dimensions.
///Sobol's original implementation is significantly enhanced through the use of Gray Codes, proposed by Antonov/Saleev (1979)
///which only require changing one bit at a time to iterate through the integers, and avoid some recomputation.
///</remarks>
let sobol_sequence n d = Seq.map (fun sq -> (Seq.map (fun x -> double x / double System.UInt32.MaxValue) sq)) (sobol_sequence_integers n d)

open PseudoRandom

let latin_permutations n random_sequence =
    let width = 1.0 / double n
    Seq.take n random_sequence |>
        Seq.mapi (fun i x -> (i, x)) |>
        Seq.sortBy (fun (i, x) -> x) |>
        Seq.map (fun (i, x) -> (double i) * width + x * width)

///<summary>
///Generates a sub-random sequence of tuples to perform Latin Hypercube Sampling on a multi-dimensional function
///</summary>
///<param name="n">The number of samples to take</param>
///<param name="d">The number of dimensions in the dataset to sample from</param>
///<param name="generator">A random number generator</param>
///<param name="seed">A random seed appropriate for the random number generator provided</param>
///<remarks>
///Although not exactly a quasi-random number generator, Latin Hypercube Sampling is a method
///for identifying sample points from a function, dataset or distribution when the allowed number
///of observations is extremely limited. To help more evenly cover the hypercube than unbiased random sampling,
///it partitions each dimension into n subspaces P and forms a hypergrid G of candidate cells out of the cartesian
///product of these partitions.
///<equation>$$P=\left ( \frac{i}{n}\ , \frac{i+1}{n}\right ) \textup{ for } i=0...n-1$$</equation>
///<equation>$$G = \{(q_0, q_1, .., q_{n-1})\ |\ q_0 \in P, q_1 \in P ... q_{n-1} \in P\}$$</equation>
///A sample is a tuple of n real numbers between zero and one, with one value for dimension:
///<equation>$$S=\{(x_0, x_1, .., x_{n-1})\ |\ x_0 \in (0,1), x1 \in (0,1), ..x_{n-1} \in (0,1) \}</equation>
///In a classical Monte Carlo simulation, we would select any random tuple for S and we'd be done. But in our case,
///a set of samples form a Latin Hypercube Sample-Set only if there is no pair (U,V) in that set such that they both
///draw from the same partition for any given dimension:
///<equation>$$\nexists\ U, V \in S, i \in \mathbb{N} : U \neq V\textup{ and }U_d \in P_i \textup{ and } V_d \in P_i\newline$$</equation>
///For example, with 10 partitions, (0.15, 0.52) (0.52, 0.15) (0.25, 0.85) is acceptable, but
///(0.15, 0.52)(0.16, 0.82)(0.35, 0.92) is not because 0.15 and 0.16 fall within the same partition.
///</remarks>
let latin_hypercube_sample n d generator seed =
    Seq.unfold (fun (i, state) ->
        if i = d
        then None
        else
            let (new_state, values) = random_sequence2 generator state n
            Some(latin_permutations n values, (i + 1, new_state))) (0, seed) |> transpose
