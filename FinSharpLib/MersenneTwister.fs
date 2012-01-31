module MersenneTwister

// This file contains the implementation of the Mersenne Twister.
// Access it through the PseudoRandom module.
// Search for "mersenne_twister_generator_MT19937" for the documentation

let u x = uint32 x
let N = 624
let M = 397

let take_upper x = x &&& u 0xFFFF0000
let take_head x  = x &&& u 0x80000000
let take_tail x  = x &&& u 0x7FFFFFFF
let mask_b x     = x &&& u 0x9D2C5680
let mask_c x     = x &&& u 0xEFC60000

let shift_u y    = y >>> 0xB
let shift_s y    = y <<< 0x7
let shift_t y    = y <<< 0xF
let shift_l y    = y >>> 0x12

let bump seed = u 0x10DCD * seed + u 1
let at i      = (i + N) % N
let mag x     = if x % u 2 = u 0 then u 0 else u 0x9908b0df

type mersenne_state = { mutable mti : int; mt : uint32 array }

let unfold_state starting_seed = Seq.unfold(fun state ->
                    let seed = fst(state)
                    let i = snd(state)
                    let left = take_upper seed
                    let reseed = bump seed
                    let right = (take_upper reseed) >>> 16
                    if i < N then Some(left ||| right, ((bump reseed), (i+1))) else None) (u starting_seed, 0)

let get_tempered state = 
    let ii = state.mti 
    let a = state.mt.[ii]
    state.mti <- state.mti + 1
    let b = a ^^^ shift_u a
    let c = b ^^^ (mask_b(shift_s b))
    let d = c ^^^ (mask_c(shift_t c))
    d ^^^ shift_l d

let twist state i x = 
    let y = (take_head state.mt.[at i]) ||| (take_tail state.mt.[at(i + 1)])
    state.mt.[i] <- state.mt.[at (i + (M - N))] ^^^ (y >>> 1) ^^^ (mag y)

let new_mersenne_state seed = { mt = Array.ofSeq(unfold_state seed); mti = N }

let next_random state = if state.mti = N then
                            state.mti <- 0
                            Array.iteri (twist state) state.mt
                        get_tempered state
