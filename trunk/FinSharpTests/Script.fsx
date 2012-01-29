//#load "Script.fsx"
//
#load "../FinSharpLib/MersenneTwister.fs"
#load "../FinSharpLib/Common.fs"
//open DistributionFunctions;;
#load "../FinSharpLib/PseudoRandom.fs";; open PseudoRandom;;


#load "../FinSharpLib/Distributions.fs";; open Distributions;;


#load "../FinSharpLib/Common.fs";; open Common;
#load "../FinSharpLib/SobolDirectionData.fs";;
#load "../FinSharpLib/QuasiRandom.fs";; open QuasiRandom;;
#load "../FinSharpLib/DistributionFunctions.fs";; open DistributionFunctions
//
//let rs = random_sequence next_mersenne_value (new_mersenne_state 0) 300;;
//
//latin_hypercube_sample 2 2 next_mersenne_value (new_mersenne_state 95)