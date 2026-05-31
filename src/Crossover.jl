module Crossover

using ..Models
using Random
using Distributions: Normal

export BinomialCrossover, SACrossover, get_λ

struct BinomialCrossover <: CrossoverStrat; λ::Float64; end
get_λ(c::BinomialCrossover, ρ) = c.λ

struct SACrossover <: CrossoverStrat; end
get_λ(c::SACrossover, ρ) = rand(ρ, Normal(0.5, 0.15))

end
