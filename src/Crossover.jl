module Crossover

using ..Models
using Random
using Distributions: Normal

export BinomialCrossover, SACrossover, get_mask

# --- Multiple Dispatch Interface ---

function get_mask(c::CrossoverStrat, d::Int, ρ)::Vector{Bool}
    error("Not implemented for $(typeof(c))")
end

# --- Implementations ---

struct BinomialCrossover <: CrossoverStrat; λ::Float64; end
function get_mask(c::BinomialCrossover, d::Int, ρ)
    mask = [rand(ρ) < c.λ for _ in 1:d]
    mask[rand(ρ, 1:d)] = true # Ensure at least one mutation
    return mask
end

struct SACrossover <: CrossoverStrat end
function get_mask(c::SACrossover, d::Int, ρ)
    # pr ~ Normal(0.5, 0.15) per dimension
    mask = [rand(ρ) < rand(ρ, Normal(0.5, 0.15)) for _ in 1:d]
    mask[rand(ρ, 1:d)] = true
    return mask
end

end
