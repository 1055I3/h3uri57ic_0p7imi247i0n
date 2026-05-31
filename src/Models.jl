module Models

using Base.Threads
using Statistics

export ζ_Stats, StopCond, SelectionMutation, CrossoverStrat, update_ζ!, compute_δ

# --- Performance Statistics (ζ) ---

mutable struct ζ_Stats
    ι::Threads.Atomic{Int}      # iteration counter
    ε::Threads.Atomic{Int}      # evaluation counter
    β_hist::Vector{Vector{Float64}}  # best solution history
    φ_hist::Vector{Float64}     # best score history
    δ_hist::Vector{Float64}     # population diversity history
    ℓ::ReentrantLock           # thread lock

    ζ_Stats() = new(Threads.Atomic{Int}(0), Threads.Atomic{Int}(0), Vector{Vector{Float64}}(), Vector{Float64}(), Vector{Float64}(), ReentrantLock())
end

function compute_δ(Π::Vector{Vector{Float64}})
    n = length(Π)
    n <= 1 && return 0.0
    total_dist = 0.0
    for i in 1:n
        for j in (i+1):n
            d_sum = 0.0
            @inbounds for k in eachindex(Π[i])
                d_sum += (Π[i][k] - Π[j][k])^2
            end
            total_dist += sqrt(d_sum)
        end
    end
    return total_dist / (n * (n - 1) / 2)
end

function update_ζ!(ζ::ζ_Stats, Π::Vector{Vector{Float64}}, Φ::Vector{Float64}, ε_batch::Int)
    lock(ζ.ℓ) do
        idx = argmin(Φ)
        Threads.atomic_add!(ζ.ε, ε_batch)
        push!(ζ.β_hist, copy(Π[idx]))
        push!(ζ.φ_hist, Φ[idx])
        push!(ζ.δ_hist, compute_δ(Π))
    end
end

# --- Abstract Types for Dispatch ---

abstract type StopCond end
abstract type SelectionMutation end
abstract type CrossoverStrat end

end
