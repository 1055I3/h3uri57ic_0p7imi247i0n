module StoppingConditions

using ..Models
using Statistics
using SpecialFunctions: loggamma

export MaxIterStop, FitnessStop, NoImproveStop, PocockSignStop, SPRTStop, PermutationStop, MaxRuntimeStop, should_continue

function should_continue(c::StopCond, ζ::ζ_Stats)::Bool
    error("Not implemented for $(typeof(c))")
end

struct MaxIterStop <: StopCond; max_ι::Int; end
should_continue(c::MaxIterStop, ζ::ζ_Stats) = ζ.ι[] < c.max_ι

struct FitnessStop <: StopCond; θ::Float64; end
should_continue(c::FitnessStop, ζ::ζ_Stats) = isempty(ζ.φ_hist) || c.θ < ζ.φ_hist[end]

struct NoImproveStop <: StopCond; max_no_ι::Int; θ::Float64; end
function should_continue(c::NoImproveStop, ζ::ζ_Stats)
    if ζ.ι[] > c.max_no_ι
        window = ζ.φ_hist[end-c.max_no_ι:end]
        improves = [abs(window[i+1] - window[i]) for i in 1:(length(window)-1)]
        return !all(imp < c.θ for imp in improves)
    end
    return true
end

struct PocockSignStop <: StopCond end
function should_continue(c::PocockSignStop, ζ::ζ_Stats)
    n_h = length(ζ.φ_hist)
    n_h < 7 && return true # Pocock minimum sample size
    
    # Δ_t = b_{t-1} - b_t
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    Δ_f = filter(x -> x != 0.0, Δ)
    n = length(Δ_f)
    
    if n < 6
        return n_h < 30 # Spirit of SPRT: eventually stop if stuck in ties
    end
    
    K = count(x -> x > 0.0, Δ_f)
    
    # mid-p = P(K > k) + 0.5 * P(K = k)
    t_f(i) = exp(loggamma(n+1) - loggamma(i+1) - loggamma(n-i+1) - n*log(2.0))
    p_mid = sum(t_f(i) for i in (K+1):n; init=0.0) + 0.5 * t_f(K)
    
    # Pocock boundary for α=0.05 is 0.02275. If p_mid > 0.02275, accept "no improvement".
    return p_mid <= 0.02275 
end

struct SPRTStop <: StopCond end
function should_continue(c::SPRTStop, ζ::ζ_Stats)
    n_h = length(ζ.φ_hist)
    n_h < 2 && return true
    
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    n, K = length(Δ), count(x -> x > 0.0, Δ)
    
    # Jeffreys mixture marginal
    lM = loggamma(K+0.5) + loggamma(n-K+0.5) - loggamma(n+1) - (2*loggamma(0.5))
    M = exp(lM)
    
    # Type I error controlled at 0.05. Stop if M <= 0.05.
    return M > 0.05
end

struct PermutationStop <: StopCond end
function should_continue(c::PermutationStop, ζ::ζ_Stats)
    m = 5
    n_h = length(ζ.φ_hist)
    n_h < (2+2m) && return true
    
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    X = Δ[end-2m+1:end]
    
    function T_s(A, B)
        μA, μB = mean(A), mean(B)
        vA, vB = length(A) > 1 ? var(A) : 0.0, length(B) > 1 ? var(B) : 0.0
        denom = sqrt(vA/m + vB/m)
        return (denom < 1e-18) ? 0.0 : (μB - μA) / denom
    end
    
    obs_T = T_s(X[1:m], X[m+1:end])
    
    # Exact studentized permutation enumeration
    c_ge, n_perms = 0, 0
    for i in 0:(2^(2m)-1)
        if count_ones(i) == m
            n_perms += 1
            idx_A = [j+1 for j in 0:(2m-1) if (i >> j) & 1 == 1]
            idx_B = [j+1 for j in 0:(2m-1) if (i >> j) & 1 == 0]
            if T_s(X[idx_A], X[idx_B]) >= obs_T; c_ge += 1; end
        end
    end
    
    # Accept "no improvement" if p_perm > 0.975 (compliment of 0.025 one-sided)
    return (c_ge + 1) / (n_perms + 1) <= 0.975
end

mutable struct MaxRuntimeStop <: StopCond
    max_time_s::Float64
    _start_time_s::Float64
    initialized::Bool

    MaxRuntimeStop(t::Float64) = new(t, 0.0, false)
end

function should_continue(c::MaxRuntimeStop, ζ::ζ_Stats)
    if !c.initialized
        c._start_time_s = time()
        c.initialized = true
    end
    return (time() - c._start_time_s) < c.max_time_s
end

end
