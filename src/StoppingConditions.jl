module StoppingConditions

using ..Models
using Statistics
using SpecialFunctions: loggamma

export MaxIterStop, FitnessStop, NoImproveStop, PocockSignStop, SPRTStop, PermutationStop, check

struct MaxIterStop <: StopCond; max_ι::Int; end
check(c::MaxIterStop, ζ::ζ_Stats) = ζ.ι[] < c.max_ι

struct FitnessStop <: StopCond; θ::Float64; end
check(c::FitnessStop, ζ::ζ_Stats) = isempty(ζ.φ_hist) || c.θ < ζ.φ_hist[end]

struct NoImproveStop <: StopCond; max_no_ι::Int; θ::Float64; end
function check(c::NoImproveStop, ζ::ζ_Stats)
    if ζ.ι[] > c.max_no_ι
        window = ζ.φ_hist[end-c.max_no_ι:end]
        improves = [abs(window[i+1] - window[i]) for i in 1:(length(window)-1)]
        return !all(imp < c.θ for imp in improves)
    end
    return true
end

struct PocockSignStop <: StopCond end
function check(c::PocockSignStop, ζ::ζ_Stats)
    n_h = length(ζ.φ_hist)
    n_h < 7 && return true
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    Δ_f = filter(x -> x != 0.0, Δ)
    n = length(Δ_f)
    n < 6 && return true
    K = count(x -> x > 0.0, Δ_f)
    t_f(i) = exp(loggamma(n+1) - loggamma(i+1) - loggamma(n-i+1) - n*log(2.0))
    p_mid = sum(t_f(i) for i in (K+1):n; init=0.0) + 0.5 * t_f(K)
    return p_mid <= 0.02275 
end

struct SPRTStop <: StopCond end
function check(c::SPRTStop, ζ::ζ_Stats)
    n_h = length(ζ.φ_hist)
    n_h < 2 && return true
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    n, K = length(Δ), count(x -> x > 0.0, Δ)
    lM = loggamma(K+0.5) + loggamma(n-K+0.5) - loggamma(n+1) - (2*loggamma(0.5))
    return exp(lM) > 0.05
end

struct PermutationStop <: StopCond end
function check(c::PermutationStop, ζ::ζ_Stats)
    m = 5
    n_h = length(ζ.φ_hist)
    n_h < (2+2m) && return true
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    X = Δ[end-2m+1:end]
    T_s(A, B) = begin
        μA, μB = mean(A), mean(B)
        vA, vB = var(A), var(B)
        d = sqrt(vA/m + vB/m)
        (d < 1e-18) ? 0.0 : (μB - μA) / d
    end
    obs_T = T_s(X[1:m], X[m+1:end])
    c_ge, n_perms = 0, 0
    for i in 0:(2^(2m)-1)
        if count_ones(i) == m
            n_perms += 1
            idx_A = [j+1 for j in 0:(2m-1) if (i >> j) & 1 == 1]
            idx_B = [j+1 for j in 0:(2m-1) if (i >> j) & 1 == 0]
            if T_s(X[idx_A], X[idx_B]) >= obs_T; c_ge += 1; end
        end
    end
    return (c_ge + 1) / (n_perms + 1) <= 0.975
end

end
