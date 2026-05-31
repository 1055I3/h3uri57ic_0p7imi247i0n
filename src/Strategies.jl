module Strategies

using ..Models
using ..Crossover
using Random
using Statistics
using Distributions: Normal
using StatsBase: sample

export Rand1, Best2, SDE, AGS, FISA, propose_trial, adapt!

# --- Selection & Mutation Strategies ---

struct Rand1 <: SelectionMutation; ω::Float64; end

struct Best2 <: SelectionMutation; ν::Float64; ω::Float64; end

mutable struct SDE <: SelectionMutation
    ωs::Vector{Float64}
    SDE(d::Int) = new([rand(Normal(0.5, 0.15)) for _ in 1:d])
end

mutable struct AGS <: SelectionMutation
    WF::Vector{Float64}; WCR::Vector{Float64}; F::Vector{Float64}; CR::Vector{Float64}; ι::Int
    AGS() = new(Float64[], Float64[], Float64[], Float64[], 0)
end

mutable struct FISA <: SelectionMutation
    σ2::Vector{Float64}; count::Threads.Atomic{Int}; G::Ref{Float64}
    FISA() = new(Float64[], Threads.Atomic{Int}(0), Ref(0.0))
end

# --- Polymorphic Logic Implementation ---

function _sample_unique(ρ, pool, n)
    # Sound sampling without replacement
    return sample(ρ, pool, n, replace=false)
end

function _enforce!(v, lb, ub, ρ)
    for k in eachindex(v)
        if !(lb[k] <= v[k] <= ub[k])
            v[k] = rand(ρ) * (ub[k] - lb[k]) + lb[k]
        end
    end
    return v
end

# --- Propose Trials (Multiple Dispatch) ---

function propose_trial(m::Rand1, c::CrossoverStrat, i, Π, Φ, ρ, Σ, β)
    n, d = length(Π), length(Π[1])
    mask = get_mask(c, d, ρ)
    pool = deleteat!(collect(1:n), i)
    s = _sample_unique(ρ, pool, 3)
    a, b, cv = Π[s[1]], Π[s[2]], Π[s[3]]
    v = copy(Π[i])
    for k in 1:d; if mask[k]; v[k] = a[k] + m.ω * (b[k] - cv[k]); end; end
    return _enforce!(v, Σ[:bounds]..., ρ)
end

function propose_trial(m::Best2, c::CrossoverStrat, i, Π, Φ, ρ, Σ, β)
    n, d = length(Π), length(Π[1])
    mask = get_mask(c, d, ρ)
    pool = deleteat!(collect(1:n), i)
    s = _sample_unique(ρ, pool, 4)
    a, b, cv, dv = Π[s[1]], Π[s[2]], Π[s[3]], Π[s[4]]
    v = copy(Π[i])
    for k in 1:d; if mask[k]; v[k] = β[k] + m.ν * (a[k] - b[k]) + m.ω * (cv[k] - dv[k]); end; end
    return _enforce!(v, Σ[:bounds]..., ρ)
end

function propose_trial(m::SDE, c::CrossoverStrat, i, Π, Φ, ρ, Σ, β)
    n, d = length(Π), length(Π[1])
    mask = get_mask(c, d, ρ)
    pool = deleteat!(collect(1:n), i)
    s = _sample_unique(ρ, pool, 3)
    a, b, cv = Π[s[1]], Π[s[2]], Π[s[3]]
    v = copy(Π[i])
    for k in 1:d; if mask[k]; v[k] = a[k] + m.ωs[k] * (b[k] - cv[k]); end; end
    return _enforce!(v, Σ[:bounds]..., ρ)
end

function propose_trial(m::AGS, c::CrossoverStrat, i, Π, Φ, ρ, Σ, β)
    n, d = length(Π), length(Π[1])
    if isempty(m.WF)
        m.WF, m.WCR, m.F, m.CR = fill(1.0, n), fill(1.0, n), fill(0.5*(1+1/sqrt(d)), n), fill(0.5, n)
    end
    if i == 1; m.ι += 1; end
    
    σ_f(x) = 1.0 / (1.0 + exp(-x))
    sc = 1.0 / (1.0 + log(1.0 + i))
    m.F[i] = clamp(2.0 * σ_f(log(m.WF[i]) * sc), 1e-12, 1.999999)
    m.CR[i] = clamp(σ_f(log(m.WCR[i]) * sc), 1e-12, 1.0 - 1e-12)
    
    if i == 1 && mean(var(hcat(Π...), dims=2)) < 1e-8*(1+n+d); m.F .*= 0.5; end
    
    s = _sample_unique(ρ, deleteat!(collect(1:n), i), 3)
    a, b, cv = Π[s[1]], Π[s[2]], Π[s[3]]
    v = copy(Π[i]); jr = rand(ρ, 1:d)
    for j in 1:d; if rand(ρ) < m.CR[i] || j == jr; v[j] = a[j] + m.F[i] * (b[j] - cv[j]); end; end
    return _enforce!(v, Σ[:bounds]..., ρ)
end

function propose_trial(m::FISA, c::CrossoverStrat, i, Π, Φ, ρ, Σ, β)
    n, d = length(Π), length(Π[1])
    if isempty(m.σ2); val=0.2*(1+1/sqrt(d)); m.σ2, m.G[] = fill(val^2, d), val; end
    
    cr = clamp(0.5 * (1.0 + tanh((i - n/2.0)/(1.0 + sqrt(d)))), 1e-12, 1.0 - 1e-12)
    σ = sqrt.(m.σ2)
    v = Π[i] .+ (randn(ρ, d) .* σ)
    
    final = copy(Π[i]); jr = rand(ρ, 1:d)
    for j in 1:d; if rand(ρ) < cr || j == jr; final[j] = v[j]; end; end
    return _enforce!(final, Σ[:bounds]..., ρ)
end

# --- Adaptation Phase (Multiple Dispatch) ---

adapt!(m::SelectionMutation, Π, Φ, Ξ, Φ_old, ρ, Σ) = nothing

function adapt!(m::SDE, Π, Φ, Ξ, Φ_old, ρ, Σ)
    d = length(Π[1]); nw = copy(m.ωs)
    for k in 1:d
        s = _sample_unique(ρ, 1:d, 3)
        o1, o2, o3 = m.ωs[s[1]], m.ωs[s[2]], m.ωs[s[3]]
        nw[k] = o1 + rand(ρ, Normal(0, 0.5)) * (o2 - o3)
    end
    m.ωs = nw
end

function adapt!(m::AGS, Π, Φ, Ξ, Φ_old, ρ, Σ)
    n = length(Π); η = 1.0 / (1.0 + log(1.0 + n*length(Π[1])))
    for i in 1:n
        if Φ[i] < Φ_old[i]
            m.WF[i] *= exp(η); m.WCR[i] *= exp(η)
        else
            m.WF[i] *= exp(-η); m.WCR[i] *= exp(-η)
        end
    end
end

function adapt!(m::FISA, Π, Φ, Ξ, Φ_old, ρ, Σ)
    n, d = length(Π), length(Π[1])
    β, ε = 1.0 / (1.0 + log(1.0 + n*d)), 1e-12 * (1 + n + d)
    acc = 0
    for i in 1:n
        if Φ[i] < Φ_old[i]
            s = Ξ[i] .- Π[i]
            m.count[] += 1
            α = 1.0 / (1.0 + sqrt(1.0 + m.count[]))
            m.σ2 .= (1.0 - α) .* m.σ2 .+ α .* (s.^2 .+ ε)
            acc += 1
        end
    end
    if acc > 0
        σ = sqrt.(m.σ2); clamp!(σ, ε, 0.5*(1+1/sqrt(d))); m.σ2 .= σ.^2
        m.G[] = exp((1.0 - β)*log(m.G[]) + β*mean(log.(σ .+ ε)))
        if mean(σ) < 0.25 * m.G[]; σ .*= 1.2; m.σ2 .= σ.^2; m.G[] *= 1.2; end
    end
end

end
