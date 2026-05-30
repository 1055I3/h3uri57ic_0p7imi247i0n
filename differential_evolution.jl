using Random
using Statistics
using Base.Threads
using Distributions: Normal
using Optim
using StatsBase: sample
using SpecialFunctions: loggamma

# --- Unicode Aliases ---
const Γ = loggamma
const Π_type = Vector{Vector{Float64}}
const Φ_type = Vector{Float64}

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

function compute_δ(Π::Π_type)
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

function update_ζ!(ζ::ζ_Stats, Π::Π_type, Φ::Φ_type, ε_batch::Int)
    lock(ζ.ℓ) do
        idx = argmin(Φ)
        Threads.atomic_add!(ζ.ε, ε_batch)
        push!(ζ.β_hist, copy(Π[idx]))
        push!(ζ.φ_hist, Φ[idx])
        push!(ζ.δ_hist, compute_δ(Π))
    end
end

# --- Stopping Conditions ---

abstract type StopCond end

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

# Mathematically rigorous parameter-free stops
struct PocockSignStop <: StopCond end
function check(c::PocockSignStop, ζ::ζ_Stats)
    n_h = length(ζ.φ_hist)
    n_h < 7 && return true
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    Δ_f = filter(x -> x != 0.0, Δ)
    n = length(Δ_f)
    n < 6 && return true
    K = count(x -> x > 0.0, Δ_f)
    t_f(i) = exp(Γ(n+1) - Γ(i+1) - Γ(n-i+1) - n*log(2.0))
    p_mid = sum(t_f(i) for i in (K+1):n; init=0.0) + 0.5 * t_f(K)
    return p_mid <= 0.02275 
end

struct SPRTStop <: StopCond end
function check(c::SPRTStop, ζ::ζ_Stats)
    n_h = length(ζ.φ_hist)
    n_h < 2 && return true
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    n, K = length(Δ), count(x -> x > 0.0, Δ)
    lM = Γ(K+0.5) + Γ(n-K+0.5) - Γ(n+1) - (2*Γ(0.5))
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

# --- Mutation and Crossover Strategies ---

abstract type CrossoverStrat end
struct BinomialCrossover <: CrossoverStrat; λ::Float64; end
struct SACrossover <: CrossoverStrat; end

abstract type SelecciónMutación end

struct Rand1 <: SelecciónMutación; ω::Float64; end
struct Best2 <: SelecciónMutación; ν::Float64; ω::Float64; end
mutable struct SDE <: SelecciónMutación
    ωs::Vector{Float64}
    SDE(d::Int) = new([rand(Normal(0.5, 0.15)) for _ in 1:d])
end

mutable struct AGS <: SelecciónMutación
    WF::Vector{Float64}; WCR::Vector{Float64}; F::Vector{Float64}; CR::Vector{Float64}; ι::Int
    AGS() = new(Float64[], Float64[], Float64[], Float64[], 0)
end

mutable struct FISA <: SelecciónMutación
    σ2::Vector{Float64}; count::Threads.Atomic{Int}; G::Ref{Float64}
    FISA() = new(Float64[], Threads.Atomic{Int}(0), Ref(0.0))
end

# --- Polymorphic Logic ---

function _sample_unique(ρ, pool, n)
    res = Int[]
    while length(res) < n
        idx = rand(ρ, pool)
        if !(idx in res); push!(res, idx); end
    end
    return res
end

function _enforce!(v, lb, ub)
    for k in eachindex(v)
        if !(lb[k] <= v[k] <= ub[k])
            v[k] = rand() * (ub[k] - lb[k]) + lb[k]
        end
    end
    return v
end

# Propose trial vectors
function propose(m::SelecciónMutación, c::CrossoverStrat, i, Π, Φ, ρ, Σ, β)
    n, d = length(Π), length(Π[1]); x = Π[i]
    p_cr = (c isa BinomialCrossover) ? c.λ : rand(ρ, Normal(0.5, 0.15))
    u = [rand(ρ) < p_cr for _ in 1:d]; u[rand(ρ, 1:d)] = true
    pool = deleteat!(collect(1:n), i); v = copy(x)
    if m isa Rand1
        s = _sample_unique(ρ, pool, 3); a, b, cv = Π[s[1]], Π[s[2]], Π[s[3]]
        for k in 1:d; if u[k]; v[k] = a[k] + m.ω * (b[k] - cv[k]); end; end
    elseif m isa Best2
        s = _sample_unique(ρ, pool, 4); a, b, cv, dv = Π[s[1]], Π[s[2]], Π[s[3]], Π[s[4]]
        for k in 1:d; if u[k]; v[k] = β[k] + m.ν * (a[k] - b[k]) + m.ω * (cv[k] - dv[k]); end; end
    elseif m isa SDE
        s = _sample_unique(ρ, pool, 3); a, b, cv = Π[s[1]], Π[s[2]], Π[s[3]]
        for k in 1:d; if u[k]; v[k] = a[k] + m.ωs[k] * (b[k] - cv[k]); end; end
    end
    return _enforce!(v, Σ[:bounds]...)
end

function propose(m::AGS, c::CrossoverStrat, i, Π, Φ, ρ, Σ, β)
    n, d = length(Π), length(Π[1])
    if isempty(m.WF); m.WF, m.WCR, m.F, m.CR = fill(1.0, n), fill(1.0, n), fill(0.5*(1+1/sqrt(d)), n), fill(0.5, n); end
    if i == 1; m.ι += 1; end
    σ_f(x) = 1.0 / (1.0 + exp(-x)); sc = 1.0 / (1.0 + log(1.0 + i))
    m.F[i] = clamp(2.0 * σ_f(log(m.WF[i]) * sc), 1e-12, 1.999999)
    m.CR[i] = clamp(σ_f(log(m.WCR[i]) * sc), 1e-12, 1.0 - 1e-12)
    if i == 1 && mean(var(hcat(Π...), dims=2)) < 1e-8*(1+n+d); m.F .*= 0.5; end
    s = _sample_unique(ρ, deleteat!(collect(1:n), i), 3); a, b, cv = Π[s[1]], Π[s[2]], Π[s[3]]
    v = copy(Π[i]); jr = rand(ρ, 1:d)
    for j in 1:d; if rand(ρ) < m.CR[i] || j == jr; v[j] = a[j] + m.F[i] * (b[j] - cv[j]); end; end
    return _enforce!(v, Σ[:bounds]...)
end

function propose(m::FISA, c::CrossoverStrat, i, Π, Φ, ρ, Σ, β)
    n, d = length(Π), length(Π[1])
    if isempty(m.σ2); val=0.2*(1+1/sqrt(d)); m.σ2, m.G[] = fill(val^2, d), val; end
    cr = clamp(0.5 * (1.0 + tanh((i - n/2.0)/(1.0 + sqrt(d)))), 1e-12, 1.0 - 1e-12)
    σ = sqrt.(m.σ2); v = Π[i] .+ (randn(ρ, d) .* σ); final = copy(Π[i]); jr = rand(ρ, 1:d)
    for j in 1:d; if rand(ρ) < cr || j == jr; final[j] = v[j]; end; end
    return _enforce!(final, Σ[:bounds]...)
end

# Adapt strategy parameters
adapt!(m::SelecciónMutación, Π, Φ, Ξ, Φ_old, ρ, Σ) = nothing

function adapt!(m::SDE, Π, Φ, Ξ, Φ_old, ρ, Σ)
    d = length(Π[1]); nw = copy(m.ωs)
    for k in 1:d
        s = _sample_unique(ρ, 1:d, 3); o1, o2, o3 = m.ωs[s[1]], m.ωs[s[2]], m.ωs[s[3]]
        nw[k] = o1 + rand(ρ, Normal(0, 0.5)) * (o2 - o3)
    end
    m.ωs = nw
end

function adapt!(m::AGS, Π, Φ, Ξ, Φ_old, ρ, Σ)
    n, d = length(Π), length(Π[1]); η = 1.0 / (1.0 + log(1.0 + n*d))
    for i in 1:n; if Φ[i] < Φ_old[i]; m.WF[i] *= exp(η); m.WCR[i] *= exp(η); else; m.WF[i] *= exp(-η); m.WCR[i] *= exp(-η); end; end
end

function adapt!(m::FISA, Π, Φ, Ξ, Φ_old, ρ, Σ)
    n, d = length(Π), length(Π[1]); β, ε = 1.0 / (1.0 + log(1.0 + n*d)), 1e-12 * (1 + n + d)
    acc = 0
    for i in 1:n; if Φ[i] < Φ_old[i]; s = Ξ[i] .- Π[i]; m.count[] += 1; α = 1.0 / (1.0 + sqrt(1.0 + m.count[])); m.σ2 .= (1.0 - α) .* m.σ2 .+ α .* (s.^2 .+ ε); acc += 1; end; end
    if acc > 0
        σ = sqrt.(m.σ2); clamp!(σ, ε, 0.5*(1+1/sqrt(d))); m.σ2 .= σ.^2
        m.G[] = exp((1.0 - β)*log(m.G[]) + β*mean(log.(σ .+ ε)))
        if mean(σ) < 0.25 * m.G[]; σ .*= 1.2; m.σ2 .= σ.^2; m.G[] *= 1.2; end
    end
end

# --- Generic DE Engine ---

function run_differential_evolution(obj, cons, ub, lb, n, stop, crossover, m_strat)
    ζ = ζ_Stats(); Σ = Dict{Symbol, Any}(:bounds => (lb, ub)); ρ = Random.default_rng()
    evaluate(x) = begin
        c_vals = [c(x) for c in cons]
        penalty = isempty(c_vals) ? 1.0 : prod([abs(c)>1e-14 ? c : 1.0 for c in c_vals])
        return obj(x) * penalty
    end
    Π = [[rand(ρ)*(ub[i]-lb[i])+lb[i] for i in 1:length(ub)] for _ in 1:n]
    Φ = evaluate.(Π); update_ζ!(ζ, Π, Φ, n)
    while check(stop, ζ)
        βest = ζ.β_hist[end]
        Ξ = [propose(m_strat, crossover, i, Π, Φ, ρ, Σ, βest) for i in 1:n]
        tasks = map(i -> Threads.@spawn(evaluate(Ξ[i])), 1:n); Φ_Ξ = fetch.(tasks)
        Φ_old = copy(Φ)
        for i in 1:n; if Φ_Ξ[i] < Φ[i]; Π[i], Φ[i] = Ξ[i], Φ_Ξ[i]; end; end
        adapt!(m_strat, Π, Φ, Ξ, Φ_old, ρ, Σ)
        Threads.atomic_add!(ζ.ι, 1); update_ζ!(ζ, Π, Φ, n)
    end
    return ζ
end

# --- Legacy API Restoration ---

function de_rand_1_max_iter(obj, cons, ub, lb, n, max_ι, p, ω)
    return run_differential_evolution(obj, cons, ub, lb, n, MaxIterStop(max_ι), BinomialCrossover(p), Rand1(ω))
end
function de_rand_1_fitness_threshold(obj, cons, ub, lb, n, θ, p, ω)
    return run_differential_evolution(obj, cons, ub, lb, n, FitnessStop(θ), BinomialCrossover(p), Rand1(ω))
end
function de_rand_1_no_improvement(obj, cons, ub, lb, n, ni, nt, p, ω)
    return run_differential_evolution(obj, cons, ub, lb, n, NoImproveStop(ni, nt), BinomialCrossover(p), Rand1(ω))
end
function de_best_2_max_iter(obj, cons, ub, lb, n, max_ι, p, ν, ω)
    return run_differential_evolution(obj, cons, ub, lb, n, MaxIterStop(max_ι), BinomialCrossover(p), Best2(ν, ω))
end
function de_best_2_fitness_threshold(obj, cons, ub, lb, n, θ, p, ν, ω)
    return run_differential_evolution(obj, cons, ub, lb, n, FitnessStop(θ), BinomialCrossover(p), Best2(ν, ω))
end
function de_best_2_no_improvement(obj, cons, ub, lb, n, ni, nt, p, ν, ω)
    return run_differential_evolution(obj, cons, ub, lb, n, NoImproveStop(ni, nt), BinomialCrossover(p), Best2(ν, ω))
end
function sde_rand_1_max_iter(obj, cons, ub, lb, n, max_ι)
    return run_differential_evolution(obj, cons, ub, lb, n, MaxIterStop(max_ι), SACrossover(), SDE(length(ub)))
end
function sde_rand_1_fitness_threshold(obj, cons, ub, lb, n, θ)
    return run_differential_evolution(obj, cons, ub, lb, n, FitnessStop(θ), SACrossover(), SDE(length(ub)))
end
function sde_rand_1_no_improvement(obj, cons, ub, lb, n, ni, nt)
    return run_differential_evolution(obj, cons, ub, lb, n, NoImproveStop(ni, nt), SACrossover(), SDE(length(ub)))
end

# --- Benchmarks ---

module Benchmarks
    using Statistics, Random
    const N, M = 16, 32
    sphere(x) = sum(x.^2); sphere_b = (fill(Float64(2^12), N), fill(Float64(-2^12), N))
    rosenbrock(x) = sum(100*(x[1:end-1].^2 .- x[2:end]).^2 .+ (1 .- x[1:end-1]).^2); rosenbrock_b = (fill(Float64(2^12), N), fill(Float64(-2^12), N))
    step(x) = sum(floor.(x)); step_b = (fill(5.12, N), fill(-5.12, N))
    griewank(x) = 1 + sum(x.^2)/4000 - prod(cos.(x ./ sqrt.(1:length(x)))); griewank_b = (fill(Float64(2^12), N), fill(Float64(-2^12), N))
    styblinski(x) = sum(x.^4 .- 16*x.^2 .+ 5*x) / 2; styblinski_b = (fill(5.0, N), fill(-5.0, N))
    Random.seed!(42); const A, C = rand(N, 32), rand(32)
    shekel(x) = -sum(1.0 ./ (sum((x .- A).^2, dims=1)[:] .+ C)); shekel_b = (fill(Float64(2^12), N), fill(Float64(-2^12), N))
    rastrigin(x) = sum(x.^2 .- 10 .* cos.(2π .* x) .+ 10); rastrigin_b = (fill(5.12, N), fill(-5.12, N))
    ackley(x) = -20*exp(-0.2*sqrt(mean(x.^2))) - exp(mean(cos.(2π .* x))) + 20 + ℯ; ackley_b = (fill(65.536, N), fill(-65.536, N))
    rotated(x) = sum((1:length(x)) .* x.^2); rotated_b = (fill(32.768, N), fill(-32.768, N))
    struct Keane; f::Function; c::Vector{Function}; b::Tuple{Vector{Float64}, Vector{Float64}}
        function Keane()
            fn(x) = -abs((sum(cos.(x).^4) - 2*prod(cos.(x).^2)) / (sqrt(sum(i * x[i]^2 for i in 1:length(x))) + 1e-14))
            c1(x) = (0.75 - prod(x) < 0) ? 1.0 : (0.75 - prod(x))*75
            c2(x) = (sum(x) - 7.5*length(x) < 0) ? 1.0 : (sum(x) - 7.5*length(x))*75
            new(fn, [c1, c2], (fill(10.0, N), fill(0.0, N)))
        end
    end
end

function main()
    println("Threads: $(Threads.nthreads())")
    kb = Benchmarks.Keane()
    tests = [
        ("Sphere", Benchmarks.sphere, [x->1.0], Benchmarks.sphere_b...), 
        ("Rosenbrock", Benchmarks.rosenbrock, [x->1.0], Benchmarks.rosenbrock_b...),
        ("Step", Benchmarks.step, [x->1.0], Benchmarks.step_b...),
        ("Griewank", Benchmarks.griewank, [x->1.0], Benchmarks.griewank_b...),
        ("Styblinski", Benchmarks.styblinski, [x->1.0], Benchmarks.styblinski_b...),
        ("Shekel", Benchmarks.shekel, [x->1.0], Benchmarks.shekel_b...),
        ("Rastrigin", Benchmarks.rastrigin, [x->1.0], Benchmarks.rastrigin_b...),
        ("Ackley", Benchmarks.ackley, [x->1.0], Benchmarks.ackley_b...),
        ("Rotated", Benchmarks.rotated, [x->1.0], Benchmarks.rotated_b...),
        ("Keane", kb.f, kb.c, kb.b...)
    ]
    
    tasks = map(tests) do (n, o, c, ub, lb)
        Threads.@spawn (n, run_differential_evolution(o, c, ub, lb, 10*Benchmarks.N, MaxIterStop(100), BinomialCrossover(0.1), AGS()))
    end

    for t in tasks
        name, ζ = fetch(t)
        println("--- $name ---\nDE: ", round(ζ.φ_hist[end], digits=6), " (ι: $(ζ.ι[]), ε: $(ζ.ε[]))")
    end
end

if abspath(PROGRAM_FILE) == @__FILE__; main(); end
