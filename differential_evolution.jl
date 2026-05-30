using Random
using Statistics
using Base.Threads
using Distributions: Normal
using Optim
using StatsBase: sample
using SpecialFunctions: loggamma

# --- Performance Stats ---

mutable struct PerformanceStats
    ι::Threads.Atomic{Int}      # iter_count
    ε::Threads.Atomic{Int}      # eval_count
    β_hist::Vector{Vector{Float64}}  # best_solution_hist
    φ_hist::Vector{Float64}     # best_score_hist
    δ_hist::Vector{Float64}     # population_diversity_hist
    ℓ::ReentrantLock           # lock

    PerformanceStats() = new(Threads.Atomic{Int}(0), Threads.Atomic{Int}(0), Vector{Vector{Float64}}(), Vector{Float64}(), Vector{Float64}(), ReentrantLock())
end

function compute_diversity(Π::Vector{Vector{Float64}})
    n = length(Π)
    n <= 1 && return 0.0
    total_dist = 0.0
    for i in 1:n
        for j in (i+1):n
            d = 0.0
            @inbounds for k in eachindex(Π[i])
                d += (Π[i][k] - Π[j][k])^2
            end
            total_dist += sqrt(d)
        end
    end
    return total_dist / (n * (n - 1) / 2)
end

function update_stats!(ζ::PerformanceStats, Π::Vector{Vector{Float64}}, Φ::Vector{Float64})
    lock(ζ.ℓ) do
        best_idx = argmin(Φ)
        push!(ζ.β_hist, copy(Π[best_idx]))
        push!(ζ.φ_hist, Φ[best_idx])
        push!(ζ.δ_hist, compute_diversity(Π))
    end
end

# --- Stopping Conditions ---

abstract type StoppingCondition end

struct MaxIterationsStop <: StoppingCondition
    max_ι::Int
end
check(c::MaxIterationsStop, ζ::PerformanceStats) = ζ.ι[] < c.max_ι

struct FitnessThresholdStop <: StoppingCondition
    θ::Float64
end
check(c::FitnessThresholdStop, ζ::PerformanceStats) = isempty(ζ.φ_hist) || c.θ < ζ.φ_hist[end]

struct NoImprovementStop <: StoppingCondition
    max_no_ι::Int
    θ::Float64
end
function check(c::NoImprovementStop, ζ::PerformanceStats)
    if ζ.ι[] > c.max_no_ι
        window = ζ.φ_hist[end-c.max_no_ι:end]
        improves = [abs(window[i+1] - window[i]) for i in 1:(length(window)-1)]
        return !all(imp < c.θ for imp in improves)
    end
    return true
end

struct PocockSignStop <: StoppingCondition end
function check(c::PocockSignStop, ζ::PerformanceStats)
    n_h = length(ζ.φ_hist)
    n_h < 7 && return true
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    Δ_f = filter(x -> x != 0.0, Δ)
    n = length(Δ_f)
    n < 6 && return true
    K = count(x -> x > 0.0, Δ_f)
    
    t_f(i) = exp(loggamma(n+1) - loggamma(i+1) - loggamma(n-i+1) - n*log(2.0))
    Peq = t_f(K)
    Pgt = sum(t_f(i) for i in (K+1):n; init=0.0)
    p_mid = Pgt + 0.5 * Peq
    
    # Continue while p_mid <= 0.02275
    return p_mid <= 0.02275 
end

struct SPRTStop <: StoppingCondition end
function check(c::SPRTStop, ζ::PerformanceStats)
    n_h = length(ζ.φ_hist)
    n_h < 2 && return true
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    n = length(Δ)
    K = count(x -> x > 0.0, Δ)
    
    lM = loggamma(K+0.5) + loggamma(n-K+0.5) - loggamma(n+1) - (2*loggamma(0.5))
    M = exp(lM)
    
    # Continue while M > 0.05
    return M > 0.05
end

struct PermutationStop <: StoppingCondition end
function check(c::PermutationStop, ζ::PerformanceStats)
    m = 5
    n_h = length(ζ.φ_hist)
    n_h < (2+2m) && return true
    Δ = [ζ.φ_hist[i-1] - ζ.φ_hist[i] for i in 2:n_h]
    X = Δ[end-2m+1:end]
    older = X[1:m]
    recent = X[m+1:end]
    
    T_s(A, B) = begin
        μA, μB = mean(A), mean(B)
        sA2, sB2 = length(A) > 1 ? var(A) : 0.0, length(B) > 1 ? var(B) : 0.0
        denom = sqrt(sA2/m + sB2/m)
        (denom < 1e-18) ? 0.0 : (μB - μA) / denom
    end
    
    obs_T = T_s(older, recent)
    
    combos = Vector{Vector{Int}}()
    function g_c(start, count, curr)
        if count == m
            push!(combos, copy(curr))
            return
        end
        for i in start:(2m)
            push!(curr, i)
            g_c(i + 1, count + 1, curr)
            pop!(curr)
        end
    end
    g_c(1, 0, Int[])
    
    c_ge = 0
    i_f = collect(1:2m)
    for idx_A in combos
        A_p = X[idx_A]
        idx_B = setdiff(i_f, idx_A)
        B_p = X[idx_B]
        if T_s(A_p, B_p) >= obs_T
            c_ge += 1
        end
    end
    
    p_perm = (c_ge + 1) / (length(combos) + 1)
    # Continue while p_perm <= 0.975
    return p_perm <= 0.975
end

# --- Strategies ---

abstract type CrossoverStrategy end
struct BasicCrossover <: CrossoverStrategy; ρ::Float64; end
struct CrossoverSA <: CrossoverStrategy; end

abstract type SelectionMutationStrategy end
struct Rand1Strategy <: SelectionMutationStrategy; ω::Float64; end
struct Best2Strategy <: SelectionMutationStrategy; ν::Float64; ω::Float64; end
mutable struct SDEStrategy <: SelectionMutationStrategy
    ωs::Vector{Float64}
    SDEStrategy(d::Int) = new([rand(Normal(0.5, 0.15)) for _ in 1:d])
end

# Self-Adaptive Strategies (AGS and FISA)

function AGS!(Π::Vector{Vector{Float64}}, Φ::Vector{Float64}, ρ_gen::AbstractRNG, Σ::Dict{Symbol, Any})
    n, d = length(Π), length(Π[1])
    ε = 1e-12 * (1 + n + d)
    η = 1.0 / (1.0 + log(1.0 + n*d))
    
    if haskey(Σ, :_pending_candidates)
        pending = Σ[:_pending_candidates]
        WF, WCR = Σ[:AGS_WF], Σ[:AGS_WCR]
        for (i, old_φ, _) in pending
            if Φ[i] < old_φ
                WF[i] *= exp(η)
                WCR[i] *= exp(η)
            else
                WF[i] *= exp(-η)
                WCR[i] *= exp(-η)
            end
        end
        delete!(Σ, :_pending_candidates)
        return Vector{Vector{Float64}}(), Σ
    end
    
    if !haskey(Σ, :AGS_WF)
        F0 = 0.5 * (1.0 + 1.0/sqrt(d))
        Σ[:AGS_WF] = fill(1.0, n)
        Σ[:AGS_WCR] = fill(1.0, n)
        Σ[:AGS_F] = fill(F0, n)
        Σ[:AGS_CR] = fill(0.5, n)
        Σ[:AGS_iter] = 0
    end
    
    Σ[:AGS_iter] += 1
    WF, WCR = Σ[:AGS_WF], Σ[:AGS_WCR]
    F, CR = Σ[:AGS_F], Σ[:AGS_CR]
    
    σ_f(x) = 1.0 / (1.0 + exp(-x))
    for i in 1:n
        scale = 1.0 / (1.0 + log(1.0 + i))
        F[i] = clamp(2.0 * σ_f(log(WF[i]) * scale), ε, 1.999999)
        CR[i] = clamp(σ_f(log(WCR[i]) * scale), ε, 1.0 - ε)
    end
    
    D = mean(var(hcat(Π...), dims=2))
    if D < 1e-8 * (1 + n + d)
        F .*= 0.5
    end
    
    Ξ = [zeros(d) for _ in 1:n]
    pending = Any[]
    for i in 1:n
        candidates = deleteat!(collect(1:n), i)
        r_inds = sample(ρ_gen, candidates, 3, replace=false)
        a, b, c = Π[r_inds[1]], Π[r_inds[2]], Π[r_inds[3]]
        
        trial = copy(Π[i])
        j_rand = rand(ρ_gen, 1:d)
        for j in 1:d
            if rand(ρ_gen) < CR[i] || j == j_rand
                trial[j] = a[j] + F[i] * (b[j] - c[j])
            end
        end
        Ξ[i] = trial
        push!(pending, (i, Φ[i], trial))
    end
    Σ[:_pending_candidates] = pending
    return Ξ, Σ
end

function FISA!(Π::Vector{Vector{Float64}}, Φ::Vector{Float64}, ρ_gen::AbstractRNG, Σ::Dict{Symbol, Any})
    n, d = length(Π), length(Π[1])
    ε = 1e-12 * (1 + n + d)
    
    if haskey(Σ, :_pending_candidates)
        pending = Σ[:_pending_candidates]
        σ2 = Σ[:FISA_σ2]
        c_p = Σ[:FISA_count]
        G_r = Σ[:FISA_G]
        β = 1.0 / (1.0 + log(1.0 + n*d))
        
        accepted_steps = 0
        for (i, old_φ, trial) in pending
            if Φ[i] < old_φ
                s = trial .- Π[i]
                c_p[] += 1
                α = 1.0 / (1.0 + sqrt(1.0 + c_p[]))
                σ2 .= (1.0 - α) .* σ2 .+ α .* (s.^2 .+ ε)
                accepted_steps += 1
            end
        end
        
        σ = sqrt.(σ2)
        clamp!(σ, ε, 0.5 * (1.0 + 1.0/sqrt(d)))
        σ2 .= σ.^2
        
        if accepted_steps > 0
            G_r[] = exp((1.0 - β)*log(G_r[]) + β*mean(log.(σ .+ ε)))
            if mean(σ) < 0.25 * G_r[]
                σ .*= 1.2
                σ2 .= σ.^2
                G_r[] *= 1.2
            end
        end
        
        delete!(Σ, :_pending_candidates)
        return Vector{Vector{Float64}}(), Σ
    end
    
    if !haskey(Σ, :FISA_σ2)
        initial_σ = 0.2 * (1.0 + 1.0/sqrt(d))
        Σ[:FISA_σ2] = fill(initial_σ^2, d)
        Σ[:FISA_count] = Threads.Atomic{Int}(0)
        Σ[:FISA_G] = Ref(initial_σ)
    end
    
    σ = sqrt.(Σ[:FISA_σ2])
    Ξ = [zeros(d) for _ in 1:n]
    pending = Any[]
    
    for i in 1:n
        CR_i = clamp(0.5 * (1.0 + tanh((i - n/2.0)/(1.0 + sqrt(d)))), ε, 1.0 - ε)
        trial = Π[i] .+ (randn(ρ_gen, d) .* σ)
        
        final_trial = copy(Π[i])
        j_rand = rand(ρ_gen, 1:d)
        for j in 1:d
            if rand(ρ_gen) < CR_i || j == j_rand
                final_trial[j] = trial[j]
            end
        end
        
        if haskey(Σ, :bounds)
            lb_loc, ub_loc = Σ[:bounds]
            for j in 1:d
                if !(lb_loc[j] <= final_trial[j] <= ub_loc[j])
                    final_trial[j] = rand(ρ_gen) * (ub_loc[j] - lb_loc[j]) + lb_loc[j]
                end
            end
        end
        
        Ξ[i] = final_trial
        push!(pending, (i, Φ[i], final_trial))
    end
    Σ[:_pending_candidates] = pending
    return Ξ, Σ
end

# --- Core Logic ---

function run_differential_evolution(
    obj_func,
    constraints,
    ub::Vector{Float64},
    lb::Vector{Float64},
    pop_size::Int,
    stop_cond::StoppingCondition,
    c_strat::CrossoverStrategy,
    m_strat::Union{SelectionMutationStrategy, Function}
)
    ζ = PerformanceStats()
    d = length(ub)
    Σ = Dict{Symbol, Any}(:bounds => (lb, ub))
    ρ = Random.default_rng()
    
    new_χ() = [rand(ρ) * (ub[i] - lb[i]) + lb[i] for i in 1:d]
    
    function evaluate(x)
        Threads.atomic_add!(ζ.ε, 1)
        res = obj_func(x)
        penalty = isempty(constraints) ? 0.0 : sum(c(x) for c in constraints)
        return res + penalty
    end

    function generate_mutant(i, Π, Φ, βest)
        x = Π[i]
        u_mask = if c_strat isa BasicCrossover
            [rand(ρ) < c_strat.ρ for _ in 1:d]
        else
            [rand(ρ) < rand(ρ, Normal(0.5, 0.15)) for _ in 1:d]
        end
        u_mask[rand(ρ, 1:d)] = true
        
        candidates = deleteat!(collect(1:pop_size), i)
        v = copy(x)
        
        if m_strat isa Rand1Strategy
            s_inds = sample(ρ, candidates, 3, replace=false)
            a, b, c = Π[s_inds[1]], Π[s_inds[2]], Π[s_inds[3]]
            for k in 1:d
                if u_mask[k]
                    v[k] = a[k] + m_strat.ω * (b[k] - c[k])
                end
            end
        elseif m_strat isa Best2Strategy
            s_inds = sample(ρ, candidates, 4, replace=false)
            a, b, c, d_p = Π[s_inds[1]], Π[s_inds[2]], Π[s_inds[3]], Π[s_inds[4]]
            for k in 1:d
                if u_mask[k]
                    v[k] = βest[k] + m_strat.ν * (a[k] - b[k]) + m_strat.ω * (c[k] - d_p[k])
                end
            end
        elseif m_strat isa SDEStrategy
            s_inds = sample(ρ, candidates, 3, replace=false)
            a, b, c = Π[s_inds[1]], Π[s_inds[2]], Π[s_inds[3]]
            for k in 1:d
                if u_mask[k]
                    v[k] = a[k] + m_strat.ωs[k] * (b[k] - c[k])
                end
            end
        end
        
        for k in 1:d
            if !(lb[k] <= v[k] <= ub[k])
                v[k] = rand(ρ) * (ub[k] - lb[k]) + lb[k]
            end
        end
        
        f = evaluate(v)
        return (f < Φ[i]) ? (v, f) : (x, Φ[i])
    end

    Π = [new_χ() for _ in 1:pop_size]
    Φ = [evaluate(x) for x in Π]
    update_stats!(ζ, Π, Φ)
    
    while check(stop_cond, ζ)
        if m_strat isa Function
            Ξ, Σ = m_strat(Π, Φ, ρ, Σ)
            if isempty(Ξ)
                Ξ, Σ = m_strat(Π, Φ, ρ, Σ)
            end
            
            tasks = map(1:pop_size) do i
                Threads.@spawn begin
                    trial = Ξ[i]
                    f = evaluate(trial)
                    if f < Φ[i]
                        return (trial, f)
                    else
                        return (Π[i], Φ[i])
                    end
                end
            end
            results = fetch.(tasks)
            for i in 1:pop_size
                Π[i], Φ[i] = results[i]
            end
            _, Σ = m_strat(Π, Φ, ρ, Σ)
        else
            βest = ζ.β_hist[end]
            tasks = map(i -> Threads.@spawn(generate_mutant(i, Π, Φ, βest)), 1:pop_size)
            results = fetch.(tasks)
            for i in 1:pop_size
                Π[i], Φ[i] = results[i]
            end
            
            if m_strat isa SDEStrategy
                new_ωs = copy(m_strat.ωs)
                for k in 1:d
                    o_inds = sample(ρ, 1:d, 3, replace=false)
                    o1, o2, o3 = m_strat.ωs[o_inds[1]], m_strat.ωs[o_inds[2]], m_strat.ωs[o_inds[3]]
                    new_ωs[k] = o1 + rand(ρ, Normal(0, 0.5)) * (o2 - o3)
                end
                m_strat.ωs = new_ωs
            end
        end
        
        Threads.atomic_add!(ζ.ι, 1)
        update_stats!(ζ, Π, Φ)
    end
    return ζ
end

# --- Convenience Functions ---

function de_rand_1_no_improvement(obj, cons, ub, lb, n, ni, nt, p, ω)
    return run_differential_evolution(obj, cons, ub, lb, n, NoImprovementStop(ni, nt), BasicCrossover(p), Rand1Strategy(ω))
end

# --- Benchmarks ---

module Benchmarks
    using Statistics, Random
    const N, M = 8, 12
    sphere(x) = sum(x.^2)
    sphere_bounds = (fill(Float64(2^M), N), fill(Float64(-2^M), N))
    rosenbrock(x) = sum(100*(x[1:end-1].^2 .- x[2:end]).^2 .+ (1 .- x[1:end-1]).^2)
    rosenbrock_bounds = (fill(Float64(2^M), N), fill(Float64(-2^M), N))
    step(x) = sum(floor.(x))
    step_bounds = (fill(5.12, N), fill(-5.12, N))
    griewank(x) = 1 + sum(x.^2)/4000 - prod(cos.(x ./ sqrt.(1:length(x))))
    griewank_bounds = (fill(Float64(2^M), N), fill(Float64(-2^M), N))
    styblinski_tang(x) = sum(x.^4 .- 16*x.^2 .+ 5*x) / 2
    styblinski_tang_bounds = (fill(5.0, N), fill(-5.0, N))
    Random.seed!(42)
    const A_SHEKEL = [rand() * 2M - M for _ in 1:N, _ in 1:M]
    const C_SHEKEL = [rand() * 2M - M for _ in 1:M]
    shekel(x) = -sum(1.0 ./ (sum((x .- A_SHEKEL[:, i]).^2) + C_SHEKEL[i]) for i in 1:10)
    shekel_bounds = (fill(5.0, N), fill(-5.0, N))
    rastrigin(x) = sum(x.^2 .- 10 .* cos.(2π .* x) .+ 10)
    rastrigin_bounds = (fill(5.12, N), fill(-5.12, N))
    ackley(x) = -20 * exp(-0.2 * sqrt(mean(x.^2))) - exp(mean(cos.(2π .* x))) + exp(1) + 20
    ackley_bounds = (fill(5.0, N), fill(-5.0, N))
    rotated_ellipsoid(x) = sum(((1:length(x)) .* x).^2)
    rotated_ellipsoid_bounds = (fill(Float64(2^M), N), fill(Float64(-2^M), N))
    struct KeaneBump
        f::Function; c::Vector{Function}; b::Tuple{Vector{Float64}, Vector{Float64}}
        function KeaneBump()
            fn(x) = -abs((sum(cos.(x).^4) - 2*prod(cos.(x).^2)) / sqrt(sum((1:length(x)) .* x.^2)))
            c1(x) = max(0.0, 0.75 - prod(x)); c2(x) = max(0.0, sum(x) - 7.5*length(x))
            new(fn, [c1, c2], (fill(10.0, N), fill(0.0, N)))
        end
    end
end

function run_comparison(name, obj, ζ)
    println("\n--- $name ---")
    println("DE: ", round(ζ.φ_hist[end], digits=6), " (ι: $(ζ.ι[]))")
    dim = length(ζ.β_hist[1])
    x0 = randn(dim)
    res_nm = optimize(obj, x0, NelderMead())
    println("NM: ", round(Optim.minimum(res_nm), digits=6))
end

function main()
    println("Running with $(Threads.nthreads()) threads.")
    kb = Benchmarks.KeaneBump()
    tests = [
        ("Sphere", Benchmarks.sphere, [], Benchmarks.sphere_bounds...),
        ("Rosenbrock", Benchmarks.rosenbrock, [], Benchmarks.rosenbrock_bounds...),
        ("Step", Benchmarks.step, [], Benchmarks.step_bounds...),
        ("Griewank", Benchmarks.griewank, [], Benchmarks.griewank_bounds...),
        ("Styblinski", Benchmarks.styblinski_tang, [], Benchmarks.styblinski_tang_bounds...),
        ("Shekel", Benchmarks.shekel, [], Benchmarks.shekel_bounds...),
        ("Rastrigin", Benchmarks.rastrigin, [], Benchmarks.rastrigin_bounds...),
        ("Ackley", Benchmarks.ackley, [], Benchmarks.ackley_bounds...),
        ("Rotated", Benchmarks.rotated_ellipsoid, [], Benchmarks.rotated_ellipsoid_bounds...),
        ("Keane", kb.f, kb.c, kb.b...)
    ]
    tasks = map(tests) do (n, o, c, ub, lb)
        Threads.@spawn (n, o, run_differential_evolution(o, c, ub, lb, 20*Benchmarks.N, PocockSignStop(), BasicCrossover(0.1), Rand1Strategy(0.8)))
    end
    for t in tasks
        n, o, ζ = fetch(t)
        run_comparison(n, o, ζ)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
