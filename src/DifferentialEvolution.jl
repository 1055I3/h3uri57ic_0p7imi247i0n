module DifferentialEvolution

using Random
using Base.Threads
using Statistics

include("Models.jl")
include("Crossover.jl")
include("StoppingConditions.jl")
include("Strategies.jl")
include("Benchmarks.jl")

using .Models
using .Crossover
using .StoppingConditions
using .Strategies

export ζ_Stats, StopCond, SelecciónMutación, CrossoverStrat, update_ζ!, compute_δ
export BinomialCrossover, SACrossover
export MaxIterStop, FitnessStop, NoImproveStop, PocockSignStop, SPRTStop, PermutationStop, check
export Rand1, Best2, SDE, AGS, FISA, propose, adapt!
export run_differential_evolution
export de_rand_1_max_iter, de_rand_1_fitness_threshold, de_rand_1_no_improvement
export de_best_2_max_iter, de_best_2_fitness_threshold, de_best_2_no_improvement
export sde_rand_1_max_iter, sde_rand_1_fitness_threshold, sde_rand_1_no_improvement

# --- Generic DE Engine ---
function run_differential_evolution(obj, cons, ub, lb, n, stop, crossover, m_strat)
    ζ = Models.ζ_Stats(); Σ = Dict{Symbol, Any}(:bounds => (lb, ub)); ρ = Random.default_rng()
    evaluate(x) = begin
        Threads.atomic_add!(ζ.ε, 1)
        c_vals = [c(x) for c in cons]
        penalty = isempty(c_vals) ? 1.0 : prod([abs(c)>1e-14 ? c : 1.0 for c in c_vals])
        return obj(x) * penalty
    end
    Π = [[rand(ρ)*(ub[i]-lb[i])+lb[i] for i in 1:length(ub)] for _ in 1:n]
    Φ = evaluate.(Π); Models.update_ζ!(ζ, Π, Φ, n)
    while StoppingConditions.check(stop, ζ)
        βest = ζ.β_hist[end]
        Ξ = [Strategies.propose(m_strat, crossover, i, Π, Φ, ρ, Σ, βest) for i in 1:n]
        tasks = map(i -> Threads.@spawn(evaluate(Ξ[i])), 1:n); Φ_Ξ = fetch.(tasks)
        Φ_old = copy(Φ)
        for i in 1:n; if Φ_Ξ[i] < Φ[i]; Π[i], Φ[i] = Ξ[i], Φ_Ξ[i]; end; end
        Strategies.adapt!(m_strat, Π, Φ, Ξ, Φ_old, ρ, Σ)
        Threads.atomic_add!(ζ.ι, 1); Models.update_ζ!(ζ, Π, Φ, n)
    end
    return ζ
end

# --- Legacy API Wrappers ---
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

end
