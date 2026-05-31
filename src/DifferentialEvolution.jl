module DifferentialEvolution

using Random
using Base.Threads
using Statistics

# --- Modular Core ---

include("Models.jl")
using .Models
export ζ_Stats, StopCond, SelectionMutation, CrossoverStrat, update_ζ!, compute_δ

include("Crossover.jl")
using .Crossover
export BinomialCrossover, SACrossover

include("StoppingConditions.jl")
using .StoppingConditions
export MaxIterStop, FitnessStop, NoImproveStop, PocockSignStop, SPRTStop, PermutationStop, MaxRuntimeStop, should_continue

include("Strategies.jl")
using .Strategies
export Rand1, Best2, SDE, AGS, FISA, propose_trial, adapt!

include("Reporting.jl")
using .Reporting
export plot_convergence, plot_diversity, save_comprehensive_report

include("Comparison.jl")
using .Comparison
export run_reference_optim, ComparisonResult

# Standard API Aliases
const max_iter = MaxIterStop
const fitness_threshold = FitnessStop
const no_improvement = NoImproveStop
const pocock_sign = PocockSignStop
const sprt = SPRTStop
const permutation = PermutationStop
const max_runtime = MaxRuntimeStop

# --- Internal Engine ---

function run_differential_evolution(obj, cons, ub, lb, n, stop, crossover, m_strat)
    ζ = Models.ζ_Stats(); Σ = Dict{Symbol, Any}(:bounds => (lb, ub)); ρ = Random.default_rng()
    
    function evaluate(x)
        Threads.atomic_add!(ζ.ε, 1)
        c_vals = [c(x) for c in cons]
        penalty = isempty(c_vals) ? 1.0 : prod([abs(cv) > 1e-15 ? cv : 1.0 for cv in c_vals])
        return obj(x) * penalty
    end

    function evolve_individual(i, current_Π, current_Φ, current_β)
        Ξ_i = Strategies.propose_trial(m_strat, crossover, i, current_Π, current_Φ, ρ, Σ, current_β)
        f_Ξ = evaluate(Ξ_i)
        return (f_Ξ < current_Φ[i]) ? (Ξ_i, f_Ξ) : (current_Π[i], current_Φ[i])
    end

    Π = [[rand(ρ)*(ub[i]-lb[i])+lb[i] for i in 1:length(ub)] for _ in 1:n]
    Φ = evaluate.(Π)
    Models.update_ζ!(ζ, Π, Φ, 0)
    
    while StoppingConditions.should_continue(stop, ζ)
        βest = ζ.β_hist[end]
        
        # Proper Parallelism
        tasks = map(i -> Threads.@spawn(evolve_individual(i, Π, Φ, βest)), 1:n)
        results = fetch.(tasks)
        
        Φ_old = copy(Φ)
        for i in 1:n
            Π[i], Φ[i] = results[i]
        end
        
        Strategies.adapt!(m_strat, Π, Φ, [r[1] for r in results], Φ_old, ρ, Σ)
        Threads.atomic_add!(ζ.ι, 1)
        Models.update_ζ!(ζ, Π, Φ, 0)
    end
    return ζ
end

# --- Public API Functions ---

# Rand1
export de_rand_1_max_iter, de_rand_1_fitness_threshold, de_rand_1_no_improvement, de_rand_1_pocock_sign, de_rand_1_sprt, de_rand_1_permutation, de_rand_1_max_runtime
de_rand_1_max_iter(o, c, ub, lb, n, m, p, ω) = run_differential_evolution(o, c, ub, lb, n, max_iter(m), BinomialCrossover(p), Rand1(ω))
de_rand_1_fitness_threshold(o, c, ub, lb, n, θ, p, ω) = run_differential_evolution(o, c, ub, lb, n, fitness_threshold(θ), BinomialCrossover(p), Rand1(ω))
de_rand_1_no_improvement(o, c, ub, lb, n, ni, nt, p, ω) = run_differential_evolution(o, c, ub, lb, n, no_improvement(ni, nt), BinomialCrossover(p), Rand1(ω))
de_rand_1_pocock_sign(o, c, ub, lb, n, p, ω) = run_differential_evolution(o, c, ub, lb, n, pocock_sign(), BinomialCrossover(p), Rand1(ω))
de_rand_1_sprt(o, c, ub, lb, n, p, ω) = run_differential_evolution(o, c, ub, lb, n, sprt(), BinomialCrossover(p), Rand1(ω))
de_rand_1_permutation(o, c, ub, lb, n, p, ω) = run_differential_evolution(o, c, ub, lb, n, permutation(), BinomialCrossover(p), Rand1(ω))
de_rand_1_max_runtime(o, c, ub, lb, n, t, p, ω) = run_differential_evolution(o, c, ub, lb, n, max_runtime(t), BinomialCrossover(p), Rand1(ω))

# Best2
export de_best_2_max_iter, de_best_2_fitness_threshold, de_best_2_no_improvement, de_best_2_pocock_sign, de_best_2_sprt, de_best_2_permutation, de_best_2_max_runtime
de_best_2_max_iter(o, c, ub, lb, n, m, p, ν, ω) = run_differential_evolution(o, c, ub, lb, n, max_iter(m), BinomialCrossover(p), Best2(ν, ω))
de_best_2_fitness_threshold(o, c, ub, lb, n, θ, p, ν, ω) = run_differential_evolution(o, c, ub, lb, n, fitness_threshold(θ), BinomialCrossover(p), Best2(ν, ω))
de_best_2_no_improvement(o, c, ub, lb, n, ni, nt, p, ν, ω) = run_differential_evolution(o, c, ub, lb, n, no_improvement(ni, nt), BinomialCrossover(p), Best2(ν, ω))
de_best_2_pocock_sign(o, c, ub, lb, n, p, ν, ω) = run_differential_evolution(o, c, ub, lb, n, pocock_sign(), BinomialCrossover(p), Best2(ν, ω))
de_best_2_sprt(o, c, ub, lb, n, p, ν, ω) = run_differential_evolution(o, c, ub, lb, n, sprt(), BinomialCrossover(p), Best2(ν, ω))
de_best_2_permutation(o, c, ub, lb, n, p, ν, ω) = run_differential_evolution(o, c, ub, lb, n, permutation(), BinomialCrossover(p), Best2(ν, ω))
de_best_2_max_runtime(o, c, ub, lb, n, t, p, ν, ω) = run_differential_evolution(o, c, ub, lb, n, max_runtime(t), BinomialCrossover(p), Best2(ν, ω))

# SDE
export sde_rand_1_max_iter, sde_rand_1_fitness_threshold, sde_rand_1_no_improvement, sde_rand_1_pocock_sign, sde_rand_1_sprt, sde_rand_1_permutation, sde_rand_1_max_runtime
sde_rand_1_max_iter(o, c, ub, lb, n, m) = run_differential_evolution(o, c, ub, lb, n, max_iter(m), SACrossover(), SDE(length(ub)))
sde_rand_1_fitness_threshold(o, c, ub, lb, n, θ) = run_differential_evolution(o, c, ub, lb, n, fitness_threshold(θ), SACrossover(), SDE(length(ub)))
sde_rand_1_no_improvement(o, c, ub, lb, n, ni, nt) = run_differential_evolution(o, c, ub, lb, n, no_improvement(ni, nt), SACrossover(), SDE(length(ub)))
sde_rand_1_pocock_sign(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, pocock_sign(), SACrossover(), SDE(length(ub)))
sde_rand_1_sprt(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, sprt(), SACrossover(), SDE(length(ub)))
sde_rand_1_permutation(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, permutation(), SACrossover(), SDE(length(ub)))
sde_rand_1_max_runtime(o, c, ub, lb, n, t) = run_differential_evolution(o, c, ub, lb, n, max_runtime(t), SACrossover(), SDE(length(ub)))

# AGS
export ags_rand_1_max_iter, ags_rand_1_fitness_threshold, ags_rand_1_no_improvement, ags_rand_1_pocock_sign, ags_rand_1_sprt, ags_rand_1_permutation, ags_rand_1_max_runtime
ags_rand_1_max_iter(o, c, ub, lb, n, m) = run_differential_evolution(o, c, ub, lb, n, max_iter(m), BinomialCrossover(0.1), AGS())
ags_rand_1_fitness_threshold(o, c, ub, lb, n, θ) = run_differential_evolution(o, c, ub, lb, n, fitness_threshold(θ), BinomialCrossover(0.1), AGS())
ags_rand_1_no_improvement(o, c, ub, lb, n, ni, nt) = run_differential_evolution(o, c, ub, lb, n, no_improvement(ni, nt), BinomialCrossover(0.1), AGS())
ags_rand_1_pocock_sign(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, pocock_sign(), BinomialCrossover(0.1), AGS())
ags_rand_1_sprt(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, sprt(), BinomialCrossover(0.1), AGS())
ags_rand_1_permutation(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, permutation(), BinomialCrossover(0.1), AGS())
ags_rand_1_max_runtime(o, c, ub, lb, n, t) = run_differential_evolution(o, c, ub, lb, n, max_runtime(t), BinomialCrossover(0.1), AGS())

# FISA
export fisa_rand_1_max_iter, fisa_rand_1_fitness_threshold, fisa_rand_1_no_improvement, fisa_rand_1_pocock_sign, fisa_rand_1_sprt, fisa_rand_1_permutation, fisa_rand_1_max_runtime
fisa_rand_1_max_iter(o, c, ub, lb, n, m) = run_differential_evolution(o, c, ub, lb, n, max_iter(m), SACrossover(), FISA())
fisa_rand_1_fitness_threshold(o, c, ub, lb, n, θ) = run_differential_evolution(o, c, ub, lb, n, fitness_threshold(θ), SACrossover(), FISA())
fisa_rand_1_no_improvement(o, c, ub, lb, n, ni, nt) = run_differential_evolution(o, c, ub, lb, n, no_improvement(ni, nt), SACrossover(), FISA())
fisa_rand_1_pocock_sign(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, pocock_sign(), SACrossover(), FISA())
fisa_rand_1_sprt(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, sprt(), SACrossover(), FISA())
fisa_rand_1_permutation(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, permutation(), SACrossover(), FISA())
fisa_rand_1_max_runtime(o, c, ub, lb, n, t) = run_differential_evolution(o, c, ub, lb, n, max_runtime(t), SACrossover(), FISA())

end
