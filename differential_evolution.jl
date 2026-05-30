using Random
using Statistics
using Base.Threads
using Distributions: Normal
using Optim
using StatsBase: sample

# --- Performance Stats ---

mutable struct PerformanceStats
    iter_count::Threads.Atomic{Int}
    eval_count::Threads.Atomic{Int}
    best_solution_hist::Vector{Vector{Float64}}
    best_score_hist::Vector{Float64}
    population_diversity_hist::Vector{Float64}
    lock::ReentrantLock

    PerformanceStats() = new(Threads.Atomic{Int}(0), Threads.Atomic{Int}(0), Vector{Vector{Float64}}(), Vector{Float64}(), Vector{Float64}(), ReentrantLock())
end

function compute_diversity(population::Vector{Vector{Float64}})
    n = length(population)
    n <= 1 && return 0.0
    total_dist = 0.0
    for i in 1:n
        for j in (i+1):n
            d = 0.0
            @inbounds for k in eachindex(population[i])
                d += (population[i][k] - population[j][k])^2
            end
            total_dist += sqrt(d)
        end
    end
    return total_dist / (n * (n - 1) / 2)
end

function update_stats!(stats::PerformanceStats, population::Vector{Vector{Float64}}, scores::Vector{Float64})
    lock(stats.lock) do
        best_idx = argmin(scores)
        push!(stats.best_solution_hist, copy(population[best_idx]))
        push!(stats.best_score_hist, scores[best_idx])
        push!(stats.population_diversity_hist, compute_diversity(population))
    end
end

# --- Stopping Conditions ---

abstract type StoppingCondition end

struct MaxIterationsStop <: StoppingCondition
    max_iterations::Int
end
check(c::MaxIterationsStop, stats::PerformanceStats) = stats.iter_count[] < c.max_iterations

struct FitnessThresholdStop <: StoppingCondition
    threshold::Float64
end
check(c::FitnessThresholdStop, stats::PerformanceStats) = isempty(stats.best_score_hist) || c.threshold < stats.best_score_hist[end]

struct NoImprovementStop <: StoppingCondition
    max_no_improve::Int
    threshold::Float64
end
function check(c::NoImprovementStop, stats::PerformanceStats)
    if stats.iter_count[] > c.max_no_improve
        window = stats.best_score_hist[end-c.max_no_improve:end]
        improves = [abs(window[i+1] - window[i]) for i in 1:(length(window)-1)]
        return !all(imp < c.threshold for imp in improves)
    end
    return true
end

# --- Strategies ---

abstract type CrossoverStrategy end
struct BasicCrossover <: CrossoverStrategy; p::Float64; end
struct CrossoverSA <: CrossoverStrategy; end

abstract type SelectionMutationStrategy end
struct Rand1Strategy <: SelectionMutationStrategy; omega::Float64; end
struct Best2Strategy <: SelectionMutationStrategy; nu::Float64; omega::Float64; end
mutable struct SDEStrategy <: SelectionMutationStrategy
    omegas::Vector{Float64}
    SDEStrategy(dim::Int) = new([rand(Normal(0.5, 0.15)) for _ in 1:dim])
end

# --- Core Logic ---

function run_differential_evolution(
    objective,
    constraints,
    upper_bounds::Vector{Float64},
    lower_bounds::Vector{Float64},
    population_size::Int,
    stopping_condition::StoppingCondition,
    crossover_strat::CrossoverStrategy,
    sel_mut_strat::SelectionMutationStrategy
)
    stats = PerformanceStats()
    dim = length(upper_bounds)
    
    new_chromosome() = [rand() * (upper_bounds[i] - lower_bounds[i]) + lower_bounds[i] for i in 1:dim]
    
    function enforce_bounds!(x)
        for i in 1:dim
            if !(lower_bounds[i] <= x[i] <= upper_bounds[i])
                x[i] = rand() * (upper_bounds[i] - lower_bounds[i]) + lower_bounds[i]
            end
        end
        return x
    end

    function evaluate(x)
        Threads.atomic_add!(stats.eval_count, 1)
        obj = objective(x)
        penalty = isempty(constraints) ? 0.0 : sum(c(x) for c in constraints)
        return obj + penalty
    end

    # Proper inner function for mutant generation
    # Uses StatsBase.sample and mapping to avoid allocations and ensure sound unique sampling
    function generate_mutant(i, current_pop, current_scores, current_best)
        x = current_pop[i]
        
        # Crossover
        d_idx = rand(1:dim)
        u_mask = if crossover_strat isa BasicCrossover
            [rand() < crossover_strat.p for _ in 1:dim]
        else
            [rand() < rand(Normal(0.5, 0.15)) for _ in 1:dim]
        end
        u_mask[d_idx] = true
        
        v = copy(x)
        if sel_mut_strat isa Rand1Strategy
            # Sample 3 unique indices from 1:NP excluding i
            raw_inds = sample(1:(population_size-1), 3, replace=false)
            s_inds = [idx >= i ? idx + 1 : idx for idx in raw_inds]
            
            a, b, c = [current_pop[idx] for idx in s_inds]
            for k in 1:dim
                if u_mask[k]
                    v[k] = a[k] + sel_mut_strat.omega * (b[k] - c[k])
                end
            end
        elseif sel_mut_strat isa Best2Strategy
            # Sample 4 unique indices from 1:NP excluding i
            raw_inds = sample(1:(population_size-1), 4, replace=false)
            s_inds = [idx >= i ? idx + 1 : idx for idx in raw_inds]
            
            a, b, c, d = [current_pop[idx] for idx in s_inds]
            for k in 1:dim
                if u_mask[k]
                    v[k] = current_best[k] + sel_mut_strat.nu * (a[k] - b[k]) + sel_mut_strat.omega * (c[k] - d[k])
                end
            end
        elseif sel_mut_strat isa SDEStrategy
            # Sample 3 unique indices from 1:NP excluding i
            raw_inds = sample(1:(population_size-1), 3, replace=false)
            s_inds = [idx >= i ? idx + 1 : idx for idx in raw_inds]
            
            a, b, c = [current_pop[idx] for idx in s_inds]
            for k in 1:dim
                if u_mask[k]
                    v[k] = a[k] + sel_mut_strat.omegas[k] * (b[k] - c[k])
                end
            end
        end
        
        enforce_bounds!(v)
        f = evaluate(v)
        
        return (f < current_scores[i]) ? (v, f) : (x, current_scores[i])
    end

    # Initialization
    population = [new_chromosome() for _ in 1:population_size]
    scores = [evaluate(x) for x in population]
    update_stats!(stats, population, scores)
    
    while check(stopping_condition, stats)
        current_best = stats.best_solution_hist[end]
        
        # Task-parallelism firing all cores
        tasks = map(i -> Threads.@spawn(generate_mutant(i, population, scores, current_best)), 1:population_size)
        results = fetch.(tasks)
        
        for i in 1:population_size
            population[i], scores[i] = results[i]
        end
        
        Threads.atomic_add!(stats.iter_count, 1)
        
        # SDE self-adaptation
        if sel_mut_strat isa SDEStrategy
            new_omegas = copy(sel_mut_strat.omegas)
            k_sample = min(dim, 3)
            for k in 1:dim
                # Sound unique sampling for strategy parameter evolution
                # If dim < 3, we take all available or use replacement
                o_indices = if dim >= 3
                    sample(1:dim, 3, replace=false)
                else
                    sample(1:dim, 3, replace=true)
                end
                o1, o2, o3 = sel_mut_strat.omegas[o_indices[1]], sel_mut_strat.omegas[o_indices[2]], sel_mut_strat.omegas[o_indices[3]]
                new_omegas[k] = o1 + rand(Normal(0, 0.5)) * (o2 - o3)
            end
            sel_mut_strat.omegas = new_omegas
        end
        
        update_stats!(stats, population, scores)
    end
    
    return stats
end

# --- Convenience Functions ---

function de_rand_1_no_improvement(objective, constraints, ub, lb, pop_size, no_imp_iters, no_imp_thresh, p, omega)
    return run_differential_evolution(objective, constraints, ub, lb, pop_size, 
        NoImprovementStop(no_imp_iters, no_imp_thresh), BasicCrossover(p), Rand1Strategy(omega))
end

function de_best_2_no_improvement(objective, constraints, ub, lb, pop_size, no_imp_iters, no_imp_thresh, p, nu, omega)
    return run_differential_evolution(objective, constraints, ub, lb, pop_size, 
        NoImprovementStop(no_imp_iters, no_imp_thresh), BasicCrossover(p), Best2Strategy(nu, omega))
end

function sde_rand_1_no_improvement(objective, constraints, ub, lb, pop_size, no_imp_iters, no_imp_thresh)
    return run_differential_evolution(objective, constraints, ub, lb, pop_size, 
        NoImprovementStop(no_imp_iters, no_imp_thresh), CrossoverSA(), SDEStrategy(length(ub)))
end

# --- Full Benchmark Suite ---

module Benchmarks
    using Statistics
    using Random
    const N = 8
    const M = 12

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
        f::Function
        c::Vector{Function}
        bounds::Tuple{Vector{Float64}, Vector{Float64}}
        function KeaneBump()
            fun(x) = -abs((sum(cos.(x).^4) - 2*prod(cos.(x).^2)) / sqrt(sum((1:length(x)) .* x.^2)))
            c1(x) = max(0.0, 0.75 - prod(x))
            c2(x) = max(0.0, sum(x) - 7.5*length(x))
            new(fun, [c1, c2], (fill(10.0, N), fill(0.0, N)))
        end
    end
end

# --- Comparison & Main ---

function run_comparison(test_name, objective, de_stats)
    println("\n--- Results for $test_name ---")
    println("DE Best Score: ", round(de_stats.best_score_hist[end], digits=6), " (Evals: $(de_stats.eval_count[]))")
    
    dim = length(de_stats.best_solution_hist[1])
    x0 = randn(dim)
    
    res_nm = optimize(objective, x0, NelderMead())
    println("Nelder-Mead Score: ", round(Optim.minimum(res_nm), digits=6))
    
    res_sa = optimize(objective, x0, SimulatedAnnealing())
    println("Simulated Annealing Score: ", round(Optim.minimum(res_sa), digits=6))
end

function main()
    n_threads = Threads.nthreads()
    println("Running with $n_threads threads. (Use `julia -t auto` for best performance)")
    
    kb = Benchmarks.KeaneBump()
    
    tests = [
        ("Sphere", Benchmarks.sphere, [], Benchmarks.sphere_bounds...),
        ("Rosenbrock", Benchmarks.rosenbrock, [], Benchmarks.rosenbrock_bounds...),
        ("Step", Benchmarks.step, [], Benchmarks.step_bounds...),
        ("Griewank", Benchmarks.griewank, [], Benchmarks.griewank_bounds...),
        ("Styblinski-Tang", Benchmarks.styblinski_tang, [], Benchmarks.styblinski_tang_bounds...),
        ("Shekel", Benchmarks.shekel, [], Benchmarks.shekel_bounds...),
        ("Rastrigin", Benchmarks.rastrigin, [], Benchmarks.rastrigin_bounds...),
        ("Ackley", Benchmarks.ackley, [], Benchmarks.ackley_bounds...),
        ("Rotated Ellipsoid", Benchmarks.rotated_ellipsoid, [], Benchmarks.rotated_ellipsoid_bounds...),
        ("Keane Bump", kb.f, kb.c, kb.bounds...)
    ]

    println("Starting parallel benchmark suite...")
    bench_tasks = map(tests) do (name, obj, cons, ub, lb)
        Threads.@spawn begin
            stats = run_differential_evolution(
                obj, cons, ub, lb, 20*Benchmarks.N,
                MaxIterationsStop(2048), BasicCrossover(0.1), Rand1Strategy(0.8)
            )
            return (name, obj, stats)
        end
    end

    for t in bench_tasks
        name, obj, stats = fetch(t)
        run_comparison(name, obj, stats)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
