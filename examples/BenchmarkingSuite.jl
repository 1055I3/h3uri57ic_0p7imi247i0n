# ==============================================================================
# PROFESSIONAL EXHAUSTIVE BENCHMARKING SUITE
# ==============================================================================

using Base.Threads
using Statistics
using Plots
using Printf

# Import the framework
include("../src/DifferentialEvolution.jl")
using .DifferentialEvolution
include("../src/Benchmarks.jl")
using .Benchmarks

function run_exhaustive_benchmarks()
    println("="^80)
    println("LAUNCHING EXHAUSTIVE BENCHMARKING SUITE")
    println("System: $(Threads.nthreads()) threads active")
    println("="^80)
    
    # 10 Standard Benchmarks (N=16)
    suite = [
        ("Sphere", Benchmarks.sphere, Benchmarks.sphere_b...),
        ("Rosenbrock", Benchmarks.rosenbrock, Benchmarks.rosenbrock_b...),
        ("Step", Benchmarks.step_func, Benchmarks.step_b...),
        ("Griewank", Benchmarks.griewank, Benchmarks.griewank_b...),
        ("Styblinski", Benchmarks.styblinski, Benchmarks.styblinski_b...),
        ("Shekel", Benchmarks.shekel, Benchmarks.shekel_b...),
        ("Rastrigin", Benchmarks.rastrigin, Benchmarks.rastrigin_b...),
        ("Ackley", Benchmarks.ackley, Benchmarks.ackley_b...),
        ("Rotated", Benchmarks.rotated, Benchmarks.rotated_b...),
        ("Keane", Benchmarks.Keane().f, Benchmarks.Keane().b...)
    ]
    
    # 5 Strategies mapped to the Public API
    # (obj, ub, lb, pop_size, iterations)
    strategies = [
        ("Rand1", (o, ub, lb, n, m) -> de_rand_1_max_ι(o, [], ub, lb, n, m, 0.5, 0.8)),
        ("Best2", (o, ub, lb, n, m) -> de_best_2_max_ι(o, [], ub, lb, n, m, 0.5, 0.5, 0.5)),
        ("SDE",   (o, ub, lb, n, m) -> sde_rand_1_max_ι(o, [], ub, lb, n, m)),
        ("AGS",   (o, ub, lb, n, m) -> ags_rand_1_max_ι(o, [], ub, lb, n, m)),
        ("FISA",  (o, ub, lb, n, m) -> fisa_rand_1_max_ι(o, [], ub, lb, n, m))
    ]
    
    n_b = length(suite); n_s = length(strategies)
    scores = zeros(n_b, n_s); evals = zeros(Int, n_b, n_s)
    ref_scores = zeros(n_b, 3)

    for (i, (b_name, obj, ub, lb)) in enumerate(suite)
        println("\nBenchmark [$i/$n_b]: $b_name")
        
        # Reference Comparison
        ub_c = clamp.(ub, -1000.0, 1000.0); lb_c = clamp.(lb, -1000.0, 1000.0)
        ref = run_reference_optim(obj, lb_c, ub_c)
        ref_scores[i, :] = [ref.nm_score, ref.sa_score, ref.ps_score]

        for (j, (s_name, api_call)) in enumerate(strategies)
            # Use a consistent budget of 150 iterations and pop_size = 4 * dim
            ζ = api_call(obj, ub, lb, 4 * dim, 150)
            
            scores[i, j] = ζ.φ_hist[end]
            evals[i, j] = ζ.ε[]
            @printf("  %-8s | Best: %10.4e | Evals: %d\n", s_name, scores[i,j], evals[i,j])
        end
    end

    # --- Summary Reporting ---
    println("\n" * "="^80); println("BENCHMARK SUMMARY REPORT"); println("="^80)
    @printf("%-15s", "Benchmark")
    for (s_name, _) in strategies; @printf(" | %-10s", s_name); end
    println("\n" * "-"^80)
    for (i, (b_name, _, _, _)) in enumerate(suite)
        @printf("%-15s", b_name)
        for j in 1:n_s; @printf(" | %10.2e", scores[i, j]); end
        println()
    end
    
    # --- Visualization ---
    println("\nGenerating visualization suite...")
    log_scores = log10.(abs.(scores) .+ 1e-15)
    
    h_plt = heatmap([s[1] for s in strategies], [b[1] for b in suite], log_scores, title="Performance Heatmap (log10|Score|)", size=(800, 600))
    savefig(h_plt, "Benchmark_Performance_Heatmap.png")
    
    r_plt = bar([s[1] for s in strategies], scores[2, :], title="Rosenbrock Strategy Comparison", color=[:blue, :red, :green, :orange, :purple], legend=false)
    savefig(r_plt, "Benchmark_Rosenbrock_Comparison.png")
    
    c_plt = bar(["DE(Rand1)", "N-M", "S-A", "P-S"], [scores[1, 1], ref_scores[1, 1], ref_scores[1, 2], ref_scores[1, 3]], title="DE vs Reference (Sphere)", yscale=:log10, legend=false)
    savefig(c_plt, "Benchmark_Reference_Comparison.png")
    
    println("="^80); println("BENCHMARKING COMPLETE. Visuals saved."); println("="^80)
end

if abspath(PROGRAM_FILE) == @__FILE__; run_exhaustive_benchmarks(); end
