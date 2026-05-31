# ==============================================================================
# EXHAUSTIVE BENCHMARKING SUITE
# ==============================================================================

using Base.Threads
using Statistics

include("../src/DifferentialEvolution.jl")
using .DifferentialEvolution
include("../src/Benchmarks.jl")
using .Benchmarks

function run_exhaustive_benchmarks()
    println("Launching exhaustive benchmark suite with $(Threads.nthreads()) threads...")
    
    # 10 Benchmarks
    suite = [
        ("Sphere", Benchmarks.sphere, Benchmarks.sphere_b...),
        ("Rosenbrock", Benchmarks.rosenbrock, Benchmarks.rosenbrock_b...),
        ("Step", Benchmarks.step, Benchmarks.step_b...),
        ("Griewank", Benchmarks.griewank, Benchmarks.griewank_b...),
        ("Styblinski", Benchmarks.styblinski, Benchmarks.styblinski_b...),
        ("Shekel", Benchmarks.shekel, Benchmarks.shekel_b...),
        ("Rastrigin", Benchmarks.rastrigin, Benchmarks.rastrigin_b...),
        ("Ackley", Benchmarks.ackley, Benchmarks.ackley_b...),
        ("Rotated", Benchmarks.rotated, Benchmarks.rotated_b...),
        ("Keane", Benchmarks.Keane().f, Benchmarks.Keane().b...)
    ]
    
    # 5 Strategies
    strategies = [
        ("Rand1", d -> Rand1(0.8), BinomialCrossover(0.1)),
        ("Best2", d -> Best2(0.5, 0.5), BinomialCrossover(0.1)),
        ("SDE", d -> SDE(d), SACrossover()),
        ("AGS", d -> AGS(), BinomialCrossover(0.1)),
        ("FISA", d -> FISA(), SACrossover())
    ]
    
    for (b_name, obj, ub, lb) in suite
        println("\n" * "="^40)
        println("BENCHMARK: $b_name")
        println("="^40)
        
        dim = length(ub)
        
        for (s_name, s_init, crossover) in strategies
            # Standard benchmarking: MaxIterStop
            stop = MaxIterStop(100)
            pop_size = 20 * dim
            
            # Clip bounds for stability in generic benchmarks if they are floatmax
            ub_c = [abs(x) > 1e100 ? fill(100.0, dim) : ub for x in ub][1]
            lb_c = [abs(x) > 1e100 ? fill(-100.0, dim) : lb for x in lb][1]
            
            # To handle the floatmax cases from bad_stuff properly without crashing
            # we use the actual provided bounds but the engine handles them.
            
            ζ = run_differential_evolution(obj, [], ub, lb, pop_size, stop, crossover, s_init(dim))
            
            println("  - Strategy: $s_name")
            println("    Best Score: ", round(ζ.φ_hist[end], digits=6))
            println("    Evaluations: ", ζ.ε[])
            
            # Run reference comparison once per benchmark
            if s_name == "Rand1"
                # Reference Optimizers
                ref = run_reference_optim(obj, lb_c, ub_c)
                println("    [REF] Nelder-Mead: ", round(ref.nm_score, digits=6))
                println("    [REF] Sim-Annl:   ", round(ref.sa_score, digits=6))
                println("    [REF] ParticleSw: ", round(ref.ps_score, digits=6))
            end
        end
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    run_exhaustive_benchmarks()
end
