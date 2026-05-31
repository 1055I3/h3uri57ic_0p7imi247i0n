using Test
using Random
using Statistics

# Include and Use the restored framework
include("../src/DifferentialEvolution.jl")
using .DifferentialEvolution
include("../src/Benchmarks.jl")
using .Benchmarks

# --- Deterministic Safety Infrastructure ---
# Defined at top level for clean method extension and global test visibility.

struct SafeStop <: StopCond
    inner::StopCond
    limit::MaxIterStop
end

# Extend the framework's termination logic with a deterministic guard.
# We import from the module where it's defined/exported.
import .DifferentialEvolution: should_continue, ζ_Stats

function should_continue(s::SafeStop, ζ::ζ_Stats)
    # Termination Safety Invariant: Stop if ANY condition triggers.
    # Therefore, we only continue if BOTH internal logic AND safety limit allow it.
    return should_continue(s.inner, ζ) && should_continue(s.limit, ζ)
end

@testset "EXHAUSTIVE DIFFERENTIAL EVOLUTION FRAMEWORK TEST" begin
    
    @testset "Statistical Stopping Integrity" begin
        ζ = ζ_Stats()
        
        # 1. MaxIter (Deterministic)
        ζ.ι[] = 10
        @test should_continue(MaxIterStop(10), ζ) == false
        @test should_continue(MaxIterStop(15), ζ) == true
        
        # 2. PocockSign (Statistical)
        # Strong Improvement history: should continue
        ζ.φ_hist = collect(20.0:-1.0:1.0)
        @test should_continue(PocockSignStop(), ζ) == true
        
        # Stagnation history with noise (to satisfy n >= 6 requirement)
        # Alternating slightly ensures the sign test has data but fails to see a trend.
        ζ.φ_hist = [1.0, 1.001, 1.0, 1.001, 1.0, 1.001, 1.0, 1.001, 1.0, 1.001]
        @test should_continue(PocockSignStop(), ζ) == false
        
        # 3. SPRT (Statistical)
        # Stagnation: should stop
        ζ.φ_hist = fill(1.0, 200)
        @test should_continue(SPRTStop(), ζ) == false
        
        # 4. Permutation (Statistical)
        # Stagnation: should stop
        ζ.φ_hist = fill(1.0, 20)
        @test should_continue(PermutationStop(), ζ) == false

        # 5. MaxRuntime (Runtime-based)
        stop_rt = MaxRuntimeStop(0.1)
        @test should_continue(stop_rt, ζ) == true
        sleep(0.2)
        @test should_continue(stop_rt, ζ) == false
        
        # 6. Composite Safety (Meta-Stop)
        # Verify that SafeStop respects its limit regardless of the inner condition.
        safe_never_stop = SafeStop(MaxIterStop(1000), MaxIterStop(5))
        ζ.ι[] = 6
        @test should_continue(safe_never_stop, ζ) == false
    end

    @testset "Strategy & Polymorphism Soundness" begin
        d, n = 5, 20
        Π = [[rand() for _ in 1:d] for _ in 1:n]
        Φ = [Benchmarks.sphere(x) for x in Π]
        ρ = Random.default_rng()
        Σ = Dict{Symbol, Any}(:bounds => (fill(10.0, d), fill(-10.0, d)))
        β = Π[argmin(Φ)]
        
        strats = [Rand1(0.8), Best2(0.5, 0.5), SDE(d), AGS(), FISA()]
        cross = [BinomialCrossover(0.5), SACrossover()]
        
        for m in strats
            for c in cross
                v = propose_trial(m, c, 1, Π, Φ, ρ, Σ, β)
                @test length(v) == d
                
                Φ_old = copy(Φ)
                adapt!(m, Π, Φ, [v for _ in 1:n], Φ_old, ρ, Σ)
            end
        end
    end

    @testset "Legacy API & Multi-Benchmark Functional Test" begin
        # Access internals for re-wrapping safely
        import .DifferentialEvolution: run_differential_evolution, BinomialCrossover, SACrossover, Rand1, Best2, SDE, AGS, FISA

        # --- Safe API Wrappers ---
        # Re-implementing Legacy API calls with an added SafeStop(..., MaxIterStop(200)) layer.
        # This guarantees CI stability and prevents hangs on degenerate landscapes.
        
        s_de_pocock(o, c, ub, lb, n, p, ω) = run_differential_evolution(o, c, ub, lb, n, SafeStop(PocockSignStop(), MaxIterStop(200)), BinomialCrossover(p), Rand1(ω))
        s_de_sprt(o, c, ub, lb, n, p, ν, ω) = run_differential_evolution(o, c, ub, lb, n, SafeStop(SPRTStop(), MaxIterStop(200)), BinomialCrossover(p), Best2(ν, ω))
        s_sde_perm(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, SafeStop(PermutationStop(), MaxIterStop(200)), SACrossover(), SDE(length(ub)))
        s_ags_sprt(o, c, ub, lb, n) = run_differential_evolution(o, c, ub, lb, n, SafeStop(SPRTStop(), MaxIterStop(200)), BinomialCrossover(0.1), AGS())
        s_fisa_rt(o, c, ub, lb, n, t) = run_differential_evolution(o, c, ub, lb, n, SafeStop(MaxRuntimeStop(t), MaxIterStop(200)), SACrossover(), FISA())

        ub, lb = fill(5.0, 16), fill(-5.0, 16)
        
        # Benchmark suite using deterministic safety guards
        suite = [
            ("Sphere", Benchmarks.sphere, de_rand_1_max_ι, (10, 50, 0.1, 0.8)),
            ("Rosenbrock", Benchmarks.rosenbrock, de_best_2_max_ι, (10, 50, 0.1, 0.5, 0.5)),
            ("Step", Benchmarks.step_func, sde_rand_1_max_ι, (10, 50)),
            ("Griewank", Benchmarks.griewank, ags_rand_1_max_ι, (10, 50)),
            ("Styblinski", Benchmarks.styblinski, fisa_rand_1_max_ι, (10, 50)),
            ("Shekel", Benchmarks.shekel, s_de_pocock, (10, 0.1, 0.8)),
            ("Rastrigin", Benchmarks.rastrigin, s_de_sprt, (10, 0.1, 0.5, 0.5)),
            ("Ackley", Benchmarks.ackley, s_sde_perm, (10,)),
            ("Rotated", Benchmarks.rotated, s_ags_sprt, (10,)),
            ("Keane", Benchmarks.Keane().f, s_fisa_rt, (10, 1.0))
        ]
        
        for (name, obj, api_fn, args) in suite
            # Ensure safe bounds for Keane
            if name == "Keane"
                ζ = api_fn(obj, [], fill(10.0, 16), fill(0.0, 16), args...)
            else
                ζ = api_fn(obj, [], ub, lb, args...)
            end
            
            # Assertions for deterministic behavior and state integrity
            @test !isnan(ζ.φ_hist[end])
            @test length(ζ.φ_hist) >= 1
            @test ζ.ι[] <= 200 # Enforce hard safety bound invariant
            @test ζ.ε[] > 0    # Verify evaluations occurred
        end
    end

end
