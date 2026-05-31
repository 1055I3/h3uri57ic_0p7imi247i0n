using Test
using Random
using Statistics

# Include and Use the restored framework
include("../src/DifferentialEvolution.jl")
using .DifferentialEvolution
include("../src/Benchmarks.jl")
using .Benchmarks

@testset "EXHAUSTIVE DIFFERENTIAL EVOLUTION FRAMEWORK TEST" begin
    
    @testset "Statistical Stopping Integrity" begin
        ζ = ζ_Stats()
        
        # 1. MaxIter
        ζ.ι[] = 10
        @test should_continue(MaxIterStop(10), ζ) == false
        @test should_continue(MaxIterStop(15), ζ) == true
        
        # 2. PocockSign
        ζ.φ_hist = collect(20.0:-1.0:1.0)
        @test should_continue(PocockSignStop(), ζ) == true
        ζ.φ_hist = fill(1.0, 25)
        @test should_continue(PocockSignStop(), ζ) == false
        
        # 3. SPRT
        ζ.φ_hist = fill(1.0, 200)
        @test should_continue(SPRTStop(), ζ) == false
        
        # 4. Permutation
        ζ.φ_hist = fill(1.0, 20)
        @test should_continue(PermutationStop(), ζ) == false

        # 5. MaxRuntime
        stop_rt = MaxRuntimeStop(0.1)
        @test should_continue(stop_rt, ζ) == true
        sleep(0.2)
        @test should_continue(stop_rt, ζ) == false
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
        ub, lb = fill(5.0, 16), fill(-5.0, 16)
        
        suite = [
            ("Sphere", Benchmarks.sphere, de_rand_1_max_ι, (10, 5, 0.1, 0.8)),
            ("Rosenbrock", Benchmarks.rosenbrock, de_best_2_max_ι, (10, 5, 0.1, 0.5, 0.5)),
            ("Step", Benchmarks.step_func, sde_rand_1_max_ι, (10, 5)),
            ("Griewank", Benchmarks.griewank, ags_rand_1_max_ι, (10, 5)),
            ("Styblinski", Benchmarks.styblinski, fisa_rand_1_max_ι, (10, 5)),
            ("Shekel", Benchmarks.shekel, de_rand_1_pocock_sign, (10, 0.1, 0.8)),
            ("Rastrigin", Benchmarks.rastrigin, de_best_2_sprt, (10, 0.1, 0.5, 0.5)),
            ("Ackley", Benchmarks.ackley, sde_rand_1_permutation, (10,)),
            ("Rotated", Benchmarks.rotated, ags_rand_1_sprt, (10,)),
            ("Keane", Benchmarks.Keane().f, fisa_rand_1_max_runtime, (10, 0.5))
        ]
        
        for (name, obj, api_fn, args) in suite
            if name == "Keane"
                ζ = api_fn(obj, [], fill(10.0, 16), fill(0.0, 16), args...)
            else
                ζ = api_fn(obj, [], ub, lb, args...)
            end
            @test !isnan(ζ.φ_hist[end])
            @test ζ.ε[] > 0
        end
    end

end
