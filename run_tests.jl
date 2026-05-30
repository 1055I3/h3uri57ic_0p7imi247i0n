using Test
using Random
using Statistics
include("differential_evolution.jl")

@testset "DE System Integrity Tests" begin
    
    @testset "Legacy API Convergence (Sphere)" begin
        Random.seed!(42)
        ub, lb = fill(10.0, 3), fill(-10.0, 3)
        
        ζ1 = de_rand_1_max_iter(Benchmarks.sphere, [], ub, lb, 20, 50, 0.1, 0.8)
        @test ζ1.φ_hist[end] < ζ1.φ_hist[1]
        @test ζ1.ε[] > 0
        
        ζ2 = sde_rand_1_no_improvement(Benchmarks.sphere, [], ub, lb, 20, 10, 1e-4)
        @test !isempty(ζ2.φ_hist)
    end

    @testset "Advanced Stopping Criteria" begin
        ζ = ζ_Stats()
        
        # SPRT needs more than 125 flat samples to reach 0.05
        # Pocock needs n >= 6 and mid-p > 0.02275.
        
        # PocockSign: Stop when p_mid > 0.02275
        # If flat for a long time, Pocock might not stop because ties are filtered (n=0).
        # But if we have some noise that is not strictly improvement:
        # K=0 (no improvements), n=6 (all noise <= 0)
        # p_mid = term(1..6) + 0.5*term(0) = 0.992 > 0.02275 => STOP (check returns false)
        ζ.φ_hist = [1.0, 1.01, 1.02, 1.01, 1.02, 1.01, 1.02] # All Δ <= 0
        @test check(PocockSignStop(), ζ) == false 
        
        # SPRT: Stop when M <= 0.05
        # K=0, n=200 => M is very small
        ζ.φ_hist = fill(1.0, 200)
        @test check(SPRTStop(), ζ) == false
    end

    @testset "Self-Adaptive Strategies" begin
        ub, lb = fill(5.0, 3), fill(-5.0, 3)
        # AGS
        ζ_ags = run_differential_evolution(Benchmarks.sphere, [], ub, lb, 20, MaxIterStop(20), BinomialCrossover(0.1), AGS())
        @test ζ_ags.ι[] == 20
        
        # FISA
        ζ_fisa = run_differential_evolution(Benchmarks.sphere, [], ub, lb, 20, MaxIterStop(20), BinomialCrossover(0.1), FISA())
        @test ζ_fisa.ι[] == 20
    end
end
