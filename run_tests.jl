using Test
using Random
using Statistics
include("differential_evolution.jl")

@testset "DE Polymorphic & Unicode Unit Tests" begin
    
    @testset "PocockSignStop Logic" begin
        ζ = ζ_Stats()
        # n_h=7, n=6, K=6. 
        # t_f(6) = binomial(6,6)*0.5^6 = 0.015625. Pgt=0. p_mid=0.0078125 <= 0.02275 => true (Continue)
        ζ.φ_hist = [1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4]
        @test check(PocockSignStop(), ζ) == true
        
        # All flat: K=0, n=6. p_mid = 0.9921875 > 0.02275 => false (Stop)
        ζ.φ_hist = fill(1.0, 7)
        @test check(PocockSignStop(), ζ) == false
    end

    @testset "SPRTStop Logic" begin
        ζ = ζ_Stats()
        # K=1, n=1. M = 0.5 > 0.05 => true (Continue)
        ζ.φ_hist = [1.0, 0.9]
        @test check(SPRTStop(), ζ) == true
        
        # K=0, n=9. M approx 0.017 <= 0.05 => false (Stop)
        ζ.φ_hist = fill(1.0, 10)
        @test check(SPRTStop(), ζ) == false
    end

    @testset "PermutationStop Logic" begin
        ζ = ζ_Stats()
        # Flat history: p_perm = 1.0 > 0.975 => false (Stop)
        ζ.φ_hist = fill(1.0, 15)
        @test check(PermutationStop(), ζ) == false 
        
        # Consistent improvement: p_perm should be small <= 0.975 => true (Continue)
        # Using larger history for block requirements
        ζ.φ_hist = collect(20.0:-1.0:1.0)
        @test check(PermutationStop(), ζ) == true
    end

    @testset "Polymorphic AGS Dispatch" begin
        n, d = 10, 2
        m = AGS()
        c = BinomialCrossover(0.5)
        Π = [randn(d) for _ in 1:n]; Φ = randn(n)
        ρ = Random.default_rng()
        Σ = Dict{Symbol, Any}(:bounds => (fill(-5.0, d), fill(5.0, d)))
        
        # Propose (i=1 initializes)
        v = propose(m, c, 1, Π, Φ, ρ, Σ, Π[1])
        @test length(v) == d
        @test !isempty(m.WF)
        
        # Adapt
        Φ_old = copy(Φ)
        adapt!(m, Π, Φ, [v for _ in 1:n], Φ_old, ρ, Σ)
        @test m.ι == 1
    end

    @testset "Polymorphic FISA Dispatch" begin
        n, d = 10, 2
        m = FISA()
        c = SACrossover()
        Π = [randn(d) for _ in 1:n]; Φ = randn(n)
        ρ = Random.default_rng()
        Σ = Dict{Symbol, Any}(:bounds => (fill(-5.0, d), fill(5.0, d)))
        
        v = propose(m, c, 1, Π, Φ, ρ, Σ, Π[1])
        @test length(v) == d
        @test !isempty(m.σ2)
        
        old_count = m.count[]
        adapt!(m, Π, Φ, [v for _ in 1:n], Φ, ρ, Σ)
        @test m.count[] == old_count # No change if Φ == Φ_old
    end
end

@testset "Integration: Polymorphic Convergence" begin
    Random.seed!(42)
    ub, lb = [5.0, 5.0], [-5.0, -5.0]
    
    @testset "Rand1 + MaxIter" begin
        ζ = run_differential_evolution(x -> sum(x.^2), [], ub, lb, 20, MaxIterStop(50), BinomialCrossover(0.1), Rand1(0.8))
        @test ζ.ι[] == 50
        @test ζ.φ_hist[end] < ζ.φ_hist[1]
    end

    @testset "AGS + MaxIter" begin
        ζ = run_differential_evolution(x -> sum(x.^2), [], ub, lb, 20, MaxIterStop(50), BinomialCrossover(0.1), AGS())
        @test ζ.ι[] == 50
    end
end
