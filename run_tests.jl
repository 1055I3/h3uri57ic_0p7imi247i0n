using Test
using Random
using Statistics
include("differential_evolution.jl")

@testset "DE Unicode & Stats Unit Tests" begin
    
    @testset "PocockSignStop Logic" begin
        ζ = PerformanceStats()
        # n_h=7, n=6, K=6. 
        # term(6) = binomial(6,6)*0.5^6 = 1/64 = 0.015625
        # Peq = 0.015625, Pgt = 0.0
        # p_mid = 0.0 + 0.5*0.015625 = 0.0078125
        # 0.0078125 <= 0.02275 => true (Continue)
        ζ.φ_hist = [1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4]
        @test check(PocockSignStop(), ζ) == true
        
        # All flat: K=0, n=6. 
        # Peq = term(0) = 0.015625
        # Pgt = term(1..6) = 1 - 0.015625 = 0.984375
        # p_mid = 0.984375 + 0.5*0.015625 = 0.9921875
        # 0.9921875 > 0.02275 => false (Stop)
        ζ.φ_hist = fill(1.0, 7)
        @test check(PocockSignStop(), ζ) == false
    end

    @testset "SPRTStop Logic" begin
        ζ = PerformanceStats()
        # K=1, n=1. M = 0.5. 0.5 > 0.05 => true (Continue)
        ζ.φ_hist = [1.0, 0.9]
        @test check(SPRTStop(), ζ) == true
        
        # K=0, n=9. M = Beta(0.5, 9.5)/Beta(0.5, 0.5)
        # log M = lgamma(0.5)+lgamma(9.5)-lgamma(10) - 2*lgamma(0.5) = lgamma(9.5)-lgamma(10)-lgamma(0.5)
        # M is approx 0.017. 0.017 <= 0.05 => false (Stop)
        ζ.φ_hist = fill(1.0, 10)
        @test check(SPRTStop(), ζ) == false
    end

    @testset "PermutationStop Logic" begin
        ζ = PerformanceStats()
        # n_h=12, Δ=[1..1] (11 ones). X=[1..1] (10 ones).
        # obs_T = 0.0. p_perm = 1.0. 1.0 > 0.975 => false (Stop)
        ζ.φ_hist = collect(12.0:-1.0:1.0) 
        @test check(PermutationStop(), ζ) == false 
        
        # Artificial improvement in recent block
        # X = [0,0,0,0,0, 1,1,1,1,1]
        # μA=0, μB=1, sA2=0, sB2=0. T_s = large? No, denom handled.
        # But let's use some noise.
        X_vals = [0.1, 0.2, 0.1, 0.2, 0.1, 10.0, 11.0, 10.0, 11.0, 10.0]
        # To get these into φ_hist, we integrate backwards from 0.
        h = [0.0]
        for v in reverse(X_vals)
            push!(h, h[end] + v)
        end
        ζ.φ_hist = reverse(h)
        @test check(PermutationStop(), ζ) == true # p_perm should be small
    end

    @testset "AGS Handshake" begin
        Π = [[1.0, 1.0], [2.0, 2.0], [3.0, 3.0], [4.0, 4.0]]
        Φ = [10.0, 10.0, 10.0, 10.0]
        ρ = Random.default_rng()
        Σ = Dict{Symbol, Any}()
        
        Ξ, Σ = AGS!(Π, Φ, ρ, Σ)
        @test length(Ξ) == 4
        @test haskey(Σ, :_pending_candidates)
        
        Ξ2, Σ = AGS!(Π, Φ, ρ, Σ)
        @test isempty(Ξ2)
        @test !haskey(Σ, :_pending_candidates)
    end

    @testset "FISA Handshake" begin
        Π = [[1.0, 1.0], [2.0, 2.0], [3.0, 3.0], [4.0, 4.0]]
        Φ = [10.0, 10.0, 10.0, 10.0]
        ρ = Random.default_rng()
        Σ = Dict{Symbol, Any}(:bounds => ([0.0, 0.0], [10.0, 10.0]))
        
        Ξ, Σ = FISA!(Π, Φ, ρ, Σ)
        @test length(Ξ) == 4
        @test haskey(Σ, :_pending_candidates)
        
        Ξ2, Σ = FISA!(Π, Φ, ρ, Σ)
        @test isempty(Ξ2)
    end
end

@testset "Upgraded DE Integration Tests" begin
    Random.seed!(42)
    ub, lb = [5.0, 5.0], [-5.0, -5.0]
    
    @testset "AGS Convergence (Sphere)" begin
        ζ = run_differential_evolution(x -> sum(x.^2), [], ub, lb, 20, MaxIterationsStop(500), BasicCrossover(0.1), AGS!)
        @test ζ.φ_hist[end] < 1.0 # Loose check for speed
    end
end
