using Test
using Random
using Statistics

# Include the source files
include("../src/DifferentialEvolution.jl")
using .DifferentialEvolution
include("../src/Benchmarks.jl")
using .Benchmarks

@testset "Differential Evolution Package Tests" begin
    
    @testset "Models & Stats" begin
        ζ = ζ_Stats()
        Π = [[1.0, 1.0], [2.0, 2.0]]
        Φ = [10.0, 5.0]
        update_ζ!(ζ, Π, Φ, 2)
        @test ζ.ε[] == 2
        @test ζ.φ_hist[end] == 5.0
        @test ζ.β_hist[end] == [2.0, 2.0]
        @test compute_δ(Π) ≈ sqrt(2.0)
    end

    @testset "Stopping Conditions" begin
        ζ = ζ_Stats()
        
        # MaxIter
        @test check(MaxIterStop(10), ζ) == true
        ζ.ι[] = 10
        @test check(MaxIterStop(10), ζ) == false
        
        # Pocock (Noise case)
        ζ.φ_hist = [1.0, 1.1, 1.0, 1.1, 1.0, 1.1, 1.0]
        @test check(PocockSignStop(), ζ) == false # Stopped
        
        # SPRT (Flat case)
        ζ.φ_hist = fill(1.0, 200)
        @test check(SPRTStop(), ζ) == false # Stopped
    end

    @testset "Strategies Handshake & Dispatch" begin
        n, d = 10, 2
        Π = [[rand() for _ in 1:d] for _ in 1:n]
        Φ = [sum(x.^2) for x in Π]
        ρ = Random.default_rng()
        Σ = Dict{Symbol, Any}(:bounds => (fill(-5.0, d), fill(5.0, d)))
        β = Π[1]
        
        # AGS
        m_ags = AGS()
        v = propose(m_ags, BinomialCrossover(0.5), 1, Π, Φ, ρ, Σ, β)
        @test length(v) == d
        @test !isempty(m_ags.WF)
        
        # adapt!
        Φ_new = copy(Φ)
        adapt!(m_ags, Π, Φ_new, [v for _ in 1:n], Φ, ρ, Σ)
        @test m_ags.ι == 1
        
        # FISA
        m_fisa = FISA()
        v_f = propose(m_fisa, SACrossover(), 1, Π, Φ, ρ, Σ, β)
        @test length(v_f) == d
        adapt!(m_fisa, Π, Φ, [v_f for _ in 1:n], Φ, ρ, Σ)
    end

    @testset "Integration (Rand1 + Sphere)" begin
        ub, lb = fill(5.0, 3), fill(-5.0, 3)
        ζ = de_rand_1_max_iter(sphere, [], ub, lb, 20, 50, 0.1, 0.8)
        @test ζ.φ_hist[end] < ζ.φ_hist[1]
    end

end
