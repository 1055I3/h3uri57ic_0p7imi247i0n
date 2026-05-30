using Test
using Random
using Statistics
include("differential_evolution.jl")

@testset "Differential Evolution Unit Tests" begin
    
    @testset "Diversity Calculation" begin
        pop = [[0.0, 0.0], [1.0, 1.0], [2.0, 2.0]]
        # distances: d(1,2)=sqrt(2), d(1,3)=sqrt(8)=2*sqrt(2), d(2,3)=sqrt(2)
        # avg = (sqrt(2) + 2*sqrt(2) + sqrt(2)) / 3 = 4*sqrt(2)/3
        expected = (sqrt(2) + sqrt(8) + sqrt(2)) / 3
        @test compute_diversity(pop) ≈ expected
        
        @test compute_diversity([[0.0], [0.0]]) == 0.0
        @test compute_diversity([[1.0]]) == 0.0
    end

    @testset "Bounds Enforcement" begin
        ub = [10.0, 10.0]
        lb = [0.0, 0.0]
        
        # Internal function test via a dummy DE setup or just duplicating logic for unit test
        # Since it's internal to run_differential_evolution, we test the behavior.
        # But we can also test if it's correct in isolation if we had exported it.
        # Let's test a sample DE run with tight bounds.
        stats = run_differential_evolution(
            x -> sum(x), [], [1.0], [1.0], 5, 
            MaxIterationsStop(1), BasicCrossover(0.5), Rand1Strategy(0.5)
        )
        @test all(stats.best_solution_hist[1] .== 1.0)
    end

    @testset "Stopping Conditions" begin
        stats = PerformanceStats()
        
        # Max Iterations
        stop_max = MaxIterationsStop(10)
        @test check(stop_max, stats) == true
        stats.iter_count[] = 10
        @test check(stop_max, stats) == false
        
        # Fitness Threshold
        stop_fit = FitnessThresholdStop(0.1)
        @test check(stop_fit, stats) == true # empty hist
        push!(stats.best_score_hist, 0.5)
        @test check(stop_fit, stats) == true
        push!(stats.best_score_hist, 0.05)
        @test check(stop_fit, stats) == false
        
        # No Improvement
        stop_no_imp = NoImprovementStop(2, 0.01)
        empty!(stats.best_score_hist)
        stats.iter_count[] = 1
        @test check(stop_no_imp, stats) == true
        
        stats.iter_count[] = 4
        push!(stats.best_score_hist, 1.0)
        push!(stats.best_score_hist, 0.9)
        push!(stats.best_score_hist, 0.8)
        push!(stats.best_score_hist, 0.7)
        @test check(stop_no_imp, stats) == true # window [0.8, 0.7], diff 0.1 > 0.01
        
        empty!(stats.best_score_hist)
        push!(stats.best_score_hist, 1.0)
        push!(stats.best_score_hist, 1.0001)
        push!(stats.best_score_hist, 1.0001)
        push!(stats.best_score_hist, 1.0001)
        @test check(stop_no_imp, stats) == false # window [1.0001, 1.0001], diff 0 < 0.01
    end
end

@testset "Differential Evolution Integration Tests" begin
    Random.seed!(42)
    
    @testset "Sphere Function Convergence" begin
        dim = 3
        ub = fill(10.0, dim)
        lb = fill(-10.0, dim)
        
        stats = run_differential_evolution(
            x -> sum(x.^2),
            [],
            ub, lb,
            20,
            MaxIterationsStop(500),
            BasicCrossover(0.1),
            Rand1Strategy(0.8)
        )
        
        @test stats.best_score_hist[end] < 1e-6
    end

    @testset "Constrained Optimization (Simple)" begin
        # Minimize x + y subject to x + y >= 2 => min is 2 at x+y=2
        # Penalty: max(0, 2 - (x+y))
        objective(x) = sum(x)
        constraints = [x -> max(0.0, 2.0 - sum(x)) * 100.0] # Heavy penalty
        
        ub = [5.0, 5.0]
        lb = [0.0, 0.0]
        
        stats = run_differential_evolution(
            objective,
            constraints,
            ub, lb,
            20,
            MaxIterationsStop(500),
            BasicCrossover(0.5),
            Rand1Strategy(0.8)
        )
        
        @test stats.best_score_hist[end] ≈ 2.0 atol=0.1
    end

    @testset "Strategies (Best/2 and SDE)" begin
        # Test if they at least run and produce a reasonable result on Sphere
        dim = 2
        ub = fill(5.0, dim)
        lb = fill(-5.0, dim)
        
        # Best/2
        stats_b2 = run_differential_evolution(
            x -> sum(x.^2), [], ub, lb, 20,
            MaxIterationsStop(200), BasicCrossover(0.1), Best2Strategy(0.5, 0.5)
        )
        @test stats_b2.best_score_hist[end] < 0.1
        
        # SDE
        stats_sde = run_differential_evolution(
            x -> sum(x.^2), [], ub, lb, 20,
            MaxIterationsStop(200), CrossoverSA(), SDEStrategy(dim)
        )
        @test stats_sde.best_score_hist[end] < 0.5
    end
end
