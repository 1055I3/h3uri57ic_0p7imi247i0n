# define variable types

using Random
using Distributions
using Optim

const seed::Int64 = 42;
Random.seed!(seed);

check_bounds(lower_bound::T, upper_bound::T) where {T<:Number} = upper_bound < lower_bound && error("1D10T :: LOWER BOUND $(lower_bound) GREATER THAN THE UPPER BOUND $(upper_bound)\n");

# the stats

mutable struct PerformanceHistory
    evaluations_counter::Int64;
    best_solution_history::Matrix{<:Number};
    best_score_history::Vector{Float64};
    population_diversity_history::Vector{Float64};

    function PerformanceHistory()
        new(0::Int64,
            Matrix{Float64}(),
            Vector{Float64}(),
            Vector{Float64}());
    end
end

function update_stats!(population::Matrix{<:Number},
                       scores::Vector{Float64},
                       evaluations::Int64,
                       stats::PerformanceHistory)
    function compute_diversity(population::Matrix{<:Number})
        n::Int64 = length(population);
        total_distance::Float64 = 0.0;
        for i in 1:n, j in i+1:n
            total_distance += sqrt(sum((population[i] - population[j]).^2));
        end
        return total_distance / (n * (n - 1) / 2);
    end
    
    best_idx::Int64 = argmin(scores);

    stats.evaluations_counter += evaluations;
    push!(stats.best_solution_history, population[best_idx]);
    push!(stats.best_score_history, scores[best_idx]);
    push!(stats.population_diversity_history, compute_diversity(population));

    return stats;
end

# the stopping conditions

function max_iterations_stop(max_iterations::Int64)
    return stats::PerformanceHistory -> length(stats.population_diversity_history) < max_iterations;
end

function fitness_threshold_stop(fitness_threshold::Float64)
    return stats::PerformanceHistory -> fitness_threshold < stats.best_score_history[end];
end

function no_improvement_stop(max_no_improve::Int64)
    return stats::PerformanceHistory -> max_no_improve < length(stats.best_score_history) && issorted(stats.best_score_history[end-max_no_improve+1:end]);
end

# the heuristic

function differential_evolution_generic(objective::Function,
                                        constraint_function::Function,
                                        boundary_constraints::Matrix{<:Number},
                                        individual::Vector{<:Number},
                                        population_size::Int64,
                                        stopping_condition::Function,
                                        selection::Function,
                                        crossover::Function,
                                        mutate::Function)
    population::Matrix{<:Number}; # TODO: generate initial population
    scores::Vector{Float64} = evaluate.(population);
    stats::PerformanceHistory = PerformanceHistory();

    # TODO: check scores for stopping condition - keep the scores in stats? update stats function with multiple methods?
    while stopping_condition(stats)
        new_generation::Matrix{<:Number} = similar(population);
        new_scores::Vector{Float64} = similar(scores);

        @threads for (i, x) in enumerate(population)
            individuals = selection(population); # select
            d, u = crossover(x); # crossover
            v = mutate(x, individuals, d, u); # mutate
            # Apply bounds to mutant vector

            # evaluate
            f = evaluate(v);
            improved = f < cohort_scores[i];
            @inbounds x = improved ? v : x;
            # done: fix evaluation to have one function with one method; it might require two steps to evaluate the new one and the old one before comparing which is better
            # done: where to keep evaluated values?
            # done: every function should do one thing and one thing only
            @inbounds new_cohort_scores = improved ? f : cohort_scores[i];
            @inbounds new_cohort[i] = x;
        end

        population = new_generation;
        scores = new_scores;
        update_stats!(population, scores, evaluations, stats);
    end


    return stats;
end

# function de_rand_1_bin(...)
function crossover(p::Float64,
                   individual::Vector{<:Number})
    d = rand(eachindex(individual));
    u = [p < a for a in rand(Float64, length(individual))];

    return d, u;
end

# consider bitrand for crossover

function selection(population::Matrix{<:Variable})
    while true
        sample = rand(population, 3);
        allunique(sample) && return sample;
    end
end

function mutate(ω::Float64,
                x::Vector{<:Variable},
                individuals::Tuple{Vector{<:Variable}},
                d::Int64,
                u::Vector{Bool})
    a, b, c = individuals;

    [(u[i] || i == d) ? a[i] + ω*(b[i] - c[i]) : x[i] for i in eachindex(x)];
end

# benchmark suite
# TODO: promlem jer Continuous implementira mutaciju ako se pregaze granice - preci na AbstractFloat - implementirati kastovanje

sphere(X::Vector{<:AbstractFloat}) = sum(X.^2);
rosenbrock(X::Vector{<:AbstractFloat}) = sum((x1, x2) -> 100*(x1^2-x2)^2+(1-x1)^2, zip(X[1:end-1], X[2::end]));
step(X::Vector{<:AbstractFloat}) = sum(floor.(X));
griewank(X::Vector{<:AbstractFloat}) = 1 + sum(X.^2)/4000 - prod(cos.(X./sqrt.(1:length(X))));
styblinski_tang(X::Vector{<:AbstractFloat}) = sum(x -> x^4-16*x^2+5*x, X)/2;
sheckel(X::Vector{<:AbstractFloat}, A=rand(length(X),32), C=rand(32)) = -sum(1 ./ (sum((X' .- A).^2, dims=1) .+ C)); # TODO: definisati A i C kao globalne konstante
rastrigin(X::Vector{<:AbstractFloat}) = sum(x -> x^2-10*cos(2*π*x)+10, X);
ackley(X::Vector{<:AbstractFloat}) = -20*exp(-0.2*sqrt(mean(X.^2)))-exp(mean(cos.(2π.*X)))+20+ℯ;
rotated_elipsoid(X::Vector{<:AbstractFloat}) = sum((1:length(X)) .* X.^2);

# end
