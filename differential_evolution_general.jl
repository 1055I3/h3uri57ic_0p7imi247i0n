using Random
using Distributions
using Iterators
using Optim

# constants
const eps::Float64 = 1.0e-12;
const seed::Int64 = 42;
Random.seed!(seed);

# the stats
mutable struct PerformanceHistory
    evaluations_counter::Int64;
    best_solution_history::Vector{Vector{<:Number}};
    best_score_history::Vector{Float64};
    population_diversity_history::Vector{Float64};

    function PerformanceHistory()
        new(0::Int64,
            Vector{Vector{Float64}}(),
            Vector{Float64}(),
            Vector{Float64}());
    end
end

function update_stats!(population::Vector{Vector{<:Number}},
                       scores::Vector{Float64},
                       evaluations::Int64,
                       stats::PerformanceHistory)
    function compute_diversity(population::Vector{Vector{<:Number}})
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
    return stats::PerformanceHistory -> length(stats.population_diversity_history) ≤ max_iterations;
end

function fitness_threshold_stop(fitness_threshold::Float64)
    return stats::PerformanceHistory -> fitness_threshold < stats.best_score_history[end];
end

function no_improvement_stop(max_no_improve::Int64, no_improve_threshold::Float64)
    return stats::PerformanceHistory -> max_no_improve ≥ length(stats.best_score_history) || !all(abs.(diff(stats.best_score_history[end-max_no_improve+1:end])) .< no_improve_threshold);
end

# the heuristic
function differential_evolution_generic(objective::Function,
                                        constraint_functions::Vector{Function},
                                        upper_bounds::Vector{<:Number},
                                        lower_bounds::Vector{<:Number},
                                        population_size::Int64,
                                        stopping_condition::Function,
                                        selection::Function,
                                        crossover::Function,
                                        mutate::Function)
    evaluate(individual::Vector{<:Number}) = begin
        cs::Vector{Float64} = [cf(individual) for cf in constraint_functions];
        return objective(individual)*prod([c>eps ? 100.0*c^2 : 1.0 for c in cs]); 
    end
    enforce_bounds(x::T, upper::T, lower::T) where {T<:Number} = lower≤x && x≤upper ? x : new_chromosome(upper, lower);
    new_chromosome(upper::T, lower::T) where {T<:Integer} = rand(upper:lower);
    new_chromosome(upper::T, lower::T) where {T<:AbstractFloat} = rand()*(upper-lower)+lower;

    # initialize the population and stats
    population::Vector{Vector{<:Number}} = [[new_chromosome(upper, lower) for (upper, lower) in zip(upper_bounds, lower_bounds)] for _ in 1:population_size]
    scores::Vector{Float64} = evaluate.(population);
    stats::PerformanceHistory = PerformanceHistory();
    update_stats!(population, scores, 1, stats);

    while stopping_condition(stats)
        new_generation::Vector{Vector{<:Number}} = similar(population);
        new_scores::Vector{Float64} = similar(scores);
        evaluations::Int64 = 0;

        @threads for i in 1:population_size
            x = pupulation[i];

            # select vectors for mutation
            individuals = selection(population,
                                    stats.best_solution_history[end]);

            # get the crossover vector for an individual
            u = crossover(x);

            # generate a new individual
            v = mutate(x, individuals, d, u);

            # apply bounds to mutant vector
            v = enforce_bounds.(v, upper_bounds, lower_bounds);

            # evaluate fitness of the new individual
            f = evaluate(v);

            if f < scores[i]
                @inbounds new_generation[i] = v;
                @inbounds new_scores[i] = f;
            else
                @inbounds new_generation[i] = population[i];
                @inbounds new_scores[i] = scores[i];
            end
        end

        # update the stats; in each iteration we make population_size number of evaluation function calls
        population = new_generation;
        scores = new_scores;
        evaluations = population_size;
        update_stats!(population, scores, evaluations, stats);
    end

    return stats;
end

# crossover
function crossover(p::Float64,
                   individual::Vector{<:Number})
    d = rand(eachindex(individual));
    u = [p < a for a in rand(Float64, length(individual))];
    u[d] = true;

    return u;
end

function crossover_sa()
    rnd = Normal(0.5, 0.15);

    return (individual::Vector{<:Number}) -> begin
        d = rand(eachindex(individual));
        pr = rand(rnd, length(individual));
        r = rand(Float64, length(individual));
        u = r .< pr;
        u[d] = true;

        return u;
    end
end

# de_rand_1

function selection_rand_1(population::Vector{Vector{<:Number}}, _)
    while true
        sample = rand(population, 3);
        allunique(sample) && return sample;
    end
end

function mutate_rand_1(ω::Float64,
                       x::Vector{<:Number},
                       individuals::Tuple{Vector{<:Number}},
                       u::Vector{Bool})
    a, b, c = individuals;

    return [(u[i]) ? a[i] + ω*(b[i] - c[i]) : x[i] for i in eachindex(x)];
end

function de_rand_1_max_iter(objective::Function,
                            constraint_functions::Vector{Function},
                            upper_bounds::Vector{<:Number},
                            lower_bounds::Vector{<:Number},
                            population_size::Int64,
                            max_iterations::Int64,
                            p::Float64,
                            ω::Float64)
    return differential_evolution_generic(objective,
                                          constraint_functions,
                                          upper_bounds,
                                          lower_bounds,
                                          population_size,
                                          max_iterations_stop(max_iterations),
                                          selection_rand_1,
                                          x -> crossover(p, x),
                                          (x, is, u) -> mutate_rand_1(ω, x, is, u));
end

function de_rand_1_fitness_threshold(objective::Function,
                                     constraint_functions::Vector{Function},
                                     upper_bounds::Vector{<:Number},
                                     lower_bounds::Vector{<:Number},
                                     population_size::Int64,
                                     fitness_threshold::Float64,
                                     p::Float64,
                                     ω::Float64)
    return differential_evolution_generic(objective,
                                          constraint_functions,
                                          upper_bounds,
                                          lower_bounds,
                                          population_size,
                                          fitness_threshold_stop(fitness_threshold),
                                          selection_rand_1,
                                          x -> crossover(p, x),
                                          (x, is, u) -> mutate_rand_1(ω, x, is, u));
end

function de_rand_1_no_improvement(objective::Function,
                                  constraint_functions::Vector{Function},
                                  upper_bounds::Vector{<:Number},
                                  lower_bounds::Vector{<:Number},
                                  population_size::Int64,
                                  no_imprevement_iters::Int64,
                                  no_improvement_threshold::Float64,
                                  p::Float64,
                                  ω::Float64)
    return differential_evolution_generic(objective,
                                          constraint_functions,
                                          upper_bounds,
                                          lower_bounds,
                                          population_size,
                                          no_improvement_stop(no_imprevement_iters,
                                                              no_improvement_threshold),
                                          selection_rand_1,
                                          x -> crossover(p, x),
                                          (x, is, u) -> mutate_rand_1(ω, x, is, u));
end

# de_best_2

function selection_best_2(population::Vector{Vector{<:Number}},
                          best::Vector{<:Number})
    sample = [[best]; rand(population, 4)];
    while !allunique(sample)
        sample = [[best]; rand(population, 4)];
    end
    return sample;
end

function mutate_best_2(λ::Float64,
                       ω::Float64,
                       x::Vector{<:Number},
                       individuals::Tuple{Vector{<:Number}},
                       u::Vector{Bool})
    best, a, b, c, d = individuals;

    return [(u[i]) ? best[i] + λ*(a[i] - b[i]) + ω*(c[i] - d[i]) : x[i] for i in eachindex(x)];
end

function de_best_2_max_iter(objective::Function,
                            constraint_functions::Vector{Function},
                            upper_bounds::Vector{<:Number},
                            lower_bounds::Vector{<:Number},
                            population_size::Int64,
                            max_iterations::Int64,
                            p::Float64,
                            λ::Float64,
                            ω::Float64)
    return differential_evolution_generic(objective,
                                          constraint_functions,
                                          upper_bounds,
                                          lower_bounds,
                                          population_size,
                                          max_iterations_stop(max_iterations),
                                          selection_best_2,
                                          x -> crossover(p, x),
                                          (x, is, u) -> mutate_best_2(λ, ω, x, is, u));
end

function de_best_2_fitness_threshold(objective::Function,
                                     constraint_functions::Vector{Function},
                                     upper_bounds::Vector{<:Number},
                                     lower_bounds::Vector{<:Number},
                                     population_size::Int64,
                                     fitness_threshold::Float64,
                                     p::Float64,
                                     λ::Float64,
                                     ω::Float64)
    return differential_evolution_generic(objective,
                                          constraint_functions,
                                          upper_bounds,
                                          lower_bounds,
                                          population_size,
                                          fitness_threshold_stop(fitness_threshold),
                                          selection_best_2,
                                          x -> crossover(p, x),
                                          (x, is, u) -> mutate_best_2(λ, ω, x, is, u));
end

function de_best_2_no_improvement(objective::Function,
                                  constraint_functions::Vector{Function},
                                  upper_bounds::Vector{<:Number},
                                  lower_bounds::Vector{<:Number},
                                  population_size::Int64,
                                  no_imprevement_iters::Int64,
                                  no_improvement_threshold::Float64,
                                  p::Float64,
                                  λ::Float64,
                                  ω::Float64)
    return differential_evolution_generic(objective,
                                          constraint_functions,
                                          upper_bounds,
                                          lower_bounds,
                                          population_size,
                                          no_improvement_stop(no_imprevement_iters,
                                                              no_improvement_threshold),
                                          selection_best_2,
                                          x -> crossover(p, x),
                                          (x, is, u) -> mutate_best_2(λ, ω, x, is, u));
end

# SDE
function mutate_sde_rand_1(dimension::Int64) 
    ω = rand(Normal(0.5, 0.15), dimension);
    rnd = Normal(0, 0.5);

    return (x, individuals, u) -> begin
        evolve_omega() = begin
            ω1, ω2, ω3 = collect(Flatten(selection_rand_1([[o] for o in ω], _)));
            return ω1 + rand(rnd)*(ω2 - ω3);
        end
        ω = [evolve_omega() for _ in ω];

        a, b, c = individuals;

        return [(u[i]) ? a[i] + ω[i]*(b[i] - c[i]) : x[i] for i in eachindex(x)];
    end
end

function sde_rand_1_max_iter(objective::Function,
                             constraint_functions::Vector{Function},
                             upper_bounds::Vector{<:Number},
                             lower_bounds::Vector{<:Number},
                             population_size::Int64,
                             max_iterations::Int64)
    return differential_evolution_generic(objective,
                                          constraint_functions,
                                          upper_bounds,
                                          lower_bounds,
                                          population_size,
                                          max_iterations(max_iterations),
                                          selection_rand_1,
                                          crossover_sa(),
                                          mutate_sde_rand_1(length(upper_bounds)));
end

# benchmark suite
sphere(X::Vector{<:AbstractFloat}) = sum(X.^2);
rosenbrock(X::Vector{<:AbstractFloat}) = sum((x1, x2) -> 100*(x1^2-x2)^2+(1-x1)^2, zip(X[1:end-1], X[2::end]));
step(X::Vector{<:AbstractFloat}) = sum(floor.(X));
griewank(X::Vector{<:AbstractFloat}) = 1 + sum(X.^2)/4000 - prod(cos.(X./sqrt.(1:length(X))));
styblinski_tang(X::Vector{<:AbstractFloat}) = sum(x -> x^4-16*x^2+5*x, X)/2;
sheckel(X::Vector{<:AbstractFloat}, A=rand(length(X),32), C=rand(32)) = -sum(1 ./ (sum((X' .- A).^2, dims=1) .+ C)); # TODO: definisati A i C kao konstante vam funkcije
rastrigin(X::Vector{<:AbstractFloat}) = sum(x -> x^2-10*cos(2*π*x)+10, X);
ackley(X::Vector{<:AbstractFloat}) = -20*exp(-0.2*sqrt(mean(X.^2)))-exp(mean(cos.(2π.*X)))+20+ℯ;
rotated_elipsoid(X::Vector{<:AbstractFloat}) = sum((1:length(X)) .* X.^2);
# TODO: keane_bump with contraits

# examples
# TODO: graph coloring

# end
