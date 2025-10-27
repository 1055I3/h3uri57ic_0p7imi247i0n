using Random
using Distributions: Normal
using Base.Threads: @spawn, fetch
using Base.Iterators: Flatten
using Optim: optimize, NelderMead, SimulatedAnnealing, ParticleSwarm

# constants
const eps::Float64 = 1.0e-14;
const N::Int64 = 16;
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
                                        constraint_functions::Vector{<:Function},
                                        upper_bounds::Vector{<:Number},
                                        lower_bounds::Vector{<:Number},
                                        population_size::Int64,
                                        stopping_condition::Function,
                                        selection::Function,
                                        crossover::Function,
                                        mutate::Function)
    evaluate(individual::Vector{<:Number}) = begin
        cs::Vector{Float64} = [cf(individual) for cf in constraint_functions];
        return objective(individual)*prod([abs(c)>eps ? c : 1.0 for c in cs]);
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

        # TODO: @threads - fix all the elusive race conditions
        for i in 1:population_size
            x = population[i];

            # select vectors for mutation
            individuals = selection(population,
                                    stats.best_solution_history[end]);

            # get the crossover vector for an individual
            u = crossover(x);

            # generate a new individual
            v = mutate(x, individuals, u);

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
                       individuals::Vector{Vector{<:Number}},
                       u::Vector{Bool})
    a, b, c = individuals;

    return [(u[i]) ? a[i] + ω*(b[i] - c[i]) : x[i] for i in eachindex(x)];
end

function de_rand_1_max_iter(objective::Function,
                            constraint_functions::Vector{<:Function},
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
                                     constraint_functions::Vector{<:Function},
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
                                  constraint_functions::Vector{<:Function},
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
                       individuals::Vector{Vector{<:Number}},
                       u::Vector{Bool})
    best, a, b, c, d = individuals;

    return [(u[i]) ? best[i] + λ*(a[i] - b[i]) + ω*(c[i] - d[i]) : x[i] for i in eachindex(x)];
end

function de_best_2_max_iter(objective::Function,
                            constraint_functions::Vector{<:Function},
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
                                     constraint_functions::Vector{<:Function},
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
                                  constraint_functions::Vector{<:Function},
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
            ω1, ω2, ω3 = collect(Flatten(selection_rand_1([[o] for o in ω], nothing)));
            return ω1 + rand(rnd)*(ω2 - ω3);
        end
        ω = [evolve_omega() for _ in ω];

        a, b, c = individuals;

        return [(u[i]) ? a[i] + ω[i]*(b[i] - c[i]) : x[i] for i in eachindex(x)];
    end
end

function sde_rand_1_max_iter(objective::Function,
                             constraint_functions::Vector{<:Function},
                             upper_bounds::Vector{<:Number},
                             lower_bounds::Vector{<:Number},
                             population_size::Int64,
                             max_iterations::Int64)
    return differential_evolution_generic(objective,
                                          constraint_functions,
                                          upper_bounds,
                                          lower_bounds,
                                          population_size,
                                          max_iterations_stop(max_iterations),
                                          selection_rand_1,
                                          crossover_sa(),
                                          mutate_sde_rand_1(length(upper_bounds)));
end

function sde_rand_1_fitness_threshold(objective::Function,
                             constraint_functions::Vector{<:Function},
                             upper_bounds::Vector{<:Number},
                             lower_bounds::Vector{<:Number},
                             population_size::Int64,
                             fitness_threshold::Float64)
    return differential_evolution_generic(objective,
                                          constraint_functions,
                                          upper_bounds,
                                          lower_bounds,
                                          population_size,
                                          fitness_threshold_stop(fitness_threshold),
                                          selection_rand_1,
                                          crossover_sa(),
                                          mutate_sde_rand_1(length(upper_bounds)));
end

function sde_rand_1_no_improvement(objective::Function,
                                   constraint_functions::Vector{<:Function},
                                   upper_bounds::Vector{<:Number},
                                   lower_bounds::Vector{<:Number},
                                   population_size::Int64,
                                   no_imprevement_iters::Int64,
                                   no_improvement_threshold::Float64)
    return differential_evolution_generic(objective,
                                          constraint_functions,
                                          upper_bounds,
                                          lower_bounds,
                                          population_size,
                                          no_improvement_stop(no_imprevement_iters,
                                                              no_improvement_threshold),
                                          selection_rand_1,
                                          crossover_sa(),
                                          mutate_sde_rand_1(length(upper_bounds)));
end

# benchmark suite
sphere(X::Vector{<:AbstractFloat}) = sum(X.^2);
sphere_upper_bound::Vector{Float64} = [floatmax(Float64) for _ in 1:N];
sphere_lower_bound::Vector{Float64} = [-floatmax(Float64) for _ in 1:N];

rosenbrock(X::Vector{<:AbstractFloat}) = sum((x1, x2) -> 100*(x1^2-x2)^2+(1-x1)^2, zip(X[1:end-1], X[2::end]));
rosenbrock_upper_bound::Vector{Float64} = [floatmax(Float64) for _ in 1:N];
rosenbrock_lower_bound::Vector{Float64} = [-floatmax(Float64) for _ in 1:N];

step(X::Vector{<:AbstractFloat}) = sum(floor.(X));
step_upper_bound::Vector{Float64} = [5.12 for _ in 1:N];
step_lower_bound::Vector{Float64} = [-5.12 for _ in 1:N];

griewank(X::Vector{<:AbstractFloat}) = 1 + sum(X.^2)/4000 - prod(cos.(X./sqrt.(1:length(X))));
griewank_upper_bound::Vector{Float64} = [floatmax(Float64) for _ in 1:N];
griewank_lower_bound::Vector{Float64} = [-floatmax(Float64) for _ in 1:N];

styblinski_tang(X::Vector{<:AbstractFloat}) = sum(x -> x^4-16*x^2+5*x, X)/2;
styblinski_tang_upper_bound::Vector{Float64} = [5 for _ in 1:N];
styblinski_tang_lower_bound::Vector{Float64} = [-5 for _ in 1:N];

const A_SHECKEL = rand(N, 32);
const C_SHECKEL = rand(32);
sheckel(X::Vector{<:AbstractFloat}) = -sum(1 ./ (sum((X' .- A_SHECKEL).^2, dims=1) .+ C_SHECKEL));
sheckel_upper_bound::Vector{Float64} = [floatmax(Float64) for _ in 1:N];
sheckel_lower_bound::Vector{Float64} = [-floatmax(Float64) for _ in 1:N];

rastrigin(X::Vector{<:AbstractFloat}) = sum(x -> x^2-10*cos(2*π*x)+10, X);
rastrigin_upper_bound::Vector{Float64} = [5.12 for _ in 1:N];
rastrigin_lower_bound::Vector{Float64} = [-5.12 for _ in 1:N];

ackley(X::Vector{<:AbstractFloat}) = -20*exp(-0.2*sqrt(mean(X.^2)))-exp(mean(cos.(2π.*X)))+20+ℯ;
ackley_upper_bound::Vector{Float64} = [65.536 for _ in 1:N];
ackley_lower_bound::Vector{Float64} = [-65.536 for _ in 1:N];

rotated_elipsoid(X::Vector{<:AbstractFloat}) = sum((1:length(X)) .* X.^2);
rotated_elipsoid_upper_bound::Vector{Float64} = [32.768 for _ in 1:N];
rotated_elipsoid_lower_bound::Vector{Float64} = [-32.768 for _ in 1:N];

keane_bump(X::Vector{<:AbstractFloat}) = -abs((sum(cos.(X).^4) - 2*prod(cos.(X).^2)) / (sqrt(sum(i * X[i]^2 for i in 1:length(X))))+eps);
keane_constraint_1(X::Vector{<:AbstractFloat}) = begin
    c1::Float64 = 0.75 - prod(X);
    return c1 < 0 ? 1 : c1*75;
end
keane_constraint_2(X::Vector{<:AbstractFloat}) = begin
    c2::Float64 = sum(X) - 7.5*length(X);
    return c2 < 0 ? 1 : c2*75;
end
keane_upper_bound::Vector{Float64} = [10 for _ in 1:N];
keane_lower_bound::Vector{Float64} = [0 for _ in 1:N];

# examples
# TODO: graph coloring

sphere_rand_1_handle = @spawn de_rand_1_no_improvement(sphere, [x -> 1], sphere_upper_bound, sphere_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.4);
rosenbrock_rand_1_handle = @spawn de_rand_1_no_improvement(rosenbrock, [x -> 1], rosenbrock_upper_bound, rosenbrock_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.4);
step_rand_1_handle = @spawn de_rand_1_no_improvement(step, [x -> 1], step_upper_bound, step_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.4);
griewank_rand_1_handle = @spawn de_rand_1_no_improvement(griewank, [x -> 1], griewank_upper_bound, griewank_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.4);
styblinski_rand_1_tang_handle = @spawn de_rand_1_no_improvement(styblinski_tang, [x -> 1], styblinski_tang_upper_bound, styblinski_tang_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.4);
sheckel_rand_1_handle = @spawn de_rand_1_no_improvement(sheckel, [x -> 1], sheckel_upper_bound, sheckel_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.4);
rastrigin_rand_1_handle = @spawn de_rand_1_no_improvement(rastrigin, [x -> 1], rastrigin_upper_bound, rastrigin_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.4);
ackley_rand_1_handle = @spawn de_rand_1_no_improvement(ackley, [x -> 1], ackley_upper_bound, ackley_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.4);
rotated_elipsoid_rand_1_handle = @spawn de_rand_1_no_improvement(rotated_elipsoid, [x -> 1], rotated_elipsoid_upper_bound, rotated_elipsoid_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.4);
keane_bump_rand_1_handle = @spawn de_rand_1_no_improvement(keane_bump, [keane_constraint_1, keane_constraint_2], keane_upper_bound, keane_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.4);

sphere_best_2_handle = @spawn de_best_2_no_improvement(sphere, [x -> 1], sphere_upper_bound, sphere_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.2, 0.2);
rosenbrock_best_2_handle = @spawn de_best_2_no_improvement(rosenbrock, [x -> 1], rosenbrock_upper_bound, rosenbrock_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.2, 0.2);
step_best_2_handle = @spawn de_best_2_no_improvement(step, [x -> 1], step_upper_bound, step_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.2, 0.2);
griewank_best_2_handle = @spawn de_best_2_no_improvement(griewank, [x -> 1], griewank_upper_bound, griewank_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.2, 0.2);
styblinski_best_2_tang_handle = @spawn de_best_2_no_improvement(styblinski_tang, [x -> 1], styblinski_tang_upper_bound, styblinski_tang_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.2, 0.2);
sheckel_best_2_handle = @spawn de_best_2_no_improvement(sheckel, [x -> 1], sheckel_upper_bound, sheckel_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.2, 0.2);
rastrigin_best_2_handle = @spawn de_best_2_no_improvement(rastrigin, [x -> 1], rastrigin_upper_bound, rastrigin_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.2, 0.2);
ackley_best_2_handle = @spawn de_best_2_no_improvement(ackley, [x -> 1], ackley_upper_bound, ackley_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.2, 0.2);
rotated_best_2_handle = @spawn de_best_2_no_improvement(rotated_elipsoid, [x -> 1], rotated_elipsoid_upper_bound, rotated_elipsoid_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.2, 0.2);
keane_bump_best_2_handle = @spawn de_best_2_no_improvement(keane_bump, [keane_constraint_1, keane_constraint_2], keane_upper_bound, keane_lower_bound, 20*N, 2^6, 1.0e-8, 0.1, 0.2, 0.2);

sphere_sde_handle = @spawn sde_rand_1_no_improvement(sphere, [x -> 1], sphere_upper_bound, sphere_lower_bound, 20*N, 2^6, 1.0e-8);
rosenbrock_sde_handle = @spawn sde_rand_1_no_improvement(rosenbrock, [x -> 1], rosenbrock_upper_bound, rosenbrock_lower_bound, 20*N, 2^6, 1.0e-8);
step_sde_handle = @spawn sde_rand_1_no_improvement(step, [x -> 1], step_upper_bound, step_lower_bound, 20*N, 2^6, 1.0e-8);
griewank_sde_handle = @spawn sde_rand_1_no_improvement(griewank, [x -> 1], griewank_upper_bound, griewank_lower_bound, 20*N, 2^6, 1.0e-8);
styblinski_sde_tang_handle = @spawn sde_rand_1_no_improvement(styblinski_tang, [x -> 1], styblinski_tang_upper_bound, styblinski_tang_lower_bound, 20*N, 2^6, 1.0e-8);
sheckel_sde_handle = @spawn sde_rand_1_no_improvement(sheckel, [x -> 1], sheckel_upper_bound, sheckel_lower_bound, 20*N, 2^6, 1.0e-8);
rastrigin_sde_handle = @spawn sde_rand_1_no_improvement(rastrigin, [x -> 1], rastrigin_upper_bound, rastrigin_lower_bound, 20*N, 2^6, 1.0e-8);
ackley_sde_handle = @spawn sde_rand_1_no_improvement(ackley, [x -> 1], ackley_upper_bound, ackley_lower_bound, 20*N, 2^6, 1.0e-8);
rotated_sde_handle = @spawn sde_rand_1_no_improvement(rotated_elipsoid, [x -> 1], rotated_elipsoid_upper_bound, rotated_elipsoid_lower_bound, 20*N, 2^6, 1.0e-8);
keane_bump_sde_handle = @spawn sde_rand_1_no_improvement(keane_bump, [keane_constraint_1, keane_constraint_2], keane_upper_bound, keane_lower_bound, 20*N, 2^6, 1.0e-8);

sphere_rand_1_result = fetch(sphere_rand_1_handle);
rosenbrock_rand_1_result = fetch(rosenbrock_rand_1_handle);
step_rand_1_result = fetch(step_rand_1_handle);
griewank_rand_1_result = fetch(griewank_rand_1_handle);
styblinski_rand_1_tang_result = fetch(styblinski_rand_1_tang_handle);
sheckel_rand_1_result = fetch(sheckel_rand_1_handle);
rastrigin_rand_1_result = fetch(rastrigin_rand_1_handle);
ackley_rand_1_result = fetch(ackley_rand_1_handle);
rotated_elipsoid_rand_1_result = fetch(rotated_elipsoid_rand_1_handle);
keane_bump_rand_1_result = fetch(keane_bump_rand_1_handle);

sphere_best_2_result = fetch(sphere_best_2_handle);
rosenbrock_best_2_result = fetch(rosenbrock_best_2_handle);
step_best_2_result = fetch(step_best_2_handle);
griewank_best_2_result = fetch(griewank_best_2_handle);
styblinski_best_2_tang_result = fetch(styblinski_best_2_tang_handle);
sheckel_best_2_result = fetch(sheckel_best_2_handle);
rastrigin_best_2_1_result = fetch(rastrigin_best_2_handle);
ackley_best_2_result = fetch(ackley_best_2_handle);
rotated_best_2_result = fetch(rotated_best_2_handle);
keane_bump_best_2_result = fetch(keane_bump_best_2_handle);

sphere_sde_result = fetch(sphere_sde_handle);
rosenbrock_sde_result = fetch(rosenbrock_sde_handle);
step_sde_result = fetch(step_sde_handle);
griewank_sde_result = fetch(griewank_sde_handle);
styblinski_sde_tang_result = fetch(styblinski_sde_tang_handle);
sheckel_sde_result = fetch(sheckel_sde_handle);
rastrigin_sde_result = fetch(rastrigin_sde_handle);
ackley_sde_result = fetch(ackley_sde_handle);
rotated_sde_result = fetch(rotated_sde_handle);
keane_bump_sde_result = fetch(keane_bump_sde_handle);

sphere_simplex_result = optimize(sphere, [0 for  _ in 1:N], NelderMead());
rosenbrock_simplex_result = optimize(rosenbrock, [0 for  _ in 1:N], NelderMead());
step_result_simplex_result = optimize(step, [0 for  _ in 1:N], NelderMead());
griewank_simplex_result = optimize(griewank, [0 for  _ in 1:N], NelderMead());
styblinski_simplex_result = optimize(styblinski_tang, [0 for  _ in 1:N], NelderMead());
sheckel_simplex_result = optimize(sheckel, [0 for  _ in 1:N], NelderMead());
rastrigin_simplex_result = optimize(rastrigin, [0 for  _ in 1:N], NelderMead());
ackley_simplex_result = optimize(ackley, [0 for  _ in 1:N], NelderMead());
rotated_simplex_result = optimize(rotated_elipsoid, [0 for  _ in 1:N], NelderMead());

sphere_simulated_annealing_result = optimize(sphere, [0 for  _ in 1:N], SimulatedAnnealing());
rosenbrock_simulated_annealing_result = optimize(rosenbrock, [0 for  _ in 1:N], SimulatedAnnealing());
step_result_simulated_annealing_result = optimize(step, [0 for  _ in 1:N], SimulatedAnnealing());
griewank_simulated_annealing_result = optimize(griewank, [0 for  _ in 1:N], SimulatedAnnealing());
styblinski_simulated_annealing_result = optimize(styblinski_tang, [0 for  _ in 1:N], SimulatedAnnealing());
sheckel_simulated_annealing_result = optimize(sheckel, [0 for  _ in 1:N], SimulatedAnnealing());
rastrigin_simulated_annealing_result = optimize(rastrigin, [0 for  _ in 1:N], SimulatedAnnealing());
ackley_simulated_annealing_result = optimize(ackley, [0 for  _ in 1:N], SimulatedAnnealing());
rotated_simulated_annealing_result = optimize(rotated_elipsoid, [0 for  _ in 1:N], SimulatedAnnealing());

sphere_particle_swarm_result = optimize(sphere, [0 for  _ in 1:N], ParticleSwarm());
rosenbrock_particle_swarm_result = optimize(rosenbrock, [0 for  _ in 1:N], ParticleSwarm());
step_result_particle_swarm_result = optimize(step, [0 for  _ in 1:N], ParticleSwarm());
griewank_particle_swarm_result = optimize(griewank, [0 for  _ in 1:N], ParticleSwarm());
styblinski_particle_swarm_result = optimize(styblinski_tang, [0 for  _ in 1:N], ParticleSwarm());
sheckel_particle_swarm_result = optimize(sheckel, [0 for  _ in 1:N], ParticleSwarm());
rastrigin_particle_swarm_result = optimize(rastrigin, [0 for  _ in 1:N], ParticleSwarm());
ackley_particle_swarm_result = optimize(ackley, [0 for  _ in 1:N], ParticleSwarm());
rotated_particle_swarm_result = optimize(rotated_elipsoid, [0 for  _ in 1:N], ParticleSwarm());

# visualize

println(sphere_rand_1_result.best_score_history[end], sphere_rand_1_result.population_diversity_history[end]);
println(sphere_best_2_result.best_score_history[end], sphere_best_2_result.population_diversity_history[end]);
println(sphere_sde_result.best_score_history[end], sphere_sde_result.population_diversity_history[end]);
println(sphere_particle_swarm_result);

# end