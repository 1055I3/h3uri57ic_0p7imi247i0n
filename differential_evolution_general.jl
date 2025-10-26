# define variable types

using Random
using Distributions
using Optim

const seed::Int64 = 42;
Random.seed!(seed);

check_bounds(lower_bound::T, upper_bound::T) where {T<:Number} = upper_bound < lower_bound && error("1D10T :: LOWER BOUND $(lower_bound) GREATER THAN THE UPPER BOUND $(upper_bound)\n");

# the heuristic

abstract type Stats end

mutable struct GenLimit <: Stats
    value::Int64;
    limit::Int64;

    function GenLimit(limit::Int64)
        starting_generation::Int64 = 0;

        new(starting_generation, limit);
    end
end

function stopping_condition!(stats::GenLimit)
    stop = stats.value < stats.limit;
    stats.value += 1;

    return stop;
end

function update_stats!(_, _)
    return nothing;
end

using Base.Iterators: flatten, partition
using Base.Threads: nthreads, @spawn

function differential_evolution_generic(objective::Function,
                                        constraint_function::Function,
                                        bounds::Matrix{<:Number},
                                        population::Matrix{<:Number}, # Matrix{Union{}}
                                        stopping_condition!::Function,
                                        selection::Function,
                                        crossover::Function,
                                        mutate::Function,
                                        evaluate::Function)
    scores = evaluate.(population);
    stats::Stats = Stats();

    # TODO: check scores for stopping condition - keep the scores in stats? update stats function with multiple methods?
    while stopping_condition!(stats)
        @threads for (i, x) in enumerate(population)
            individuals = selection(population); # select
            d, u = crossover(x); # crossover
            v = mutate(x, individuals, d, u); # mutate

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
    end

        update_stats!(population, scores);
    end

    return population, evaluate.(population) # TODO: fix this to return population and eval like tuple or the best and eval tuple, or some n of the best with evals
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
