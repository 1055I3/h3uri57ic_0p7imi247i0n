# define variable types

abstract type Variable end

struct Discrete <: Variable # Discrete
    value::Int64;
    lower_bound::Int64;
    upper_bound::Int64;

    function Discrete(lower_bound::Int64, upper_bound::Int64)
        check_bounds(lower_bound, upper_bound);

        new(rand(lower_bound:upper_bound), lower_bound, upper_bound);
    end

    function Discrete(value::Int64, lower_bound::Int64, upper_bound::Int64)
        check_bounds(lower_bound, upper_bound);
        check_bounds(lower_bound, value);
        check_bounds(value, upper_bound);

        new(value, lower_bound, upper_bound);
    end
end

struct Continuous <: Variable
    value::Float64;
    lower_bound::Float64;
    upper_bound::Float64;

    function Continuous(lower_bound::Float64, upper_bound::Float64)
        check_bounds(lower_bound, upper_bound);

        value::Float64 = rand()*(upper_bound - lower_bound) + lower_bound;
        new(value, lower_bound, upper_bound);
    end

    function Continuous(value::Float64, lower_bound::Float64, upper_bound::Float64)
        check_bounds(lower_bound, upper_bound);
        check_bounds(lower_bound, value);
        check_bounds(value, upper_bound);
        
        new(value, lower_bound, upper_bound)
    end
end

# TODO: kill this
# struct Categorical <: Variable # implement categorical
#     value::UInt64;
#     domain::Tuple{String};

#     function Categorical(domain::Tuple{String})
#         value = findfirst(x -> x == rand(domain), domain);

#         new(value, domain);
#     end
# end

# helper functions

check_bounds(lower_bound::T, upper_bound::T) where {T<:Number} = upper_bound < lower_bound && error("1D10T :: LOWER BOUND $(lower_bound) GREATER THAN THE UPPER BOUND $(upper_bound)\n");

# define operations

# TODO: write generic functions that will do the the bound checking and return random val if out of bounds

function Base.(*)(first::Float64, second::Discrete)
    x = first * second.value;

    return Discrete(second.lower_bound <= x && x <= second.upper_bound ? x : rand(second.lower_bound:second.upper_bound), # check discrete # overload comparison ops
                    second.lower_bound,
                    second.upper_bound);
end

function Base.(*)(first::Float64, second::Continuous)
    x = first * second.value;

    return Continuous(second.lower_bound <= x && x <= second.upper_bound ? x : rand()*(first.upper_bound - first.lower_bound) + first.lower_bound, # check Continuous
                      second.lower_bound,
                      second.upper_bound);
end

function Base.(+)(first::Discrete, second::Discrete)
    (first.lower_bound ≠ second.lower_bound || first.upper_bound ≠ second.upper_bound) && error("1D10T :: ONLY THE VALUES DEFINED FOR THE SAME BOUNDS CAN BE ADDED\n");

    x = first.value + second.value();

    return Discrete(first.lower_bound <= x && x <= first.upper_bound ? x : rand(first.lower_bound:first.upper_bound),
                    first.lower_bound,
                    first.upper_bound);
end

function Base.(+)(first::Continuous, second::Continuous)
    (first.lower_bound ≆ second.lower_bound || first.upper_bound ≆ second.upper_bound) && error("1D10T :: ONLY THE VALUES DEFINED FOR THE SAME BOUNDS CAN BE ADDED\n");

    x = first.value + second.value();

    return Discrete(first.lower_bound <= x && x <= first.upper_bound ? x : rand()*(first.upper_bound - first.lower_bound) + first.lower_bound,
                    first.lower_bound,
                    first.upper_bound);
end

function Base.(-)(first::Discrete, second::Discrete)
    (first.lower_bound ≠ second.lower_bound || first.upper_bound ≠ second.upper_bound) && error("1D10T :: ONLY THE VALUES DEFINED FOR THE SAME BOUNDS CAN BE ADDED\n");

    x = first.value - second.value();

    return Discrete(first.lower_bound <= x && x <= first.upper_bound ? x : rand(first.lower_bound:first.upper_bound),
                    first.lower_bound,
                    first.upper_bound);
end

function Base.(-)(first::Continuous, second::Continuous)
    (first.lower_bound ≆ second.lower_bound || first.upper_bound ≆ second.upper_bound) && error("1D10T :: ONLY THE VALUES DEFINED FOR THE SAME BOUNDS CAN BE ADDED\n");

    x = first.value - second.value();

    return Discrete(first.lower_bound <= x && x <= first.upper_bound ? x : rand()*(first.upper_bound - first.lower_bound) + first.lower_bound,
                    first.lower_bound,
                    first.upper_bound);
end

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

function update_stats!(_)
    return nothing;
end

using Base.Iterators: flatten, partition
using Base.Threads: nthreads, @spawn

function differential_evolution_generic(population::Matrix{<:Variable}, # Matrix{Union{}}
                                        selection::Function,
                                        crossover::Function,
                                        mutate::Function,
                                        evaluate::Function,
                                        stats::Stats; # fix this abstract horror, functions too # first make everything else work
                                        tasks_per_thread::Int64 = 2)
    cohort_size = max(1, length(population) ÷ (tasks_per_thread * nthreads()));
    scores = evaluate.(population);

    # TODO: check scores for stopping condition - keep the scores in stats? update stats function with multiple methods?
    while stopping_condition!(stats)
        cohorts = partition(population, cohort_size);
        cohorts_scores = partition(scores, cohort_size);

        tasks = map(cohorts, cohorts_scores) do cohort, cohort_scores
            @spawn begin
                new_cohort = similar(cohort);
                new_cohort_scores = similar(cohort_scores);

                for (i, x) in enumerate(cohort)
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

                return new_cohort, new_cohort_scores;
            end
        end

        new_cohorts, new_scores = fetch.(tasks);
        population = (collect ∘ flatten)(new_cohorts);
        scores = (collect ∘ flatten)(new_scores);
        update_stats!(scores);
    end

    return population, evaluate.(population) # TODO: fix this to return population and eval like tuple or the best and eval tuple, or some n of the best with evals
end

# TODO: kill this and use the struct instead
# function differential_evolution_generations_limit_generic(population::Matrix{<:Variable},
#                                                           gen_limit::UInt64,
#                                                           selection::Function,
#                                                           crossover::Function,
#                                                           mutate::Function)
#     function number_of_generations_limit(gen_limit::UInt64)
#         condition = generation < gen_limit;
#         generation += 1;

#         return condition
#     end

#     generation::UInt64 = 0;

#     return differential_evolution_generic(population,
#                                           ()->number_of_generations_limit(gen_limit),
#                                           selection,
#                                           crossover,
#                                           mutate)
# end

# TODO: writie initialization
function initialize()
    _
end

# function DE_rand_1_bin(...)
function crossover(p::Float64,
                   individual::Vector{<:Variable})
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


# end
