# define types

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

struct Categorical <: Variable
    value::UInt64;

    function Categorical()
        new();
    end
end

check_bounds(lower_bound::T, upper_bound::T) where {T<:Number} = upper_bound < lower_bound && error("1D10T :: LOWER BOUND $(lower_bound) GREATER THAN THE UPPER BOUND $(upper_bound)\n");

# define operations

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

# heuristic

function differential_evolution_generic(population::Matrix{<:Variable}, # Matrix{Union{}}
                                        stopping_condition::Function,
                                        selection::Function,
                                        crossover::Function,
                                        mutate::Function)
    while stopping_condition()
        for x in population
            individuals = selection(population); # select
            d, u = crossover(x); # crossover
            v = mutate(x, individuals, d, u); # mutate
            x = evaluate(x, v); # evaluate
        end
    end

    return evaluate(population)
end

function differential_evolution_generations_limit_generic(population::Matrix{<:Variable},
                                                          gen_limit::UInt64,
                                                          selection::Function,
                                                          crossover::Function,
                                                          mutate::Function)
    function number_of_generations_limit(gen_limit::UInt64)
        condition = generation < gen_limit;
        generation += 1;

        return condition
    end

    generation::UInt64 = 0;

    return differential_evolution_generic(population,
                                          ()->number_of_generations_limit(gen_limit),
                                          selection,
                                          crossover,
                                          mutate)
end

# function DE_rand_1_bin(...)
function crossover(p::Float64,
                   individual::Vector{<:Variable})
    d = rand(eachindex(individual));
    u = [p < a for a in rand(Float64, length(individual))];

    return d, u;
end

function selection(population::Matrix{<:Variable})
    while true
        sample = rand(population, 3);
        allunique(sample) && return sample;
    end
end

function mutate(ω::Float64,
                x::Vector{<:Variable},
                individuals::Tuple{Vector{<:Variable}},
                d::UInt64,
                u::Vector{Bool})
    a, b, c = individuals;

    [(u[i] || i == d) ? a[i] + ω*(b[i] - c[i]) : x[i] for i in eachindex(x)];
end


# end
