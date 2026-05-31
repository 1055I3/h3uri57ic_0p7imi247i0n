module Benchmarks

using Statistics, Random

export sphere, rosenbrock, step_func, griewank, styblinski, shekel, rastrigin, ackley, rotated, Keane
export sphere_b, rosenbrock_b, step_b, griewank_b, styblinski_b, shekel_b, rastrigin_b, ackley_b, rotated_b

# Standard dimensions from bad_stuff
const N, M = 16, 32

sphere(x) = sum(x.^2); sphere_b = (fill(1e10, N), fill(-1e10, N))

rosenbrock(x) = sum(100*(x[1:end-1].^2 .- x[2:end]).^2 .+ (1 .- x[1:end-1]).^2); rosenbrock_b = (fill(100.0, N), fill(-100.0, N))

# Renamed to avoid Base collision while keeping logic intact
step_func(x) = sum(floor.(x)); step_b = (fill(5.12, N), fill(-5.12, N))

griewank(x) = 1 + sum(x.^2)/4000 - prod(cos.(x ./ sqrt.(1:length(x)))); griewank_b = (fill(600.0, N), fill(-600.0, N))

styblinski(x) = sum(x.^4 .- 16*x.^2 .+ 5*x) / 2; styblinski_b = (fill(5.0, N), fill(-5.0, N))

# Restored Shekel from bad_stuff parameters
Random.seed!(42)
const A_SHEKEL = rand(N, 32)
const C_SHEKEL = rand(32)
shekel(x) = -sum(1.0 ./ (sum((x .- A_SHEKEL).^2, dims=1)[:] .+ C_SHEKEL)); shekel_b = (fill(10.0, N), fill(-10.0, N))

rastrigin(x) = sum(x.^2 .- 10 .* cos.(2π .* x) .+ 10); rastrigin_b = (fill(5.12, N), fill(-5.12, N))

ackley(x) = -20*exp(-0.2*sqrt(mean(x.^2))) - exp(mean(cos.(2π .* x))) + 20 + ℯ; ackley_b = (fill(32.768, N), fill(-32.768, N))

rotated(x) = sum((1:length(x)) .* x.^2); rotated_b = (fill(65.536, N), fill(-65.536, N))

struct Keane
    f::Function; c::Vector{Function}; b::Tuple{Vector{Float64}, Vector{Float64}}
    function Keane()
        fn(x) = -abs((sum(cos.(x).^4) - 2*prod(cos.(x).^2)) / (sqrt(sum(i * x[i]^2 for i in 1:length(x))) + 1e-15))
        c1(x) = (0.75 - prod(x) < 0) ? 1.0 : (0.75 - prod(x))*75.0
        c2(x) = (sum(x) - 7.5*length(x) < 0) ? 1.0 : (sum(x) - 7.5*length(x))*75.0
        new(fn, [c1, c2], (fill(10.0, N), fill(0.0, N)))
    end
end

end
