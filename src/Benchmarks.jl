module Benchmarks

using Statistics, Random

export sphere, rosenbrock, step, griewank, styblinski, shekel, rastrigin, ackley, rotated, Keane
export sphere_b, rosenbrock_b, step_b, griewank_b, styblinski_b, shekel_b, rastrigin_b, ackley_b, rotated_b

const N, M = 16, 32

sphere(x) = sum(x.^2); sphere_b = (fill(Float64(2^12), N), fill(Float64(-2^12), N))
rosenbrock(x) = sum(100*(x[1:end-1].^2 .- x[2:end]).^2 .+ (1 .- x[1:end-1]).^2); rosenbrock_b = (fill(Float64(2^12), N), fill(Float64(-2^12), N))
step(x) = sum(floor.(x)); step_b = (fill(5.12, N), fill(-5.12, N))
griewank(x) = 1 + sum(x.^2)/4000 - prod(cos.(x ./ sqrt.(1:length(x)))); griewank_b = (fill(Float64(2^12), N), fill(Float64(-2^12), N))
styblinski(x) = sum(x.^4 .- 16*x.^2 .+ 5*x) / 2; styblinski_b = (fill(5.0, N), fill(-5.0, N))

Random.seed!(42); const A, C = rand(N, 32), rand(32)
shekel(x) = -sum(1.0 ./ (sum((x .- A).^2, dims=1)[:] .+ C)); shekel_b = (fill(Float64(2^12), N), fill(Float64(-2^12), N))

rastrigin(x) = sum(x.^2 .- 10 .* cos.(2π .* x) .+ 10); rastrigin_b = (fill(5.12, N), fill(-5.12, N))
ackley(x) = -20*exp(-0.2*sqrt(mean(x.^2))) - exp(mean(cos.(2π .* x))) + 20 + ℯ; ackley_b = (fill(65.536, N), fill(-65.536, N))
rotated(x) = sum((1:length(x)) .* x.^2); rotated_b = (fill(32.768, N), fill(-32.768, N))

struct Keane
    f::Function; c::Vector{Function}; b::Tuple{Vector{Float64}, Vector{Float64}}
    function Keane()
        fn(x) = -abs((sum(cos.(x).^4) - 2*prod(cos.(x).^2)) / (sqrt(sum(i * x[i]^2 for i in 1:length(x))) + 1e-14))
        c1(x) = (0.75 - prod(x) < 0) ? 1.0 : (0.75 - prod(x))*75
        c2(x) = (sum(x) - 7.5*length(x) < 0) ? 1.0 : (sum(x) - 7.5*length(x))*75
        new(fn, [c1, c2], (fill(10.0, N), fill(0.0, N)))
    end
end

end
