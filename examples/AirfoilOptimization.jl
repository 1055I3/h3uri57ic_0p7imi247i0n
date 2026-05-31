# ==============================================================================
# AMBITIOUS AIRFOIL SHAPE OPTIMIZATION STUDY
# ==============================================================================

using LinearAlgebra
using Statistics
using Random
using Plots
using Base.Threads

# Use the package source
include("../src/DifferentialEvolution.jl")
using .DifferentialEvolution
include("../src/Benchmarks.jl")
using .Benchmarks

# --- Engineering Constants ---
const RE_CRUISE = 3.0e6      
const AOAS = [-2.0, 0.0, 2.0, 4.0, 6.0, 8.0, 10.0] 
const WEIGHTS = [0.05, 0.15, 0.35, 0.20, 0.10, 0.10, 0.05] 
const N_SAMPLES = 1200       
const MC_SAMPLES = 8         
const TOL = 1e-14

# --- Geometric Helper: Cubic Spline ---
struct SimpleSpline
    x::Vector{Float64}; y::Vector{Float64}; M::Vector{Float64}
end

function build_spline(x, y)
    n = length(x); h = diff(x)
    λ = [h[i] / (h[i] + h[i-1]) for i in 2:n-1]; μ = 1.0 .- λ
    g = [6.0 * ((y[i+1]-y[i])/h[i] - (y[i]-y[i-1])/h[i-1]) / (h[i]+h[i-1]) for i in 2:n-1]
    A = zeros(n-2, n-2)
    for i in 1:n-2
        A[i, i] = 2.0
        if i > 1; A[i, i-1] = μ[i]; end
        if i < n-2; A[i, i+1] = λ[i]; end
    end
    M_inner = A \ g
    return SimpleSpline(x, y, [0.0; M_inner; 0.0])
end

function eval_spline(s::SimpleSpline, xi)
    idx = findfirst(v -> v >= xi, s.x)
    idx === nothing && return s.y[end]
    idx == 1 && return s.y[1]
    i = idx - 1; h = s.x[i+1] - s.x[i]
    a = (s.x[i+1] - xi) / h; b = 1.0 - a
    return a * s.y[i] + b * s.y[i+1] + (h^2/6.0) * ( (a^3 - a)*s.M[i] + (b^3 - b)*s.M[i+1] )
end

# --- Airfoil Model ---
struct AirfoilGeometry
    x::Vector{Float64}; yc::Vector{Float64}; yt::Vector{Float64}; yu::Vector{Float64}; yl::Vector{Float64}
    dyc_dx::Vector{Float64}; curvature::Vector{Float64}
    max_t_c::Float64; max_c_c::Float64; x_t_max::Float64; x_c_max::Float64
    le_radius::Float64; te_angle::Float64; smoothness::Float64; valid::Bool
end

function generate_airfoil(p::Vector{Float64})
    cx = collect(range(0.0, 1.0, length=6)); tx = [0.0, 0.05, 0.2, 0.4, 0.7, 1.0]
    cy = [0.0; p[1:4]; 0.0]; ty = [0.0; p[7:11]; 0.001]
    c_s = build_spline(cx, cy); t_s = build_spline(tx, ty)
    θ = range(0, π, length=N_SAMPLES); x = 0.5 .* (1.0 .- cos.(θ))
    yc = [eval_spline(c_s, xi) for xi in x]; yt = [max(0.0, eval_spline(t_s, xi)) for xi in x]
    dyc = zeros(N_SAMPLES)
    for i in 2:N_SAMPLES-1; dyc[i] = (yc[i+1] - yc[i-1]) / (x[i+1] - x[i-1] + TOL); end
    dyc[1] = (yc[2] - yc[1]) / (x[2] - x[1] + TOL); dyc[end] = (yc[end] - yc[end-1]) / (x[end] - x[end-1] + TOL)
    yu, yl = yc .+ 0.5 .* yt, yc .- 0.5 .* yt
    ddyc = zeros(N_SAMPLES)
    for i in 2:N_SAMPLES-1; ddyc[i] = (yc[i+1] - 2yc[i] + yc[i-1]) / ((x[i+1] - x[i-1])/2 + TOL)^2; end
    κ = abs.(ddyc) ./ (1.0 .+ dyc.^2).^1.5; energy = sum(κ.^2) / N_SAMPLES
    le_r = (yt[2]^2) / (8.0 * x[2] + TOL); te_a = atan((yu[end-5]-yu[end])/(x[end]-x[end-5] + TOL)) + atan((yl[end]-yl[end-5])/(x[end]-x[end-5] + TOL))
    t_m, t_i = findmax(yt); c_m, c_i = findmax(yc); valid = all(yt .>= 0.0) && (x[t_i] > 0.05) && (x[t_i] < 0.7)
    return AirfoilGeometry(x, yc, yt, yu, yl, dyc, κ, t_m, c_m, x[t_i], x[c_i], le_r, te_a, energy, valid)
end

function aerodynamic_eval(geom::AirfoilGeometry, α_deg::Float64, Re::Float64)
    α = deg2rad(α_deg); integral_α0 = 0.0
    for i in 1:N_SAMPLES-1; integral_α0 += (geom.dyc_dx[i] * (1.0 - cos(geom.x[i]*π))) * (geom.x[i+1] - geom.x[i]); end
    α0 = -integral_α0; Cl = 2π * (α - α0) * (1.0 + 0.77 * geom.max_t_c)
    Cf = 0.455 / (log10(Re)^2.58 + TOL); Cd_v = 2.0 * Cf * (1.0 + 1.2*geom.max_t_c + 60.0*geom.max_t_c^4)
    sep = 0.0; for i in Int(floor(0.6*N_SAMPLES)):N_SAMPLES
        slope = (geom.yu[i] - geom.yu[max(1, i-10)]) / (geom.x[i] - geom.x[max(1, i-10)] + TOL)
        if slope < -0.18; sep += abs(slope + 0.18)^2 * 0.5; end
    end
    Cd = Cd_v + sep + 0.012 * Cl^2
    return Cl, max(0.0005, Cd), -0.25 * Cl
end

function airfoil_objective(p::Vector{Float64})
    geom = generate_airfoil(p); penalty = 0.0
    if !geom.valid; penalty += 5000.0; end
    if geom.max_t_c < 0.09; penalty += (0.09 - geom.max_t_c)*5000; end
    if geom.max_t_c > 0.16; penalty += (geom.max_t_c - 0.16)*5000; end
    if geom.max_c_c > 0.05; penalty += (geom.max_c_c - 0.05)*10000; end
    if geom.le_radius < 0.008; penalty += (0.008 - geom.le_radius)*20000; end
    if geom.smoothness > 40.0; penalty += (geom.smoothness - 40.0)*50; end
    avg_L_D = 0.0
    for (i, α) in enumerate(AOAS)
        cl, cd, _ = aerodynamic_eval(geom, α, RE_CRUISE); l_d = cl / cd
        if cl < 0 && α > 0; l_d -= 100.0; end
        if α == 4.0 && cl < 0.3; l_d -= 50.0; end
        avg_L_D += l_d * WEIGHTS[i]
    end
    robust = 0.0; for _ in 1:MC_SAMPLES
        g_e = generate_airfoil(p .+ randn(length(p)) .* 0.0002)
        cl_e, cd_e, _ = aerodynamic_eval(g_e, 4.0, RE_CRUISE); robust += abs((cl_e/cd_e) - avg_L_D)
    end
    return -avg_L_D + penalty + (robust / MC_SAMPLES) * 1.5
end

function main()
    println("--- Airfoil Optimization Example ---")
    lb, ub = [fill(-0.01, 6); fill(0.005, 6)], [fill(0.06, 6); fill(0.18, 6)]
    t0 = time(); ζ = run_differential_evolution(airfoil_objective, [], ub, lb, 40, MaxIterStop(100), BinomialCrossover(0.85), AGS())
    runtime = time() - t0; best_geom = generate_airfoil(ζ.β_hist[end])
    println("\nRESULTS:\nRuntime: $(round(runtime, digits=2))s\nBest L/D: $(round(-ζ.φ_hist[end], digits=2))")
    println("Max Thickness: $(round(best_geom.max_t_c*100, digits=2))%\nMax Camber: $(round(best_geom.max_c_c*100, digits=2))%")
    
    p = plot(best_geom.x, [best_geom.yu, best_geom.yl], aspect_ratio=:equal, title="Optimized Airfoil")
    savefig("optimized_airfoil.png")
end

if abspath(PROGRAM_FILE) == @__FILE__; main(); end
