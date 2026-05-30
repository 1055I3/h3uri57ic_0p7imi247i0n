# ==============================================================================
# AMBITIOUS AIRFOIL SHAPE OPTIMIZATION STUDY
# Objective: Maximize Multi-Condition L/D with Robustness Constraints
# Optimizer: Generic Differential Evolution (implemented in differential_evolution.jl)
# ==============================================================================

using LinearAlgebra
using Statistics
using Random
using Plots
using Base.Threads

# Ensure the core implementation is available
include("differential_evolution.jl")

# --- Engineering Constants and Configuration ---
const RE_CRUISE = 3.0e6      # Reynolds Number (Cruise)
const AOAS = [-2.0, 0.0, 2.0, 4.0, 6.0, 8.0, 10.0] # Operating angles
const WEIGHTS = [0.05, 0.15, 0.35, 0.20, 0.10, 0.10, 0.05] # Multi-point weights
const N_SAMPLES = 1200       # Dense chord discretization points
const MC_SAMPLES = 8         # Monte Carlo samples for manufacturing robustness
const TOL = 1e-14

# --- Geometric Helper: Cubic Spline Interpolation ---
# Implemented from scratch to avoid dependency issues while maintaining realism.
struct SimpleSpline
    x::Vector{Float64}
    y::Vector{Float64}
    M::Vector{Float64}
end

function build_spline(x, y)
    n = length(x)
    h = diff(x)
    λ = [h[i] / (h[i] + h[i-1]) for i in 2:n-1]
    μ = 1.0 .- λ
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
    i = idx - 1
    h = s.x[i+1] - s.x[i]
    a = (s.x[i+1] - xi) / h
    b = 1.0 - a
    return a * s.y[i] + b * s.y[i+1] + (h^2/6.0) * ( (a^3 - a)*s.M[i] + (b^3 - b)*s.M[i+1] )
end

# --- Airfoil Geometry Model ---

struct AirfoilGeometry
    x::Vector{Float64}
    yc::Vector{Float64}   # Camber line
    yt::Vector{Float64}   # Thickness distribution
    yu::Vector{Float64}   # Upper surface
    yl::Vector{Float64}   # Lower surface
    dyc_dx::Vector{Float64}
    curvature::Vector{Float64}
    
    # Critical Geometric Metrics
    max_t_c::Float64
    max_c_c::Float64
    x_t_max::Float64
    x_c_max::Float64
    le_radius::Float64
    te_angle::Float64
    smoothness::Float64
    valid::Bool
end

function generate_airfoil(p::Vector{Float64})
    # p[1:6]  : Camber control points
    # p[7:12] : Thickness control points
    
    cx = collect(range(0.0, 1.0, length=6))
    tx = [0.0, 0.05, 0.2, 0.4, 0.7, 1.0]
    
    cy = [0.0; p[1:4]; 0.0] # LE and TE fixed at 0 camber
    ty = [0.0; p[7:11]; 0.001] # Small finite TE thickness
    
    c_spline = build_spline(cx, cy)
    t_spline = build_spline(tx, ty)
    
    # Dense Cosine spacing (more points at LE and TE)
    θ = range(0, π, length=N_SAMPLES)
    x = 0.5 .* (1.0 .- cos.(θ))
    
    yc = [eval_spline(c_spline, xi) for xi in x]
    yt = [max(0.0, eval_spline(t_spline, xi)) for xi in x]
    
    # Slopes via Central Difference
    dyc = zeros(N_SAMPLES)
    for i in 2:N_SAMPLES-1
        dyc[i] = (yc[i+1] - yc[i-1]) / (x[i+1] - x[i-1] + TOL)
    end
    dyc[1] = (yc[2] - yc[1]) / (x[2] - x[1] + TOL)
    dyc[end] = (yc[end] - yc[end-1]) / (x[end] - x[end-1] + TOL)
    
    # Coordinate Construction
    yu = yc .+ 0.5 .* yt
    yl = yc .- 0.5 .* yt
    
    # Curvature Analysis
    ddyc = zeros(N_SAMPLES)
    for i in 2:N_SAMPLES-1
        ddyc[i] = (yc[i+1] - 2yc[i] + yc[i-1]) / ((x[i+1] - x[i-1])/2 + TOL)^2
    end
    κ = abs.(ddyc) ./ (1.0 .+ dyc.^2).^1.5
    energy = sum(κ.^2) / N_SAMPLES
    
    # LE Radius Estimate
    le_r = (yt[2]^2) / (8.0 * x[2] + TOL)
    
    # TE Angle
    te_a = atan((yu[end-5]-yu[end])/(x[end]-x[end-5] + TOL)) + atan((yl[end]-yl[end-5])/(x[end]-x[end-5] + TOL))
    
    # Monotonicity check for thickness (simple validity)
    t_max, t_idx = findmax(yt)
    c_max, c_idx = findmax(yc)
    valid = all(yt .>= 0.0) && (x[t_idx] > 0.05) && (x[t_idx] < 0.7)

    return AirfoilGeometry(x, yc, yt, yu, yl, dyc, κ, t_max, c_max, x[t_idx], x[c_idx], le_r, te_a, energy, valid)
end

# --- Aerodynamic Evaluation Model ---

function aerodynamic_eval(geom::AirfoilGeometry, α_deg::Float64, Re::Float64)
    α = deg2rad(α_deg)
    
    # 1. Thin Airfoil Theory - Zero Lift Angle Approximation
    # Approximating the integral for α0
    integral_α0 = 0.0
    for i in 1:N_SAMPLES-1
        dx = geom.x[i+1] - geom.x[i]
        # Weighting towards the leading edge (Glauert's integral)
        val = geom.dyc_dx[i] * (1.0 - cos(geom.x[i]*π)) 
        integral_α0 += val * dx
    end
    α0 = -integral_α0 
    
    # 2. Lift Coefficient (Cl) with Thickness correction
    # Cl = 2π(α - α0) * (1 + 0.77 * t/c)
    Cl = 2π * (α - α0) * (1.0 + 0.77 * geom.max_t_c)
    
    # 3. Drag Modeling (Viscous + Form + Pressure Recovery)
    # a) Skin Friction (Schlichting turbulent approximation)
    Cf = 0.455 / (log10(Re)^2.58 + TOL)
    
    # b) Form Factor (Empirical correction for thickness)
    K_form = 1.0 + 1.2*geom.max_t_c + 60.0*geom.max_t_c^4
    Cd_visc = 2.0 * Cf * K_form
    
    # c) Pressure Recovery / Separation Penalty
    # We penalize sharp slopes in the recovery region (trailing 40%)
    sep_penalty = 0.0
    for i in Int(floor(0.6*N_SAMPLES)):N_SAMPLES
        slope = (geom.yu[i] - geom.yu[max(1, i-10)]) / (geom.x[i] - geom.x[max(1, i-10)] + TOL)
        if slope < -0.18 # Conservative separation threshold
            sep_penalty += abs(slope + 0.18)^2 * 0.5
        end
    end
    
    # d) Induced-like profile drag (Quadratic with Cl)
    Cd_induced = 0.012 * Cl^2 
    
    Cd = Cd_visc + sep_penalty + Cd_induced
    
    # 4. Moment Coefficient (at AC)
    Cm_ac = -0.25 * Cl # Conceptual
    
    return Cl, max(0.0005, Cd), Cm_ac
end

# --- Objective Function ---

function airfoil_objective(p::Vector{Float64})
    # 1. Geometry Generation
    geom = generate_airfoil(p)
    
    # 2. Hard Geometric Constraints (via heavy soft penalties)
    penalty = 0.0
    if !geom.valid; penalty += 5000.0; end
    if geom.max_t_c < 0.09; penalty += (0.09 - geom.max_t_c)*5000; end # Structural min
    if geom.max_t_c > 0.16; penalty += (geom.max_t_c - 0.16)*5000; end # Drag max
    if geom.max_c_c > 0.05; penalty += (geom.max_c_c - 0.05)*10000; end # Handling max
    if geom.le_radius < 0.008; penalty += (0.008 - geom.le_radius)*20000; end # Stall behavior
    if geom.smoothness > 40.0; penalty += (geom.smoothness - 40.0)*50; end # Fairness/Smoothness
    
    # 3. Aerodynamic Evaluation (Multi-point weighted aggregation)
    avg_L_D = 0.0
    for (i, α) in enumerate(AOAS)
        cl, cd, _ = aerodynamic_eval(geom, α, RE_CRUISE)
        l_d = cl / cd
        
        # Severe penalty for negative lift at positive AoA
        if cl < 0 && α > 0; l_d -= 100.0; end
        # Penalty for low lift coefficients at cruise
        if α == 4.0 && cl < 0.3; l_d -= 50.0; end
        
        avg_L_D += l_d * WEIGHTS[i]
    end
    
    # 4. Robustness Analysis (Monte Carlo Jitter)
    # Evaluates sensitivity to small geometric deformations
    robustness_score = 0.0
    for _ in 1:MC_SAMPLES
        p_err = p .+ randn(length(p)) .* 0.0002 # 0.02% 제조 error simulation
        g_err = generate_airfoil(p_err)
        cl_e, cd_e, _ = aerodynamic_eval(g_err, 4.0, RE_CRUISE)
        robustness_score += abs((cl_e/cd_e) - avg_L_D)
    end
    rob_penalty = (robustness_score / MC_SAMPLES) * 1.5
    
    return -avg_L_D + penalty + rob_penalty
end

# --- Engineering Study Execution ---

function run_airfoil_study()
    println("================================================================")
    println("   AIRFOIL SHAPE OPTIMIZATION: L/D MAXIMIZATION STUDY")
    println("================================================================")
    println("Problem Dimension: 12 Design Variables")
    println("Discretization:    $(N_SAMPLES) points per surface")
    println("Conditions:        Multi-point (AoA -2 to 10 deg)")
    println("Robustness:        8-sample Monte Carlo Manufacturing Simulation")
    println("Parallelism:       Active (Threads: $(Threads.nthreads()))")
    
    # Design Variable Bounds: 6 Camber Points, 6 Thickness Points
    lb = [fill(-0.01, 6); fill(0.005, 6)]
    ub = [fill(0.06, 6); fill(0.18, 6)]
    
    t0 = time()
    
    # Optimization Setup using the restored DE engine
    # We use AGS (Adaptive Genetic Scaling) for this complex landscape.
    ζ = run_differential_evolution(
        airfoil_objective, 
        [], 
        ub, lb, 
        48,                     # Population Size
        MaxIterStop(200),        # Max Generations
        BinomialCrossover(0.85), 
        AGS()                   # Self-adaptive selection/mutation
    )
    
    runtime = time() - t0
    best_p = ζ.β_hist[end]
    best_geom = generate_airfoil(best_p)
    
    # --- Detailed Reporting ---
    println("\nOPTIMIZATION RESULTS:")
    println("-"^30)
    println("Total Runtime:      ", round(runtime, digits=2), " seconds")
    println("Total Evaluations:  ", ζ.ε[])
    println("Best Score (-L/D):  ", round(ζ.φ_hist[end], digits=4))
    println("-"^30)
    println("Max Thickness:      ", round(best_geom.max_t_c * 100, digits=2), "% at x/c = ", round(best_geom.x_t_max, digits=3))
    println("Max Camber:         ", round(best_geom.max_c_c * 100, digits=2), "% at x/c = ", round(best_geom.x_c_max, digits=3))
    println("LE Radius:          ", round(best_geom.le_radius, digits=6))
    println("TE Angle:           ", round(rad2deg(best_geom.te_angle), digits=2), "°")
    println("Surface Smoothness: ", round(best_geom.smoothness, digits=4))
    
    # --- Professional Visualization ---
    println("\nRendering Engineering Visualization...")
    
    # 1. Geometry Plot
    plt_geom = plot(best_geom.x, [best_geom.yu, best_geom.yl], 
                    label=["Upper Surface" "Lower Surface"], color=[:blue :blue], 
                    fillrange=[best_geom.yl, best_geom.yu], fillalpha=0.1,
                    aspect_ratio=:equal, lw=1.5,
                    title="Optimized Airfoil Geometry", xlabel="x/c", ylabel="y/c")
    plot!(plt_geom, best_geom.x, best_geom.yc, label="Mean Camber", color=:red, linestyle=:dash)
    
    # 2. Thickness Distribution
    plt_thick = plot(best_geom.x, best_geom.yt, title="Thickness Distribution", 
                     xlabel="x/c", ylabel="t/c", color=:green, fillrange=0, fillalpha=0.1, lw=1.5)
    
    # 3. Curvature Distribution (LE focus)
    plt_curv = plot(best_geom.x, best_geom.curvature, title="Surface Curvature", 
                    xlabel="x/c", ylabel="κ", color=:purple, yscale=:log10, lw=1.2)
    
    # 4. Performance Envelopes
    alphas = -4:0.25:12
    cl_v, cd_v, ld_v = Float64[], Float64[], Float64[]
    for a in alphas
        l, d, _ = aerodynamic_eval(best_geom, a, RE_CRUISE)
        push!(cl_v, l); push!(cd_v, d); push!(ld_v, l/d)
    end
    
    plt_ld = plot(alphas, ld_v, title="L/D Envelope", xlabel="α (deg)", ylabel="L/D", color=:orange, lw=2)
    plt_polar = plot(cd_v, cl_v, title="Drag Polar", xlabel="Cd", ylabel="Cl", color=:black, lw=2)
    
    final_plt = plot(plt_geom, plt_thick, plt_curv, plt_ld, plt_polar, 
                     layout=(3,2), size=(1200, 1000), dpi=300)
    
    savefig("airfoil_optimization_results.png")
    println("Success. Results saved to 'airfoil_optimization_results.png'.")
end

# Execute the study
if abspath(PROGRAM_FILE) == @__FILE__
    run_airfoil_study()
end
