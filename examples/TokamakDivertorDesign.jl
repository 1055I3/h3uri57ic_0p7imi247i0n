# ==============================================================================
# TOKAMAK OPERATING-POINT & DIVERTOR DESIGN OPTIMIZATION
# Objective: Maximize Fusion Confinement Performance (nTτ)
# Optimizer: Restoration-Grade Differential Evolution Framework
# ==============================================================================

using LinearAlgebra
using Statistics
using Random
using Plots
using Base.Threads

# Import the professional framework (DifferentialEvolution.jl)
include("../src/DifferentialEvolution.jl")
using .DifferentialEvolution

# --- Physics Constants ---
const μ0 = 4π * 1e-7
const m_p = 1.67e-27      # Proton mass
const e = 1.602e-19       # Electron charge
const λ_q_coeff = 1e-3    # Scaling for power scrape-off width (m)
const σ_boltz = 5.67e-8   # Stefan-Boltzmann

# --- Problem Configuration ---
const N_PARAMS = 14
const MC_SAMPLES = 10     # Manufacturing/Control robustness samples
const TOL = 1e-15

# --- Plasma Diagnostics Structure ---

struct TokamakState
    R::Float64; a::Float64; κ::Float64; δ::Float64; Ip::Float64; Bt::Float64; ne::Float64
    P_aux::Float64; f_edge::Float64; f_exp::Float64; R_sp::Float64; L_leg::Float64; f_rad::Float64; q_target::Float64
    
    # Derived Quantities
    ε::Float64; V::Float64; S::Float64; q95::Float64; n_GW::Float64
    β_N::Float64; τ_E::Float64; W::Float64; P_loss::Float64; P_fus_proxy::Float64
    q_peak::Float64; valid::Bool
end

# --- The Physics Model ---

function evaluate_tokamak_physics(p::Vector{Float64})
    # 1. Parameter Mapping
    # R [4.0, 8.5], a [1.0, 2.5], κ [1.5, 2.1], δ [0.2, 0.7]
    # Ip [5.0, 18.0], Bt [3.0, 9.0], ne [0.5, 2.0]
    # P_aux [20.0, 120.0], f_edge [0.3, 0.8], f_exp [5.0, 50.0]
    # R_sp_off [-0.5, 0.5], L_leg [0.5, 3.0], f_rad [0.1, 0.9], q_scale [1.0, 3.0]
    R, a, κ, δ = p[1], p[2], p[3], p[4]
    Ip, Bt = p[5], p[6]
    ne = p[7]       # line-averaged density in 10^20 m^-3
    P_aux = p[8]    # auxiliary heating power in MW
    f_edge = p[9]   # edge density fraction
    f_exp = p[10]   # flux expansion
    R_sp = R + p[11] * a # strike point relative to minor radius
    L_leg = p[12]   # divertor leg length
    f_rad = p[13]   # radiative power fraction
    q_scale = p[14] # profile peaking factor
    
    # 2. Geometry
    ε = a / R
    # Plasma Volume (Elliptical approximation with triangularity correction)
    V = 2π^2 * R * a^2 * κ * (1.0 - 0.1*δ)
    # Surface Area
    S = 4π^2 * R * a * sqrt((1.0 + κ^2)/2.0)
    
    # 3. MHD Stability & Limits
    # Safety Factor q95 (Uckan approximation for elongated plasmas)
    q95 = (5.0 * a^2 * Bt / (R * Ip)) * ((1.0 + κ^2*(1.0 + 2.0*δ^2 - 1.2*δ^3))/2.0) * ((1.17 - 0.65*ε)/(1.0 - ε^2)^2)
    # Greenwald Density Limit
    n_GW = Ip / (π * a^2) # units of 10^20 m^-3
    
    # 4. Energy Confinement (IPB98(y,2) Scaling)
    # Standard ITER H-mode scaling law
    # P_sep is the net heating power crossing the separatrix
    P_sep = max(1.0, P_aux * (1.0 - f_rad))
    τ_E = 0.0562 * (Ip^0.93) * (Bt^0.15) * (ne^0.41) * (P_sep^-0.69) * (R^1.97) * (κ^0.78) * (ε^0.58)
    
    # 5. Power Balance & Fusion Proxy
    W = P_sep * τ_E # Stored energy (MJ)
    # Average thermal pressure
    pressure = (W * 1e6) / V 
    # Normalized Beta (Troyon limit proxy)
    β_tot = (2.0 * μ0 * pressure) / (Bt^2 + TOL)
    β_N = β_tot * 100.0 / (Ip / (a * Bt) + TOL)
    
    # Fusion Performance Proxy (based on pressure-squared scaling)
    P_fus_proxy = 0.1 * pressure^2 * V
    
    # 6. Divertor Heat Load Modeling
    # SOL power width λq (Eich-style scaling)
    λq = λ_q_coeff * (Bt^-0.8) * (q95^1.1) * (R^0.1)
    # Parallel heat flux upstream
    q_par = (P_sep * 1e6) / (4π * R * λq * (a/R))
    # Target heat flux (Geometric projection + Flux expansion)
    θ_strike = atan(L_leg / (max(0.1, R_sp - R)))
    q_peak = (q_par * sin(θ_strike)) / (f_exp + TOL)
    
    # 7. Operational Validity
    valid = (q95 > 2.0) && (ne / n_GW < 1.2) && (β_N < 4.0) && (a > 0.2) && (R > a)

    return TokamakState(R, a, κ, δ, Ip, Bt, ne, P_aux, f_edge, f_exp, R_sp, L_leg, f_rad, q_scale,
                        ε, V, S, q95, n_GW, β_N, τ_E, W, P_sep, P_fus_proxy, q_peak, valid)
end

# --- Objective Function ---

function tokamak_objective(p::Vector{Float64})
    # 1. Physics Analysis
    s = evaluate_tokamak_physics(p)
    
    # 2. Comprehensive Penalty System
    # Massive penalties for unphysical or unstable regimes
    P = 0.0
    if !s.valid; P += 100000.0; end
    
    # Greenwald limit: avoid density disruptions
    if s.ne > s.n_GW; P += (s.ne/s.n_GW - 1.0)*20000; end
    # Troyon limit: avoid beta collapses
    if s.β_N > 3.2; P += (s.β_N - 3.2)*15000; end
    # Stability margin for safety factor
    if s.q95 < 3.0; P += (3.0 - s.q95)*12000; end
    # Engineering heat load limit (Targeting < 10 MW/m^2)
    if s.q_peak > 10.0; P += (s.q_peak - 10.0)*5000; end
    # Structural aspect ratio limit
    if s.ε > 0.45; P += (s.ε - 0.45)*25000; end
    
    # 3. Performance Metric
    # Fusion gain proxy weighted by confinement time
    performance = s.P_fus_proxy * s.τ_E
    
    # 4. Robustness Analysis (Monte Carlo Jitter)
    # Penalize operating points that are sensitive to small perturbations
    jitter_variance = 0.0
    for _ in 1:MC_SAMPLES
        p_j = p .* (1.0 .+ 0.01 .* randn(N_PARAMS))
        sj = evaluate_tokamak_physics(p_j)
        jitter_variance += (sj.P_fus_proxy - s.P_fus_proxy)^2
    end
    P_rob = sqrt(jitter_variance / MC_SAMPLES) * 1.0
    
    # Differential Evolution minimizes, so we negate performance
    return -performance + P + P_rob
end

# --- Reporting & Visualization ---

function report_tokamak_results(ζ::ζ_Stats, best_p::Vector{Float64})
    s = evaluate_tokamak_physics(best_p)
    
    println("\n" * "="^60)
    println("TOKAMAK OPERATING POINT & DIVERTOR OPTIMIZATION REPORT")
    println("="^60)
    println("Engineering Geometry:")
    println("  - Major Radius R:     ", round(s.R, digits=2), " m")
    println("  - Minor Radius a:     ", round(s.a, digits=2), " m")
    println("  - Aspect Ratio ε:     ", round(s.ε, digits=3))
    println("  - Elongation κ:       ", round(s.κ, digits=2))
    println("  - Triangularity δ:    ", round(s.δ, digits=2))
    
    println("\nMHD & Stability:")
    println("  - Safety Factor q95:  ", round(s.q95, digits=2))
    println("  - Greenwald Frac:     ", round(s.ne/s.n_GW, digits=2))
    println("  - Normalized Beta:    ", round(s.β_N, digits=2))
    
    println("\nPerformance Envelopes:")
    println("  - Confinement τ_E:    ", round(s.τ_E, digits=3), " s")
    println("  - Stored Energy W:    ", round(s.W, digits=2), " MJ")
    println("  - Fusion Power Proxy: ", round(s.P_fus_proxy, digits=2), " a.u.")
    
    println("\nDivertor Heat Flux:")
    println("  - Peak Heat Flux:     ", round(s.q_peak, digits=2), " MW/m^2")
    println("  - Flux Expansion:     ", round(s.f_exp, digits=1))
    println("  - Radiative Frac:     ", round(s.f_rad * 100, digits=1), "%")
    println("-"^60)

    # Visualization
    println("Generating diagnostic plots...")
    p1 = plot(1:length(ζ.φ_hist), -ζ.φ_hist, title="Convergence", xlabel="Iteration", ylabel="Fusion Performance", lw=2, color=:blue)
    p2 = bar(1:N_PARAMS, best_p, title="Optimal Design Vector", xlabel="Parameter ID", ylabel="Value", legend=false, color=:orange)
    
    # Sensitivity sample
    bs, ts = Float64[], Float64[]
    for _ in 1:150
        p_r = best_p .* (1.0 .+ 0.04 .* randn(N_PARAMS))
        sr = evaluate_tokamak_physics(p_r)
        push!(bs, sr.β_N); push!(ts, sr.τ_E)
    end
    p3 = scatter(bs, ts, title="Sensitivity Map (Beta vs Tau)", xlabel="β_N", ylabel="τ_E (s)", alpha=0.6, marker=:circle)
    p4 = plot(1:length(ζ.δ_hist), ζ.δ_hist, title="Population Diversity", xlabel="Iteration", ylabel="Avg Dist", color=:green)
    
    study_plt = plot(p1, p2, p3, p4, layout=(2,2), size=(1100, 900))
    savefig("Tokamak_Optimization_Study.png")
    println("Success. Diagnostic visual saved to 'Tokamak_Optimization_Study.png'.")
end

function main()
    println("Launching Tokamak Operation-Point Optimization (14 Design Variables)...")
    
    # Parameter Search Space Bounds
    # [R, a, κ, δ, Ip, Bt, ne, P_aux, f_edge, f_exp, R_sp_off, L_leg, f_rad, q_scale]
    lb = [4.0, 1.0, 1.5, 0.2, 5.0,  3.0, 0.5, 20.0, 0.3, 5.0,  -0.5, 0.5, 0.1, 1.0]
    ub = [8.5, 2.5, 2.1, 0.7, 18.0, 9.0, 2.5, 120.0, 0.8, 50.0, 0.5, 3.0, 0.9, 3.0]
    
    t0 = time()
    # Execute Optimization using Restored Legacy API
    ζ = ags_rand_1_max_ι(tokamak_objective, [], ub, lb, 4 * N_PARAMS, 250)
    
    report_tokamak_results(ζ, ζ.β_hist[end])
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
