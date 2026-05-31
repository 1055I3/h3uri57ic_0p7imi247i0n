# ==============================================================================
# ELECTRICAL DISTRIBUTION NETWORK DESIGN & OPERATION OPTIMIZATION
# Objective: Minimize Lifecycle Cost & Maximize Performance (Mixed-Variable)
# Optimizer: Restoration-Grade Differential Evolution Framework (Black Box)
# ==============================================================================

using LinearAlgebra
using Statistics
using Random
using Plots
using Printf

# Import the professional framework
# We use the existing API without any modifications to the core.
include("../src/DifferentialEvolution.jl")
using .DifferentialEvolution

# --- Engineering Data: Cable Catalog ---

struct Cable
    size_name::String
    r::Float64 # Ω/km
    x::Float64 # Ω/km
    ampacity::Float64 # A
    cost_per_km::Float64 # $
end

# Realistic ACSR Conductor Catalog
const CABLE_CATALOG = [
    Cable("ACSR 1/0", 0.534, 0.402, 230.0, 5200.0),
    Cable("ACSR 2/0", 0.423, 0.391, 270.0, 6400.0),
    Cable("ACSR 4/0", 0.268, 0.372, 340.0, 8800.0),
    Cable("ACSR 266.8", 0.211, 0.360, 460.0, 11500.0),
    Cable("ACSR 336.4", 0.167, 0.350, 530.0, 14200.0),
    Cable("ACSR 477", 0.118, 0.339, 670.0, 17500.0),
    Cable("ACSR 636", 0.088, 0.328, 780.0, 21800.0),
    Cable("ACSR 795", 0.071, 0.319, 900.0, 26500.0)
]

const TRANSFORMER_RATINGS = [500.0, 1000.0, 1500.0, 2000.0, 2500.0, 3000.0] # kVA
const CAPACITOR_RATINGS = [0.0, 100.0, 200.0, 300.0, 400.0, 500.0] # kVAR

# --- Network Topology & Load Model ---

struct Branch
    from::Int; to::Int; length_km::Float64
end

struct LoadBus
    bus::Int; p_kw::Float64; q_kvar::Float64
end

const BUS_COUNT = 10
const SLACK_BUS = 1
const NOMINAL_KV = 12.47
const BASE_MVA = 1.0
const BASE_Z = (NOMINAL_KV^2) / BASE_MVA

# Radial layout: Slack -> 2 -> 3 (Reg) -> 4 -> 5 (Cap) -> 6 -> 7 (DG) -> 8 -> 9 -> 10
const BRANCHES = [
    Branch(1, 2, 2.8), Branch(2, 3, 1.5), Branch(3, 4, 1.2),
    Branch(4, 5, 1.8), Branch(5, 6, 2.2), Branch(6, 7, 1.1),
    Branch(7, 8, 1.4), Branch(8, 9, 0.9), Branch(9, 10, 1.3)
]

const LOADS = [
    LoadBus(2, 180.0, 80.0), LoadBus(3, 220.0, 110.0), LoadBus(4, 110.0, 50.0),
    LoadBus(5, 320.0, 160.0), LoadBus(6, 130.0, 70.0), LoadBus(7, 260.0, 130.0),
    LoadBus(8, 190.0, 95.0),  LoadBus(9, 150.0, 75.0), LoadBus(10, 240.0, 120.0)
]

# --- Core Physics: Iterative Load Flow ---

function run_power_flow(branch_z::Vector{ComplexF64}, p_bus::Vector{Float64}, q_bus::Vector{Float64}, slack_v::ComplexF64)
    v = fill(slack_v, BUS_COUNT)
    s_pu = complex.(p_bus, q_bus) ./ (BASE_MVA * 1000.0)
    i_branch = zeros(ComplexF64, length(BRANCHES))
    
    # Backward-Forward Sweep (BFS)
    for iter in 1:150
        v_old = copy(v)
        
        # 1. Backward Sweep: Summing currents from leaves to root
        i_node = conj.(s_pu ./ v)
        i_branch .= 0.0
        for i in length(BRANCHES):-1:1
            b = BRANCHES[i]
            i_branch[i] += i_node[b.to]
            # Add downstream branch currents
            for j in 1:length(BRANCHES)
                if BRANCHES[j].from == b.to
                    i_branch[i] += i_branch[j]
                end
            end
        end
        
        # 2. Forward Sweep: Updating voltages from root to leaves
        for i in 1:length(BRANCHES)
            b = BRANCHES[i]
            v[b.to] = v[b.from] - i_branch[i] * (branch_z[i] / BASE_Z)
        end
        
        if maximum(abs.(v .- v_old)) < 1e-7; break; end
    end
    return v, i_branch
end

# --- Professional Evaluation Framework ---

function distribution_network_evaluation(p::Vector{Float64})
    # 1. Mapping Continuous DE Vector to Discrete Engineering Choices
    # Variables 1-6: Feeder Cable Indices
    c_idx = [clamp(Int(floor(p[i])), 1, length(CABLE_CATALOG)) for i in 1:6]
    # Smooth fallback for distal branches to maintain 14 variables
    tail_idx = clamp(Int(round(mean(p[1:6]))), 1, length(CABLE_CATALOG))
    all_c_idx = [c_idx; fill(tail_idx, 3)]
    
    # Variable 7: Transformer rating
    t_idx = clamp(Int(floor(p[7])), 1, length(TRANSFORMER_RATINGS))
    t_rating = TRANSFORMER_RATINGS[t_idx]
    
    # Variable 8: Capacitor rating
    cap_idx = clamp(Int(floor(p[8])), 1, length(CAPACITOR_RATINGS))
    cap_rating = CAPACITOR_RATINGS[cap_idx]
    
    # Continuous Operating Points
    tap_ratio = p[9]        # Transformer Tap
    reg_set = p[10]         # Regulator Setpoint at Bus 3
    dg_factor = p[11]       # DG Dispatch (0 to 1)
    comp_factor = p[12]     # Capacitor Support (0 to 1)
    deadband = p[13]        # Control Deadband
    reserve = p[14]         # Reserve / Load Growth Factor
    
    # 2. Capital Cost Calculation
    cost_cap = 0.0
    branch_z = ComplexF64[]
    for i in 1:length(BRANCHES)
        cable = CABLE_CATALOG[all_c_idx[i]]
        cost_cap += cable.cost_per_km * BRANCHES[i].length_km
        branch_z_val = complex(cable.r * BRANCHES[i].length_km, cable.x * BRANCHES[i].length_km)
        push!(branch_z, branch_z_val)
    end
    cost_cap += t_rating * 55.0 + cap_rating * 22.0 # Equipment pricing
    
    # 3. Electrical Simulation Setup
    p_bus, q_bus = zeros(BUS_COUNT), zeros(BUS_COUNT)
    for l in LOADS
        p_bus[l.bus] = l.p_kw * reserve
        q_bus[l.bus] = l.q_kvar * reserve
    end
    
    # Apply Distributed Resources
    p_bus[7] -= dg_factor * 600.0   # 600 kW DG nameplate
    q_bus[5] -= comp_factor * cap_rating
    
    # Run Load Flow with Tap Adjustment
    v_raw, i_branch = run_power_flow(branch_z, p_bus, q_bus, complex(tap_ratio, 0.0))
    
    # 4. Realistic Voltage Regulator Logic (Bus 3)
    # If Bus 3 is out of band, we shift the voltage reference for all downstream buses
    v_3 = abs(v_raw[3])
    v_final = copy(v_raw)
    if abs(v_3 - reg_set) > deadband
        boost = reg_set / (v_3 + 1e-15)
        for i in 3:BUS_COUNT; v_final[i] *= boost; end
    end
    
    # 5. Technical Metrics & Loss Calculation
    p_loss_kw = 0.0
    for i in 1:length(BRANCHES)
        p_loss_kw += abs(i_branch[i])^2 * real(branch_z[i] / BASE_Z) * 1000.0 * BASE_MVA
    end
    
    # Lifecycle Operating Cost (25 years @ $0.12/kWh)
    cost_op = p_loss_kw * 8760 * 25 * 0.12
    
    # 6. Comprehensive Penalty Framework
    P = 0.0
    # Voltage Stability [0.95, 1.05]
    for i in 1:BUS_COUNT
        vm = abs(v_final[i])
        if vm < 0.95; P += (0.95 - vm)^2 * 5e8; end
        if vm > 1.05; P += (vm - 1.05)^2 * 5e8; end
    end
    
    # Thermal Ampacity Limits
    current_conv = (BASE_MVA * 1000.0) / (sqrt(3) * NOMINAL_KV)
    for i in 1:length(BRANCHES)
        im = abs(i_branch[i]) * current_conv
        limit = CABLE_CATALOG[all_c_idx[i]].ampacity
        if im > limit; P += (im - limit) * 2e5; end
    end
    
    # Transformer Rating Limit
    total_load_kva = abs(sum(complex.(p_bus, q_bus)))
    if total_load_kva > t_rating; P += (total_load_kva - t_rating) * 5e4; end
    
    return cost_cap + cost_op + P
end

# --- Post-Optimization Diagnostics & Rendering ---

function export_network_results(ζ::ζ_Stats, best_p::Vector{Float64})
    # Extract best physical state
    c_idx = [clamp(Int(floor(best_p[i])), 1, length(CABLE_CATALOG)) for i in 1:6]
    tail_idx = clamp(Int(round(mean(best_p[1:6]))), 1, length(CABLE_CATALOG))
    all_c_idx = [c_idx; fill(tail_idx, 3)]
    t_idx = clamp(Int(floor(best_p[7])), 1, length(TRANSFORMER_RATINGS))
    cap_idx = clamp(Int(floor(best_p[8])), 1, length(CAPACITOR_RATINGS))
    
    # Re-run for diagnostic data
    p_bus, q_bus = zeros(BUS_COUNT), zeros(BUS_COUNT)
    for l in LOADS
        p_bus[l.bus] = l.p_kw * best_p[14]
        q_bus[l.bus] = l.q_kvar * best_p[14]
    end
    p_bus[7] -= best_p[11] * 600.0
    q_bus[5] -= best_p[12] * CAPACITOR_RATINGS[cap_idx]
    
    branch_z = [complex(CABLE_CATALOG[idx].r * BRANCHES[i].length_km, CABLE_CATALOG[idx].x * BRANCHES[i].length_km) for (i, idx) in enumerate(all_c_idx)]
    v_raw, i_branch = run_power_flow(branch_z, p_bus, q_bus, complex(best_p[9], 0.0))
    v_3 = abs(v_raw[3]); v_final = copy(v_raw)
    if abs(v_3 - best_p[10]) > best_p[13]
        boost = best_p[10] / (v_3 + 1e-15)
        for i in 3:BUS_COUNT; v_final[i] *= boost; end
    end
    
    p_loss_kw = 0.0
    for i in 1:length(BRANCHES); p_loss_kw += abs(i_branch[i])^2 * real(branch_z[i]/BASE_Z) * 1000 * BASE_MVA; end
    cost_cap = sum(CABLE_CATALOG[all_c_idx[i]].cost_per_km * BRANCHES[i].length_km for i in 1:9) + TRANSFORMER_RATINGS[t_idx]*55 + CAPACITOR_RATINGS[cap_idx]*22
    cost_op = p_loss_kw * 8760 * 25 * 0.12

    println("\n" * "*"^60)
    println("ELECTRICAL DISTRIBUTION DESIGN STUDY COMPLETE")
    println("*"^60)
    println("Optimal Infrastructure Choices:")
    for i in 1:9; println("  - Br $(BRANCHES[i].from)-$(BRANCHES[i].to): ", CABLE_CATALOG[all_c_idx[i]].size_name); end
    println("  - Main Transformer:  ", TRANSFORMER_RATINGS[t_idx], " kVA")
    println("  - Capacitor Bank:    ", CAPACITOR_RATINGS[cap_idx], " kVAR")
    
    println("\nOperating Settings:")
    @printf("  - Tap / Setpoint:    %.3f / %.3f pu\n", best_p[9], best_p[10])
    @printf("  - DG / Cap Factor:   %.2f / %.2f\n", best_p[11], best_p[12])
    @printf("  - Network Losses:    %.2f kW\n", p_loss_kw)
    @printf("  - Lifecycle Cost:    \$%.2f\n", cost_cap + cost_op)
    println("-"^60)

    # --- Visual Suite ---
    println("Generating high-resolution distribution visuals...")
    
    v_plt = plot(1:BUS_COUNT, abs.(v_final), title="Voltage Profile", ylabel="V (pu)", xlabel="Bus", marker=:square, color=:blue, ylim=(0.93, 1.07), label="Profile")
    hline!([0.95, 1.05], linestyle=:dash, color=:red, label="Limits")
    
    curr_plt = bar(1:9, abs.(i_branch) .* (1000/ (sqrt(3)*NOMINAL_KV)), title="Branch Ampacity Loading", ylabel="Current (A)", xlabel="Branch ID", color=:orange, label="Actual")
    scatter!(1:9, [CABLE_CATALOG[idx].ampacity for idx in all_c_idx], color=:black, marker=:hline, label="Limit")
    
    conv_plt = plot(1:length(ζ.φ_hist), ζ.φ_hist, title="Optimizer Convergence", ylabel="Cost (\$)", xlabel="Generation", yscale=:log10, color=:green, label="Objective")
    
    cost_plt = pie(["Capital", "Losses"], [cost_cap, cost_op], title="Lifecycle Cost Breakdown", color=[:gold, :lightgreen])
    
    full_plt = plot(v_plt, curr_plt, conv_plt, cost_plt, layout=(2,2), size=(1100, 950))
    savefig("Electrical_Network_Design_Report.png")
    println("Success. Study report saved to 'Electrical_Network_Design_Report.png'.")
end

# --- Main Optimizer Entry ---

function main()
    println("Initializing Distribution Network Mixed-Variable Optimization...")
    
    # 14 Variables Search Space Bounds
    # [C1-C6 (1..9), Trans(1..7), Cap(1..7), Tap(0.9..1.1), Reg(0.95..1.05), DG(0..1), Comp(0..1), Dead(0.01..0.05), Res(1.0..1.5)]
    lb = [fill(1.0, 8); 0.90; 0.95; 0.00; 0.00; 0.01; 1.00]
    ub = [fill(8.99, 8); 1.10; 1.05; 1.00; 1.00; 0.05; 1.50]
    
    # Execute Restoration-Grade DE (Self-Adaptive Strategy)
    # Using 4x dimension population for robust mixed-variable convergence
    ζ = ags_rand_1_max_ι(
        distribution_network_evaluation, 
        [], 
        ub, lb, 
        56, 
        250
    )
    
    export_network_results(ζ, ζ.β_hist[end])
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
