# ==============================================================================
# MULTIPLEX PCR PANEL CONFIGURATION OPTIMIZATION
# Objective: Maximize Panel Compatibility & Performance (Mixed-Variable)
# Optimizer: Restoration-Grade Differential Evolution Framework (Black Box)
# ==============================================================================

using LinearAlgebra
using Statistics
using Random
using Plots
using Printf

# Import the professional framework
include("../src/DifferentialEvolution.jl")
using .DifferentialEvolution

# --- Candidate Metadata Model ---

struct PCRCandidate
    id::Int
    target_group::Int
    specificity::Float64
    efficiency::Float64
    robustness::Float64
    cost::Float64
end

# --- Problem Configuration ---
const N_TARGETS = 10
const CANDIDATES_PER_TARGET = 20
const N_PARAMS = 14 # 10 selections + 4 weights
const TOTAL_LIBRARY_SIZE = N_TARGETS * CANDIDATES_PER_TARGET

# --- Synthetic Library Generation (Deterministic) ---
function generate_candidate_library()
    Random.seed!(42) # Reproducible library
    library = Vector{PCRCandidate}()
    for t in 1:N_TARGETS
        for c in 1:CANDIDATES_PER_TARGET
            # Realistic score distributions
            spec = 0.7 + 0.3 * rand()
            eff  = 0.6 + 0.4 * rand()
            rob  = 0.5 + 0.5 * rand()
            cost = 10.0 + 90.0 * rand()
            push!(library, PCRCandidate((t-1)*CANDIDATES_PER_TARGET + c, t, spec, eff, rob, cost))
        end
    end
    
    # Generate a static Pairwise Interaction Risk Matrix
    # Simulates thermodynamic/kinetic incompatibility (0.0 = Safe, 1.0 = High Risk)
    interaction_matrix = rand(TOTAL_LIBRARY_SIZE, TOTAL_LIBRARY_SIZE)
    # Ensure symmetry and zero-diagonal
    interaction_matrix = (interaction_matrix + interaction_matrix') / 2.0
    for i in 1:TOTAL_LIBRARY_SIZE; interaction_matrix[i, i] = 0.0; end
    
    return library, interaction_matrix
end

const LIB, INT_MAT = generate_candidate_library()

# --- Evaluation Model ---

struct PanelMetrics
    selected_ids::Vector{Int}
    avg_spec::Float64
    avg_eff::Float64
    total_cost::Float64
    max_interaction::Float64
    sum_interaction::Float64
    redundancy_penalty::Float64
    valid::Bool
end

function analyze_panel(p::Vector{Float64})
    # 1. Discrete Mapping: Map continuous [1.0, 20.99] to discrete candidate index for each target
    selected_indices = zeros(Int, N_TARGETS)
    for t in 1:N_TARGETS
        # p[t] selects candidate 1..20 for target t
        local_idx = clamp(Int(floor(p[t])), 1, CANDIDATES_PER_TARGET)
        # Global library ID
        selected_indices[t] = (t-1) * CANDIDATES_PER_TARGET + local_idx
    end
    
    candidates = [LIB[idx] for idx in selected_indices]
    
    # 2. Performance Aggregation
    avg_spec = mean(c.specificity for c in candidates)
    avg_eff  = mean(c.efficiency for c in candidates)
    tot_cost = sum(c.cost for c in candidates)
    
    # 3. Interaction Analysis
    interaction_scores = Float64[]
    for i in 1:N_TARGETS
        for j in (i+1):N_TARGETS
            push!(interaction_scores, INT_MAT[selected_indices[i], selected_indices[j]])
        end
    end
    max_int = maximum(interaction_scores)
    sum_int = sum(interaction_scores)
    
    # 4. Redundancy/Duplicate Check
    unique_ids = length(unique(selected_indices))
    red_penalty = (N_TARGETS - unique_ids) * 500.0
    
    valid = (avg_spec > 0.8) && (max_int < 0.85)
    
    return PanelMetrics(selected_indices, avg_spec, avg_eff, tot_cost, max_int, sum_int, red_penalty, valid)
end

# --- Objective Function ---

function pcr_panel_objective(p::Vector{Float64})
    # Phase 1: Configuration Analysis
    m = analyze_panel(p)
    
    # Phase 2: Dynamic Weighting (Variables 11-14)
    w_sum = sum(abs.(p[11:14])) + 1e-15
    w_spec = abs(p[11]) / w_sum
    w_eff  = abs(p[12]) / w_sum
    w_rob  = abs(p[13]) / w_sum
    w_cost = abs(p[14]) / w_sum
    
    # Phase 3: Penalty Framework
    P = m.redundancy_penalty
    if !m.valid; P += 1000.0; end
    if m.max_interaction > 0.6; P += (m.max_interaction - 0.6) * 500.0; end
    
    # Phase 4: Composite Score
    performance = (w_spec * m.avg_spec + w_eff * m.avg_eff)
    health      = (1.0 - m.sum_interaction / (N_TARGETS * (N_TARGETS-1)/2))
    economy     = (1.0 - m.total_cost / (N_TARGETS * 100.0))
    
    score = performance * health * economy
    return -score + P
end

# --- Diagnostics & Reporting ---

function report_pcr_results(ζ::ζ_Stats, best_p::Vector{Float64})
    m = analyze_panel(best_p)
    
    println("\n" * "="^60)
    println("MULTIPLEX PCR PANEL CONFIGURATION REPORT")
    println("="^60)
    println("Selected Candidate IDs:")
    for t in 1:N_TARGETS
        println("  Target $(Char(64+t)): ID $(m.selected_ids[t]) (Spec: $(round(LIB[m.selected_ids[t]].specificity, digits=3)))")
    end
    
    println("\nAggregate Statistics:")
    @printf("  - Panel Specificity: %.3f\n", m.avg_spec)
    @printf("  - Panel Efficiency:  %.3f\n", m.avg_eff)
    @printf("  - Cumulative Risk:   %.3f\n", m.sum_interaction)
    @printf("  - Peak Pairing Risk: %.3f\n", m.max_interaction)
    @printf("  - Configuration Cost: %.2f units\n", m.total_cost)
    println("-"^60)

    # Visualization
    println("Generating diagnostic visualizations...")
    p1 = plot(1:length(ζ.φ_hist), -ζ.φ_hist, title="Optimization Trend", xlabel="Iteration", ylabel="Panel Fitness", color=:blue, lw=2)
    
    risk_map = zeros(N_TARGETS, N_TARGETS)
    for i in 1:N_TARGETS, j in 1:N_TARGETS
        risk_map[i, j] = INT_MAT[m.selected_ids[i], m.selected_ids[j]]
    end
    p2 = heatmap(1:N_TARGETS, 1:N_TARGETS, risk_map, title="Pairwise Interaction Risk", 
                 xticks=(1:N_TARGETS, [Char(64+i) for i in 1:10]), 
                 yticks=(1:N_TARGETS, [Char(64+i) for i in 1:10]), color=:magma)
    
    specs = [LIB[id].specificity for id in m.selected_ids]
    effs  = [LIB[id].efficiency for id in m.selected_ids]
    p3 = bar(1:N_TARGETS, [specs effs], title="Target Performance", 
             label=["Spec" "Eff"], alpha=0.7, xticks=(1:N_TARGETS, [Char(64+i) for i in 1:10]))
    
    p4 = plot(1:length(ζ.δ_hist), ζ.δ_hist, title="Search Diversity", xlabel="Iteration", ylabel="Avg Dist", color=:green)
    
    study_plt = plot(p1, p2, p3, p4, layout=(2,2), size=(1100, 900))
    savefig("PCR_Panel_Optimization_Study.png")
    println("Study complete. Report saved to 'PCR_Panel_Optimization_Study.png'.")
end

function main()
    println("Launching Multiplex PCR Configuration Analysis...")
    lb = [fill(1.0, 10); fill(0.0, 4)]
    ub = [fill(20.99, 10); fill(1.0, 4)]
    
    ζ = ags_rand_1_max_ι(pcr_panel_objective, [], ub, lb, 4 * N_PARAMS, 250)
    report_pcr_results(ζ, ζ.β_hist[end])
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
