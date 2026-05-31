module Comparison

using Optim
using Statistics

export run_reference_optim, ComparisonResult

struct ComparisonResult
    nm_score::Float64
    sa_score::Float64
    ps_score::Float64
end

function run_reference_optim(objective, lb, ub)
    d = length(lb)
    x0 = [rand() * (ub[i] - lb[i]) + lb[i] for i in 1:d]
    
    # Nelder-Mead
    res_nm = optimize(objective, x0, NelderMead())
    nm_val = Optim.minimum(res_nm)
    
    # Simulated Annealing
    res_sa = optimize(objective, x0, SimulatedAnnealing())
    sa_val = Optim.minimum(res_sa)
    
    # Particle Swarm
    res_ps = optimize(objective, lb, ub, ParticleSwarm())
    ps_val = Optim.minimum(res_ps)
    
    return ComparisonResult(nm_val, sa_val, ps_val)
end

end
