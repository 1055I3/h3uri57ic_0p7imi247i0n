module Reporting

using Plots
using ..Models
using Statistics

export plot_convergence, plot_diversity, save_comprehensive_report

function plot_convergence(ζ::ζ_Stats; filename="convergence.png")
    p = plot(1:length(ζ.φ_hist), ζ.φ_hist, 
             title="Convergence History", 
             xlabel="Iteration", ylabel="Best Score", 
             lw=2, color=:blue, legend=false)
    savefig(p, filename)
    return p
end

function plot_diversity(ζ::ζ_Stats; filename="diversity.png")
    p = plot(1:length(ζ.δ_hist), ζ.δ_hist, 
             title="Population Diversity", 
             xlabel="Iteration", ylabel="Avg Euclidean Distance", 
             lw=2, color=:green, legend=false)
    savefig(p, filename)
    return p
end

function save_comprehensive_report(ζ::ζ_Stats, name::String)
    println("\n" * "="^40)
    println("FINAL REPORT: $name")
    println("-"^40)
    println("Best Score:      ", round(ζ.φ_hist[end], digits=10))
    println("Iterations (ι):  ", ζ.ι[])
    println("Evaluations (ε): ", ζ.ε[])
    println("Final Diversity: ", round(ζ.δ_hist[end], digits=6))
    println("="^40)
    
    plot_convergence(ζ, filename="$(name)_convergence.png")
    plot_diversity(ζ, filename="$(name)_diversity.png")
end

end
