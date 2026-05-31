[![Julia](https://img.shields.io/badge/Made%20with-Julia-9558B2?logo=julia&logoColor=white)](https://julialang.org/)
[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Windows](https://img.shields.io/badge/Windows-0078D6)](https://en.wikipedia.org/wiki/Windows_10)
[![No Maintenance Intended](http://unmaintained.tech/badge.svg)](http://unmaintained.tech/)

# Pure Julia Differential Evolution Framework

A high-fidelity, modular, and parallelized Differential Evolution (DE) framework implemented in pure Julia (1.10+).

## Project Overview

This repository provides an engineering-grade Differential Evolution framework designed for parallelized, robust, and statistically sound global optimization. It emphasizes multiple dispatch, type safety, and modularity, making it suitable for both academic benchmarking and complex engineering optimization problems.

## Features

- **Parallel Execution:** Leverages `Threads.@spawn` for high-throughput population evolution.
- **Multiple Dispatch Architecture:** Defines clear abstractions (`StopCond`, `SelectionMutation`, `CrossoverStrat`) to allow easy extension of strategies without modifying core logic.
- **Statistical Stopping Conditions:** Includes advanced, parameter-free stopping rules (`PocockSignStop`, `SPRTStop`, `PermutationStop`) based on the convergence history.
- **Legacy API Support:** Re-exports standard DE API entry points (e.g., `de_rand_1_max_ι`) for backward compatibility with common benchmarking conventions.
- **Engineering-Grade Examples:** Includes high-fidelity models for:
    - Airfoil Shape Optimization
    - Tokamak Divertor Design
    - Electrical Distribution Network Optimization
    - Multiplex PCR Panel Configuration
- **Robustness:** Includes Monte Carlo simulation hooks and multi-point objective aggregation for real-world design stability.

## Installation

This project requires Julia 1.10 or higher. Ensure the dependencies are installed:

```bash
julia --project -e 'using Pkg; Pkg.instantiate()'
```

## Basic Usage

The framework utilizes a modular API. Example usage for an optimization problem:

```julia
using DifferentialEvolution
# Note: Ensure the framework modules are in the load path

# Define objective, bounds, population size, and iteration limit
function my_objective(p::Vector{Float64})
    # ... implementation ...
end

lb = fill(-1.0, 5)
ub = fill(1.0, 5)
n_pop = 20
max_iter = 100

# Execute using self-adaptive strategy
ζ = ags_rand_1_max_ι(my_objective, [], ub, lb, n_pop, max_iter)

# Access best solution
best_solution = ζ.β_hist[end]
```

## Main Modules

- `Models.jl`: Core data structures (`ζ_Stats`) for performance tracking and statistics.
- `StoppingConditions.jl`: Polymorphic termination criteria (`should_continue`).
- `Strategies.jl`: Implementation of DE mutation/selection strategies (`Rand1`, `Best2`, `AGS`, `FISA`, etc.).
- `Crossover.jl`: Modular crossover strategies (`Binomial`, `SA`).
- `Benchmarks.jl`: Standard mathematical test functions for framework validation.
- `DifferentialEvolution.jl`: Primary module exporting the public API and engine logic.

## Engineering Examples

The `examples/` directory contains complete, runnable engineering studies. To run a study, use:

```bash
julia -t auto examples/AirfoilOptimization.jl
julia -t auto examples/TokamakDivertorDesign.jl
julia -t auto examples/ElectricalNetworkDesign.jl
julia -t auto examples/PCRPanelOptimization.jl
```

## Important Assumptions
- The framework assumes objective functions return a single scalar value.
- Constraints are handled via soft penalties within the evaluation function.
- Statistical stopping conditions (`SPRT`, `Permutation`) assume sufficient iteration history for validity. All examples use a `CompositeStop` wrapper for deterministic iteration bounds in testing.

## License

This project is licensed under the terms described in the `LICENSE` file.
