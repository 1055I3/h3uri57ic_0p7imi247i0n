# DifferentialEvolution.jl

A high-performance, parallelized, and feature-complete Differential Evolution (DE) optimization framework implemented in pure Julia.

## Project Structure

- `src/`: Core package source code.
  - `Models.jl`: Data structures and abstract types.
  - `Strategies.jl`: Selection and mutation strategies (Rand1, Best2, SDE, AGS, FISA).
  - `Crossover.jl`: Recombination logic (Binomial, SA).
  - `StoppingConditions.jl`: Rigorous statistical stopping criteria (Pocock, SPRT, Permutation).
  - `Benchmarks.jl`: Standard optimization test functions (Sphere, Rosenbrock, Shekel, etc.).
  - `DifferentialEvolution.jl`: Main module and generic engine.
- `test/`: Comprehensive unit and integration test suite.
- `examples/`: Real-world engineering applications.
  - `AirfoilOptimization.jl`: 12-parameter aerodynamic shape optimization study.
- `config/`: Configuration files (if any).

## Installation

Ensure you have Julia 1.10+ installed.

```bash
# Clone the repository
git clone <repo-url>
cd h3uri57ic_0p7imi247i0n

# Install dependencies
julia -e 'using Pkg; Pkg.activate("."); Pkg.instantiate()'
```

## Usage

### Running Tests
To verify the system integrity:
```bash
julia -t auto test/runtests.jl
```

### Running Examples
To run the airfoil optimization study:
```bash
julia -t auto examples/AirfoilOptimization.jl
```

## Features

- **High-Fidelity Restoration:** 100% API compatibility with legacy implementations.
- **Advanced Parallelism:** Efficient multi-core utilization via `Threads.@spawn`.
- **Mathematical Rigor:** Exact statistical stopping conditions with Unicode/Greek notation support.
- **Polymorphic Dispatch:** extensible architecture using Julia's multiple dispatch system.
- **Self-Adaptive Strategies:** State-of-the-art AGS and FISA algorithms included.

## Contributing

Follow standard Julia development practices. Ensure all new features are modularized in `src/` and covered by tests in `test/`.
