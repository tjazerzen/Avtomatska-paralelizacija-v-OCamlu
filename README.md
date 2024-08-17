# Parallelization of graph algorithms in functional programming languages

As part of my diploma thesis, I will research the parallelization of graph algorithms in functional programming languages.

I implemented:
- parallel and sequential BFS algorithm (the performance of parallel algorithm is roughly the same as the sequential algorithm)
- parallel and sequential Dijkstra's algorithm (the performance of parallel algorithm is worse than the sequential algorithm)
- parallel and sequential Floyd-Warshall algorithm (parellel algorithm outperforms the sequential algorithm)

## Setup

- install `OCaml` and `opam` by following the installation instructions at https://ocaml.org/docs/up-and-running#installation-on-unix
- install dune by running `opam install dune`

## Running the code
The code is stored inside the `playground/graph` folder. `cd` into this folder.
- run `dune build` with the desired dune file.
  Examples of various dune files are written in `playground/graph/dune_file_contents.ml`.
  I mainly recommend running `*_examples.ml` files, where `*` is the name of the algorithm you want
  to run (e.g., `bfs`, `dijkstra`, ...).
- Run this binary with `./*_examples.exe` from the `./playground/graph/` directory.

## Project structure
The `/diploma` folder stores the written products of the diploma thesis. Probably the most important among them is
[the diploma thesis itself](https://github.com/tjazerzen/parallelisation-of-graph-algorithms-in-functional-programming-languages/blob/master/diploma/dokument-diplomska-naloga.pdf).
The `/diploma` folder uses the results from the `/playground/graph/` folder.
Code structure (within the folder `playground/`):
- `graph/`: The Graph folder contains the graph representation and their algorithms
    - `graph.ml`: Representation of weighted and unweighted graphs. The graph is implemented through the `Node`, `UnweightedGraph`, and `WeightedGraph` modules.
    - `fib.ml`: Implementation of naive parallel calculation of Fibonacci numbers.
    - `bfs.ml`: Implementation of parallel and sequential BFS algorithm.
    - `dijkstra.ml`: Implementation of parallel and sequential Dijkstra's algorithm.
    - `floyd.ml`: Implementation of parallel and sequential Floyd-Warshall algorithm
    - `computation_time_analysis/computation_time_analysis.ipynb`: Comparison of execution times of various parallel and sequential algorithms.

