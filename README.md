# Paralelizacija grafovskih algoritmov v funkcijskih programskih jezikih

V okviru diplomske naloge bom raziskoval paralelizacijo grafovskih algoritmov v funkcijskih programskih jezikih.

## Nastavitev okolja

- Inštalacija `OCaml`-a ter `opam`-a: Sledite inštalacijskim navodilom na https://ocaml.org/docs/up-and-running#installation-on-unix. Treba je:
    - `opam` instalirati
    - `opam` inicializirati
    - Ustvariti `switch`, ki je verzije `>= 5.0.0`
- `opam install dune`
- `cd ./playground/graph`
- `dune build`. Primeri raznih dune datotek so zapisani na `./playground/graph/dune_file_contents.ml`. V glavnem priporočam poganjanje datotek `*_examples.ml`, kjer je `*` ime algoritma, ki ga želite pognati (npr. `bfs`, `dijkstra`, ...).
- Ta binary poženete z `./*_examples.exe` iz direktorija `./playground/graph/`.

## Struktura projekta

V folderju `/diploma` so shranjeni pisni izdelki diplomske naloge (najpomembnejši izmed njih je verjetno [diplomska naloga sama](https://github.com/tjazerzen/parallelisation-of-graph-algorithms-in-functional-programming-languages/blob/master/diploma/dokument-diplomska-naloga.pdf)). Mapa `/diploma` uporablja rezultate iz mape `/playground/graph/`.


Struktura kode (znotraj folderja `/playground`):

- **Graph**: Znotraj folderja Graph je shranjena grafovska predstavitev ter njihovi algoritmi
    - `graph.ml`: Predstavitev uteženega in neuteženega grafa. Graf je implementiran preko modulov `Node`, `UnweightedGraph` ter `WeightedGraph`.
    - `fib.ml`: Implemetacija naivnega paralelnega računanja Fibonaccijevih števil.
    - `bfs.ml`: Implementacija paralelnega in sekvenčnega algoritma BFS.
    - `dijkstra.ml`: Implementacija paralelnega in sekvenčnega algoritma Dijkstre.
    - `floyd.ml`: Implementacija paralelnega ter sekvenčnega Floyd-Warshallovega algoritma
    - `computation_time_analysis/computation_time_analysis.ipynb`: Primerjava časov izvajanja paralelnih in sekvenčnih algoritmov.