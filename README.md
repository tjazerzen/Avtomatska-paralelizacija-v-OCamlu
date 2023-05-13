# Paralelizacija grafovskih algoritmov v funkcijskih programskih jezikih

V okviru diplomske naloge bom raziskoval paralelizacijo grafovskih algoritmov v funkcijskih programskih jezikih.

## Nastavitev okolja

Za poganjanje projekta se zahteva package manager **opam** (inštalacija: https://opam.ocaml.org/doc/Install.html) ter paket `dune`, ki se po uspešni inštalaciji `opam`-a naloži z ukazom `opam install dune`.

Po inštalaciji paketov `opam` in `dune`, iz poti `./playground/graph` poženite ukaz `dune build`, ki bo generirala binarno izvedljivo datoteko, specificirano v datoteki `playground/graph/dune`.


## Struktura projekta

V folderju `/diploma` so shranjeni pisni izdelki diplomske naloge (najpomembnejši izmed njih je verjetno [diplomska naloga sama](https://github.com/tjazerzen/parallelisation-of-graph-algorithms-in-functional-programming-languages/blob/master/diploma/dokument-diplomska-naloga.pdf)). Mapa `/diploma` uporablja rezultate iz mape `/playground`.


Predvidena struktura kode (znotraj folderja `/playground`) [Opomba: Tekom razvoja se struktura zna spreminjati. Z razvojem diplomske se bo struktura folderja večala.]:

- **Graph**: Znotraj folderja Graph je shranjena grafovska predstavitev ter njihovi algoritmi
    - `graph.ml`: WIP - Predstavitev grafa. Graf je implementiran preko modulov `Node` in `Graph`.
    - `tree.ml`: WIP
    - `data_structures`: WIP - Znotraj te mape so shranjene podatkovne strukture, potrebne za grafovske algoritme
    - `algorithms`: WIP - Znotraj te mape so implementirani dejanski paralelni grafovski algoritmi
- **other**: Algoritmi, ki niso grafovski, pa vseeno pridejo v poštev za diplomsko nalogo