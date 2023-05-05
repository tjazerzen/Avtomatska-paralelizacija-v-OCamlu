# Parallelisation of Graph Algorithms in Functional Programming Languages

V okviru diplomske naloge bom raziskoval paralelizacijo grafovskih algoritmov v funkcijskih programskih jezikih.

V folderju /diploma so shranjeni pisni izdelki diplomske naloge (najpomembnejši izmed njih je verjetno [diplomska naloga sama](https://github.com/tjazerzen/parallelisation-of-graph-algorithms-in-functional-programming-languages/blob/master/diploma/dokument-diplomska-naloga.pdf)). Mapa /diploma uporablja rezultate iz mape /playground.


Predvidena struktura kode (znotraj kode /koda) [Opomba: Tekom razvoja se struktura zna spreminjati. Z razvojem diplomske se bo struktura folderja večala.]:

- **Graph**: Znotraj folderja Graph je shranjena grafovska predstavitev ter njihovi algoritmi
    - `node.ml`: WIP - Predstavitev vozlišča
    - `edge.ml`: WIP - Predstavitev povezave
    - `graph.ml`: WIP - Predstavitev grafa. Se bo nanašala na `node.ml` in `edge.ml`
    - `tree.ml`: WIP - Posebna vrsta predstavitve grafa. Mogoče se bo nanašala na `node.ml` in `edge.ml`, ali pa na `graph.ml`, bomo videli.
    - `data_structures`: Znotraj te mape so shranjene podatkovne strukture, potrebne za grafovske algoritme
        - WIP
    - `algorithms`: Znotraj te mape so implementirani dejanski paralelni grafovski algoritmi
- **other**: Algoritmi, ki niso grafovski, pa vseeno pridejo v poštev za diplomsko nalogo
    - WIP