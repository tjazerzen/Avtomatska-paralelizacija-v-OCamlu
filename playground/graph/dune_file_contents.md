- `fib_examples.ml`:
```
(executable
 (name fib_examples)
 (promote (until-clean))
 (libraries domainslib unix fib)
 (modules fib_examples)
 )

(library
 (name fib)
 (modules fib)
 (libraries domainslib unix)
)
```

- `graph_examples.ml`:
```
(executable
 (name graph_examples)
 (libraries unix)
 (promote
  (until-clean)))
```

- `bfs.ml`:
```
(executable
  (name bfs)
  (libraries base stdio domainslib)
  (modules bfs graph)
  (flags :standard -w "-32-70") ; If needed - suppress warnings 32 and 70
)
```

- `bfs_examples.ml`:
```
(executable
  (name bfs_examples)
  (libraries base stdio domainslib)
  (modules bfs graph bfs_examples graph_examples)
  (promote (until-clean)))
```

- `dijkstra.ml`:
```
(executable
  (name dijkstra)
  (libraries base stdio domainslib)
  (modules dijkstra graph)
)
```

- `dijkstra_examples.ml`:
```
(executable
  (name dijkstra_examples)
  (libraries base stdio domainslib)
  (modules dijkstra graph dijkstra_examples graph_examples)
  (promote (until-clean))
  (flags :standard -w "-32-33-70")
  )
```

- `floyd.ml`:

```
(executable
  (name floyd_warshall)
  (libraries base stdio domainslib)
  (modules floyd_warshall graph)
  (flags :standard -w "-27-32")
)
```

- `floyd_examples.ml`:
```
(executable
  (name floyd_examples)
  (libraries base stdio domainslib)
  (modules floyd graph floyd_examples)
  (promote (until-clean))
  (flags :standard -w "-32-33")
  )
```
