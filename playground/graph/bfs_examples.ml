open Graph
open Bfs

let node1 = Node.create 4
let node2 = Node.create 5
let node3 = Node.create 6

let my_graph =
  Graph.empty ~directed:false
  |> Graph.add_node node1
  |> Graph.add_node node2
  |> Graph.add_node node3
  |> Graph.add_edge node1 node3
  |> Graph.add_edge node3 node2
  (* |> Graph.add_edge node1 node3 *)

let () = Printf.printf "%s\n" (Graph.to_string my_graph)

let () = Bfs.bfs my_graph node1;