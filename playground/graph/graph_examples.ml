open Graph

(* ----------------- UNWEIGHTED GRAPH ----------------- *)

let () = print_endline "UNWEIGHTED GRAPH"

let () = print_endline "Creating nodes with values 4, 5, 6..."
let node1 = Node.create 4
let node2 = Node.create 5
let node3 = Node.create 6

let () =
  print_endline
    "Creating graph with nodes: {node1, node2, node3} and creating edges \
     node1-node2 and node2-node3..."

let my_graph =
  UnweightedGraph.empty ~directed:false
  |> UnweightedGraph.add_node node1 |> UnweightedGraph.add_node node2 |> UnweightedGraph.add_node node3
  |> UnweightedGraph.add_edge node1 node2 |> UnweightedGraph.add_edge node2 node3
  |> UnweightedGraph.add_edge node1 node3

let () = print_endline "Printing graph to string..."
let () = Printf.printf "UnweightedGraph: %s\n" (UnweightedGraph.to_string my_graph)
let () = print_endline "Printing neighbors of node1..."

let () =
  Printf.printf "Neighbors: %s\n"
    (my_graph |> UnweightedGraph.neighbours node1 |> List.map Node.to_string
   |> String.concat ", ")

let () = print_endline "Removing node1 from graph..."
let my_graph = UnweightedGraph.remove_node node1 my_graph
let () = print_endline "Printing graph to string once again..."
let () = Printf.printf "UnweightedGraph: %s\n" (UnweightedGraph.to_string my_graph)
let () = print_endline "Removing edge node2-node3 from graph..."
let my_graph = UnweightedGraph.remove_edge node2 node3 my_graph
let () = Printf.printf "UnweightedGraph: %s\n" (UnweightedGraph.to_string my_graph)
let nodes = UnweightedGraph.nodes my_graph
let () = print_endline "Printing nodes of graph..."

let () =
  Printf.printf "Nodes: %s\n"
    (nodes |> List.map Node.to_string |> String.concat ", ")

let () = print_endline "Connecting node2 and node3 with edge..."
let my_graph = UnweightedGraph.add_edge node2 node3 my_graph
let () = print_endline "Printing edges of graph..."
let edges = UnweightedGraph.edges my_graph

let () =
  Printf.printf "Edges: %s\n"
    (edges
    |> List.map (fun (node1, node2) ->
           Printf.sprintf "(%s, %s)" (Node.to_string node1)
             (Node.to_string node2))
    |> String.concat ",\n")


(* ----------------- WEIGHTED GRAPH ----------------- *)

let () = print_endline "\nWEIGHTED GRAPH"

let () = print_endline "Creating nodes with values 7, 8, 9..."
let node1 = Node.create 7
let node2 = Node.create 8
let node3 = Node.create 9

let () =
  print_endline
    "Creating weighted graph with nodes: {node1, node2, node3} and creating edges \
     node1-node2 (weight 0.3), node2-node3 (weight 0.4), and node1-node3 (weight 0.5)..."

let my_weighted_graph =
  WeightedGraph.empty ~directed:false
  |> WeightedGraph.add_node node1 |> WeightedGraph.add_node node2 |> WeightedGraph.add_node node3
  |> WeightedGraph.add_edge node1 node2 0.3 |> WeightedGraph.add_edge node2 node3 0.4
  |> WeightedGraph.add_edge node1 node3 0.5

let () = print_endline "Printing weighted graph to string..."
let () = Printf.printf "WeightedGraph: %s\n" (WeightedGraph.to_string my_weighted_graph)
let () = print_endline "Printing neighbors of node1..."

let () =
  Printf.printf "Neighbors: %s\n"
    (my_weighted_graph |> WeightedGraph.neighbours node1 |> List.map Node.to_string
   |> String.concat ", ")

let () = print_endline "Removing node1 from weighted graph..."
let my_weighted_graph = WeightedGraph.remove_node node1 my_weighted_graph
let () = print_endline "Printing weighted graph to string once again..."
let () = Printf.printf "WeightedGraph: %s\n" (WeightedGraph.to_string my_weighted_graph)
let () = print_endline "Removing edge node2-node3 from weighted graph..."
let my_weighted_graph = WeightedGraph.remove_edge node2 node3 my_weighted_graph
let () = Printf.printf "WeightedGraph: %s\n" (WeightedGraph.to_string my_weighted_graph)
