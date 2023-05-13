open Graph

(* #use "graph_new_examples.ml";; *)
let () = print_endline "Creating nodes with values 4, 5, 6..."
let node1 = Node.create 4
let node2 = Node.create 5
let node3 = Node.create 6

let () = print_endline "Creating graph with nodes: {node1, node2, node3} and creating edges node1-node2 and node2-node3..."
let my_graph = 
  Graph.empty ~directed:true
  |> Graph.add_node node1 
  |> Graph.add_node node2 
  |> Graph.add_node node3
  |> Graph.add_edge node1 node2
  |> Graph.add_edge node2 node3

let () = print_endline "Printing graph to string..."
let () = Printf.printf "Graph: %s\n" (Graph.to_string my_graph)

let () = print_endline "Removing node1 from graph..."
let my_graph = Graph.remove_node node1 my_graph

let () = print_endline "Printing graph to string once again..."
let () = Printf.printf "Graph: %s\n" (Graph.to_string my_graph)

let () = print_endline "Removing edge node2-node3 from graph..."
let my_graph = Graph.remove_edge node2 node3 my_graph
let () = Printf.printf "Graph: %s\n" (Graph.to_string my_graph)

let nodes = Graph.nodes my_graph
let () = print_endline "Printing nodes of graph..."
let () = Printf.printf "Nodes: %s\n" (nodes |> List.map Node.to_string |> String.concat ", ")

let () = print_endline "Connecting node2 and node3 with edge..."
let my_graph = Graph.add_edge node2 node3 my_graph

let () = print_endline "Printing edges of graph..."

let edges = Graph.edges my_graph
let () = Printf.printf "Edges: %s\n" 
  (edges 
   |> List.map (fun (node1, node2) -> Printf.sprintf "(%s, %s)" (Node.to_string node1) (Node.to_string node2)) 
   |> String.concat ",\n")
