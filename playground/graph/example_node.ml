#use "node.ml"

let () =
  let node_table = NodeHashtbl.create 10 in

  (*Two ways to construct a node*)
  let node1 = create_node 1 42 in
  let node2 = {created_time = 1683359999.138; id = 2; value = 48} in

  NodeHashtbl.add node_table node1 node1.value;
  NodeHashtbl.add node_table node2 node2.value;

  NodeHashtbl.iter (fun node value ->
    print_endline (node_to_string node ^ " => " ^ string_of_int value)
  ) node_table

(*
Prints:
  Id: 1, Value: 42 => 42
  Id: 2, Value: 48 => 48
*)