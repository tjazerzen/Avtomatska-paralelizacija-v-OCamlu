let () =
  let node_table = NodeHashtbl.create 10 in

  (*Two ways to construct a node*)
  let node1 = create_node 1 42 in
  let node2 = {created_time = 1683359999.138; id = 2; value = 42} in

  NodeHashtbl.iter (fun node value ->
    print_endline (node_to_string node ^ " => " ^ string_of_int value)
  ) node_table
