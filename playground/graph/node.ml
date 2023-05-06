open Unix

(* To run node.ml directly from terminal: ocamlc -o node -I +unix unix.cma node.ml && ./node *)

(* Each node is going to be identified with the properties created_time and its value. The values will have homogeneus type 'a *)
type 'a node = {
  mutable created_time: float;  (*This probably doesn't need to be mutable*)
  id: int;
  mutable value: 'a;
}

(* Not sure if I'll need the ID though *)
let create_node id value = {
  created_time = Unix.gettimeofday ();
  id;
  value;
}

let equal_node a b =
  (a.created_time = b.created_time) && (a.value = b.value)

(* Not sure if this will be ok, but fine for now *)
let hash_node node =
  int_of_float node.created_time

let node_to_string node = 
  Printf.sprintf "Id: %s, Value: %s" (string_of_int node.id) (string_of_int node.value)

module NodeHashtbl = Hashtbl.Make(struct
    type t = int node
    let equal = equal_node
    let hash = hash_node
end)
