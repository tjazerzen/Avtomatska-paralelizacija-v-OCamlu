open Graph

module PQ : sig
  type t
  exception Queue_is_empty

  val empty : unit -> t
  val insert : t -> Node.t -> t
  val remove_top : t -> t
  val extract : t -> (Node.t * t)
end = struct
  type t = Empty | PQNode of Node.t * t * t

  exception Queue_is_empty

  let empty () = Empty

  let rec insert (queue: t) (new_node: Node.t) : t =
    match queue with
      Empty -> PQNode(new_node, Empty, Empty)
    | PQNode(existing_node, left, right) ->
        if Node.compare new_node existing_node <= 0
        then PQNode(new_node, insert right existing_node, left)
        else PQNode(existing_node, insert right new_node, left)

  let rec remove_top : t -> t = function
      Empty -> raise Queue_is_empty
    | PQNode(_, left, Empty) -> left
    | PQNode(_, Empty, right) -> right
    | PQNode(_, (PQNode(left_node, _, _) as left),
                      (PQNode(right_node, _, _) as right)) ->
        if Node.compare left_node right_node <= 0
        then PQNode(left_node, remove_top left, right)
        else PQNode(right_node, left, remove_top right)

  let extract : t -> (Node.t * t) = function
      Empty -> raise Queue_is_empty
    | PQNode(elt, _, _) as queue -> (elt, remove_top queue)
end
