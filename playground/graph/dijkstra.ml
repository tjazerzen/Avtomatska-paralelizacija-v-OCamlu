open Graph


module PQ : sig
  type t
  type priority = float
  exception Queue_is_empty

  val empty : unit -> t
  val insert : Node.t -> priority -> t -> t
  val remove_top : t -> t
  val extract : t -> (Node.t * t)
end = struct
  type priority = float
  type t = Empty | PQNode of priority * Node.t * t * t

  exception Queue_is_empty

  let empty () = Empty

  let rec insert new_node new_priority queue =
    match queue with
      Empty -> PQNode(new_priority, new_node, Empty, Empty)
    | PQNode(existing_priority, existing_node, left, right) ->
        if new_priority <= existing_priority
        then PQNode(new_priority, new_node, insert existing_node existing_priority right, left)
        else PQNode(existing_priority, existing_node, insert new_node new_priority right, left)

  let rec remove_top = function
      Empty -> raise Queue_is_empty
    | PQNode(_, _, left, Empty) -> left
    | PQNode(_, _, Empty, right) -> right
    | PQNode(_, _, (PQNode(left_priority, left_node, _, _) as left),
                      (PQNode(right_priority, right_node, _, _) as right)) ->
        if left_priority <= right_priority
        then PQNode(left_priority, left_node, remove_top left, right)
        else PQNode(right_priority, right_node, left, remove_top right)

  let extract = function
      Empty -> raise Queue_is_empty
    | PQNode(_, elt, _, _) as queue -> (elt, remove_top queue)
end

