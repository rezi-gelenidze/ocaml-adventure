(* Good resorce to learn Binary Search Tree
  https://www.geeksforgeeks.org/binary-search-tree-data-structure/ *)


(* Define a signature to specify mandatory methods for any tree structure *)
module type Tree = sig
  type 'a tree

  (* Inserts an element into the tree. *)
  val insert : 'a tree -> 'a -> 'a tree

  (* Find minimum node value in a given tree *)
  val min_value : 'a tree -> 'a

  (* Removes an element from the tree and returns a tuple 
     with a boolean indicating success and the new tree. *)
  val remove_flagged : 'a tree -> 'a -> bool * 'a tree

  (* Removes an element from a tree if exists and return
    a resulting tree *)
  val remove : 'a tree -> 'a -> 'a tree

  (* Constructs a tree from a list of elements. *)
  val from_list : 'a list -> 'a tree

  (* Constructs a balanced tree from a list of elements *)
  val from_list_to_balanced_tree : 'a list -> 'a tree

  (* Converts a tree to a list of elements. *)
  val to_list : 'a tree -> 'a list

  (* Searches for an element in the tree, returning true if the element is present. *)
  val search : 'a tree -> 'a -> bool

  (* Update value if found *)
  val update : 'a tree -> 'a -> 'a -> 'a tree

  (* Get height of the given tree *)
  val height : 'a tree -> int
  
  (* Get depth of the given element in a tree *)
  val depth : 'a tree -> 'a -> int

  (* Count number of nodes *)
  val size : 'a tree -> int

  (* Count number of leaves *)
  val leaves : 'a tree -> int

  (* Pretty pring a given tree *)
  val pretty_print : ('a -> string) -> 'a tree -> unit
end


module BinarySearchTree : Tree = struct
  (* bst definition *)
  type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

  (* Insertion happens as bst is intended to,
    - if greater than current node, then its place is in the right subtree
    - if less than current node, them its place is in the left subtree
    - the first leaf we reach is the corect possition 
  *)
  let rec insert tree x = match tree with
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (a, l, r) as current_node ->
      if x > a then 
        Node (a, l, insert r x)
      else if x < a then 
        Node (a, insert l x, r)
      else 
        current_node
  
  (* *minimum value is always on the left subtree of a node *)
  let rec min_value = function
    | Leaf -> raise (Invalid_argument "Empty Tree.")
    | Node (a, Leaf, _) -> a
    | Node (_, l, _) -> min_value l

  (* Same as below function, but we add bool flag to indicate deletion *)
  let rec remove_flagged tree x = match tree with
    | Leaf -> (false, Leaf)
    | Node (a, l, r) ->
      if x > a then
        let (found, rest) = remove_flagged r x in
        (found, Node (a, l, rest))
      else if x < a then
        let (found, rest) = remove_flagged l x in
        (found, Node (a, rest, r))
      else
        (* Element found, now correctly extract it *)
        match l, r with
          | Leaf, Leaf -> (true, Leaf)
          | Leaf, _ -> (true, r)
          | _, Leaf -> (true, l)
          | _, _ ->
            let succ = min_value r in
            let (_, r') = remove_flagged r succ in
            (true, Node (succ, l, r'))

  (* Remove a node, by preserving the structure *)
  let rec remove tree x = match tree with
    | Leaf -> Leaf (* Nothing to do, return current structure *)
    | Node (a, l, r) ->
      (* if target is on the right, preserve structure and proceed removal on the right *)
      if x > a then
        Node (a, l, remove r x)
      (* if target is on the left, preserve structure and procees removal on the left *)
      else if x < a then
        Node (a, remove l x, r)
      (* 
        We found the target element, now we need to safely 
        remove node, without messing up its children 
      *)
      else
        match l, r with
          (* 
            If targets children are leaves, there is nothing to 
            mess up, so we return the Leaf instead of target
          *)
          | Leaf, Leaf -> Leaf  
          (* 
            If one of the children is a Leaf, then Node child 
            can be used to substitute current target node 
          *)
          | Leaf, _ -> r
          | _, Leaf -> l
          (* 
            If both children are nodes, then we have to find minimal value
            on the right and bubble it up and replace with our element
            view Case 3 in "https://www.geeksforgeeks.org/deletion-in-binary-search-tree/"
          *)
          | _, _ ->
            let succ = min_value r in
            let r' = remove r succ in
            Node (succ, l, r')

  (* Use instertion method iteratively and get a resulting tree *)
  let from_list lst = List.fold_left insert Leaf lst

  let rec sorted_list_to_bst lst l r =
    if l > r then Leaf
    else
      let m = (l + r) / 2 in
      let node_value = List.nth lst m in
      Node (
        node_value,
        sorted_list_to_bst lst l (m - 1),  (* Recurse on the left half *)
        sorted_list_to_bst lst (m + 1) r  (* Recurse on the right half *)
      )

  let from_list_to_balanced_tree lst =
    let sorted_unique = List.sort_uniq compare lst in 
    sorted_list_to_bst sorted_unique 0 (List.length sorted_unique - 1)

  (* Convert a tree into a list, by preserving sorted sequence *)
  let to_list tree =
    let rec aux acc = function
      | Leaf -> acc
      | Node (a, l, r) -> aux (a :: (aux acc r)) l
    in aux [] tree

  (* Search an element and return bool flag indicating discovery *)
  let rec search tree x = match tree with
    | Leaf -> false
    | Node (a, l, r) ->
      if a = x then true
      else if x > a then search r x
      else search l x

  (* Simialr to search, but we update an element and return a tree *)
  let rec update tree x' x = match tree with
    | Leaf -> Leaf
    | Node (a, l, r) ->
      if x = a then Node (x', l, r)
      else if x > a then Node (a, l, update r x' x)
      else Node (a, update l x' x, r) 

  (* Return a height, longest path in a tree *)
  let rec height = function
    | Leaf -> 0
    | Node (_, l, r) -> 1 + (max (height l) (height r))

  (* 
    Return a depth of an alement in a tree, aka number 
    of edges from root to the target element 
  *)
  let rec depth tree x = match tree with
    | Leaf -> -1
    | Node (a, l, r) ->
        if a = x then 0
        else
          let l_depth = depth l x in
          if l_depth >= 0 then 1 + l_depth
          else 
            let r_depth = depth r x in
            if r_depth >= 0 then 1 + r_depth 
            else -1

  (* Count number of nodes *)
  let rec size = function
    | Leaf -> 0
    | Node (_, l, r) -> 1 + (size l) + (size r)

  (* Count number of leaves *)
  let rec leaves = function
    | Leaf -> 1
    | Node (_, l, r) -> leaves l + leaves r

  (*
    Pretty print the graph feat. ChatGPT :) 
    You should pass the 'a -> string converter
    (for intance, if you have integer type tree
    pass the "string_of_int" function to it)
    *)
  let pretty_print to_string tree =
    let rec aux prefix = function
      | Leaf -> print_endline (prefix ^ "Leaf")
      | Node (v, l, r) ->
        print_endline (prefix ^ "Node (" ^ to_string v ^ ")");
        aux (prefix ^ "├── ") l;
        aux (prefix ^ "└── ") r
    in aux "" tree
end