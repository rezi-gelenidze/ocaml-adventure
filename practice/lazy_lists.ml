module LazyList = struct
  type 'a lazy_list = Nil | Cons of 'a * 'a lazy_list Lazy.t

  let head = function
    | Nil -> None
    | Cons (h, _) -> Some h

  let tail = function
    | Nil -> None
    | Cons (_, tl) -> Some (Lazy.force tl)

  let rec take n = function
    | Nil -> []
    | Cons (h, tl) ->
      if n > 0 then h :: take (n - 1) (Lazy.force tl)
      else []

  let rec drop lst n =
    match lst with
      | Nil -> lst
      | Cons (h, tl) when n = 0 -> lst
      | Cons (h, tl) -> drop (Lazy.force tl) (n - 1)

  let rec map f = function
    | Nil -> Nil
    | Cons (h, tl) -> Cons ((f h), lazy (map f (Lazy.force tl)))

  let rec filter predicate = function
    | Nil -> Nil
    | Cons (h, tl) ->
      let lazy_tail = lazy (filter predicate (Lazy.force tl)) in
      if predicate h then
        Cons (h, lazy_tail)
      else
        Lazy.force lazy_tail
  
  let rec fold_left f acc = function
    | Nil -> acc
    | Cons (h, tl) ->
      fold_left f (f acc h) (Lazy.force tl)

      
  let rec of_list = function
    | [] -> Nil
    | h::tl -> Cons (h, lazy (of_list tl))

  let to_list llst = 
    List.rev @@ fold_left (fun acc x -> x :: acc) [] llst
end
