let binary_search target lst =
  let rec aux l r =
    if l > r then -1
    else
      let m = (l + r) / 2 in
      let m_element = List.nth lst m 
      in
        if target = m_element then m
        else if target > m_element then aux (m + 1) r
        else aux l (m - 1)
  in
  aux 0 (List.length lst - 1)


let linear_search target lst =
  let rec aux i = function
    | [] -> -1
    | h::tl when h = target -> i
    | _::tl -> aux (i + 1) tl
  in aux 0 lst


let rec merge_sort comp lst =
  let rec split lst = 
    let rec aux l r = function
      | [] -> (List.rev l, List.rev r)
      | [x] -> (List.rev (x::l), List.rev r)
      | x1::x2::xs -> aux (x1::l) (x2::r) xs
    in
    aux [] [] lst
  in
    let rec merge comp left right = match left, right with
      | [], right -> right
      | left, [] -> left
      | h1::t1, h2::t2 ->
        if comp h1 h2 <= 0 then h1 :: merge comp t1 right
        else h2 :: merge comp left t2
  in
    match lst with
    | [] -> []
    | [x] -> [x]
    | _ ->
      let left, right = split lst in
      let sorted_left = merge_sort comp left in
      let sorted_right = merge_sort comp right in
      merge comp sorted_left sorted_right
