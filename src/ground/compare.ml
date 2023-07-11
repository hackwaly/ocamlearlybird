type 'a t = 'a -> 'a -> int

let by ?(cmp = compare) f a b = cmp (f a) (f b)

let by2 ?(cmp = compare) f1 f2 a b = cmp (f1 a) (f2 b)

let opp cmp a b = cmp b a

let chain l a b =
  let rec aux l =
    match l with
    | [] -> 0
    | cmp :: l -> ( match cmp a b with 0 -> aux l | r -> r )
  in
  aux l

let to_equal cmp a b = cmp a b = 0
