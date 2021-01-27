let binary_search ~cmp ~get container left right key =
  let rec aux left right =
    if left > right then `Just_after right
    else
      let middle = (left + right) / 2 in
      match cmp (get container middle) key with
      | 0 -> `At middle
      | n when n > 0 -> aux left (middle - 1)
      | _ -> aux (middle + 1) right
  in
  if left >= right then `Empty else aux left right
