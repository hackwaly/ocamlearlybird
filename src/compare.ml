type 'a t = 'a -> 'a -> int

type ordering = Ascending | Descending

let by ?(cmp = compare) f a b = cmp (f a) (f b)

let reverse compare a b = compare b a
