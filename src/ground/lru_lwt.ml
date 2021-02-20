let memo (type k v) ?(hashed = (Hashtbl.hash, ( = ))) ?(weight = fun _ -> 1)
    ~cap f =
  let module Hashed = struct
    type t = k

    let hash = fst hashed

    let equal = snd hashed
  end in
  let module Weighted = struct
    type t = v

    let weight = weight
  end in
  let module PC = Hashtbl.Make (Hashed) in
  let module C = Lru.M.Make (Hashed) (Weighted) in
  let pc = PC.create 0 in
  let c = C.create cap in
  let rec g k =
    match C.find k c with
    | Some v ->
        C.promote k c;
        Lwt.return v
    | None -> (
        match PC.find_opt pc k with
        | Some p -> p
        | None ->
            let p = f g k in
            PC.replace pc k p;
            let%lwt v = p in
            C.add k v c;
            PC.remove pc k;
            Lwt.return v)
  in
  g
