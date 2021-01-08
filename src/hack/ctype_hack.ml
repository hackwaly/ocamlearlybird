include Ctype

let apply env =
  Ctype.apply (Obj.magic env)

let matches env =
  Ctype.matches (Obj.magic env)
