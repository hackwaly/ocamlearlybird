(* Fixture for the value rendering test: one local per shape of OCaml value the
   adapter knows how to render. Keep the line numbers stable as the tests set
   breakpoints by line.

   Every local is referenced at the end so none can be dropped as unused, which
   would otherwise make the set of visible locals depend on the compiler. *)

type person = { name : string; age : int }
type shape = Circle of float | Square of int | Point

let () =
  let int_ = 42 in
  let float_ = 1.5 in
  let string_ = "hi" in
  let char_ = 'x' in
  let bool_ = true in
  let unit_ = () in
  let int64_ = 9L in
  let list_ = [ 1; 2; 3 ] in
  let array_ = [| 4; 5 |] in
  let tuple_ = (1, "two") in
  let option_ = Some 7 in
  let none_ = (None : int option) in
  let record_ = { name = "bob"; age = 30 } in
  let variant_ = Circle 2.5 in
  let constant_ = Point in
  let square_ = Square 3 in
  let closure_ = fun n -> n + int_ in
  let lazy_ = lazy (int_ + 1) in
  ignore
    ( int_, float_, string_, char_, bool_, unit_, int64_, list_, array_, tuple_,
      option_, none_, record_, variant_, constant_, square_, closure_ 1,
      Lazy.force lazy_ );
  print_endline "done"
