open Global
include Unix

module Sockaddr = struct
  type t = Unix.sockaddr

  let to_string = function
    | Unix.ADDR_UNIX addr -> addr
    | ADDR_INET (addr, port) ->
        Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port
end
