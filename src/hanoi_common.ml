module Hanoi_common :
sig
  (* Global step counter *)
  class counter: int -> object
    val count : int ref
    method step : unit -> unit
    method get : unit -> int
  end

  val movement : int -> int -> unit

end =
struct
  class counter init = object
    val count = ref init
    method step () = count := !count+1
    method get () = !count
  end

  let movement origin destination =
    print_string ("| move a disc from rod " ^ (string_of_int origin) ^ " to rod " ^ (string_of_int destination));
    print_newline ();;
end;;