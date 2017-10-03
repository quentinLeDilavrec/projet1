open Graphics;;

module Common :
sig
  val random_color : unit -> Graphics.color
  val set_random_color : unit -> unit
  val open_graph2 : int -> int -> unit
  val keep_open : unit -> unit
end =
struct
  Random.self_init;;
  let random_color() =
    let r = Random.int 255
    and g = Random.int 255
    and b = Random.int 255
    in rgb r g b;;

  (* Set the frawing color to a random one *)
  let set_random_color() =
    set_color (random_color());;

  (* Create a new window *)
  let open_graph2 width height =
    begin
      (* Close any possible open window *)
      close_graph();
      (* Create a new window *)
      open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"+0-0")
    end;;

  let keep_open () =
    ignore (Graphics.read_key ());;

end;;