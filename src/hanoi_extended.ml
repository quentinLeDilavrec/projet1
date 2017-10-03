#use "hanoi_common.ml";;
#use "mystack.ml";;
open Graphics;;
open Unix;;
open Mystack;;


let rec build nb f = 
  if nb=0 then [] 
  else f() :: build (nb-1) f
;;


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

(* Initialize random number generator *)
Random.self_init;;

let random_color() =
  let r = Random.int 255
  and g = Random.int 255
  and b = Random.int 255
  in rgb r g b;;


let movement origin destination =
  print_string ("| move a disc from rod " ^ (string_of_int origin) ^ " to rod " ^ (string_of_int destination));
  print_newline ();;


(*-------------Parameters and constants----------------*)

(* Global step counter *)
class counter (init:int) = object
  val count = ref init
  method step () = count := !count+1
  method get () = !count
end;;

(* Types *)
type disc = {taille:int ; color:Graphics.color}
type peg = disc Mystack.t;;

(* Global discs & pegs parameters *)
let nb_discs = 12;;
let pegs: peg array = Array.of_list (build 3 Mystack.create);;
(*let disc_colors = 
  let rec build nb color = 
    if nb=0 then [] 
    else color() :: build (nb-1) color
  in
  Array.of_list (build nb_discs random_color);;*)

(* Window parameters *)
let width = 8 * (15 + 10 * (nb_discs + 1)) + 80
and height = (nb_discs + 2) * 20 + 50;;

let disc_height = 20;;
let disc_base_width = 30;;
let peg_width = 20;;

(* Animation setup *)
let animation_speed = 10.;;

(*--------------Main functions---------------*)

(* Draw the three pegs *)
let draw_pegs () =
  let peg_height = height - 50 in
  let draw_single_peg () =
    draw_rect (current_x () - peg_width / 2) (current_y ()) peg_width peg_height in
  let step = width / 4 in
  begin
    set_color (rgb 0 0 0);
    moveto step 0;
    draw_single_peg ();
    rmoveto step 0;
    draw_single_peg ();
    rmoveto step 0;
    draw_single_peg ();
  end;;

(* Initialize the scene *)
let init () =
  draw_pegs ();
  for i = (nb_discs - 1) downto 0 do
    Mystack.push {taille=i;color=random_color()} pegs.(0)
  done;;

(* Update the graphics state *)
let update_window () =
  let draw_disc x y taille =
    let disc_width = disc_base_width * (taille+1) in
    fill_rect (x - disc_width / 2) y disc_width disc_height in
  begin
    clear_graph ();
    draw_pegs ();
    let handle_peg i peg = (*iter over peg*)
      let call_draw j disc = (*draw disc*)
        begin
          set_color disc.color;
          draw_disc (i*width/((Array.length pegs)-1)) (j*20) disc.taille;
        end in
      Mystack.iteri call_draw peg in
    Array.iteri handle_peg pegs (*iter over pegs*)
  end;;

let counter = new counter 0;;

(* Print a movement and update the program state accordingly *)
let movement_big pegs origin destination =
  print_string ("| move a disc from peg " ^ (string_of_int origin) ^ " to peg " ^ (string_of_int destination));
  print_newline ();
  Mystack.push (Mystack.pop pegs.(origin)) pegs.(destination);
  update_window ();
  counter#step ();
  sleepf (1. /. animation_speed);;


(* Recursively solve the Hanoi towers problem. height is the height of the tower to move from src to dst, and other is the middle rod *)
let launch_hanoi height src other dst =
  begin
    init ();
    let movement = movement_big pegs in
    let rec hanoi height src other dst =
      if height = 1 then movement src dst 
      else begin
        hanoi (height - 1) src dst other;
        movement src dst;
        hanoi (height - 1) other src dst;
      end in  
    hanoi height src other dst
  end;;

(*----------------Main program-----------------*)




open_graph2 width height;;

(* Assign identifiers (e.g. 0 1 2) to the three pegs *)
launch_hanoi nb_discs 0 1 2;;
print_string ("Total number of moves: " ^ (string_of_int (counter#get ())));;

(* Keep the window open until the next key press *)
keep_open ()

