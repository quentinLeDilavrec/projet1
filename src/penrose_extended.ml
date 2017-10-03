(* #load "graphics.cma";; *)
(* #use "common.ml"
#use "penrose_common.ml" *)
open Graphics;;
open Common;;
open Penrose_common;;


(*------Types, Constants and Parameters---------*)

(* Generation parameters *)
let start_with_acute_triangle = true;;
let iterations = 6;;

(* Window parameters *)
let height = 800;;

(*------------Triangle division algorithm------------------------------------------------
  Apply one step of the recursive subdivision process which generates a Penrose tiling.
  Precondition:
    t: - apex is always the first point
       - points aren't aligned
*)
let divide_once (penrose_t : penrose_triangle) =
  let t = get_triangle penrose_t in
  if is_obtuse penrose_t then
    let btw = split_line t.s2 t.s1 in
    let a = {apex=t.s1; s1=t.apex; s2=btw}
    and o = {apex=btw; s1=t.apex; s2=t.s2} in
    begin
      set_random_color();
      draw_triangle a;
      set_random_color();
      draw_triangle o;
      [(Acute a);(Obtuse o)]
    end
  else
    let btw = split_line t.s1 t.apex
    and o_h = split_line t.apex t.s2 in
    let a1 = {apex=t.s2; s1=btw; s2=t.s1}
    and a2 = {apex=t.s2; s1=btw; s2=o_h}
    and o  = {apex=o_h; s1=btw; s2=t.apex} in
    begin
      set_random_color();
      draw_triangle a1;
      set_random_color();
      draw_triangle a2;
      set_random_color();
      draw_triangle o;
      [(Acute a1);
       (Acute a2);
       (Obtuse o)]
    end;;


(*------------------Animation management-----------------------
  Manages the animation, the inputs and the only mutable state *)
let handler tri_state x =
  match x.key with
  | '\027' -> raise Exit;
  | ' '    -> tri_state := !tri_state
                           |> List.map divide_once
                           |> List.concat;
  | other  -> draw_string ((Char.escaped other)^" ")

class pen_animation handler = object
  inherit [penrose_triangle] animation handler as super
  method start first_penrose_triangle = 
    set_random_color();
    let triangle = get_triangle first_penrose_triangle in
    draw_triangle triangle;
    super#start first_penrose_triangle;
end;;

let animation = new animation handler;;

let animation = object(self)

  val tri_state = ((ref []):penrose_triangle list ref)

  (* Handle the keybord inputs*)
  method private handler x =
    (*match used as a switch case*)
    match x.key with
    | '\027' -> raise Exit;
    | ' '    -> tri_state := !tri_state
                             |> List.map divide_once
                             |> List.concat;
    | other  -> draw_string ((Char.escaped other)^" ")

  (* Initialise tri_state *)
  method start first_penrose_triangle =
    set_random_color();
    let triangle = get_triangle first_penrose_triangle in
    draw_triangle triangle;
    tri_state := [first_penrose_triangle];
    (loop_at_exit [Key_pressed] self#handler);

    (* Reinitialise tri_state *)
  method restart first_penrose_triangle =
    set_random_color();
    let triangle = get_triangle  first_penrose_triangle in
    draw_triangle triangle;
    tri_state := [first_penrose_triangle];

end;;


(*-------------Main program--------------------------------*)

let width = (* Make sure the triangle fits in the window space *)
  if start_with_acute_triangle 
  then int_of_float ((float_of_int height) *. phi)
  else height;;

(* Create a new window *)
open_graph2 width height;;

(* Setup the first triangle before starting*)
let s1 = Point (0.,0.)
and s2 = Point (0.,float_of_int height) in
let dist = distance s1 s2 in
let height =
  if start_with_acute_triangle
  then (sqrt (phi**2. -. 0.25)) *. dist
  else (sqrt (   1.   -. 0.25*.phi**2.)) *. (dist /. phi) in
let apex = Point (height, dist/.2.) in
let starting_triangle = {apex=apex; s1=s1; s2=s2} in
animation#start (if start_with_acute_triangle 
                 then Acute starting_triangle 
                 else Obtuse starting_triangle);;

let aaaa = ();;