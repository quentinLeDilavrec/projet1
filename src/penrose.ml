open Graphics;;
open Common;;
open Penrose_common;;


(*--------Parameters---------*)

(* Generation parameters *)
let start_with_acute_triangle = true;;
let iterations = 6;;

(* Window parameter *)
let height = 800;;

(*------------Recursive triangle division algorithm------------
  Precondition:
    t: - apex is always the first point
       - points aren't aligned
*)
let rec divide generation (penrose_t : penrose_triangle) =
  let t = get_triangle penrose_t in
  if generation = 0
  then begin
    set_random_color();
    draw_triangle t;
  end
  else if is_obtuse penrose_t then
    let btw = split_line t.s2 t.s1 in
    let a = {apex=t.s1; s1=t.apex; s2=btw}
    and o = {apex=btw; s1=t.apex; s2=t.s2} in
    begin
      divide (generation-1) (Acute a);
      divide (generation-1) (Obtuse o);
    end
  else
    let btw = split_line t.s1 t.apex
    and o_h = split_line t.apex t.s2 in
    let a1 = {apex=t.s2; s1=btw; s2=t.s1}
    and a2 = {apex=t.s2; s1=btw; s2=o_h}
    and o  = {apex=o_h; s1=btw; s2=t.apex} in
    begin
      divide (generation-1) (Acute a1);
      divide (generation-1) (Acute a2);
      divide (generation-1) (Obtuse o);
    end;;


(*----------------Main program----------------*)


let width = 
  (* Make sure the triangle fits in the window space *)
  if start_with_acute_triangle then int_of_float ((float_of_int height) *. phi)
  else height;;

(* Create a new window *)
open_graph2 width height;;


(* Setup the starting triangle for the first iteration *)
let s1 = Point (0.,0.)
and s2 = Point(0.,float_of_int height) in
let dist = distance s1 s2 in
let height =
  if start_with_acute_triangle
  then (sqrt (phi**2. -. 0.25)) *. dist
  else (sqrt (   1.   -. 0.25*.phi**2.)) *. (dist /. phi) in
let apex = Point (height, dist/.2.) in
let starting_triangle = {apex=apex; s1=s1; s2=s2} in

divide iterations (if start_with_acute_triangle 
                   then Acute starting_triangle 
                   else Obtuse starting_triangle);;

(* Keep the graph open until a key is pressed *)
keep_open();