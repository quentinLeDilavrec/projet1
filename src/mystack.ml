
(*taken from ocaml repo on github
 I just added the iteri*)

module type Mystack =
sig

  type 'a t
  (** The type of stacks containing elements of type ['a]. *)
  
  exception Empty
  (** Raised when {!Stack.pop} or {!Stack.top} is applied to an empty stack. *)

  val create : unit -> 'a t
  (** Return a new stack, initially empty. *)
  
  val push : 'a -> 'a t -> unit
  (** [push x s] adds the element [x] at the top of stack [s]. *)
  
  val pop : 'a t -> 'a
  (** [pop s] removes and returns the topmost element in stack [s],
     or raises {!Empty} if the stack is empty. *)
  
  val top : 'a t -> 'a
  (** [top s] returns the topmost element in stack [s],
     or raises {!Empty} if the stack is empty. *)
  
  val clear : 'a t -> unit
  (** Discard all elements from a stack. *)
  
  val copy : 'a t -> 'a t
  (** Return a copy of the given stack. *)
  
  val is_empty : 'a t -> bool
  (** Return [true] if the given stack is empty, [false] otherwise. *)
  
  val length : 'a t -> int
  (** Return the number of elements in a stack. Time complexity O(1) *)
  
  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f s] applies [f] in turn to all elements of [s],
     from the element at the top of the stack to the element at the
     bottom of the stack. The stack itself is unchanged. *)

  val iteri : ('a -> int -> unit) -> 'a t -> unit
  (** [iteri f s] applies [f] in turn to all elements of [s],
     from the element at the top of the stack to the element at the
     bottom of the stack. The stack itself is unchanged. 
     plus it give to f the position in s*)
  
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** [fold f accu s] is [(f (... (f (f accu x1) x2) ...) xn)]
      where [x1] is the top of the stack, [x2] the second element,
      and [xn] the bottom element. The stack is unchanged.
      @since 4.03 *)
end
;;
 
module Mystack = 
struct
  type 'a t = { mutable c : 'a list; mutable len : int; }
  
  exception Empty
  
  let create () = { c = []; len = 0; }
  
  let clear s = s.c <- []; s.len <- 0
  
  let copy s = { c = s.c; len = s.len; }
  
  let push x s = s.c <- x :: s.c; s.len <- s.len + 1
  
  let pop s =
    match s.c with
    | hd::tl -> s.c <- tl; s.len <- s.len - 1; hd
    | []     -> raise Empty
  
  let top s =
    match s.c with
    | hd::_ -> hd
    | []     -> raise Empty
  
  let is_empty s = (s.c = [])
  
  let length s = s.len
  
  let iter f s = List.iter f s.c
  
  let iteri f s = List.iteri f s.c
  
  let fold f acc s = List.fold_left f acc s.c
end
;;