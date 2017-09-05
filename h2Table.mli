type 'a t
(**
create hsize index
@ hsize : the size of the hash-table
@ index : index of the first element
**)
val create : int -> int -> 'a t
val memA : 'a t -> 'a -> bool
val memI : 'a t -> int -> bool
val push : 'a t -> 'a -> int
val pull : 'a t -> int -> 'a
val length : 'a t -> int
val iter : 'a t -> ('a -> int -> unit) -> unit
val map  : 'a t -> (int -> 'a -> 'b  ) -> 'b list
val mapreduce : 'a t -> 'c -> (int -> 'a -> 'b) -> ('b -> 'c -> 'c) -> 'c

val strdump : ('a -> Tree.stree) ->'a t -> Tree.stree
val strload : int -> (Tree.stree -> 'a) -> Tree.stree -> 'a t
