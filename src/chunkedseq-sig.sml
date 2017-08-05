signature CHUNKEDSEQ = sig

    type 'a persistent
            
    type 'a transient
                                    
    structure Persistent : sig

      type 'a t

      val size : 'a t -> int

      val concat : ('a t * 'a t) -> 'a t

      (* take (xs, n) *)
      (* raises exception Subscript if n < 0 or n > (size xs) *)
      val take : ('a t * int) -> 'a t

      (* drop (xs, n) *)
      (* raises exception Subscript if n < 0 or n > (size xs) *)
      val drop : ('a t * int) -> 'a t

      val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b

      val transient : 'a t -> 'a transient
                  
    end where type 'a t = 'a persistent

    structure Transient : sig

      type 'a t

      val size : 'a t -> int

      val tabulate : int * (int -> 'a) -> 'a t
                                     
      val pushFront : ('a t * 'a) -> 'a t

      val pushBack : ('a t * 'a) -> 'a t

      (* popFront xs *)
      (* raises exception Empty if (size xs) = 0 *)
      val popFront : 'a t -> ('a t * 'a)

      (* popBack xs *)
      (* raises exception Empty if (size xs) = 0 *)
      val popBack : 'a t -> ('a t * 'a)

      val concat : ('a t * 'a t) -> 'a t

      (* take (xs, n) *)
      (* raises exception Subscript if n < 0 or n > (size xs) *)
      val take : ('a t * int) -> 'a t

      (* drop (xs, n) *)
      (* raises exception Subscript if n < 0 or n > (size xs) *)
      val drop : ('a t * int) -> 'a t

      val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b

      val persistent : 'a t -> 'a persistent

    end where type 'a t = 'a transient
    
end
