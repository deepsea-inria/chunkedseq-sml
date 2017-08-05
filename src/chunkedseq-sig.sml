signature CHUNKEDSEQ = sig

    structure Chunk : CHUNK

    type ('a, 'b) persistent
            
    type ('a, 'b) transient

    type ('a, 'b) descr = ('a, 'b) Chunk.sequence_descriptor
                                    
    structure Persistent : sig

      type ('a, 'b) t

      val size : ('a, 'b) t -> int

      val cachedValue : ('a, 'b) t -> 'b

      val concat : ('a, 'b) descr
                   -> (('a, 'b) t * ('a, 'b) t) -> ('a, 'b) t

      (* take (xs, n) *)
      (* raises exception Subscript if n < 0 or n > (size xs) *)
      val take : ('a, 'b) descr
                 -> (('a, 'b) t * int) -> ('a, 'b) t

      (* drop (xs, n) *)
      (* raises exception Subscript if n < 0 or n > (size xs) *)
      val drop : ('a, 'b) descr
                 -> (('a, 'b) t * int) -> ('a, 'b) t

      val foldr : ('a * 'b -> 'b) -> 'b -> ('a, 'b) t -> 'b

      val transient : ('a, 'b) t -> ('a, 'b) transient
                  
    end where type ('a, 'b) t = ('a, 'b) persistent

    structure Transient : sig

      type ('a, 'b) t

      val size : ('a, 'b) t -> int

      val cachedValue : ('a, 'b) t -> 'b

      val tabulate : ('a, 'b) descr
                     -> int * (int -> ('a, 'b)) -> ('a, 'b) t
                                     
      val pushFront : ('a, 'b) descr
                      -> (('a, 'b) t * 'a) -> ('a, 'b) t

      val pushBack : ('a, 'b) descr
                     -> (('a, 'b) t * 'a) -> ('a, 'b) t

      (* popFront xs *)
      (* raises exception Empty if (size xs) = 0 *)
      val popFront : ('a, 'b) descr
                     -> ('a, 'b) t -> (('a, 'b) t * 'a)

      (* popBack xs *)
      (* raises exception Empty if (size xs) = 0 *)
      val popBack : ('a, 'b) descr
                    -> ('a, 'b) t -> (('a, 'b) t * 'a)

      val concat : ('a, 'b) descr
                   -> (('a, 'b) t * ('a, 'b) t) -> ('a, 'b) t

      (* take (xs, n) *)
      (* raises exception Subscript if n < 0 or n > (size xs) *)
      val take : ('a, 'b) descr
                 -> (('a, 'b) t * int) -> ('a, 'b) t

      (* drop (xs, n) *)
      (* raises exception Subscript if n < 0 or n > (size xs) *)
      val drop : ('a, 'b) descr
                 -> (('a, 'b) t * int) -> ('a, 'b) t

      val foldr : ('a * 'b -> 'b) -> 'b -> ('a, 'b) t -> 'b

      val persistent : ('a, 'b) t -> ('a, 'b) persistent

    end where type ('a, 'b) t = ('a, 'b) transient
    
end
