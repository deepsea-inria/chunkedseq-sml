signature CHUNKEDSEQ = sig

    structure Search : SEARCH

    type measure =
         Search.measure

    datatype 'a metadata
      = MetaData of {
          measure : 'a Search.Measure.measure_fn,
          trivialItem : 'a,
          itemOverwrite : bool
      }

    type 'a persistent
            
    type 'a transient
                                    
    structure Persistent : sig

      type 'a t

      val length : 'a t -> int

      val measure : 'a t -> measure

      val concat : 'a metadata
                   -> ('a t * 'a t) -> 'a t

      (* If the find by measure fails, the exception Find_by is raised. *)
      val take : 'a metadata
                 -> ('a t * Search.find_by) -> 'a t

      (* If the find by measure fails, the exception Find_by is raised. *)
      val drop : 'a metadata
                 -> ('a t * Search.find_by) -> 'a t

      val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b

      val transient : 'a t -> 'a transient
                  
    end where type 'a t = 'a persistent

    structure Transient : sig

      type 'a t

      val length : 'a t -> int

      val measure : 'a t -> measure

      (* It raises Length if n < 0. *)
      val tabulate : 'a metadata
                     -> int * (int -> 'a) -> 'a t

      val pushFront : 'a metadata
                      -> ('a t * 'a) -> 'a t

      val pushBack : 'a metadata
                     -> ('a t * 'a) -> 'a t

      (* It raises Empty if the input sequence is empty. *)
      val popFront : 'a metadata
                     -> 'a t -> ('a t * 'a)

      (* It raises Empty if the input sequence is empty. *)
      val popBack : 'a metadata
                    -> 'a t -> ('a t * 'a)

      val concat : 'a metadata
                   -> ('a t * 'a t) -> 'a t

      (* If the find by measure fails, the exception Find_by is raised. *)
      val split : 'a metadata
                  -> ('a t * Search.find_by)
                  -> ('a t * 'a * 'a t)

      val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b

      val persistent : 'a t -> 'a persistent

    end where type 'a t = 'a transient
    
end
