signature CHUNKEDSEQ = sig

    datatype ('a, 'b) metadata
      = MetaData of {
          measure : ('a, 'b) Measure.t,
          trivialItem : 'a,
          itemOverwrite : bool
      }

    type ('a, 'b) persistent
            
    type ('a, 'b) transient
                                    
    structure Persistent : sig

      type ('a, 'b) t

      val length : ('a, 'b) t -> int

      val measure : ('a, 'b) t -> 'b

      val concat : ('a, 'b) metadata
                   -> (('a, 'b) t * ('a, 'b) t) -> ('a, 'b) t

      (* If the find by measure fails, the exception Find_by is raised. *)
      val take : ('a, 'b) metadata
                 -> (('a, 'b) t * 'b Measure.find_by) -> ('a, 'b) t

      (* If the find by measure fails, the exception Find_by is raised. *)
      val drop : ('a, 'b) metadata
                 -> (('a, 'b) t * 'b Measure.find_by) -> ('a, 'b) t

      val foldr : ('a * 'b -> 'b) -> 'b -> ('a, 'b) t -> 'b

      val transient : ('a, 'b) t -> ('a, 'b) transient
                  
    end where type ('a, 'b) t = ('a, 'b) persistent

    structure Transient : sig

      type ('a, 'b) t

      val length : ('a, 'b) t -> int

      val measure : ('a, 'b) t -> 'b

      (* It raises Size if n < 0. *)
      val tabulate : ('a, 'b) metadata
                     -> int * (int -> 'a) -> ('a, 'b) t

      val pushFront : ('a, 'b) metadata
                      -> (('a, 'b) t * 'a) -> ('a, 'b) t

      val pushBack : ('a, 'b) metadata
                     -> (('a, 'b) t * 'a) -> ('a, 'b) t

      (* It raises Empty if the input sequence is empty. *)
      val popFront : ('a, 'b) metadata
                     -> ('a, 'b) t -> (('a, 'b) t * 'a)

      (* It raises Empty if the input sequence is empty. *)
      val popBack : ('a, 'b) metadata
                    -> ('a, 'b) t -> (('a, 'b) t * 'a)

      val concat : ('a, 'b) metadata
                   -> (('a, 'b) t * ('a, 'b) t) -> ('a, 'b) t

      (* If the find by measure fails, the exception Find_by is raised. *)
      val split : ('a, 'b) metadata
                  -> (('a, 'b) t * 'b Measure.find_by)
                  -> (('a, 'b) t * 'a * ('a, 'b) t)

      val foldr : ('a * 'b -> 'b) -> 'b -> ('a, 'b) t -> 'b

      val persistent : ('a, 'b) t -> ('a, 'b) persistent

    end where type ('a, 'b) t = ('a, 'b) transient
    
end
