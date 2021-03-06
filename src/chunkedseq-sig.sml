signature CHUNKEDSEQ = sig

    structure Search : SEARCH

    type measure = Search.measure

    datatype 'a metadata
      = MetaData of {
          measure : 'a Search.Measure.measure_fn
      }

    type 'a persistent
            
    type 'a transient
                                    
    structure Persistent : sig

      type 'a t = 'a persistent

      val length : 'a t -> int

      val measure : 'a t -> measure

      val concat : 'a metadata
                   -> ('a t * 'a t) -> 'a t

      (* If the find by measure fails, the exception Find_by is raised. *)
      val find : 'a metadata
                -> ('a t * Search.find_by) -> 'a

      (* If the find by measure fails, the exception Find_by is raised. *)
      val take : 'a metadata
                 -> ('a t * Search.find_by) -> 'a t

      (* If the find by measure fails, the exception Find_by is raised. *)
      val drop : 'a metadata
                 -> ('a t * Search.find_by) -> 'a t

      val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b

      val transient : 'a t -> 'a transient
                  
    end

    structure Transient : sig

      type 'a t = 'a transient

      val length : 'a t -> int

      val measure : 'a t -> measure

      (* It raises Length if n < 0. *)
      val tabulate : 'a metadata ->
                     (int * (int -> 'a)) -> 'a t

      structure Front : END_ACCESS
                            where type 'a t = 'a t
                              and type 'a metadata = 'a metadata

      structure Back  : END_ACCESS
                            where type 'a t = 'a t
                              and type 'a metadata = 'a metadata

      val concat : 'a metadata
                   -> ('a t * 'a t) -> 'a t

      (* If the find by measure fails, the exception Find_by is raised. *)
      val find : 'a metadata
                -> ('a t * Search.find_by) -> 'a

      (* If the find by measure fails, the exception Find_by is raised. *)
      val split : 'a metadata
                  -> ('a t * Search.find_by)
                  -> ('a t * 'a * 'a t)

      val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b

      val persistent : 'a t -> 'a persistent

    end
    
end

