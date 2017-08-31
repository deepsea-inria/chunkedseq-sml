signature END_ACCESS = sig

    type 'a metadata

    type 'a t
    
    (* It raises Empty if the input sequence is empty. *)
    val read : 'a t -> 'a
                           
    val push : 'a metadata
               -> ('a t * 'a) -> 'a t
                                    
    (* It raises Empty if the input sequence is empty. *)
    val pop : 'a metadata
              -> 'a t -> ('a t * 'a)
                             
    (* It raises Subscript if di < 0 or if length dst < di + length src. *)
    val readn : 'a metadata
                -> { src : 'a t, dst : 'a Array.array, di : int} -> unit
                                                                        
    val pushn : 'a metadata
                -> ('a t * 'a ArraySlice.slice) -> 'a t
                                                      
end

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
      val sub : 'a metadata
                -> ('a t * Search.find_by) -> 'a

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
      val sub : 'a metadata
                -> ('a t * Search.find_by) -> 'a

      (* If the find by measure fails, the exception Find_by is raised. *)
      val split : 'a metadata
                  -> ('a t * Search.find_by)
                  -> ('a t * 'a * 'a t)

      val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b

      val persistent : 'a t -> 'a persistent

    end where type 'a t = 'a transient
    
end
