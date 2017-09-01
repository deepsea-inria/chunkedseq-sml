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
