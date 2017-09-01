(* TODO: 

Refactor chunk so that we can use END_ACCESS in a manner similar to
that of the chunkedseq. The only difference is going to be that we
have to deal with transient version numbers. To deal, we can just pair
up metadata and transient version for all operations.

 *)

signature CHUNK = sig
    
    type 'a chunk

    structure Search : SEARCH

    type measure =
         Search.measure

    datatype 'a metadata
      = MetaData of {
          measure : 'a Search.Measure.measure_fn,
          trivialItem : 'a,
          itemOverwrite : bool
      }

    type transient_version =
         int

    val capacity : int

    val create : 'a metadata -> transient_version
                 -> 'a chunk

    val length : 'a chunk -> int

    val measure : 'a chunk -> measure

    val sub : 'a metadata
              -> ('a chunk * Search.find_by) -> 'a

    val pushFront : 'a metadata -> transient_version
                    -> ('a chunk * 'a) -> 'a chunk
                                             
    val pushBack : 'a metadata -> transient_version
                   -> ('a chunk * 'a) -> 'a chunk

    val popFront : 'a metadata -> transient_version
                   -> ('a chunk) -> ('a chunk * 'a)

    val popBack : 'a metadata -> transient_version
                  -> ('a chunk) -> ('a chunk * 'a)
                                       
    val concat : 'a metadata -> transient_version
                 -> ('a chunk * 'a chunk) -> 'a chunk

    val split : 'a metadata -> transient_version
                -> ('a chunk * Search.find_by)
                -> ('a chunk * 'a * 'a chunk)

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a chunk -> 'b

end
