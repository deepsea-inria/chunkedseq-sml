signature CHUNK = sig
    
    type 'a chunk

    structure Measure : MEASURE

    datatype 'a metadata
      = MetaData of {
          measure : 'a Measure.measure_fn,
          trivialItem : 'a,
          itemOverwrite : bool
      }

    type transient_version =
         int

    val capacity : int

    val create : 'a metadata -> transient_version
                 -> 'a chunk

    val size : 'a chunk -> int

    val measure : 'a metadata
                  -> 'a chunk -> Measure.t

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
                -> ('a chunk * Measure.find_by)
                -> ('a chunk * 'a * 'a chunk)

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a chunk -> 'b

end
