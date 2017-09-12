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

    type 'a metadata' =
         ('a metadata * transient_version)

    val capacity : int

    val create : 'a metadata'
                 -> 'a chunk

    val length : 'a chunk -> int

    val measure : 'a chunk -> measure

    val sub : 'a metadata
              -> ('a chunk * Search.find_by) -> 'a
                                                    
    structure Front : END_ACCESS
                          where type 'a t = 'a chunk
                            and type 'a metadata = 'a metadata'

    structure Back  : END_ACCESS
                          where type 'a t = 'a chunk
                            and type 'a metadata = 'a metadata'
                                       
    val concat : 'a metadata'
                 -> ('a chunk * 'a chunk) -> 'a chunk

    val split : 'a metadata'
                -> ('a chunk * Search.find_by)
                -> ('a chunk * 'a * 'a chunk)

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a chunk -> 'b

end
