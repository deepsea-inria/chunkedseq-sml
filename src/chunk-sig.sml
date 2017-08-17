signature CHUNK = sig
                      
    type ('a, 'b) chunk

    datatype ('a, 'b) metadata
      = MetaData of {
          measure : ('a, 'b) Measure.t,
          trivialItem : 'a,
          itemOverwrite : bool
      }

    type transient_version =
         int

    val capacity : int

    val create : ('a, 'b) metadata -> transient_version
                 -> ('a, 'b) chunk

    val size : ('a, 'b) chunk -> int

    val measure : ('a, 'b) metadata
                  -> ('a, 'b) chunk -> 'b

    val pushFront : ('a, 'b) metadata -> transient_version
                    -> (('a, 'b) chunk * 'a) -> ('a, 'b) chunk
                                                         
    val pushBack : ('a, 'b) metadata -> transient_version
                   -> (('a, 'b) chunk * 'a) -> ('a, 'b) chunk

    val popFront : ('a, 'b) metadata -> transient_version
                   -> (('a, 'b) chunk) -> (('a, 'b) chunk * 'a)

    val popBack : ('a, 'b) metadata -> transient_version
                  -> (('a, 'b) chunk) -> (('a, 'b) chunk * 'a)
                                                                 
    val concat : ('a, 'b) metadata -> transient_version
                 -> (('a, 'b) chunk * ('a, 'b) chunk) -> ('a, 'b) chunk

    val split : ('a, 'b) metadata -> transient_version
                -> (('a, 'b) chunk * 'b Measure.find_by)
                -> (('a, 'b) chunk * 'a * ('a, 'b) chunk)

    val foldr : ('a * 'b -> 'b) -> 'b -> ('a, 'b) chunk -> 'b

end
