signature CHUNK = sig

    type weight =
         SequenceDescriptor.weight

    type ('a, 'b) descr =
         ('a, 'b) SequenceDescriptor.sequence_descriptor
                      
    type ('a, 'b) chunk

    type transient_version = int

    val capacity : int

    val create : ('a, 'b) descr -> transient_version
                 -> ('a, 'b) chunk

    val size : ('a, 'b) chunk -> int

    val weight : ('a, 'b) chunk -> weight

    val cachedValue : ('a, 'b) chunk -> 'b

    val pushFront : ('a, 'b) descr -> transient_version
                    -> (('a, 'b) chunk * 'a) -> ('a, 'b) chunk
                                                         
    val pushBack : ('a, 'b) descr -> transient_version
                   -> (('a, 'b) chunk * 'a) -> ('a, 'b) chunk

    val popFront : ('a, 'b) descr -> transient_version
                   -> (('a, 'b) chunk) -> (('a, 'b) chunk * 'a)

    val popBack : ('a, 'b) descr -> transient_version
                  -> (('a, 'b) chunk) -> (('a, 'b) chunk * 'a)
                                                                 
    val concat : ('a, 'b) descr -> transient_version
                 -> (('a, 'b) chunk * ('a, 'b) chunk) -> ('a, 'b) chunk

    val split : ('a, 'b) descr -> transient_version
                -> (('a, 'b) chunk * int) -> (('a, 'b) chunk * 'a * ('a, 'b) chunk)

    val foldr : ('a * 'b -> 'b) -> 'b -> ('a, 'b) chunk -> 'b

end
