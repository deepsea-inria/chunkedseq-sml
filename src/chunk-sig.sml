signature CHUNK = sig

    type ('a, 'b) chunk

    type weight = int

    type 'a algebra = {
        combine : 'a * 'a -> 'a,
        identity : 'a,
        inverseOpt : ('a -> 'a) option
    }

    datatype ('a, 'b) sequence_descriptor =
             SequenceDescriptor of {
                 weight  : 'a -> weight,
                 measure : 'a -> 'b,
                 algebra : 'b algebra,
                 trivialItem : 'a,
                 itemOverwrite : bool
             }

    type transient_version = int

    val capacity : int

    val create : ('a, 'b) sequence_descriptor -> transient_version
                 -> ('a, 'b) chunk

    val size : ('a, 'b) chunk -> int

    val weight : ('a, 'b) chunk -> weight

    val cachedValue : ('a, 'b) chunk -> 'b

    val pushFront : ('a, 'b) sequence_descriptor -> transient_version
                    -> (('a, 'b) chunk * 'a) -> ('a, 'b) chunk
                                                         
    val pushBack : ('a, 'b) sequence_descriptor -> transient_version
                   -> (('a, 'b) chunk * 'a) -> ('a, 'b) chunk

    val popFront : ('a, 'b) sequence_descriptor -> transient_version
                   -> (('a, 'b) chunk) -> (('a, 'b) chunk * 'a)

    val popBack : ('a, 'b) sequence_descriptor -> transient_version
                  -> (('a, 'b) chunk) -> (('a, 'b) chunk * 'a)
                                                                 
    val concat : ('a, 'b) sequence_descriptor -> transient_version
                 -> (('a, 'b) chunk * ('a, 'b) chunk) -> ('a, 'b) chunk

    val split : ('a, 'b) sequence_descriptor -> transient_version
                -> (('a, 'b) chunk * int) -> (('a, 'b) chunk * 'a * ('a, 'b) chunk)

    val foldr : ('a * 'b -> 'b) -> 'b -> ('a, 'b) chunk -> 'b

end
