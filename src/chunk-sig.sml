signature CHUNK = sig

    type 'a chunk

    type weight = int

    type 'a weight_fn = 'a -> weight

    type transient_version = int

    val capacity : int

    val create : transient_version -> 'a chunk

    val size : 'a chunk -> int

    val weight : 'a chunk -> weight

    val pushFront : 'a weight_fn -> ('a chunk * transient_version * 'a) -> 'a chunk
                                                         
    val pushBack : 'a weight_fn -> ('a chunk * transient_version * 'a) -> 'a chunk

    val popFront : 'a weight_fn -> ('a chunk * transient_version) -> ('a chunk * 'a)

    val popBack : 'a weight_fn -> ('a chunk * transient_version) -> ('a chunk * 'a)

    val concat : 'a weight_fn -> ('a chunk * transient_version * 'a chunk * transient_version) -> 'a chunk

    val split : 'a weight_fn -> ('a chunk * transient_version * int) -> ('a chunk * 'a * 'a chunk)

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a chunk -> 'b

end
