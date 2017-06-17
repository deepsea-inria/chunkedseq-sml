signature CHUNK = sig

    type weight = int

    type 'a weight_fn = 'a -> weight

    type 'a chunk

    val k : int

    val create : 'a chunk

    val size : 'a chunk -> int

    val empty : 'a chunk -> bool

    val full : 'a chunk -> bool

    val weight : 'a chunk -> weight

    val push_front : 'a weight_fn -> ('a chunk * 'a) -> 'a chunk
                                                         
    val push_back : 'a weight_fn -> ('a chunk * 'a) -> 'a chunk

    val pop_front : 'a weight_fn -> 'a chunk -> ('a chunk * 'a)

    val pop_back : 'a weight_fn -> 'a chunk -> ('a chunk * 'a)

    val concat : 'a weight_fn -> ('a chunk * 'a chunk) -> 'a chunk

    val split : 'a weight_fn -> ('a chunk * int) -> ('a chunk * 'a * 'a chunk)

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a chunk -> 'b

end