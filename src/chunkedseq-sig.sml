signature CHUNKEDSEQ = sig

    type 'a chunkedseq

    val create : 'a chunkedseq

    val size : 'a chunkedseq -> int

    val empty : 'a chunkedseq -> bool

    val push_front : ('a chunkedseq * 'a) -> 'a chunkedseq
                                                         
    val push_back : ('a chunkedseq * 'a) -> 'a chunkedseq

    val pop_front : 'a chunkedseq -> ('a chunkedseq * 'a)

    val pop_back : 'a chunkedseq -> ('a chunkedseq * 'a)

    val concat : ('a chunkedseq * 'a chunkedseq) -> 'a chunkedseq

    val split : ('a chunkedseq * int) -> ('a chunkedseq * 'a * 'a chunkedseq)

    val sub : 'a chunkedseq * int -> 'a

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a chunkedseq -> 'b
    
end
