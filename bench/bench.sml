structure CS = ChunkedseqFn(ListChunk)

val xs = CS.push_back (CS.create, 123)
val xs = CS.push_front (CS.create, 321)
val (xs, _) = CS.pop_back xs

fun main () = raise Fail "todo"

val _ = main ()
