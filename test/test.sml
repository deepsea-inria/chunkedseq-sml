structure Test = struct

structure CS = ChunkedseqFn(ListChunk)

fun list_of cs =
  CS.foldr (fn (x, y) => x :: y) [] cs

val c = CS.push_back (CS.create, 123)
val xs = list_of c

end
