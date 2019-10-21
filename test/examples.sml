structure Algebra = WeightAlgebra
structure Measure = MeasureFn(
    structure Algebra = Algebra
    val weightOpt = SOME(fn w => w)
)
structure Search = SearchFn(structure Measure = Measure)

structure C = ChunkedseqSpecFn(structure Search = Search)

fun printList (sep, printItem, xs) =
  let fun pl (sep, printItem, xs) =
        (case xs of
             [] =>
             ()
           | [x] =>
             printItem x
           | x :: xs => (
               printItem x;
               print sep;
               pl (sep, printItem, xs)))
  in
      print "[";
      pl (sep, printItem, xs);
      print "]"
  end

structure ListChunk = ListChunkFn(
    structure Search = Search
    val capacity = 3)
structure U = BootstrappedChunkedseqFn(
    structure Chunk = ListChunk)

fun listOfTransient cs =
  C.Transient.foldr (fn (x, y) => x :: y) [] cs

fun listOfPersistent cs =
  C.Persistent.foldr (fn (x, y) => x :: y) [] cs

fun listOfTransientU cs =
  U.Transient.foldr (fn (x, y) => x :: y) [] cs

fun listOfPersistentU cs =
  U.Persistent.foldr (fn (x, y) => x :: y) [] cs

fun printItem x = print (Int.toString x)
fun printTransient c = printList (",", printItem, listOfTransient c)
fun printPersistent c = printList (",", printItem, listOfPersistent c)
fun printTransientU c = printList (",", printItem, listOfTransientU c)
fun printPersistentU c = printList (",", printItem, listOfPersistentU c)

local
    val md = C.MetaData{measure=fn _ => 1}
    val x = C.Transient.persistent (C.Transient.tabulate md (3, fn i => i))
    val y = C.Persistent.drop md (x, C.Search.Index 2)
    val _ = printPersistent y
in
val _ = print "\n"
end

local
    val md = U.MetaData{measure=fn _ => 1}
    val x = U.Transient.persistent (U.Transient.tabulate md (3, fn i => i))
    val y = U.Persistent.drop md (x, U.Search.Index 2)
    val _ = printPersistentU y
in
val _ = print "\n"
end
