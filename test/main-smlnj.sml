(* 
 * ml-build -Ctdp.instrument=true \$smlnj-tdp/back-trace.cm sources.cm SMLNJTest.main test && sml @SMLload=test 20
 *)
structure SMLNJTest = struct

structure Algebra = WeightAlgebra
structure Measure = MeasureFn(structure Algebra = Algebra)
structure Search = SearchFn(structure Measure = Measure)

structure ChunkedseqSpec = ChunkedseqSpecFn(structure Search = Search)
structure ListChunk = ListChunkFn(
    structure Search = Search
    val capacity = 32)
structure Chunkedseq = BootstrappedChunkedseqFn(
    structure Chunk = ListChunk
    val getWeight = SOME(fn (w:ListChunk.measure) => w))

structure Test = ChunkedseqTestFn(
    structure Trusted = ChunkedseqSpec
    structure Untrusted = Chunkedseq
    fun measureEq (w1 : Trusted.Search.measure, w2 : Untrusted.Search.measure) = w1 = w2
    val metaDataTrusted = Trusted.MetaData { measure = fn _ => 1 }
    val metaDataUntrusted = Untrusted.MetaData { measure = fn _ => 1 })

fun main (name, args) =
  BackTrace.monitor(fn () => (
                        (case args of
                             [n] => (case Int.fromString n of
                                         SOME n =>
                                         let val t = Test.randomTraceTransient ()
                                         in
                                             Test.check t
                                         end
                                       | NONE => raise Fail "bogus command line argument")
                           | _ => raise Fail "bogus command line");
                        0))

end
