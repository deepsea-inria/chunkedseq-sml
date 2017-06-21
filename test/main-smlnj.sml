(* 
 * ml-build -Ctdp.instrument=true \$smlnj-tdp/back-trace.cm sources.cm SMLNJTest.main test && sml @SMLload=test 20
 *)
structure SMLNJTest = struct

fun main (name, args) =
  BackTrace.monitor(fn () => (
                        (case args of
                             [n] => (case Int.fromString n of
                                         SOME n => Test.check_loop n
                                       | NONE => raise Fail "bogus command line argument")
                           | _ => raise Fail "bogus command line");
                        0))

end
