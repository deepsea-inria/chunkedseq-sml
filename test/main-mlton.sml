val n = (case CommandLine.arguments() of
             [n] => (case Int.fromString n of
                         SOME n => n
                       | NONE => raise Fail "bogus number")
           | _ => raise Fail "bogus command line")

val _ = Test.check_loop n
