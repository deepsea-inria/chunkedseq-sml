functor ChunkedseqTestFn (
    structure Trusted : CHUNKEDSEQ
    structure Untrusted : CHUNKEDSEQ
    val measureEq : (Trusted.Search.measure * Untrusted.Search.measure) -> bool
    val metaDataTrusted : 'a Trusted.metadata
    val metaDataUntrusted : 'a Untrusted.metadata
) = struct

    structure T = Trusted
    structure U = Untrusted

    type item = int

    datatype orientation = EndFront | EndBack

    and trace_transient
        = TTLength of trace_transient
        | TTMeasure of trace_transient
        | TTTabulate of int * trace_transient
        | TTPush of orientation * item * trace_transient
        | TTPop of orientation * trace_transient
        | TTSplitConcat of (int * trace_transient * trace_transient * trace_transient)
        | TTFoldr of trace_transient
        | TTPersistent of trace_persistent * trace_transient option

    and trace_persistent
        = TPNil
        | TPLength of trace_persistent
        | TPMeasure of trace_persistent
        | TPTakeDropConcat of (int * trace_persistent * trace_persistent * trace_persistent)
        | TPFoldr of trace_persistent
        | TPTransient of trace_transient * trace_persistent

    local
        val r = Random.rand (1, 1)
    in
    
    fun randomItem () =
      Random.randRange (1, 2048) r
                       
    fun randomOrientation () =
      if Random.randRange (0, 1) r = 0 then
          EndFront
      else
          EndBack

    fun randomTraceTransient' (n, d) =
      if n = 0 then
          let val (r, tr') = randomTracePersistent' (n, d)
          in
              (r, TTPersistent (tr', NONE))
          end
      else if Random.randRange (0, d + 3) r = 0 then
          let val i = Random.randRange (0, n) r mod n
              val (_, tr1) = randomTraceTransient' (i, d + 1)
              val (_, tr2) = randomTraceTransient' (n - i - 1, d + 1)
              val (r, tr') = randomTraceTransient' (n, d + 2)
          in
              (r, TTSplitConcat (i, tr1, tr2, tr'))
          end
      else if (Random.randRange (0, 2 + Word.toInt (Word.<< (0wx1, Word.fromInt n))) r) < 3 then
          let val or = randomOrientation ()
              val x = randomItem ()
              val (r, t) = randomTraceTransient' (n + 1, d)
          in
              (r, TTPush (or, x, t))
          end
      else
          let val nb = 5
          in
              case Random.randRange (0, nb) r
               of 0 =>
                  let val (r, tr') = randomTraceTransient' (n, d)
                  in
                      (r, TTLength tr')
                  end
                | 1 =>
                  let val (r, tr') = randomTraceTransient' (n, d)
                  in
                      (r, TTMeasure tr')
                  end
                | 2 =>
                  let val n = Random.randRange (0, 256) r
                      val (r, tr') = randomTraceTransient' (n, d)
                  in
                      (r, TTTabulate (n, tr'))
                  end
                | 3 =>
                  let val (r, tr') = randomTraceTransient' (n, d)
                  in
                      (r, TTFoldr tr')
                  end
                | 4 =>
                  let val ((n, d), trP) = randomTracePersistent' (n, d)
                      val (r, trT) = randomTraceTransient' (n, d)
                  in
                      (r, TTPersistent (trP, SOME trT))
                  end
                | _ =>
                  raise Fail "impossible"
          end

    and randomTracePersistent' (n, d) =
      if n = 0 then
          ((n, d), TPNil)
      else if Random.randRange (0, d + 3) r = 0 then
          let val i = Random.randRange (0, n) r mod n
              val (_, tr1) = randomTracePersistent' (i, d + 1)
              val (_, tr2) = randomTracePersistent' (n - i - 1, d + 1)
              val (r, tr') = randomTracePersistent' (n, d + 2)
          in
              (r, TPTakeDropConcat (i, tr1, tr2, tr'))
          end
      else
          let val nb = 4
          in
              case Random.randRange (0, nb) r
               of 0 =>
                  let val (r, tr') = randomTracePersistent' (n, d)
                  in
                      (r, TPLength tr')
                  end
                | 1 =>
                  let val (r, tr') = randomTracePersistent' (n, d)
                  in
                      (r, TPMeasure tr')
                  end
                | 2 => 
                  let val (r, tr') = randomTracePersistent' (n, d)
                  in
                      (r, TPFoldr tr')
                  end
                | 3 =>
                  let val ((n, d), trT) = randomTraceTransient' (n, d)
                      val (r, trP) = randomTracePersistent' (n, d)
                  in
                      (r, TPTransient (trT, trP))
                  end
                | _ =>
                  raise Fail "impossible"
          end

    fun randomTraceTransient () : trace_transient =
      let val (_, tr) = randomTraceTransient' (1, 1)
      in
          tr
      end
        
    end

    fun stringOfOrientation e =
      (case e of
           EndFront => "F"
         | EndBack => "B")

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

    fun printTraceTransient tr =
      raise Fail "todo"

    and printTrancePersistent tr =
        raise Fail "todo"
          
    fun listOfTrustedTransient cs =
      T.Transient.foldr (fn (x, y) => x :: y) [] cs

    fun listOfUntrustedTransient cs =
      U.Transient.foldr (fn (x, y) => x :: y) [] cs

    fun listOfTrustedPersistent cs =
      T.Persistent.foldr (fn (x, y) => x :: y) [] cs

    fun listOfUntrustedPersistent cs =
      U.Persistent.foldr (fn (x, y) => x :: y) [] cs

    datatype 'a list_compare_res
      = ListsEqual
      | ListsItemMismatch of int * 'a * 'a (* position of mismatch, 
                                              * and positions in first and second lists, 
                                              * respectively *)
      | ListsUnequalLengths of int * int   (* length of first list, length of second *)
                                           
    fun compareLists (xs, ys) =
      let val (n1, n2) = (List.length xs, List.length ys)
      in
          if n1 <> n2 then
              ListsUnequalLengths (n1, n2)
          else
              let fun f (i, xs : item list, ys : item list) =
                    (case (xs, ys) of
                         ([], []) =>
                         ListsEqual
                       | (x :: xs, y :: ys) =>
                         if x <> y then
                             ListsItemMismatch (i, x, y)
                         else
                             f (i + 1, xs, ys)
                       | _ =>
                         raise Fail "impossible")
              in
                  f (0, xs, ys)
              end
  end

    fun check tr0 =
      let fun ok' (t, u) =
            (case compareLists (t, u) of
                 ListsEqual =>
                 ()
               | ListsItemMismatch (i, x, y) =>
                 let val s = "item mismatch at " ^ Int.toString i ^
                             " with x=" ^ Int.toString x ^
                             " and y=" ^ Int.toString y ^ "\n"
                 in
                     raise Fail s
                 end
               | ListsUnequalLengths (nr, ns) =>
                 let val s = "unequal lengths |r|=" ^ Int.toString nr ^
                             " and |s|=" ^ Int.toString ns ^ "\n"
                 in
                     raise Fail s
                 end)
          fun okTransient (t, u) =
            ok' (listOfTrustedTransient t, listOfUntrustedTransient u)
          fun okPersistent (t, u) =
            ok' (listOfTrustedPersistent t, listOfUntrustedPersistent u)
          val mdT = metaDataTrusted
          val mdU = metaDataUntrusted
          fun chkTransient (tr, t, u) = (
              okTransient (t, u);
              (case tr
                of TTLength tr' =>
                   if T.Transient.length t = U.Transient.length u then
                       chkTransient (tr', t, u)
                   else
                       raise Fail "Transient.length"
                 | TTMeasure tr' =>
                   if measureEq (T.Transient.measure t, U.Transient.measure u) then
                       chkTransient (tr', t, u)
                   else
                       raise Fail "Transient.measure"
                 | TTTabulate (n, tr') =>
                   let fun f i =
                         i
                       val t' = T.Transient.tabulate mdT (n, f)
                       val u' = U.Transient.tabulate mdU (n, f)
                   in
                       chkTransient (tr', t', u')
                   end
                 | TTPush (EndFront, x, tr') =>
                   chkTransient (tr',
                                 T.Transient.pushFront mdT (t, x),
                                 U.Transient.pushFront mdU (u, x))
                 | TTPush (EndBack, x, tr') =>
                   chkTransient (tr',
                                 T.Transient.pushBack mdT (t, x),
                                 U.Transient.pushBack mdU (u, x))
                 | TTPop (EndFront, tr') =>
                   let val (t', xT) = T.Transient.popFront mdT t
                       val (u', xU) = U.Transient.popFront mdU u
                   in
                       if xT = xU then
                           chkTransient (tr', t', u')
                       else
                           raise Fail "Transient.popFront"
                   end
                 | TTPop (EndBack, tr') =>
                   let val (t', xT) = T.Transient.popBack mdT t
                       val (u', xU) = U.Transient.popBack mdU u
                   in
                       if xT = xU then
                           chkTransient (tr', t', u')
                       else
                           raise Fail "Transient.popBack"
                   end
                 | TTSplitConcat (i, tr1, tr2, tr') =>
                   let val (t1, xT, t2) = T.Transient.split mdT (t, T.Search.Index i)
                       val (u1, xU, u2) = U.Transient.split mdU (u, U.Search.Index i)
                       val _ =
                           if xT <> xU then
                               raise Fail "Transient.split"
                           else
                               ()
                       val (t1', u1') = chkTransient (tr1, t1, u1)
                       val (t2', u2') = chkTransient (tr2, t2, u2)
                       val t' = T.Transient.concat mdT (t1', t2')
                       val u' = U.Transient.concat mdU (u1', u2')
                   in
                       chkTransient (tr', t', u')
                   end
                 | TTFoldr tr' =>
                   if T.Transient.foldr (op +) 0 t = U.Transient.foldr (op +) 0 u then
                       chkTransient (tr', t, u)
                   else
                       raise Fail "Transient.foldr"
                 | TTPersistent (tr', trOpt) =>
                   let val (t', u') = chkPersistent (tr', T.Transient.persistent t, U.Transient.persistent u)
                   in
                       case trOpt
                        of NONE =>
                           (T.Persistent.transient t', U.Persistent.transient u')
                         | SOME tr'' => 
                           chkTransient (tr'', T.Persistent.transient t', U.Persistent.transient u')
                   end)
          )
          and chkPersistent (tr, t, u) = (
              okPersistent (t, u);              
              (case tr
                of TPNil =>
                   (t, u)
                 | TPLength tr' =>
                   if T.Persistent.length t = U.Persistent.length u then
                       chkPersistent (tr', t, u)
                   else
                       raise Fail "Persistent.length"
                 | TPMeasure tr' =>
                   if measureEq (T.Persistent.measure t, U.Persistent.measure u) then
                       chkPersistent (tr', t, u)
                   else
                       raise Fail "Persistent.measure"
                 | TPTakeDropConcat (i, tr1, tr2, tr') =>
                   let val (t1, t2) =
                           (T.Persistent.take mdT (t, T.Search.Index i),
                            T.Persistent.drop mdT (t, T.Search.Index i))
                       val (u1, u2) =
                           (U.Persistent.take mdU (u, U.Search.Index i),
                            U.Persistent.drop mdU (u, U.Search.Index i))
                       val (t1', u1') = chkPersistent (tr1, t1, u1)
                       val (t2', u2') = chkPersistent (tr2, t2, u2)
                       val t' = T.Persistent.concat mdT (t1', t2')
                       val u' = U.Persistent.concat mdU (u1', u2')
                   in
                       chkPersistent (tr', t', u')
                   end
                 | TPFoldr tr' =>
                   if T.Persistent.foldr (op +) 0 t = U.Persistent.foldr (op +) 0 u then
                       chkPersistent (tr', t, u)
                   else
                       raise Fail "Persistent.foldr" 
                 | TPTransient (tr', tr'') =>
                   let val (t', u') = chkTransient (tr', T.Persistent.transient t, U.Persistent.transient u)
                   in
                       chkPersistent (tr'', T.Transient.persistent t', U.Transient.persistent u')
                   end
              )
          )
          val t0 = T.Transient.tabulate mdT (0, fn _ => 1)
          val u0 = U.Transient.tabulate mdU (0, fn _ => 1)
      in
          chkTransient (tr0, t0, u0)
      end


end
