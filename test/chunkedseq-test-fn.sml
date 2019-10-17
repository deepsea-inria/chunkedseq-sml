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
    type seqlen = int

    datatype orientation = EndFront | EndBack

    (* The first component of each trace item is to denote the number of items
     * that are expected before the corresponding trace item is performed. *)

    and trace_transient
        = TTLength of seqlen * trace_transient
        | TTMeasure of seqlen * trace_transient
        | TTTabulate of seqlen * int * trace_transient
        | TTPush of seqlen * orientation * item * trace_transient
        | TTPop of seqlen * orientation * trace_transient
        | TTSplitConcat of seqlen * int * trace_transient * trace_transient * trace_transient
        | TTFoldr of seqlen * trace_transient
        | TTPersistent of seqlen * trace_persistent * trace_transient option

    and trace_persistent
        = TPNil of seqlen
        | TPLength of seqlen * trace_persistent
        | TPMeasure of seqlen * trace_persistent
        | TPTakeDropConcat of seqlen * int * trace_persistent * trace_persistent * trace_persistent
        | TPFoldr of seqlen * trace_persistent
        | TPTransient of seqlen * trace_transient * trace_persistent

    local
        val r = Random.rand (1, 1)
    in

    fun stringOfOrientation or =
        (case or of
             EndFront => "Front"
           | EndBack => "Back")

    fun stringOfTraceTransient t =
        let fun str ts = String.concatWith " " ts
        in
            case t of
                TTLength (nref, _) =>
                str ["TTLength", Int.toString nref]
              | TTMeasure (nref, _) =>
                str ["TTMeasure ", Int.toString nref]
              | TTTabulate (nref, n, _) =>
                str ["TTTabulate", "(", Int.toString nref, Int.toString n, ")"]
              | TTPush (nref, or, x, _) =>
                str ["TTPush", "(", Int.toString nref, stringOfOrientation or, "x=", Int.toString x, ")"]
              | TTPop (nref, or, _) =>
                str ["TTPop", "(", Int.toString nref, stringOfOrientation or, ")"]
              | TTSplitConcat (nref, i, _, _, _) =>
                str ["TTSplitConcat ", "(", Int.toString nref, "i=", Int.toString i, ")"]
              | TTFoldr (nref, _) =>
                str ["TTFoldr", Int.toString nref]
              | TTPersistent (nref, _, _) =>
                str ["TTPersistent", Int.toString nref]
        end

    fun stringOfTracePersistent t =
        let fun str ts = String.concatWith " " ts
        in
            case t of 
                TPNil nref =>
                str ["TPNil", Int.toString nref]
              | TPLength (nref, _) =>
                str ["TPLength", Int.toString nref]
              | TPMeasure (nref, _) =>
                str ["TPMeasure", Int.toString nref]
              | TPTakeDropConcat (nref, i, _, _, _) =>
                str ["TPDropConcat", "(", Int.toString nref, "i=", Int.toString i, ")"]
              | TPFoldr (nref, _) =>
                str ["TPFoldr", Int.toString nref]
              | TPTransient (nref, _, _) =>
                str ["TPTransient", Int.toString nref]
        end
    
    fun randomItem () =
      Random.randRange (1, 2048) r
                       
    fun randomOrientation () =
      if Random.randRange (0, 1) r = 0 then
          EndFront
      else
          EndBack

    val maxDepth = 8

    fun seqLengthOfTraceTransient t =
      (case t of
           TTLength (nref, _) => nref
         | TTMeasure (nref, _) => nref
         | TTTabulate (nref, _, _) => nref
         | TTPush (nref, _, _, _) => nref
         | TTPop (nref, _, _) => nref
         | TTSplitConcat (nref, _, _, _, _) => nref
         | TTFoldr (nref, _) => nref
         | TTPersistent (nref, _, _) => nref)

    fun seqLengthOfTracePersistent t =
      (case t of 
           TPNil nref => nref
         | TPLength (nref, _) => nref
         | TPMeasure (nref, _) => nref
         | TPTakeDropConcat (nref, _, _, _, _) => nref
         | TPFoldr (nref, _) => nref
         | TPTransient (nref, _, _) => nref)             

    val nMaxTabulate = 16

    fun randomTraceTransient' (n, d) =
      if n = 0 orelse d >= maxDepth then
          let val (r, tr') = randomTracePersistent' (n, d)
          in
              (r, TTPersistent (n, tr', NONE))
          end
      else if n > 0 andalso Random.randRange (0, d + 3) r = 0 then
          let val i = Random.randRange (0, n) r mod n
              val (_, tr1) = randomTraceTransient' (i, d + 1)
              val (_, tr2) = randomTraceTransient' (n - i - 1, d + 1)
              (* FIXME: the size annotation, namely n, is bogus, because the
               * intended meaning is the number of items to be expected before
               * the continuation tr'. what's provided here is bogus because
               * the actual number depends on the operations issued by tr1
               * and tr2. the fix is to propagate information up, and then
               * the proper result for below is the sum of the sizes after
               * performing trace items tr1 and tr2.
               *)
              val (r, tr') = randomTraceTransient' (n, d + 2)
          in
              (r, TTSplitConcat (n, i, tr1, tr2, tr'))
          end
      else if (Random.randRange (0, 2 + Word.toInt (Word.<< (0wx1, Word.fromInt n))) r) < 3 then
          let val or = randomOrientation ()
              val x = randomItem ()
              val (r, t) = randomTraceTransient' (n + 1, d)
          in
              (r, TTPush (n, or, x, t))
          end
      else if n > 0 andalso Random.randRange (0, n) r = 0 then
	  let val or = randomOrientation ()
	      val (r, t) = randomTraceTransient' (n - 1, d)
	  in
	      (r, TTPop (n, or, t))
	  end
      else
          let val nb = 4
          in
              case Random.randRange (0, nb) r
               of 0 =>
                  let val (r, tr') = randomTraceTransient' (n, d)
                  in
                      (r, TTLength (n, tr'))
                  end
                | 1 =>
                  let val (r, tr') = randomTraceTransient' (n, d)
                  in
                      (r, TTMeasure (n, tr'))
                  end
                | 2 =>
                  let val n = Random.randRange (0, nMaxTabulate) r
                      val (r, tr') = randomTraceTransient' (n, d)
                  in
                      (r, TTTabulate (n, n, tr'))
                  end
                | 3 =>
                  let val (r, tr') = randomTraceTransient' (n, d)
                  in
                      (r, TTFoldr (n, tr'))
                  end
                | 4 =>
                  let val ((n, d), trP) = randomTracePersistent' (n, d)
                      val (r, trT) = randomTraceTransient' (n, d)
                  in
                      (r, TTPersistent (n, trP, SOME trT))
                  end
                | n =>
                  raise Fail ("impossible " ^ Int.toString n)
          end

    and randomTracePersistent' (n, d) =
      if n = 0 orelse d >= maxDepth then
          ((n, d), TPNil n)
      else if Random.randRange (0, d + 3) r = 0 then
          let val i = Random.randRange (0, n) r mod n
              val (_, tr1) = randomTracePersistent' (i, d + 1)
              val (_, tr2) = randomTracePersistent' (n - i - 1, d + 1)
              val (r, tr') = randomTracePersistent' (n, d + 2)
          in
              (r, TPTakeDropConcat (n, i, tr1, tr2, tr'))
          end
      else
          let val nb = 3
          in
              case Random.randRange (0, nb) r
               of 0 =>
                  let val (r, tr') = randomTracePersistent' (n, d)
                  in
                      (r, TPLength (n, tr'))
                  end
                | 1 =>
                  let val (r, tr') = randomTracePersistent' (n, d)
                  in
                      (r, TPMeasure (n, tr'))
                  end
                | 2 => 
                  let val (r, tr') = randomTracePersistent' (n, d)
                  in
                      (r, TPFoldr (n, tr'))
                  end
                | 3 =>
                  let val ((n, d), trT) = randomTraceTransient' (n, d)
                      val (r, trP) = randomTracePersistent' (n, d)
                  in
                      (r, TPTransient (n, trT, trP))
                  end
                | _ =>
                  raise Fail "impossible"
          end

    fun randomTraceTransient () =
        TTPush (0, EndFront, 123, #2 (randomTraceTransient' (1, 1)))
        
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
          fun okLength (nref, nt, nu) =
            if not (nref = nt andalso nref = nu) then
              raise Fail ("Length nref=" ^ (Int.toString nref) ^
                          " nt=" ^ (Int.toString nt) ^
                          " nu=" ^ (Int.toString nu))
            else
              ()
          fun okTransient (nref, t, u) = (
(*            okLength (nref, T.Transient.length t, U.Transient.length u);*)
            ok' (listOfTrustedTransient t, listOfUntrustedTransient u))
          fun okPersistent (nref, t, u) = (
(*            okLength (nref, T.Persistent.length t, U.Persistent.length u);*)
            ok' (listOfTrustedPersistent t, listOfUntrustedPersistent u))
          val mdT = metaDataTrusted
          val mdU = metaDataUntrusted 
          fun printNext s nt nu =
              print (s ^ "\t\t |t|=" ^ Int.toString nt ^ " |u|=" ^ Int.toString nu ^ "\n")
          fun chkTransient (tr, t, u) = (
              printNext (stringOfTraceTransient tr) (T.Transient.length t) (U.Transient.length u);
              okTransient (seqLengthOfTraceTransient tr, t, u);
              (case tr
                of TTLength (nref, tr') => (
(*                  okLength (nref, T.Transient.length t, U.Transient.length u);*)
                  chkTransient (tr', t, u))
                 | TTMeasure (_, tr') =>
                   if measureEq (T.Transient.measure t, U.Transient.measure u) then
                       chkTransient (tr', t, u)
                   else
                       raise Fail "Transient.measure"
                 | TTTabulate (_, n, tr') =>
                   let fun f i = i
                       val t' = T.Transient.tabulate mdT (n, f)
                       val u' = U.Transient.tabulate mdU (n, f)
                   in
                       chkTransient (tr', t', u')
                   end
                 | TTPush (_, EndFront, x, tr') =>
                   chkTransient (tr',
                                 T.Transient.Front.push mdT (t, x),
                                 U.Transient.Front.push mdU (u, x))
                 | TTPush (_, EndBack, x, tr') =>
                   chkTransient (tr',
                                 T.Transient.Back.push mdT (t, x),
                                 U.Transient.Back.push mdU (u, x))
                 | TTPop (_, EndFront, tr') =>
                   let val (t', xT) = T.Transient.Front.pop mdT t
                       val (u', xU) = U.Transient.Front.pop mdU u
                   in
                       if xT = xU then
                           chkTransient (tr', t', u')
                       else
                           raise Fail "Transient.popFront"
                   end
                 | TTPop (_, EndBack, tr') =>
                   let val (t', xT) = T.Transient.Back.pop mdT t
                       val (u', xU) = U.Transient.Back.pop mdU u
                   in
                       if xT = xU then
                           chkTransient (tr', t', u')
                       else
                           raise Fail "Transient.popBack"
                   end  
                 | TTSplitConcat (_, i, tr1, tr2, tr') =>
                   let
                       val (t1, xT, t2) = T.Transient.split mdT (t, T.Search.Index i)
                       val (u1, xU, u2) = U.Transient.split mdU (u, U.Search.Index i)
                       val _ =
                           if xT <> xU then
                               raise Fail "Transient.split"
                           else
                               ()
                       val (t1', u1') = chkTransient (tr1, t1, u1)
                       val (t2', u2') = chkTransient (tr2, t2, u2)
                       val t' = T.Transient.concat mdT (t1', T.Transient.Front.push mdT (t2', xT))
                       val u' = U.Transient.concat mdU (u1', U.Transient.Front.push mdU (u2', xU))
                   in
                       chkTransient (tr', t', u')
                   end
                 | TTFoldr (_, tr') =>
                   if T.Transient.foldr (op +) 0 t = U.Transient.foldr (op +) 0 u then
                       chkTransient (tr', t, u)
                   else
                       raise Fail "Transient.foldr"
                 | TTPersistent (_, tr', trOpt) =>
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
              printNext (stringOfTracePersistent tr) (T.Persistent.length t) (U.Persistent.length u);
              okPersistent (seqLengthOfTracePersistent tr, t, u);
              (case tr
                of TPNil _ =>
                   (t, u)
                 | TPLength (_, tr') =>
                   if T.Persistent.length t = U.Persistent.length u then
                       chkPersistent (tr', t, u)
                   else
                       raise Fail "Persistent.length"
                 | TPMeasure (_, tr') =>
                   if measureEq (T.Persistent.measure t, U.Persistent.measure u) then
                       chkPersistent (tr', t, u)
                   else
                       raise Fail "Persistent.measure"
                 | TPTakeDropConcat (_, i, tr1, tr2, tr') =>
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
                 | TPFoldr (_, tr') =>
                   if T.Persistent.foldr (op +) 0 t = U.Persistent.foldr (op +) 0 u then
                       chkPersistent (tr', t, u)
                   else
                       raise Fail "Persistent.foldr" 
                 | TPTransient (_, tr', tr'') =>
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
