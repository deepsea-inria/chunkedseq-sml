signature FORK_JOIN = sig

    val fork : ((unit -> 'a) * (unit -> 'b)) -> ('a * 'b)
                                                  
    val spguard : {complexity : (unit -> int),
                   parallel : (unit -> 'a),
                   serial : (unit -> 'a) option}
                  -> 'a
                                                    
end

signature MERGESORT = sig

    structure Chunkedseq : CHUNKEDSEQ

    type 'a chunkedseq = 'a Chunkedseq.Persistent.t

    val merge : ('a chunkedseq * 'a chunkedseq) -> 'a chunkedseq

    val mergesort : 'a chunkedseq -> 'a chunkedseq

end

functor MergeSortFn (
    structure ForkJoin : FORK_JOIN
    structure Chunkedseq : CHUNKEDSEQ
    val metadata : 'a Chunkedseq.metadata
    val < : (Chunkedseq.Search.measure * Chunkedseq.Search.measure) -> order
) : MERGESORT
        where Chunkedseq = Chunkedseq = struct

    structure C = Chunkedseq
    structure P = C.Persistent
    structure T = C.Transient

    datatype find_by = datatype C.find_by

    val md = metadata

    val emptyP = P.tabulate md (0, fn _ => raise Fail "")
    val emptyT = T.tabulate md (0, fn _ => raise Fail "")

    fun singletonP md x =
      P.pushFront md (x, emptyP)
                           
    fun min (m1, m2) =
      if m1 < m2 then
          m1
      else
          m2

    fun max (m1, m2) =
      if m1 > m2 then
          m1
      else
          m2
                      
    fun merge (s1, s2) =
      let val n1 = P.length s1
          val n2 = P.length s2
      in
          spguard {
              complexity =
              fn () =>
                 n1 + n2,
              parallel =
              fn () =>
                 if n1 < n2 then
                     merge (s2, s1)
                 else if n1 = 0 then
                     if n2 = 0 then
                         s1
                     else
                         let val x1 = P.sub md (s1, Index 0)
                             val x2 = P.sub md (s2, Index 0)
                         in
                             P.concat md (singletonP md (min (x1, x2)),
                                          singletonP md (max (x1, x2)))
                         end
                 else
                     let val mid = n1 div 2
                         val (s11, s12) = (P.take md (s1, Index mid),
                                           P.drop md (s1, Index mid))
                         fun p m =
                           m > P.measure s11
                         val (s21, s22) = (P.take md (s2, Predicate p),
                                           P.drop md (s2, Predicate p))
                         val (s1', s2') =
                             fork2 (fn () => merge (s11, s21),
                                    fn () => merge (s12, s22))
                     in
                         P.concat md (s1', s2')
                     end,
              serial = SOME (
              fn () =>
                 let fun lp (s1, s2) =
                       (case (T.length s1, T.length s2) =>
                         of (0, 0) =>
                            emptyT
                          | (1, 0) =>
                            s1
                          | (0, 1) =>
                            s2
                          | _ =>
                            let val x1 = T.sub md (s1, 0)
                                val x2 = T.sub md (s2, 0)
                            in
                                if x1 < x2 then
                                    let val (s1', x1) = T.popFront md s1
                                    in
                                        T.pushFront md (lp (s1', s2), x1)
                                    end
                                else
                                    let val (s2', x2) = T.popFront md s2
                                    in
                                        T.pushFront md (lp (s1, s2'), x2)
                                    end
                            end)
                 in
                     T.persistent (lp (P.transient s1, P.transient s2))
                 end)
          }
      end

    fun mergesort s =
      let val n = P.length s
      in
          spguard {
              complexity =
              fn () =>
                 n (* * log n *),
              parallel =
              fn () =>
                 if n < 2 then
                     s
                 else
                     let val mid = P.length s div 2
                         val (s1, s2) = (P.take md (s, mid), P.drop md (s, mid))
                         val (s1', s2') =
                             fork (fn () => mergesort s1,
                                   fn () => mergesort s2)
                     in
                         merge (s1', s2')
                     end,
              serial = NONE
          }
      end
          
end
