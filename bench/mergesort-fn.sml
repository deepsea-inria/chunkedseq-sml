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
    val lessThan : (Chunkedseq.Search.measure * Chunkedseq.Search.measure) -> bool
) : MERGESORT
        where Chunkedseq = Chunkedseq = struct

    structure Chunkedseq = Chunkedseq
    structure FJ = ForkJoin
    structure C = Chunkedseq
    structure P = C.Persistent
    structure T = C.Transient

    type 'a chunkedseq = 'a Chunkedseq.Persistent.t

    datatype find_by = datatype C.Search.find_by

    val md = metadata

    val C.MetaData {measure, ...} = md

    fun emptyT md = T.tabulate md (0, fn _ => raise Fail "")

    fun singletonP md x =
      T.persistent (T.pushFront md (emptyT md, x))
                           
    fun min (x1, x2) =
      if lessThan (measure x1, measure x2)
      then
          x1
      else
          x2

    fun max (x1, x2) =
      if lessThan (measure x1, measure x2)
      then
          x2
      else
          x1

    fun merge (s1, s2) =
      let val n1 = P.length s1
          val n2 = P.length s2
      in
          FJ.spguard {
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
                           not (lessThan (m, P.measure s11))
                         val (s21, s22) = (P.take md (s2, Predicate p),
                                           P.drop md (s2, Predicate p))
                         val (s1', s2') =
                             FJ.fork (fn () => merge (s11, s21),
                                      fn () => merge (s12, s22))
                     in
                         P.concat md (s1', s2')
                     end,
              serial = SOME (
              fn () =>
                 let fun lp (s1, s2, acc) =
                       (case (T.length s1, T.length s2)
                         of (0, 0) =>
                            acc
                          | (1, 0) =>
                            T.concat md (s1, acc)
                          | (0, 1) =>
                            T.concat md (s2, acc)
                          | _ =>
                            let val x1 = T.sub md (s1, Index 0)
                                val x2 = T.sub md (s2, Index 0)
                            in
                                if lessThan (measure x1, measure x2) then
                                    let val (s1', x1) = T.popFront md s1
                                    in
                                        lp (s1', s2, T.pushBack md (acc, x1))
                                    end
                                else
                                    let val (s2', x2) = T.popFront md s2
                                    in
                                        lp (s1, s2', T.pushBack md (acc, x2))
                                    end
                            end)
                 in
                     T.persistent (lp (P.transient s1, P.transient s2, emptyT md))
                 end)
          }
      end

    fun mergesort s =
      let val n = P.length s
      in
          FJ.spguard {
              complexity =
              fn () =>
                 n (* * log n *),
              parallel =
              fn () =>
                 if n < 2 then
                     s
                 else
                     let val mid = P.length s div 2
                         val (s1, s2) = (P.take md (s, Index mid),
                                         P.drop md (s, Index mid))
                         val (s1', s2') =
                             FJ.fork (fn () => mergesort s1,
                                      fn () => mergesort s2)
                     in
                         merge (s1', s2')
                     end,
              serial = NONE
          }
      end
          
end
