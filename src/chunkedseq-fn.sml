functor ChunkedseqFn (C : CHUNK) :> CHUNKEDSEQ = struct

    type weight = C.weight

    type 'a weight_fn = 'a C.weight_fn
                      
    type 'a chunk = 'a C.chunk

    datatype 'a node
      = Item of 'a
      | Interior of 'a node chunk

    type 'a buffer = 'a node chunk

    datatype 'a chunkedseq
      = Shallow of 'a buffer
      | Deep of weight * 'a deep

    and 'a deep = DC of {
       fo : 'a buffer,
       fi : 'a buffer,
       mid : 'a chunkedseq,
       bi : 'a buffer,
       bo : 'a buffer
     }
          
    fun weight cs =
      (case cs of
           Shallow c =>
           C.weight c
         | Deep (w, _) =>
           w)

    val create = Shallow C.create

    fun mk_deep (d as DC {fo, fi, mid, bi, bo}) =
      let val w = C.weight fo + C.weight fi +
                  weight mid +
                  C.weight bi + C.weight bo
      in
          Deep (w, d)
      end

    val ec = C.create

    fun weight_of_item n =
      (case n of
           Item x => 1
         | Interior c => C.weight c)

    type level = int

    val rec push_front' : ('a chunkedseq * 'a node) -> 'a chunkedseq =
     fn (cs, x) =>
        (case cs of
             Shallow c =>
             if C.full c then
                 push_front' (mk_deep (DC {fo=ec, fi=ec, mid=create, bi=ec, bo=c}), x)
             else
                 Shallow (C.push_front weight_of_item (c, x))
           | Deep (_, DC {fo, fi, mid, bi, bo}) =>
             if C.full fo then
                 if C.empty fi then
                     push_front' (mk_deep (DC {fo=ec, fi=fo, mid=mid, bi=bi, bo=bo}), x)
                 else
                     let val mid' = push_front' (mid, Interior fo)
                     in
                         push_front' (mk_deep (DC {fo=ec, fi=fo, mid=mid', bi=bi, bo=bo}), x)
                     end
             else
                 let val fo' = C.push_front weight_of_item (fo, x)
                 in
                     mk_deep (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo})
                 end)
        
    val size = weight

    fun empty xs =
      (size xs = 0)

    fun push_front (xs, x) =
      push_front' (xs, Item x)

    fun push_back (xs, x) = raise Fail "todo"

    fun mk_shallow (c1, c2, c3, c4) =
      let val c =
              if C.weight c1 > 0 then
                  c1
              else if C.weight c2 > 0 then
                  c2
              else if C.weight c3 > 0 then
                  c3
              else if C.weight c4 > 0 then
                  c4
              else
                  raise Fail "impossible"
      in
          Shallow c
      end
          
    fun force_item n =
      (case n of
           Item x => x
         | Interior _ => raise Fail "impossible")

    fun force_interior n =
      (case n of
           Item _ => raise Fail "impossible"
         | Interior c => c)

    fun mk_deep' d = 
      check (mk_deep d)
            
    and check cs =
        (case cs of
             Shallow c =>
             raise Fail "impossible"
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             let val w = C.weight fo + C.weight fi +
                         C.weight bi + C.weight bo
             in
                 if w = 0 andalso not (empty mid) then
                     let val (mid', n) = pop_front' mid
                         val fo' = force_interior n
                     in
                         mk_deep (DC {fo=fo', fi=fi, mid=mid', bi=bi, bo=bo})
                     end
                 else if w = 1 andalso empty mid then
                     mk_shallow (fo, fi, bi, bo)
                 else if w = 0 andalso empty mid then
                     create
                 else
                     cs
             end)

    and pop_front' (cs : 'a chunkedseq) : ('a chunkedseq * 'a node) =
        (case cs of
             Shallow c =>
             let val (c', x) = C.pop_front weight_of_item c
             in
                 (Shallow c', x)
             end
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             if C.empty fo then
                 if not (C.empty fi) then
                     pop_front' (mk_deep' (DC {fo=fi, fi=ec, mid=mid, bi=bi, bo=bo}))
                 else if not (empty mid) then
                     let val (mid', n) = pop_front' mid
                         val c = force_interior n               
                     in
                         pop_front' (mk_deep' (DC {fo=c, fi=fi, mid=mid', bi=bi, bo=bo}))
                     end
                 else if not (C.empty bi) then
                     pop_front' (mk_deep' (DC {fo=bi, fi=fi, mid=mid, bi=ec, bo=bo}))
                 else
                     let val (bo', x) = C.pop_front weight_of_item fo
                     in
                         (mk_deep'  (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}), x)
                     end
             else
                 let val (fo', x) = C.pop_front weight_of_item fo
                 in
                     (mk_deep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}), x)
                 end)
            
    fun pop_front xs =
      let val (xs', n) = pop_front' xs
      in
          (xs', force_item n)
      end
          
    fun pop_back xs = raise Fail "todo"

    fun concat (xs1, xs2) = raise Fail "todo"

    fun split (xs, i) = raise Fail "todo"

    fun sub (xs, i) = raise Fail "todo"

    fun foldr f xs i = raise Fail "todo"

end
