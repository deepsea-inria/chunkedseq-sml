functor ChunkedseqFn (C : CHUNK) :> CHUNKEDSEQ = struct

    type weight = C.weight

    type 'a weight_fn = 'a C.weight_fn
                      
    type 'a chunk = 'a C.chunk              

    datatype 'a chunkedseq
         = Shallow of 'a chunk
         | Deep of weight * 'a deep

    and 'a deep = DC of {
       fo : 'a chunk,
       fi : 'a chunk,
       mid : ('a chunk) chunkedseq,
       bi : 'a chunk,
       bo : 'a chunk
     }

    fun unit_weight_fn _ = 1
                            
    val create = Shallow C.create

    fun weight cs =
      (case cs of
           Shallow c =>
           C.weight c
         | Deep (w, _) =>
           w)

    fun mk_deep (d as DC {fo, fi, mid, bi, bo}) =
      let val w = C.weight fo + C.weight fi +
                  weight mid +
                  C.weight bi + C.weight bo
      in
          Deep (w, d)
      end

    val ec = C.create

    val rec push_front' : 'a weight_fn -> ('a chunkedseq * 'a) -> 'a chunkedseq =
     fn wf => fn (cs, x) =>
        raise Fail "todo"
              (*
        (case cs of
             Shallow c =>
             raise Fail "todo"
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             if C.full fo then
                 if C.empty fi then
                     push_front' wf (mk_deep (DC {fo=ec, fi=fo, mid=mid, bi=bi, bo=bo}), x)
                 else
                     let val mid' = push_front' C.weight (mid, fo)
                     in
                         push_front' wf (mk_deep (DC {fo=ec, fi=fo, mid=mid', bi=bi, bo=bo}), x)
                     end
             else
                 raise Fail "todo") *)
        
    fun size xs = raise Fail "todo"

    fun empty xs = raise Fail "todo"

    fun push_front (xs, x) =
      push_front' unit_weight_fn (xs, x)

    fun push_back (xs, x) = raise Fail "todo"
                                                         
    fun pop_front xs = raise Fail "todo"

    fun pop_back xs = raise Fail "todo"

    fun concat (xs1, xs2) = raise Fail "todo"

    fun split (xs, i) = raise Fail "todo"

    fun sub (xs, i) = raise Fail "todo"

    fun foldr f xs i = raise Fail "todo"

end
