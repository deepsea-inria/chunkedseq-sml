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

    fun push_front' (cs, x) =
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

    fun push_back' (xs, x) = raise Fail "todo"
                  
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

    and pop_front' cs =
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
            
    and pop_front xs =
      let val (xs', n) = pop_front' xs
      in
          (xs', force_item n)
      end

    and pop_back' xs = raise Fail "todo"
          
    and pop_back xs = raise Fail "todo"

    and push_buffer_front (cs, c) =
        if C.empty c then
            cs
        else if empty cs then
            Shallow (C.push_front weight_of_item (C.create, Interior c))
        else
            let val (cs', n) = pop_front' cs
                val c' = force_interior n
            in
                if C.size c + C.size c' <= C.k then
                    push_front' (cs', Interior (C.concat weight_of_item (c, c')))
                else
                    push_front' (cs, Interior c)
            end

    and push_buffer_back (cs, c) =
        raise Fail "todo"

    and transfer_contents_front (cs, c) =
        if C.empty c then
            cs
        else
            let val (c', x) = C.pop_back weight_of_item c
            in
                transfer_contents_front (push_front' (cs, x), c')
            end

    and transfer_contents_back (cs, c) =
        raise Fail "todo"
        
    and concat (cs1, cs2) =
        if empty cs1 then
            cs2
        else if empty cs2 then
            cs1
        else
            (case (cs1, cs2) of
                 (Shallow c1, _) =>
                 transfer_contents_front (cs2, c1)
               | (_, Shallow c2) =>
                 transfer_contents_back (cs1, c2)
               | (Deep (_, DC {fo=fo1, fi=fi1, mid=mid1, bi=bi1, bo=bo1}),
                  Deep (_, DC {fo=fo2, fi=fi2, mid=mid2, bi=bi2, bo=bo2})) =>
                 let val mid1' = push_buffer_back (mid1, bi1)
                     val mid1'' = push_buffer_back (mid1', bo1)
                     val mid2' = push_buffer_front (mid1, fi2)
                     val mid2'' = push_buffer_front (mid2', fo2)
                     val (mid1''', mid2''') =
                         if empty mid1'' orelse empty mid2'' then
                             (mid1'', mid2'')
                         else
                             let val (mid1''', n1) = pop_back' mid1''
                                 val (mid2''', n2) = pop_front' mid2''
                                 val (c1, c2) = (force_interior n1, force_interior n2)
                             in
                                 if C.weight c1 + C.weight c2 <= C.k then
                                     let val c' = C.concat weight_of_item (c1, c2)
                                     in
                                         (push_back' (mid1''', Interior c'), mid2''')
                                     end
                                 else
                                     (mid1'', mid2'')
                             end
                     val mid12 = concat (mid1''', mid2''')
                 in
                     mk_deep' (DC {fo=fo1, fi=fi1, mid=mid12, bi=bi2, bo=bo2})
                 end)

    and split' (cs : 'a chunkedseq, i : int) : ('a chunkedseq * 'a node * 'a chunkedseq) =
        (case cs of
             Shallow c =>
             let val (c1, x, c2) = C.split weight_of_item (c, i)
             in
                 (Shallow c1, x, Shallow c2)
             end
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             let val (wfo, wfi) = (C.weight fo, C.weight fi)
                 val wm = weight mid
                 val (wbi, wbo) = (C.weight bi, C.weight bo)
                 val (cs1, x, cs2) =
                     if i < wfo then
                         let val (fo1, x, fo2) = C.split weight_of_item (fo, i)
                             val cs1 = mk_deep (DC {fo=fo1, fi=ec, mid=create, bi=ec, bo=ec})
                             val cs2 = mk_deep (DC {fo=fo2, fi=fi, mid=mid, bi=bi, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi then
                         let val j = i - wfo
                             val (fi1, x, fi2) = C.split weight_of_item (fi, j)
                             val cs1 = mk_deep (DC {fo=fo, fi=ec, mid=create, bi=ec, bo=fi1})
                             val cs2 = mk_deep (DC {fo=fi2, fi=ec, mid=mid, bi=bi, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi + wm then
                         let val j = i - wfo - wfi
                             val (mid1, n, mid2) = split' (mid, j)
                             val c = force_interior n
                             val (c1, x, c2) = C.split weight_of_item (c, j - weight mid1)
                             val cs1 = mk_deep (DC {fo=fo, fi=fi, mid=mid1, bi=ec, bo=c1})
                             val cs2 = mk_deep (DC {fo=c2, fi=ec, mid=mid2, bi=bi, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi + wm + wbi then
                         let val j = i - wfo - wfi - wm
                             val (bi1, x, bi2) = C.split weight_of_item (bi, j)
                             val cs1 = mk_deep (DC {fo=fo, fi=fi, mid=mid, bi=ec, bo=bi1})
                             val cs2 = mk_deep (DC {fo=bi2, fi=ec, mid=create, bi=ec, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else
                         let val j = i - wfo - wfi - wm - wbi
                             val (bo1, x, bo2) = C.split weight_of_item (bo, j)
                             val cs1 = mk_deep (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo1})
                             val cs2 = mk_deep (DC {fo=bo2, fi=ec, mid=create, bi=ec, bo=ec})
                         in
                             (cs1, x, cs2)
                         end
             in
                 (check cs1, x, check cs2)
             end)

    and split (cs, i) =
        let val (cs1, n, cs2) = split' (cs, i)
        in
            (cs1, force_item n, cs2)
        end

    fun sub (xs, i) = raise Fail "todo"

    fun foldr f xs i = raise Fail "todo"

end
