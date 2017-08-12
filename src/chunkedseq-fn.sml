functor ChunkedseqFn (C : CHUNK) :> CHUNKEDSEQ = struct

    structure C = C

    type ('a, 'b) descr = ('a, 'b) C.sequence_descriptor

    type ('a, 'b) chunk =
         ('a, 'b) C.chunk

    datatype ('a, 'b) node
      = Nil
      | Item of 'a
      | Interior of ('a, 'b) node chunk

    type ('a, 'b) buffer =
         ('a, 'b) node chunk

    type weight =
         C.weight

    datatype ('a, 'b) persistent
      = Shallow of ('a, 'b) buffer
      | Deep of weight * 'b * ('a, 'b) deep

    and ('a, 'b) deep = DC of {
       fo : ('a, 'b) buffer,
       fi : ('a, 'b) buffer,
       mid : ('a, 'b) persistent,
       bi : ('a, 'b) buffer,
       bo : ('a, 'b) buffer
     }

    type transient_version =
         C.transient_version
                            
    type ('a, 'b) transient =
         (('a, 'b) persistent * transient_version)
                            
    fun weight cs =
      (case cs of
           Shallow c =>
           C.weight c
         | Deep (w, _) =>
           w)

    fun chunkEmpty c =
      C.size c = 0
                               
    fun chunkFull c =
      C.size c = C.capacity
                     
    fun create tv =
        Shallow (C.create tv)
                
    val size =
        weight

    fun empty xs =
      (size xs = 0)
          
    fun mkDeep (d as DC {fo, fi, mid, bi, bo}) =
      let val w = C.weight fo + C.weight fi +
                  weight mid +
                  C.weight bi + C.weight bo
      in
          Deep (w, d)
      end

    val ec =
        C.create

    fun pushFront' (cs, tv, x) =
      (case cs of
           Shallow c =>
           if chunkFull c then
               pushFront' (mkDeep (DC {fo=ec tv, fi=ec tv, mid=create tv, bi=ec tv, bo=c}), tv, x)
           else
               Shallow (C.pushFront weightOfItem (c, tv, x))
         | Deep (_, DC {fo, fi, mid, bi, bo}) =>
           if chunkFull fo then
               if chunkEmpty fi then
                   pushFront' (mkDeep (DC {fo=ec tv, fi=fo, mid=mid, bi=bi, bo=bo}), tv, x)
               else
                   let val mid' = pushFront' (mid, tv, Interior fi)
                   in
                       pushFront' (mkDeep (DC {fo=ec tv, fi=fo, mid=mid', bi=bi, bo=bo}), tv, x)
                   end
           else
               let val fo' = C.pushFront weightOfItem (fo, tv, x)
               in
                   mkDeep (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo})
               end)


    fun pushBack' (cs, tv, x) =
      (case cs of
           Shallow c =>
           if chunkFull c then
               pushBack' (mkDeep (DC {fo=c, fi=ec tv, mid=create tv, bi=ec tv, bo=ec tv}), tv, x)
           else
               Shallow (C.pushBack weightOfItem (c, tv, x))
         | Deep (_, DC {fo, fi, mid, bi, bo}) =>
           if chunkFull bo then
               if chunkEmpty bi then
                   pushBack' (mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bo, bo=ec tv}), tv, x)
               else
                   let val mid' = pushBack' (mid, tv, Interior bi)
                   in
                       pushBack' (mkDeep (DC {fo=fo, fi=fi, mid=mid', bi=bo, bo=ec tv}), tv, x)
                   end
           else
               let val bo' = C.pushBack weightOfItem (bo, tv, x)
               in
                   mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'})
               end)

    fun mkShallow (c1, c2, c3, c4) =
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

    fun mkDeep' (d, tv) = 
      check (mkDeep d, tv)
            
    and check (cs, tv) =
        (case cs of
             Shallow c =>
             raise Fail "impossible"
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             let val w = C.weight fo + C.weight fi +
                         C.weight bi + C.weight bo
             in
                 if w = 0 andalso not (empty mid) then
                     let val (mid', n) = popFront' (mid, tv)
                         val fo' = force_interior n
                     in
                         mkDeep (DC {fo=fo', fi=fi, mid=mid', bi=bi, bo=bo})
                     end
                 else if w = 1 andalso empty mid then
                     mkShallow (fo, fi, bi, bo)
                 else if w = 0 andalso empty mid then
                     create tv
                 else
                     cs
             end)

    and popFront' (cs, tv) =
        (case cs of
             Shallow c =>
             let val (c', x) = C.popFront weightOfItem (c, tv)
             in
                 (Shallow c', x)
             end
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             if chunkEmpty fo then
                 if not (chunkEmpty fi) then
                     popFront' (mkDeep' (DC {fo=fi, fi=ec tv, mid=mid, bi=bi, bo=bo}, tv), tv)
                 else if not (empty mid) then
                     let val (mid', n) = popFront' (mid, tv)
                         val c = force_interior n               
                     in
                         popFront' (mkDeep' (DC {fo=c, fi=fi, mid=mid', bi=bi, bo=bo}, tv), tv)
                     end
                 else if not (chunkEmpty bi) then
                     popFront' (mkDeep' (DC {fo=bi, fi=fi, mid=mid, bi=ec tv, bo=bo}, tv), tv)
                 else
                     let val (bo', x) = C.popFront weightOfItem (bo, tv)
                     in
                         (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}, tv), x)
                     end
             else
                 let val (fo', x) = C.popFront weightOfItem (fo, tv)
                 in
                     (mkDeep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}, tv), x)
                 end)

    and popBack' (cs, tv) =
        (case cs of
             Shallow c =>
             let val (c', x) = C.popBack weightOfItem (c, tv)
             in
                 (Shallow c', x)
             end
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             if chunkEmpty bo then
                 if not (chunkEmpty bi) then
                     popBack' (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=ec tv, bo=bi}, tv), tv)
                 else if not (empty mid) then
                     let val (mid', n) = popBack' (mid, tv)
                         val c = force_interior n               
                     in
                         popBack' (mkDeep' (DC {fo=fo, fi=fi, mid=mid', bi=bi, bo=c}, tv), tv)
                     end
                 else if not (chunkEmpty fi) then
                     popBack' (mkDeep' (DC {fo=fo, fi=ec tv, mid=mid, bi=bi, bo=fi}, tv), tv)
                 else
                     let val (fo', x) = C.popBack weightOfItem (fo, tv)
                     in
                         (mkDeep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}, tv), x)
                     end
             else
                 let val (bo', x) = C.popBack weightOfItem (bo, tv)
                 in
                     (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}, tv), x)
                 end)

    and pushBufferFront (cs, tv, c) =
        if chunkEmpty c then
            cs
        else if empty cs then
            Shallow (C.pushFront weightOfItem (ec tv, tv, Interior c))
        else
            let val (cs', n) = popFront' (cs, tv)
                val c' = force_interior n
            in
                if C.size c + C.size c' <= C.capacity then
                    pushFront' (cs', tv, Interior (C.concat weightOfItem (c, tv, c', tv)))
                else
                    pushFront' (cs, tv, Interior c)
            end

    and pushBufferBack (cs, tv, c) =
        if chunkEmpty c then
            cs
        else if empty cs then
            Shallow (C.pushBack weightOfItem (ec tv, tv, Interior c))
        else
            let val (cs', n) = popBack' (cs, tv)
                val c' = force_interior n
            in
                if C.size c + C.size c' <= C.capacity then
                    pushBack' (cs', tv, Interior (C.concat weightOfItem (c', tv, c, tv)))
                else
                    pushBack' (cs, tv, Interior c)
            end

    and transferContentsFront (cs, csTv, c, cTv) =
        if chunkEmpty c then
            cs
        else
            let val (c', x) = C.popBack weightOfItem (c, cTv)
            in
                transferContentsFront (pushFront' (cs, csTv, x), csTv, c', cTv)
            end

    and transferContentsBack (cs, csTv, c, cTv) =
        if chunkEmpty c then
            cs
        else
            let val (c', x) = C.popFront weightOfItem (c, cTv)
            in
                transferContentsBack (pushBack' (cs, csTv, x), csTv, c', cTv)
            end
        
    and concat' (cs1, tv1, cs2, tv2) =
        if empty cs1 then
            cs2
        else if empty cs2 then
            cs1
        else
            (case (cs1, cs2) of
                 (Shallow c1, _) =>
                 transferContentsFront (cs2, tv2, c1, tv1)
               | (_, Shallow c2) =>
                 transferContentsBack (cs1, tv1, c2, tv2)
               | (Deep (_, DC {fo=fo1, fi=fi1, mid=mid1, bi=bi1, bo=bo1}),
                  Deep (_, DC {fo=fo2, fi=fi2, mid=mid2, bi=bi2, bo=bo2})) =>
                 let val mid1' = pushBufferBack (mid1, tv1, bi1)
                     val mid1'' = pushBufferBack (mid1', tv1, bo1)
                     val mid2' = pushBufferFront (mid2, tv2, fi2)
                     val mid2'' = pushBufferFront (mid2', tv2, fo2)
                     val (mid1''', mid2''') =
                         if empty mid1'' orelse empty mid2'' then
                             (mid1'', mid2'')
                         else
                             let val (mid1''', n1) = popBack' (mid1'', tv1)
                                 val (mid2''', n2) = popFront' (mid2'', tv2)
                                 val (c1, c2) = (force_interior n1, force_interior n2)
                             in
                                 if C.size c1 + C.size c2 <= C.capacity then
                                     let val c' = C.concat weightOfItem (c1, tv1, c2, tv2)
                                     in
                                         (pushBack' (mid1''', tv1, Interior c'), mid2''')
                                     end
                                 else
                                     (mid1'', mid2'')
                             end
                     val mid12 = concat' (mid1''', tv1, mid2''', tv2)
                 in
                     mkDeep' (DC {fo=fo1, fi=fi1, mid=mid12, bi=bi2, bo=bo2}, tv1)
                 end)

    and split' (cs, tv, i) =
        (case cs of
             Shallow c =>
             let val (c1, x, c2) = C.split weightOfItem (c, tv, i)
             in
                 (Shallow c1, x, Shallow c2)
             end
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             let val (wfo, wfi) = (C.weight fo, C.weight fi)
                 val wm = weight mid
                 val (wbi, wbo) = (C.weight bi, C.weight bo)
                 val (cs1, x, cs2) =
                     if i < wfo then
                         let val (fo1, x, fo2) = C.split weightOfItem (fo, tv, i)
                             val cs1 = mkDeep (DC {fo=fo1, fi=ec tv, mid=create tv, bi=ec tv, bo=ec tv})
                             val cs2 = mkDeep (DC {fo=fo2, fi=fi, mid=mid, bi=bi, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi then
                         let val j = i - wfo
                             val (fi1, x, fi2) = C.split weightOfItem (fi, tv, j)
                             val cs1 = mkDeep (DC {fo=fo, fi=ec tv, mid=create tv, bi=ec tv, bo=fi1})
                             val cs2 = mkDeep (DC {fo=fi2, fi=ec tv, mid=mid, bi=bi, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi + wm then
                         let val j = i - wfo - wfi
                             val (mid1, n, mid2) = split' (mid, tv, j)
                             val c = force_interior n
                             val (c1, x, c2) = C.split weightOfItem (c, tv, j - weight mid1)
                             val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid1, bi=ec tv, bo=c1})
                             val cs2 = mkDeep (DC {fo=c2, fi=ec tv, mid=mid2, bi=bi, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi + wm + wbi then
                         let val j = i - wfo - wfi - wm
                             val (bi1, x, bi2) = C.split weightOfItem (bi, tv, j)
                             val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=ec tv, bo=bi1})
                             val cs2 = mkDeep (DC {fo=bi2, fi=ec tv, mid=create tv, bi=ec tv, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi + wm + wbi + wbo then
                         let val j = i - wfo - wfi - wm - wbi
                             val (bo1, x, bo2) = C.split weightOfItem (bo, tv, j)
                             val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo1})
                             val cs2 = mkDeep (DC {fo=bo2, fi=ec tv, mid=create tv, bi=ec tv, bo=ec tv})
                         in
                             (cs1, x, cs2)
                         end
                     else
                         raise Fail "ChunkedseqFn.split: out of bounds"
             in
                 (check (cs1, tv), x, check (cs2, tv))
             end)

    fun foldrNode f i n =
      (case n of
           Item x =>
           f (x, i)
         | Interior c =>
           foldrBuffer f i c)

    and foldrBuffer f i b =
      C.foldr (fn (n, i) => foldrNode f i n) i b

    fun foldr' f i cs =
      (case cs of
           Shallow c =>
           foldrBuffer f i c
         | Deep (_, DC {fo, fi, mid, bi, bo}) =>
           let val i = foldrBuffer f i bo
               val i = foldrBuffer f i bi
               val i = foldr' f i mid
               val i = foldrBuffer f i fi
           in
               foldrBuffer f i fo
           end)

    fun tabulate' (n, f, tv) =
      let val n' = n - 1
      in
          if n' = 0 then
              create tv
          else
              pushFront' (tabulate' (n', f, tv), tv, Item (f n'))
      end

    val alwaysInvalidTransient = ~1
                                      
    val transientGuid = ref 0
                            
    fun newGuid () =
      let val id = ! transientGuid
      in
          transientGuid := id + 1;
          id
      end
            
    structure Persistent = struct

      type ('a, 'b) t = ('a, 'b) persistent

      val size =
          size

      fun concat (cs1, cs2) =
        concat' (cs1, alwaysInvalidTransient, cs2, alwaysInvalidTransient)

      fun take (cs, n) =
        let val (cs1, x, _) = split' (cs, alwaysInvalidTransient, n)
        in
            pushBack' (cs1, alwaysInvalidTransient, x)
        end

      fun drop (cs, n) =
        let val (_, _, cs2) = split' (cs, alwaysInvalidTransient, n)
        in
            cs2
        end

      val foldr =
          foldr'

      fun transient cs =
        (cs, newGuid ())
            
    end (* Persistent *)

    structure Transient = struct

      type ('a, 'b) t = ('a, 'b) transient

      val size = fn (cs, _) =>
                    size cs

      fun tabulate (f, n) =
        let val tv = newGuid ()
        in
            (tabulate' (f, n, tv), tv)
        end
                         
      fun pushFront ((cs, tv), x) =
        (pushFront' (cs, tv, Item x), tv)
            
      fun pushBack ((cs, tv), x) =
        (pushBack' (cs, tv, Item x), tv)
            
      fun popFront (cs, tv) =
          let val (cs', n) = popFront' (cs, tv)
          in
              ((cs, tv), force_item n)
          end
              
      fun popBack (cs, tv) =
          let val (cs', n) = popBack' (cs, tv)
          in
              ((cs', tv), force_item n)
          end        
              
      fun concat ((cs1, tv1), (cs2, tv2)) =
        (concat' (cs1, tv1, cs2, tv2), tv1)
            
      fun take ((cs, tv), n) =
        let val (cs1, x, _) = split' (cs, tv, n)
        in
            (pushBack' (cs1, tv, x), tv)
        end
            
      fun drop ((cs, tv), n) =
        let val (_, _, cs2) = split' (cs, tv, n)
        in
            (cs2, tv)
        end
            
      val foldr = fn f => fn a => fn (cs, tv) =>
                     foldr' f a cs
                            
      fun persistent (cs, _) =
        cs

    end (* Transient *)

end
