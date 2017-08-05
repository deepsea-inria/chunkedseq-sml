functor ChunkedseqFn (C : CHUNK) :> CHUNKEDSEQ = struct

    type 'a chunk =
         'a C.chunk

    datatype 'a node
      = Item of 'a
      | Interior of 'a node chunk

    type 'a buffer =
         'a node chunk

    type weight =
         C.weight

    datatype 'a persistent
      = Shallow of 'a buffer
      | Deep of weight * 'a deep

    and 'a deep = DC of {
       fo : 'a buffer,
       fi : 'a buffer,
       mid : 'a persistent,
       bi : 'a buffer,
       bo : 'a buffer
     }

    type transient_version =
         C.transient_version
                            
    type 'a transient =
         ('a persistent * transient_version)

    type 'a weight_fn =
         'a C.weight_fn

    fun weightOfItem n =
      (case n of
           Item x => 1
         | Interior c => C.weight c)
                            
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
                     
    fun create tid =
        Shallow (C.create tid)
                
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

    fun pushFront' (cs, tid, x) =
      (case cs of
           Shallow c =>
           if chunkFull c then
               pushFront' (mkDeep (DC {fo=ec tid, fi=ec tid, mid=create tid, bi=ec tid, bo=c}), tid, x)
           else
               Shallow (C.pushFront weightOfItem (c, tid, x))
         | Deep (_, DC {fo, fi, mid, bi, bo}) =>
           if chunkFull fo then
               if chunkEmpty fi then
                   pushFront' (mkDeep (DC {fo=ec tid, fi=fo, mid=mid, bi=bi, bo=bo}), tid, x)
               else
                   let val mid' = pushFront' (mid, tid, Interior fi)
                   in
                       pushFront' (mkDeep (DC {fo=ec tid, fi=fo, mid=mid', bi=bi, bo=bo}), tid, x)
                   end
           else
               let val fo' = C.pushFront weightOfItem (fo, tid, x)
               in
                   mkDeep (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo})
               end)


    fun pushBack' (cs, tid, x) =
      (case cs of
           Shallow c =>
           if chunkFull c then
               pushBack' (mkDeep (DC {fo=c, fi=ec tid, mid=create tid, bi=ec tid, bo=ec tid}), tid, x)
           else
               Shallow (C.pushBack weightOfItem (c, tid, x))
         | Deep (_, DC {fo, fi, mid, bi, bo}) =>
           if chunkFull bo then
               if chunkEmpty bi then
                   pushBack' (mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bo, bo=ec tid}), tid, x)
               else
                   let val mid' = pushBack' (mid, tid, Interior bi)
                   in
                       pushBack' (mkDeep (DC {fo=fo, fi=fi, mid=mid', bi=bo, bo=ec tid}), tid, x)
                   end
           else
               let val bo' = C.pushBack weightOfItem (bo, tid, x)
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

    fun mkDeep' (d, tid) = 
      check (mkDeep d, tid)
            
    and check (cs, tid) =
        (case cs of
             Shallow c =>
             raise Fail "impossible"
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             let val w = C.weight fo + C.weight fi +
                         C.weight bi + C.weight bo
             in
                 if w = 0 andalso not (empty mid) then
                     let val (mid', n) = popFront' (mid, tid)
                         val fo' = force_interior n
                     in
                         mkDeep (DC {fo=fo', fi=fi, mid=mid', bi=bi, bo=bo})
                     end
                 else if w = 1 andalso empty mid then
                     mkShallow (fo, fi, bi, bo)
                 else if w = 0 andalso empty mid then
                     create tid
                 else
                     cs
             end)

    and popFront' (cs, tid) =
        (case cs of
             Shallow c =>
             let val (c', x) = C.popFront weightOfItem (c, tid)
             in
                 (Shallow c', x)
             end
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             if chunkEmpty fo then
                 if not (chunkEmpty fi) then
                     popFront' (mkDeep' (DC {fo=fi, fi=ec tid, mid=mid, bi=bi, bo=bo}, tid), tid)
                 else if not (empty mid) then
                     let val (mid', n) = popFront' (mid, tid)
                         val c = force_interior n               
                     in
                         popFront' (mkDeep' (DC {fo=c, fi=fi, mid=mid', bi=bi, bo=bo}, tid), tid)
                     end
                 else if not (chunkEmpty bi) then
                     popFront' (mkDeep' (DC {fo=bi, fi=fi, mid=mid, bi=ec tid, bo=bo}, tid), tid)
                 else
                     let val (bo', x) = C.popFront weightOfItem (bo, tid)
                     in
                         (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}, tid), x)
                     end
             else
                 let val (fo', x) = C.popFront weightOfItem (fo, tid)
                 in
                     (mkDeep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}, tid), x)
                 end)

    and popBack' (cs, tid) =
        (case cs of
             Shallow c =>
             let val (c', x) = C.popBack weightOfItem (c, tid)
             in
                 (Shallow c', x)
             end
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             if chunkEmpty bo then
                 if not (chunkEmpty bi) then
                     popBack' (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=ec tid, bo=bi}, tid), tid)
                 else if not (empty mid) then
                     let val (mid', n) = popBack' (mid, tid)
                         val c = force_interior n               
                     in
                         popBack' (mkDeep' (DC {fo=fo, fi=fi, mid=mid', bi=bi, bo=c}, tid), tid)
                     end
                 else if not (chunkEmpty fi) then
                     popBack' (mkDeep' (DC {fo=fo, fi=ec tid, mid=mid, bi=bi, bo=fi}, tid), tid)
                 else
                     let val (fo', x) = C.popBack weightOfItem (fo, tid)
                     in
                         (mkDeep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}, tid), x)
                     end
             else
                 let val (bo', x) = C.popBack weightOfItem (bo, tid)
                 in
                     (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}, tid), x)
                 end)

    and pushBufferFront (cs, tid, c) =
        if chunkEmpty c then
            cs
        else if empty cs then
            Shallow (C.pushFront weightOfItem (ec tid, tid, Interior c))
        else
            let val (cs', n) = popFront' (cs, tid)
                val c' = force_interior n
            in
                if C.size c + C.size c' <= C.capacity then
                    pushFront' (cs', tid, Interior (C.concat weightOfItem (c, tid, c', tid)))
                else
                    pushFront' (cs, tid, Interior c)
            end

    and pushBufferBack (cs, tid, c) =
        if chunkEmpty c then
            cs
        else if empty cs then
            Shallow (C.pushBack weightOfItem (ec tid, tid, Interior c))
        else
            let val (cs', n) = popBack' (cs, tid)
                val c' = force_interior n
            in
                if C.size c + C.size c' <= C.capacity then
                    pushBack' (cs', tid, Interior (C.concat weightOfItem (c', tid, c, tid)))
                else
                    pushBack' (cs, tid, Interior c)
            end

    and transferContentsFront (cs, csTid, c, cTid) =
        if chunkEmpty c then
            cs
        else
            let val (c', x) = C.popBack weightOfItem (c, cTid)
            in
                transferContentsFront (pushFront' (cs, csTid, x), csTid, c', cTid)
            end

    and transferContentsBack (cs, csTid, c, cTid) =
        if chunkEmpty c then
            cs
        else
            let val (c', x) = C.popFront weightOfItem (c, cTid)
            in
                transferContentsBack (pushBack' (cs, csTid, x), csTid, c', cTid)
            end
        
    and concat' (cs1, tid1, cs2, tid2) =
        if empty cs1 then
            cs2
        else if empty cs2 then
            cs1
        else
            (case (cs1, cs2) of
                 (Shallow c1, _) =>
                 transferContentsFront (cs2, tid2, c1, tid1)
               | (_, Shallow c2) =>
                 transferContentsBack (cs1, tid1, c2, tid2)
               | (Deep (_, DC {fo=fo1, fi=fi1, mid=mid1, bi=bi1, bo=bo1}),
                  Deep (_, DC {fo=fo2, fi=fi2, mid=mid2, bi=bi2, bo=bo2})) =>
                 let val mid1' = pushBufferBack (mid1, tid1, bi1)
                     val mid1'' = pushBufferBack (mid1', tid1, bo1)
                     val mid2' = pushBufferFront (mid2, tid2, fi2)
                     val mid2'' = pushBufferFront (mid2', tid2, fo2)
                     val (mid1''', mid2''') =
                         if empty mid1'' orelse empty mid2'' then
                             (mid1'', mid2'')
                         else
                             let val (mid1''', n1) = popBack' (mid1'', tid1)
                                 val (mid2''', n2) = popFront' (mid2'', tid2)
                                 val (c1, c2) = (force_interior n1, force_interior n2)
                             in
                                 if C.size c1 + C.size c2 <= C.capacity then
                                     let val c' = C.concat weightOfItem (c1, tid1, c2, tid2)
                                     in
                                         (pushBack' (mid1''', tid1, Interior c'), mid2''')
                                     end
                                 else
                                     (mid1'', mid2'')
                             end
                     val mid12 = concat' (mid1''', tid1, mid2''', tid2)
                 in
                     mkDeep' (DC {fo=fo1, fi=fi1, mid=mid12, bi=bi2, bo=bo2}, tid1)
                 end)

    and split' (cs, tid, i) =
        (case cs of
             Shallow c =>
             let val (c1, x, c2) = C.split weightOfItem (c, tid, i)
             in
                 (Shallow c1, x, Shallow c2)
             end
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             let val (wfo, wfi) = (C.weight fo, C.weight fi)
                 val wm = weight mid
                 val (wbi, wbo) = (C.weight bi, C.weight bo)
                 val (cs1, x, cs2) =
                     if i < wfo then
                         let val (fo1, x, fo2) = C.split weightOfItem (fo, tid, i)
                             val cs1 = mkDeep (DC {fo=fo1, fi=ec tid, mid=create tid, bi=ec tid, bo=ec tid})
                             val cs2 = mkDeep (DC {fo=fo2, fi=fi, mid=mid, bi=bi, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi then
                         let val j = i - wfo
                             val (fi1, x, fi2) = C.split weightOfItem (fi, tid, j)
                             val cs1 = mkDeep (DC {fo=fo, fi=ec tid, mid=create tid, bi=ec tid, bo=fi1})
                             val cs2 = mkDeep (DC {fo=fi2, fi=ec tid, mid=mid, bi=bi, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi + wm then
                         let val j = i - wfo - wfi
                             val (mid1, n, mid2) = split' (mid, tid, j)
                             val c = force_interior n
                             val (c1, x, c2) = C.split weightOfItem (c, tid, j - weight mid1)
                             val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid1, bi=ec tid, bo=c1})
                             val cs2 = mkDeep (DC {fo=c2, fi=ec tid, mid=mid2, bi=bi, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi + wm + wbi then
                         let val j = i - wfo - wfi - wm
                             val (bi1, x, bi2) = C.split weightOfItem (bi, tid, j)
                             val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=ec tid, bo=bi1})
                             val cs2 = mkDeep (DC {fo=bi2, fi=ec tid, mid=create tid, bi=ec tid, bo=bo})
                         in
                             (cs1, x, cs2)
                         end
                     else if i < wfo + wfi + wm + wbi + wbo then
                         let val j = i - wfo - wfi - wm - wbi
                             val (bo1, x, bo2) = C.split weightOfItem (bo, tid, j)
                             val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo1})
                             val cs2 = mkDeep (DC {fo=bo2, fi=ec tid, mid=create tid, bi=ec tid, bo=ec tid})
                         in
                             (cs1, x, cs2)
                         end
                     else
                         raise Fail "ChunkedseqFn.split: out of bounds"
             in
                 (check (cs1, tid), x, check (cs2, tid))
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

    fun tabulate' (n, f, tid) =
      let val n' = n - 1
      in
          if n' = 0 then
              create tid
          else
              pushFront' (tabulate' (n', f, tid), tid, Item (f n'))
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

      type 'a t = 'a persistent

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

      type 'a t = 'a transient

      val size = fn (cs, _) =>
                    size cs

      fun tabulate (f, n) =
        let val tid = newGuid ()
        in
            (tabulate' (f, n, tid), tid)
        end
                         
      fun pushFront ((cs, tid), x) =
        (pushFront' (cs, tid, Item x), tid)
            
      fun pushBack ((cs, tid), x) =
        (pushBack' (cs, tid, Item x), tid)
            
      fun popFront (cs, tid) =
          let val (cs', n) = popFront' (cs, tid)
          in
              ((cs, tid), force_item n)
          end
              
      fun popBack (cs, tid) =
          let val (cs', n) = popBack' (cs, tid)
          in
              ((cs', tid), force_item n)
          end        
              
      fun concat ((cs1, tid1), (cs2, tid2)) =
        (concat' (cs1, tid1, cs2, tid2), tid1)
            
      fun take ((cs, tid), n) =
        let val (cs1, x, _) = split' (cs, tid, n)
        in
            (pushBack' (cs1, tid, x), tid)
        end
            
      fun drop ((cs, tid), n) =
        let val (_, _, cs2) = split' (cs, tid, n)
        in
            (cs2, tid)
        end
            
      val foldr = fn f => fn a => fn (cs, tid) =>
                     foldr' f a cs
                            
      fun persistent (cs, _) =
        cs

    end (* Transient *)

end
