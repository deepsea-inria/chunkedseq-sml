functor ChunkedseqFn (C : CHUNK) :> CHUNKEDSEQ = struct

    type weight =
         C.weight

    datatype descr =
        datatype SequenceDescriptor.sequence_descriptor

    datatype ('a, 'b) node
      = Nil
      | Item of 'a
      | Interior of (('a, 'b) node, 'b) C.chunk

    type ('a, 'b) buffer =
         (('a, 'b) node, 'b) C.chunk

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
         | Deep (w, _, _) =>
           w)

    fun cachedValue cs =
      (case cs of
           Shallow c =>
           C.cachedValue c
         | Deep (_, cv, _) =>
           cv)
          
    fun chunkEmpty c =
      C.size c = 0
                               
    fun chunkFull c =
      C.size c = C.capacity
                     
    fun ec sd tv =
      C.create sd tv
                     
    fun create sd tv =
        Shallow (ec sd tv)
                
    fun empty xs =
      (weight xs = 0)

    fun sum ws =
      List.foldr (op +) 0 ws

    fun combineCachedValues algebra cvs =
      let val {combine, identity, inverseOpt} = algebra
      in
          List.foldr combine identity cvs
      end

    fun mkDeep sd (d as DC {fo, fi, mid, bi, bo}) =
      let val SequenceDescriptor {algebra, ...} = sd
          val w = sum [C.weight fo, C.weight fi,
                       weight mid,
                       C.weight bi, C.weight bo]
          val cv = combineCachedValues algebra [C.cachedValue fo, C.cachedValue fi,
                                                cachedValue mid,
                                                C.cachedValue bi, C.cachedValue bo]
      in
          Deep (w, cv, d)
      end

    fun pushFront' sd tv (cs, x) =
      let val ec = ec sd tv
          val mkDeep = mkDeep sd
          val create = create sd tv
          val pushFront' = pushFront' sd tv
      in
          case cs of
              Shallow c =>
              if chunkFull c then
                  pushFront' (mkDeep (DC {fo=ec, fi=ec, mid=create, bi=ec, bo=c}), x)
              else
                  Shallow (C.pushFront sd tv (c, x))
            | Deep (_, _, DC {fo, fi, mid, bi, bo}) =>
              if chunkFull fo then
                  if chunkEmpty fi then
                      pushFront' (mkDeep (DC {fo=ec, fi=fo, mid=mid, bi=bi, bo=bo}), x)
                  else
                      let val mid' = pushFront' (mid, Interior fi)
                      in
                          pushFront' (mkDeep (DC {fo=ec, fi=fo, mid=mid', bi=bi, bo=bo}), x)
                      end
              else
                  let val fo' = C.pushFront sd tv (fo, x)
                  in
                      mkDeep (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo})
                  end
      end

    fun pushBack' sd tv (cs, x) =
      let val ec = ec sd tv
          val mkDeep = mkDeep sd
          val create = create sd tv
          val pushBack' = pushBack' sd tv
      in
          case cs of
              Shallow c =>
              if chunkFull c then
               pushBack' (mkDeep (DC {fo=c, fi=ec, mid=create, bi=ec, bo=ec}), x)
              else
                  Shallow (C.pushBack sd tv (c, x))
            | Deep (_, _, DC {fo, fi, mid, bi, bo}) =>
              if chunkFull bo then
                  if chunkEmpty bi then
                      pushBack' (mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bo, bo=ec}), x)
                  else
                      let val mid' = pushBack' (mid, Interior bi)
                      in
                          pushBack' (mkDeep (DC {fo=fo, fi=fi, mid=mid', bi=bo, bo=ec}), x)
                      end
              else
                  let val bo' = C.pushBack sd tv (bo, x)
                  in
                      mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'})
                  end
      end

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
           Nil => raise Fail "impossible"
         | Item x => x
         | Interior _ => raise Fail "impossible")

    fun force_interior n =
      (case n of
           Nil => raise Fail "impossible"
         | Item _ => raise Fail "impossible"
         | Interior c => c)
          
    fun mkDeep' sd tv d = 
      check sd tv (mkDeep sd d)
            
    and check sd tv cs =
        (case cs of
             Shallow c =>
             raise Fail "impossible"
           | Deep (_, _, DC {fo, fi, mid, bi, bo})  =>
             let val w = sum (List.map C.weight [fo, fi, bi, bo])
             in
                 if w = 0 andalso not (empty mid) then
                     let val (mid', n) = popFront' sd tv mid
                         val fo' = force_interior n
                     in
                         mkDeep sd (DC {fo=fo', fi=fi, mid=mid', bi=bi, bo=bo})
                     end
                 else if w = 1 andalso empty mid then
                     mkShallow (fo, fi, bi, bo)
                 else if w = 0 andalso empty mid then
                     create sd tv
                 else
                     cs
             end)

    and popFront' sd tv cs =
      let val ec = ec sd tv
          val mkDeep = mkDeep sd
          val mkDeep' = mkDeep' sd tv
          val create = create sd tv
          val popFront' = popFront' sd tv
      in
          case cs of
              Shallow c =>
              let val (c', x) = C.popFront sd tv c
              in
                  (Shallow c', x)
              end
            | Deep (_, _, DC {fo, fi, mid, bi, bo})  =>
              if chunkEmpty fo then
                  if not (chunkEmpty fi) then
                      popFront' (mkDeep' (DC {fo=fi, fi=ec, mid=mid, bi=bi, bo=bo}))
                  else if not (empty mid) then
                      let val (mid', n) = popFront' mid
                          val c = force_interior n               
                      in
                          popFront' (mkDeep' (DC {fo=c, fi=fi, mid=mid', bi=bi, bo=bo}))
                      end
                  else if not (chunkEmpty bi) then
                      popFront' (mkDeep' (DC {fo=bi, fi=fi, mid=mid, bi=ec, bo=bo}))
                  else
                      let val (bo', x) = C.popFront sd tv bo
                      in
                          (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}), x)
                      end
              else
                  let val (fo', x) = C.popFront sd tv fo
                  in
                      (mkDeep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}), x)
                  end
      end

    and popBack' sd tv cs =
      let val ec = ec sd tv
          val mkDeep = mkDeep sd
          val mkDeep' = mkDeep' sd tv
          val create = create sd tv
          val popBack' = popBack' sd tv
      in
          case cs of
              Shallow c =>
              let val (c', x) = C.popBack sd tv c
              in
                  (Shallow c', x)
              end
            | Deep (_, _, DC {fo, fi, mid, bi, bo})  =>
              if chunkEmpty bo then
                  if not (chunkEmpty bi) then
                      popBack' (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=ec, bo=bi}))
                  else if not (empty mid) then
                      let val (mid', n) = popBack' mid
                          val c = force_interior n           
                      in
                          popBack' (mkDeep' (DC {fo=fo, fi=fi, mid=mid', bi=bi, bo=c}))
                      end
                  else if not (chunkEmpty fi) then
                      popBack' (mkDeep' (DC {fo=fo, fi=ec, mid=mid, bi=bi, bo=fi}))
                  else
                      let val (fo', x) = C.popBack sd tv fo
                      in
                          (mkDeep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}), x)
                      end
              else
                  let val (bo', x) = C.popBack sd tv bo
                  in
                      (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}), x)
                  end
      end

    and pushBufferFront sd tv (cs, c) =
        let val ec = ec sd tv
        in
            if chunkEmpty c then
                cs
            else if empty cs then
                Shallow (C.pushFront sd tv (ec, Interior c))
            else
                let val (cs', n) = popFront' sd tv cs
                    val c' = force_interior n
                in
                    if C.size c + C.size c' <= C.capacity then
                        pushFront' sd tv (cs', Interior (C.concat sd tv (c, c')))
                    else
                        pushFront' sd tv (cs, Interior c)
                end
        end

    and pushBufferBack sd tv (cs, c) =
        let val ec = ec sd tv
        in
            if chunkEmpty c then
                cs
            else if empty cs then
                Shallow (C.pushBack sd tv (ec, Interior c))
            else
                let val (cs', n) = popBack' sd tv cs
                    val c' = force_interior n
                in
                    if C.size c + C.size c' <= C.capacity then
                        pushBack' sd tv (cs', Interior (C.concat sd tv (c', c)))
                    else
                        pushBack' sd tv (cs, Interior c)
                end
        end

    and transferContentsFront sd tv (cs, c) =
        if chunkEmpty c then
            cs
        else
            let val (c', x) = C.popBack sd tv c
            in
                transferContentsFront sd tv (pushFront' sd tv (cs, x), c')
            end

    and transferContentsBack sd tv (cs, c) =
        if chunkEmpty c then
            cs
        else
            let val (c', x) = C.popFront sd tv c
            in
                transferContentsBack sd tv (pushBack' sd tv (cs, x), c')
            end
        
    and concat' sd tv (cs1, cs2) =
        let val pushBufferBack = pushBufferBack sd tv
            val pushBufferFront = pushBufferFront sd tv
        in
            if empty cs1 then
                cs2
            else if empty cs2 then
                cs1
            else
                (case (cs1, cs2) of
                     (Shallow c1, _) =>
                     transferContentsFront sd tv (cs2, c1)
                   | (_, Shallow c2) =>
                     transferContentsBack sd tv (cs1, c2)
                   | (Deep (_, _, DC {fo=fo1, fi=fi1, mid=mid1, bi=bi1, bo=bo1}),
                      Deep (_, _, DC {fo=fo2, fi=fi2, mid=mid2, bi=bi2, bo=bo2})) =>
                     let val mid1' = pushBufferBack (mid1, bi1)
                         val mid1'' = pushBufferBack (mid1', bo1)
                         val mid2' = pushBufferFront (mid2, fi2)
                         val mid2'' = pushBufferFront (mid2', fo2)
                         val (mid1''', mid2''') =
                             if empty mid1'' orelse empty mid2'' then
                                 (mid1'', mid2'')
                             else
                                 let val (mid1''', n1) = popBack' sd tv mid1''
                                     val (mid2''', n2) = popFront' sd tv mid2''
                                     val (c1, c2) = (force_interior n1, force_interior n2)
                                 in
                                     if C.size c1 + C.size c2 <= C.capacity then
                                         let val c' = C.concat sd tv (c1, c2)
                                         in
                                             (pushBack' sd tv (mid1''', Interior c'), mid2''')
                                         end
                                     else
                                         (mid1'', mid2'')
                                 end
                         val mid12 = concat' sd tv (mid1''', mid2''')
                     in
                         mkDeep' sd tv (DC {fo=fo1, fi=fi1, mid=mid12, bi=bi2, bo=bo2})
                     end)
        end

    and split' sd tv (cs, i) =
        let val ec = ec sd tv
            val create = create sd tv
            val mkDeep = mkDeep sd
        in
            case cs of
                Shallow c =>
                let val (c1, x, c2) = C.split sd tv (c, i)
                in
                    (Shallow c1, x, Shallow c2)
                end
              | Deep (_, _, DC {fo, fi, mid, bi, bo})  =>
                let val (wfo, wfi) = (C.weight fo, C.weight fi)
                    val wm = weight mid
                    val (wbi, wbo) = (C.weight bi, C.weight bo)
                    val (cs1, x, cs2) =
                        if i < wfo then
                            let val (fo1, x, fo2) = C.split sd tv (fo, i)
                                val cs1 = mkDeep (DC {fo=fo1, fi=ec, mid=create, bi=ec, bo=ec})
                                val cs2 = mkDeep (DC {fo=fo2, fi=fi, mid=mid, bi=bi, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi then
                            let val j = i - wfo
                                val (fi1, x, fi2) = C.split sd tv (fi, j)
                                val cs1 = mkDeep (DC {fo=fo, fi=ec, mid=create, bi=ec, bo=fi1})
                                val cs2 = mkDeep (DC {fo=fi2, fi=ec, mid=mid, bi=bi, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi + wm then
                            let val j = i - wfo - wfi
                                val (mid1, n, mid2) = split' sd tv (mid, j)
                                val c = force_interior n
                                val (c1, x, c2) = C.split sd tv (c, j - weight mid1)
                                val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid1, bi=ec, bo=c1})
                                val cs2 = mkDeep (DC {fo=c2, fi=ec, mid=mid2, bi=bi, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi + wm + wbi then
                            let val j = i - wfo - wfi - wm
                                val (bi1, x, bi2) = C.split sd tv (bi, j)
                                val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=ec, bo=bi1})
                                val cs2 = mkDeep (DC {fo=bi2, fi=ec, mid=create, bi=ec, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi + wm + wbi + wbo then
                            let val j = i - wfo - wfi - wm - wbi
                                val (bo1, x, bo2) = C.split sd tv (bo, j)
                                val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo1})
                                val cs2 = mkDeep (DC {fo=bo2, fi=ec, mid=create, bi=ec, bo=ec})
                            in
                                (cs1, x, cs2)
                            end
                        else
                            raise Fail "ChunkedseqFn.split: out of bounds"
                in
                    (check sd tv cs1, x, check sd tv cs2)
                end
        end

    fun foldrNode f i n =
      (case n of
           Nil =>
           i
         | Item x =>
           f (x, i)
         | Interior c =>
           foldrBuffer f i c)

    and foldrBuffer f i b =
      C.foldr (fn (n, i) => foldrNode f i n) i b

    fun foldr' f i cs =
      (case cs of
           Shallow c =>
           foldrBuffer f i c
         | Deep (_, _, DC {fo, fi, mid, bi, bo}) =>
           let val i = foldrBuffer f i bo
               val i = foldrBuffer f i bi
               val i = foldr' f i mid
               val i = foldrBuffer f i fi
           in
               foldrBuffer f i fo
           end)

    fun tabulate' sd tv (n, f) =
      let val n' = n - 1
      in
          if n' = 0 then
              create sd tv
          else
              pushFront' sd tv (tabulate' sd tv (n', f), Item (f n'))
      end

    val alwaysInvalidTv = ~1
                                      
    val transientGuid = ref 0
                            
    fun newGuid () =
      let val id = ! transientGuid
      in
          transientGuid := id + 1;
          id
      end

    fun mkSD (SequenceDescriptor {weight, measure, algebra, ...}) =
      let val {combine, identity, inverseOpt} = algebra
          fun weight' n =
            (case n
              of Nil =>
                 0
               | Item x =>
                 weight x
               | Interior c =>
                 C.weight c)
          fun measure' n =
            (case n
              of Nil =>
                 identity
               | Item x =>
                 measure x
               | Interior c =>
                 C.cachedValue c)            
      in
          SequenceDescriptor {
              weight = weight',
              measure = measure',
              algebra = algebra,
              trivialItem = Nil,
              itemOverwrite = true
          }
      end

    structure Persistent = struct

      type ('a, 'b) t = ('a, 'b) persistent

      val weight =
          weight

      val cachedValue =
          cachedValue

      fun concat sd (cs1, cs2) =
        let val sd = mkSD sd
        in
            concat' sd alwaysInvalidTv (cs1, cs2)
        end

      fun take sd (cs, n) =
        let val sd = mkSD sd
            val (cs1, x, _) = split' sd alwaysInvalidTv(cs, n)
        in
            pushBack' sd alwaysInvalidTv (cs1, x)
        end

      fun drop sd (cs, n) =
        let val sd = mkSD sd
            val (_, _, cs2) = split' sd alwaysInvalidTv (cs, n)
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

      val weight =
       fn (cs, _) =>
          weight cs

      val cachedValue =
       fn (cs, _) =>
          cachedValue cs

      fun tabulate sd (f, n) =
        let val sd = mkSD sd
            val tv = newGuid ()
        in
            (tabulate' sd tv (f, n), tv)
        end
                         
      fun pushFront sd ((cs, tv), x) =
        let val sd = mkSD sd
        in
            (pushFront' sd tv (cs, Item x), tv)
        end
            
      fun pushBack sd ((cs, tv), x) =
        let val sd = mkSD sd
        in
            (pushBack' sd tv (cs, Item x), tv)
        end
            
      fun popFront sd (cs, tv) =
        let val sd = mkSD sd
            val (cs', n) = popFront' sd tv cs
        in
            ((cs, tv), force_item n)
        end
              
      fun popBack sd (cs, tv) =
        let val sd = mkSD sd
            val (cs', n) = popBack' sd tv cs
        in
            ((cs', tv), force_item n)
        end        
              
      fun concat sd ((cs1, tv1), (cs2, _)) =
        let val sd = mkSD sd
        in
            (concat' sd tv1 (cs1, cs2), tv1)
        end
            
      fun take sd ((cs, tv), n) =
        let val sd = mkSD sd
            val (cs1, x, _) = split' sd tv (cs, n)
        in
            (pushBack' sd tv (cs1, x), tv)
        end
            
      fun drop sd ((cs, tv), n) =
        let val sd = mkSD sd
            val (_, _, cs2) = split' sd tv (cs, n)
        in
            (cs2, tv)
        end
            
      val foldr =
       fn f => fn a => fn (cs, tv) =>
          foldr' f a cs
                            
      fun persistent (cs, _) =
        cs

    end (* Transient *)

end
