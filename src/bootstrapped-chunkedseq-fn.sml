functor BootstrappedChunkedseqFn (
    functor ChunkFn (S : SEARCH)
            : CHUNK where type Search.Measure.t = S.measure
                      and type Search.find_by = S.find_by
    structure Search : SEARCH
) :> CHUNKEDSEQ = struct

    structure Search = Search

    structure Measure = Search.Measure

    structure Algebra = Measure.Algebra
                            
    type measure =
         Search.measure
                       
    datatype find_by = datatype Search.find_by

    datatype 'a metadata
      = MetaData of {
          measure : 'a Measure.measure_fn,
          trivialItem : 'a,
          itemOverwrite : bool
      }

    structure WithWeight = struct

        structure Algebra = CombineAlgebrasFn (
            structure A = WeightAlgebra
            structure B = Algebra)

        structure Measure = MeasureFn (
            structure Algebra = Algebra)

        structure Search = SearchFn (
            structure Measure = Measure)

        structure Chunk = ChunkFn (Search)

        structure Search = Chunk.Search
                                    
        type measure =
             Search.measure

        datatype find_by = datatype Search.find_by

        datatype metadata = datatype Chunk.metadata

        type weight =
             int

        val weight : measure -> weight =
         fn (w, _) : measure =>
            w

        val client =
         fn (_, cv) =>
            cv
                                               
        val identity =
            Algebra.identity
                
        val combine =
            Algebra.combine
                
        val inverseOpt =
            Algebra.inverseOpt

        val measureOfMetaData =
         fn (MetaData {measure, ...}) =>
            measure

    end

    structure WW = WithWeight

    structure C = WW.Chunk

    datatype 'a node
      = Nil
      | Item of 'a
      | Interior of 'a node C.chunk

    type 'a buffer =
         'a node C.chunk

    datatype 'a persistent
      = Shallow of 'a buffer
      | Deep of WW.measure * 'a deep

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

    fun measureWithWeight cs =
      (case cs of
           Shallow c =>
           C.measure c
         | Deep (mv, _) =>
           mv)
             
    fun chunkEmpty c =
      (C.length c = 0)
                               
    fun chunkFull c =
      (C.length c = C.capacity)

    fun chunkWeight c =
      WW.weight (C.measure c)
                     
    fun ec md tv =
      C.create md tv
                     
    fun create md tv =
      Shallow (ec md tv)
                
    fun measure cs =
        WW.client (measureWithWeight cs)

    fun weight cs =
        WW.weight (measureWithWeight cs)

    val length =
        weight
            
    fun empty xs =
      (weight xs = 0)

    val combineMeasures =
        List.foldr WW.combine WW.identity

    fun mkDeep (d as DC {fo, fi, mid, bi, bo}) =
      let val mv = combineMeasures [C.measure fo, C.measure fi,
                                    measureWithWeight mid,
                                    C.measure bi, C.measure bo]
      in
          Deep (mv, d)
      end

    fun pushFront' md tv (cs, x) =
      let val ec = ec md tv
          val create = create md tv
          val pushFront' = pushFront' md tv
      in
          case cs of
              Shallow c =>
              if chunkFull c then
                  pushFront' (mkDeep (DC {fo=ec, fi=ec, mid=create, bi=ec, bo=c}), x)
              else
                  Shallow (C.pushFront md tv (c, x))
            | Deep (_, DC {fo, fi, mid, bi, bo}) =>
              if chunkFull fo then
                  if chunkEmpty fi then
                      pushFront' (mkDeep (DC {fo=ec, fi=fo, mid=mid, bi=bi, bo=bo}), x)
                  else
                      let val mid' = pushFront' (mid, Interior fi)
                      in
                          pushFront' (mkDeep (DC {fo=ec, fi=fo, mid=mid', bi=bi, bo=bo}), x)
                      end
              else
                  let val fo' = C.pushFront md tv (fo, x)
                  in
                      mkDeep (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo})
                  end
      end

    fun pushBack' md tv (cs, x) =
      let val ec = ec md tv
          val create = create md tv
          val pushBack' = pushBack' md tv
      in
          case cs of
              Shallow c =>
              if chunkFull c then
               pushBack' (mkDeep (DC {fo=c, fi=ec, mid=create, bi=ec, bo=ec}), x)
              else
                  Shallow (C.pushBack md tv (c, x))
            | Deep (_, DC {fo, fi, mid, bi, bo}) =>
              if chunkFull bo then
                  if chunkEmpty bi then
                      pushBack' (mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bo, bo=ec}), x)
                  else
                      let val mid' = pushBack' (mid, Interior bi)
                      in
                          pushBack' (mkDeep (DC {fo=fo, fi=fi, mid=mid', bi=bo, bo=ec}), x)
                      end
              else
                  let val bo' = C.pushBack md tv (bo, x)
                  in
                      mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'})
                  end
      end

    fun mkShallow (c1, c2, c3, c4) =
      let val c =
              if C.length c1 > 0 then
                  c1
              else if C.length c2 > 0 then
                  c2
              else if C.length c3 > 0 then
                  c3
              else if C.length c4 > 0 then
                  c4
              else
                  raise Fail "impossible"
      in
          Shallow c
      end
          
    fun forceItem n =
      (case n of
           Nil => raise Fail "impossible"
         | Item x => x
         | Interior _ => raise Fail "impossible")

    fun forceInterior n =
      (case n of
           Nil => raise Fail "impossible"
         | Item _ => raise Fail "impossible"
         | Interior c => c)
          
    fun mkDeep' md tv d = 
      check md tv (mkDeep d)
            
    and check md tv cs =
        (case cs of
             Shallow c =>
             raise Fail "impossible"
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             let val nbItemsInBufs =
                     List.foldr (fn (c, acc) => C.length c + acc) 0 [fo, fi, bi, bo]
             in
                 if nbItemsInBufs = 0 andalso not (empty mid) then
                     let val (mid', n) = popFront' md tv mid
                         val fo' = forceInterior n
                     in
                         mkDeep (DC {fo=fo', fi=fi, mid=mid', bi=bi, bo=bo})
                     end
                 else if nbItemsInBufs = 1 andalso empty mid then
                     mkShallow (fo, fi, bi, bo)
                 else if nbItemsInBufs = 0 andalso empty mid then
                     create md tv
                 else
                     cs
             end)

    and popFront' md tv cs =
      let val ec = ec md tv
          val mkDeep' = mkDeep' md tv
          val create = create md tv
          val popFront' = popFront' md tv
      in
          case cs of
              Shallow c =>
              let val (c', x) = C.popFront md tv c
              in
                  (Shallow c', x)
              end
            | Deep (_, DC {fo, fi, mid, bi, bo})  =>
              if chunkEmpty fo then
                  if not (chunkEmpty fi) then
                      popFront' (mkDeep' (DC {fo=fi, fi=ec, mid=mid, bi=bi, bo=bo}))
                  else if not (empty mid) then
                      let val (mid', n) = popFront' mid
                          val c = forceInterior n               
                      in
                          popFront' (mkDeep' (DC {fo=c, fi=fi, mid=mid', bi=bi, bo=bo}))
                      end
                  else if not (chunkEmpty bi) then
                      popFront' (mkDeep' (DC {fo=bi, fi=fi, mid=mid, bi=ec, bo=bo}))
                  else
                      let val (bo', x) = C.popFront md tv bo
                      in
                          (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}), x)
                      end
              else
                  let val (fo', x) = C.popFront md tv fo
                  in
                      (mkDeep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}), x)
                  end
      end

    and popBack' md tv cs =
      let val ec = ec md tv
          val mkDeep' = mkDeep' md tv
          val create = create md tv
          val popBack' = popBack' md tv
      in
          case cs of
              Shallow c =>
              let val (c', x) = C.popBack md tv c
              in
                  (Shallow c', x)
              end
            | Deep (_, DC {fo, fi, mid, bi, bo})  =>
              if chunkEmpty bo then
                  if not (chunkEmpty bi) then
                      popBack' (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=ec, bo=bi}))
                  else if not (empty mid) then
                      let val (mid', n) = popBack' mid
                          val c = forceInterior n           
                      in
                          popBack' (mkDeep' (DC {fo=fo, fi=fi, mid=mid', bi=bi, bo=c}))
                      end
                  else if not (chunkEmpty fi) then
                      popBack' (mkDeep' (DC {fo=fo, fi=ec, mid=mid, bi=bi, bo=fi}))
                  else
                      let val (fo', x) = C.popBack md tv fo
                      in
                          (mkDeep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}), x)
                      end
              else
                  let val (bo', x) = C.popBack md tv bo
                  in
                      (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}), x)
                  end
      end

    and pushBufferFront md tv (cs, c) =
        let val ec = ec md tv
        in
            if chunkEmpty c then
                cs
            else if empty cs then
                Shallow (C.pushFront md tv (ec, Interior c))
            else
                let val (cs', n) = popFront' md tv cs
                    val c' = forceInterior n
                in
                    if C.length c + C.length c' <= C.capacity then
                        pushFront' md tv (cs', Interior (C.concat md tv (c, c')))
                    else
                        pushFront' md tv (cs, Interior c)
                end
        end

    and pushBufferBack md tv (cs, c) =
        let val ec = ec md tv
        in
            if chunkEmpty c then
                cs
            else if empty cs then
                Shallow (C.pushBack md tv (ec, Interior c))
            else
                let val (cs', n) = popBack' md tv cs
                    val c' = forceInterior n
                in
                    if C.length c + C.length c' <= C.capacity then
                        pushBack' md tv (cs', Interior (C.concat md tv (c', c)))
                    else
                        pushBack' md tv (cs, Interior c)
                end
        end

    and transferContentsFront md tv (cs, c) =
        if chunkEmpty c then
            cs
        else
            let val (c', x) = C.popBack md tv c
            in
                transferContentsFront md tv (pushFront' md tv (cs, x), c')
            end

    and transferContentsBack md tv (cs, c) =
        if chunkEmpty c then
            cs
        else
            let val (c', x) = C.popFront md tv c
            in
                transferContentsBack md tv (pushBack' md tv (cs, x), c')
            end
        
    and concat' md tv (cs1, cs2) =
        let val pushBufferBack = pushBufferBack md tv
            val pushBufferFront = pushBufferFront md tv
        in
            if empty cs1 then
                cs2
            else if empty cs2 then
                cs1
            else
                (case (cs1, cs2) of
                     (Shallow c1, _) =>
                     transferContentsFront md tv (cs2, c1)
                   | (_, Shallow c2) =>
                     transferContentsBack md tv (cs1, c2)
                   | (Deep (_, DC {fo=fo1, fi=fi1, mid=mid1, bi=bi1, bo=bo1}),
                      Deep (_, DC {fo=fo2, fi=fi2, mid=mid2, bi=bi2, bo=bo2})) =>
                     let val mid1' = pushBufferBack (mid1, bi1)
                         val mid1'' = pushBufferBack (mid1', bo1)
                         val mid2' = pushBufferFront (mid2, fi2)
                         val mid2'' = pushBufferFront (mid2', fo2)
                         val (mid1''', mid2''') =
                             if empty mid1'' orelse empty mid2'' then
                                 (mid1'', mid2'')
                             else
                                 let val (mid1''', n1) = popBack' md tv mid1''
                                     val (mid2''', n2) = popFront' md tv mid2''
                                     val (c1, c2) = (forceInterior n1, forceInterior n2)
                                 in
                                     if C.length c1 + C.length c2 <= C.capacity then
                                         let val c' = C.concat md tv (c1, c2)
                                         in
                                             (pushBack' md tv (mid1''', Interior c'), mid2''')
                                         end
                                     else
                                         (mid1'', mid2'')
                                 end
                         val mid12 = concat' md tv (mid1''', mid2''')
                     in
                         mkDeep' md tv (DC {fo=fo1, fi=fi1, mid=mid12, bi=bi2, bo=bo2})
                     end)
        end

    and splitByIndex md tv (cs, i) =
        let val ec = ec md tv
            val create = create md tv
        in
            case cs of
                Shallow c =>
                let val (c1, x, c2) = C.split md tv (c, WW.Index i)
                in
                    (Shallow c1, x, Shallow c2)
                end
              | Deep (_, DC {fo, fi, mid, bi, bo})  =>
                let val (wfo, wfi) = (chunkWeight fo, chunkWeight fi)
                    val wm = weight mid
                    val (wbi, wbo) = (chunkWeight bi, chunkWeight bo)
                    val (cs1, x, cs2) =
                        if i < wfo then
                            let val (fo1, x, fo2) = C.split md tv (fo, WW.Index i)
                                val cs1 = mkDeep (DC {fo=fo1, fi=ec, mid=create, bi=ec, bo=ec})
                                val cs2 = mkDeep (DC {fo=fo2, fi=fi, mid=mid, bi=bi, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi then
                            let val j = i - wfo
                                val (fi1, x, fi2) = C.split md tv (fi, WW.Index j)
                                val cs1 = mkDeep (DC {fo=fo, fi=ec, mid=create, bi=ec, bo=fi1})
                                val cs2 = mkDeep (DC {fo=fi2, fi=ec, mid=mid, bi=bi, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi + wm then
                            let val j = i - wfo - wfi
                                val (mid1, n, mid2) = splitByIndex md tv (mid, j)
                                val c = forceInterior n
                                val (c1, x, c2) = C.split md tv (c, WW.Index (j - weight mid1))
                                val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid1, bi=ec, bo=c1})
                                val cs2 = mkDeep (DC {fo=c2, fi=ec, mid=mid2, bi=bi, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi + wm + wbi then
                            let val j = i - wfo - wfi - wm
                                val (bi1, x, bi2) = C.split md tv (bi, WW.Index j)
                                val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=ec, bo=bi1})
                                val cs2 = mkDeep (DC {fo=bi2, fi=ec, mid=create, bi=ec, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi + wm + wbi + wbo then
                            let val j = i - wfo - wfi - wm - wbi
                                val (bo1, x, bo2) = C.split md tv (bo, WW.Index j)
                                val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo1})
                                val cs2 = mkDeep (DC {fo=bo2, fi=ec, mid=create, bi=ec, bo=ec})
                            in
                                (cs1, x, cs2)
                            end
                        else
                            raise Search.Find_by
                in
                    (check md tv cs1, x, check md tv cs2)
                end
        end

    and splitByPredicate md tv (cs, p) =
        raise Fail "todo"

    fun split' md tv (cs, sb) =
      (case sb
        of Index i =>
           splitByIndex md tv (cs, i)
         | Predicate p =>
           let fun p' (_, mv) =
                 p mv
           in
               splitByPredicate md tv (cs, p')
           end)
      
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
         | Deep (_, DC {fo, fi, mid, bi, bo}) =>
           let val i = foldrBuffer f i bo
               val i = foldrBuffer f i bi
               val i = foldr' f i mid
               val i = foldrBuffer f i fi
           in
               foldrBuffer f i fo
           end)

    fun tabulate' md tv (n, f) =
      let val n' = n - 1
      in
          if n' = 0 then
              create md tv
          else
              pushFront' md tv (tabulate' md tv (n', f), Item (f n'))
      end

    val alwaysInvalidTv = ~1
                                      
    val transientGuid = ref 0
                            
    fun newGuid () =
      let val id = ! transientGuid
      in
          transientGuid := id + 1;
          id
      end

    fun mkMD (MetaData {measure, trivialItem, itemOverwrite}) =
      WW.MetaData {
          measure = (fn nd =>
                        (case nd
                          of Nil =>
                             WW.identity
                           | Item x =>
                             (#1 WW.identity, measure x)
                           | Interior c =>
                             (#1 WW.identity, WW.client (C.measure c)))),
          trivialItem = Nil,
          itemOverwrite = true
      }

    structure Persistent = struct

      type 'a t = 'a persistent

      val length =
          length

      val measure =
          measure          

      fun concat md (cs1, cs2) =
        let val md = mkMD md
        in
            concat' md alwaysInvalidTv (cs1, cs2)
        end

      fun sub md (cs, sb) =
        raise Fail "todo"

      fun take md (cs, sb) =
        let val md = mkMD md
            val (cs1, x, _) = split' md alwaysInvalidTv (cs, sb)
        in
            pushBack' md alwaysInvalidTv (cs1, x)
        end

      fun drop md (cs, sb) =
        let val md = mkMD md
            val (_, _, cs2) = split' md alwaysInvalidTv (cs, sb)
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

      val length =
       fn (cs, _) =>
          length cs

      val measure =
       fn (cs, _) =>
          measure cs

      fun tabulate md (f, n) =
        let val md = mkMD md
            val tv = newGuid ()
        in
            (tabulate' md tv (f, n), tv)
        end

      structure Front : END_ACCESS = struct

        datatype metadata = datatype metadata

        type 'a t = 'a transient

        fun read (cs, tv) =
          raise Fail "todo"

        fun push md ((cs, tv), x) =
          let val md = mkMD md
          in
              (pushFront' md tv (cs, Item x), tv)
          end

        fun pop md (cs, tv) =
          let val md = mkMD md
              val (cs', n) = popFront' md tv cs
          in
              ((cs, tv), forceItem n)
          end

        fun readn md {src, dst, di} =
          raise Fail "todo"

        fun pushn md ((cs, tv), slice) =
          raise Fail "todo"
                
      end

      structure Back : END_ACCESS = struct
      
        datatype metadata = datatype metadata

        type 'a t = 'a transient

        fun read (cs, tv) =
          raise Fail "todo"

        fun push md ((cs, tv), x) =
          let val md = mkMD md
          in
          (pushBack' md tv (cs, Item x), tv)
          end


        fun pop md (cs, tv) =
          let val md = mkMD md
              val (cs', n) = popBack' md tv cs
          in
              ((cs', tv), forceItem n)
          end

        fun readn md {src, dst, di} =
          raise Fail "todo"

        fun pushn md ((cs, tv), slice) =
          raise Fail "todo"
                
      end
              
      fun concat md ((cs1, tv1), (cs2, _)) = 
        let val md = mkMD md
        in
            (concat' md tv1 (cs1, cs2), tv1)
        end

      fun sub md ((cs, _), sb) =
        raise Fail "todo"
            
      fun split md ((cs, tv), sb) =
        let val md = mkMD md
            val (cs1, nd, cs2) = split' md tv (cs, sb)
        in
            ((cs1, tv), forceItem nd, (cs2, tv))
        end
            
      val foldr =
       fn f => fn a => fn (cs, tv) =>
          foldr' f a cs
                            
      fun persistent (cs, _) =
        cs

    end (* Transient *)

end
