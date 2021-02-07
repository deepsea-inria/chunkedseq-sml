functor BootstrappedChunkedseqFn (
    structure Chunk : CHUNK
) :> CHUNKEDSEQ where Search = Chunk.Search = struct

    (*************************************************)
    (* Shortcuts *)
			       
    structure Search = Chunk.Search

    structure Measure = Search.Measure

    structure Algebra = Measure.Algebra
                            
    type measure
         = Search.measure
                       
    datatype find_by
      = datatype Search.find_by

    structure C = Chunk

    structure TV = C.TransientVersion

    structure A = Algebra

    structure S = Search

    (*************************************************)
    (* Datastructure definitions *)
                      
    datatype 'a metadata
      = MetaData of {
          measure : 'a Measure.measure_fn
      }

    datatype 'a node
      = Item of 'a
      | Interior of 'a node C.chunk

    type 'a buffer =
         'a node C.chunk

    type ('a, 'b, 'c, 'd, 'e) dc
         = { fo : 'a, fi : 'b, mid : 'c, bi : 'd, bo : 'e }
               
    datatype 'a persistent
      = Shallow of 'a buffer
      | Deep of measure * 'a deep

    and 'a deep
        =  DC of ('a buffer, 'a buffer, 'a persistent, 'a buffer, 'a buffer) dc
                                                                             
    type 'a transient
         = ('a persistent * TV.t)
                   
    (*************************************************)
    (* Zipper-based iterator *)

    type 'a buffer_context
         = int * 'a buffer

    datatype 'a node_context
      = EmptyNodeContext
      | InteriorNodeContext of 'a buffer_context * 'a node_context
                                                      
    datatype 'a persistent_context
      = EmptyPersistentContext
      | ShallowPersistentContext of 'a buffer_context * 'a persistent_context
      | DeepPersistentContext of 'a deep_context

    and 'a deep_context
      = FrontOuterDeepContext of ('a buffer_context, 'a buffer, 'a persistent, 'a buffer, 'a buffer) dc * 'a deep_context
      | FrontInnerDeepContext of ('a buffer, 'a buffer_context, 'a persistent, 'a buffer, 'a buffer) dc * 'a deep_context
      | MidDeepContext of ('a buffer, 'a buffer, 'a persistent_context, 'a buffer, 'a buffer) dc * 'a deep_context
      | BackInnerDeepContext of ('a buffer, 'a buffer, 'a persistent, 'a buffer_context, 'a buffer) dc * 'a deep_context
      | BackOuterDeepContext of ('a buffer, 'a buffer, 'a persistent, 'a buffer, 'a buffer_context) dc * 'a deep_context

    datatype 'm progress
      = More of 'm
      | Done

    datatype direction
      = Left
      | Right

    fun nextBufferContext ((i, b), d) =
        (case d of
             Left =>
             if i <> 0 then
                 More (i-1, b)
             else
                 Done
          | Right =>
             if i = C.length b then
                 More (i+1, b)
             else
                 Done)

    (* TODO: extend chunk to support nth, which returns nth item in a chunk. find
     * doesn't implement this behavior, because find is based on the weight *) 
(*    fun zoomInLeft md (n, c) =
        (case n of
             Item x =>
             (x, c)
           | Interior b => 
             zoomInLeft (C.nth md (b, 0), InteriorNodeContext (0, b)))
 *)
            
(*             
    fun nextNodeContext md ((n, c), d) =
        (case c of
             EmptyNodeContext =>
             Done
           | InteriorNodeContext (bc, c') => 
             (case d of
                  Left =>
                  raise Fail "todo"
                | Right => 
                  (case nextBufferContext (bc, d) of
                       More bc' =>
                       
                     | Done =>
*)
                                                                                                            
    (* TODO: finish zipper navigation *)

    (*************************************************)
    (* Searching *)
                                                                
    datatype deep_position
      = FrontOuter
      | FrontInner
      | Middle
      | BackInner
      | BackOuter
      | None

    fun chunkEmpty c =
      (C.length c = 0)
                               
    fun chunkFull c =
      (C.length c = C.capacity)

    fun measure cs =
      (case cs of
           Shallow c =>
           C.measure c
         | Deep (mv, _) =>
           mv)
             
    val weightOpt = Measure.weightOpt

    fun chunkWeight c =
        (Option.valOf weightOpt) (C.measure c)
                     
    fun ec md' =
      C.create md'
                     
    fun create md' =
      Shallow (ec md')
                
    fun weight cs =
        (Option.valOf weightOpt) (measure cs)

    val length =
        weight
            
    fun empty cs =
      (length cs = 0)

    val combineMeasures =
        List.foldr A.combine A.identity

    fun searchByMeasureWithWeight (DC {fo, fi, mid, bi, bo}, prefix, pred) =
      let fun f (posn, prefix) =
            (case posn
              of FrontOuter =>
                 let val cur = A.combine (prefix, C.measure fo)
                 in
                     if not (chunkEmpty fo) andalso pred cur then
                         (FrontOuter, prefix)
                     else
                         f (FrontInner, cur)
                 end
               | FrontInner =>
                 let val cur = A.combine (prefix, C.measure fi)
                 in
                     if not (chunkEmpty fi) andalso pred cur then
                         (FrontInner, prefix)
                     else
                         f (FrontOuter, cur)
                 end
               | Middle =>
                 let val cur = A.combine (prefix, measure mid)
                 in
                     if not (empty mid) andalso pred cur then
                         (Middle, prefix)
                     else
                         f (BackInner, cur)
                 end
               | BackInner =>
                 let val cur = A.combine (prefix, C.measure bi)
                 in
                     if not (chunkEmpty bi) andalso pred cur then
                         (BackInner, prefix)
                     else
                         f (BackOuter, cur)
                 end
               | BackOuter =>
                 let val cur = A.combine (prefix, C.measure bo)
                 in
                     if not (chunkEmpty bo) andalso pred cur then
                         (BackOuter, prefix)
                     else
                         f (None, cur)
                 end
               | None =>
                 (None, prefix))
      in
          f (FrontOuter, prefix)
      end

    fun searchByIndex (d, i) =
      let fun pred m =
              (Option.valOf weightOpt) m > i
      in
          searchByMeasureWithWeight (d, A.identity, pred)
      end

    fun searchByMeasure (d, pred) =
      let fun pred' (_, m) =
            pred m
      in
          searchByMeasureWithWeight (d, A.identity, pred)
      end

    fun sub md (cs, i) =
      (case cs
        of Shallow c =>
           C.find md (c, S.Index i)
         | Deep (_, d as DC {fo, fi, mid, bi, bo}) =>
           let val (posn, m) = searchByIndex (d, i)
               val j = (Option.valOf weightOpt) m
               val k = i - j
           in
               case posn of
		   FrontOuter =>
                   C.find md (fo, S.Index k)
                 | FrontInner =>
                   C.find md (fi, S.Index k)
                 | Middle =>
                   sub md (mid, k)
                 | BackInner =>
                   C.find md (bi, S.Index k)
                 | BackOuter =>
                   C.find md (bo, S.Index k)
                 | None =>
                   raise Subscript
           end)
          
    (*************************************************)
    (* Push, Pop, Split, Concat & foldr *)
    
    fun mkDeep (d as DC {fo, fi, mid, bi, bo}) =
      let val mv = combineMeasures [C.measure fo, C.measure fi,
                                    measure mid,
                                    C.measure bi, C.measure bo]
      in
          Deep (mv, d)
      end

    fun pushFront' md' (cs, x) =
	let val ec = ec md'
          val create = create md'
          val pushFront' = pushFront' md'
      in
          case cs of
              Shallow c =>
              if chunkFull c then
                  pushFront' (mkDeep (DC {fo=ec, fi=ec, mid=create, bi=ec, bo=c}), x)
              else
                  Shallow (C.Front.push md' (c, x))
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
                  let val fo' = C.Front.push md' (fo, x)
                  in
                      mkDeep (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo})
                  end
      end

    fun pushBack' md' (cs, x) =
      let val ec = ec md'
          val create = create md'
          val pushBack' = pushBack' md'
      in
          case cs of
              Shallow c =>
              if chunkFull c then
               pushBack' (mkDeep (DC {fo=c, fi=ec, mid=create, bi=ec, bo=ec}), x)
              else
                  Shallow (C.Back.push md' (c, x))
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
                  let val bo' = C.Back.push md' (bo, x)
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
           Item x => x
         | Interior _ => raise Fail "impossible")

    fun forceInterior n =
      (case n of
           Item _ => raise Fail "impossible"
         | Interior c => c)
          
    fun mkDeep' md' d = 
      check md' (mkDeep d)
            
    and check md' cs =
        (case cs of
             Shallow c =>
             raise Fail "impossible"
           | Deep (_, DC {fo, fi, mid, bi, bo})  =>
             let val nbItemsInBufs =
                     List.foldr (fn (c, acc) => C.length c + acc) 0 [fo, fi, bi, bo]
             in
                 if nbItemsInBufs = 0 andalso not (empty mid) then
                     let val (mid', n) = popFront' md' mid
                         val fo' = forceInterior n
                     in
                         mkDeep (DC {fo=fo', fi=fi, mid=mid', bi=bi, bo=bo})
                     end
                 else if nbItemsInBufs = 1 andalso empty mid then
                     mkShallow (fo, fi, bi, bo)
                 else if nbItemsInBufs = 0 andalso empty mid then
                     create md'
                 else
                     cs
             end)

    and popFront' md' cs =
      let val ec = ec md'
          val mkDeep' = mkDeep' md'
          val create = create md'
          val popFront' = popFront' md'
      in
          case cs of
              Shallow c =>
              let val (c', x) = C.Front.pop md' c
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
                      let val (bo', x) = C.Front.pop md' bo
                      in
                          (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}), x)
                      end
              else
                  let val (fo', x) = C.Front.pop md' fo
                  in
                      (mkDeep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}), x)
                  end
      end

    and popBack' md' cs =
      let val ec = ec md'
          val mkDeep' = mkDeep' md'
          val create = create md'
          val popBack' = popBack' md'
      in
          case cs of
              Shallow c =>
              let val (c', x) = C.Back.pop md' c
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
                      let val (fo', x) = C.Back.pop md' fo
                      in
                          (mkDeep' (DC {fo=fo', fi=fi, mid=mid, bi=bi, bo=bo}), x)
                      end
              else
                  let val (bo', x) = C.Back.pop md' bo
                  in
                      (mkDeep' (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo'}), x)
                  end
      end

    and pushBufferFront md' (cs, c) =
        let val ec = ec md'
        in
            if chunkEmpty c then
                cs
            else if empty cs then
                Shallow (C.Front.push md' (ec, Interior c))
            else
                let val (cs', n) = popFront' md' cs
                    val c' = forceInterior n
                in
                    if C.length c + C.length c' <= C.capacity then
                        pushFront' md' (cs', Interior (C.concat md' (c, c')))
                    else
                        pushFront' md' (cs, Interior c)
                end
        end

    and pushBufferBack md' (cs, c) =
        let val ec = ec md'
        in
            if chunkEmpty c then
                cs
            else if empty cs then
                Shallow (C.Back.push md' (ec, Interior c))
            else
                let val (cs', n) = popBack' md' cs
                    val c' = forceInterior n
                in
                    if C.length c + C.length c' <= C.capacity then
                        pushBack' md' (cs', Interior (C.concat md' (c', c)))
                    else
                        pushBack' md' (cs, Interior c)
                end
        end

    and transferContentsFront md' (cs, c) =
        if chunkEmpty c then
            cs
        else
            let val (c', x) = C.Back.pop md' c
            in
                transferContentsFront md' (pushFront' md' (cs, x), c')
            end

    and transferContentsBack md' (cs, c) =
        if chunkEmpty c then
            cs
        else
            let val (c', x) = C.Front.pop md' c
            in
                transferContentsBack md' (pushBack' md' (cs, x), c')
            end
        
    and concat' md' (cs1, cs2) =
        let val pushBufferBack = pushBufferBack md'
            val pushBufferFront = pushBufferFront md'
        in
            if empty cs1 then
                cs2
            else if empty cs2 then
                cs1
            else
                (case (cs1, cs2) of
                     (Shallow c1, _) =>
                     transferContentsFront md' (cs2, c1)
                   | (_, Shallow c2) =>
                     transferContentsBack md' (cs1, c2)
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
                                 let val (mid1''', n1) = popBack' md' mid1''
                                     val (mid2''', n2) = popFront' md' mid2''
                                     val (c1, c2) = (forceInterior n1, forceInterior n2)
                                 in
                                     if C.length c1 + C.length c2 <= C.capacity then
                                         let val c' = C.concat md' (c1, c2)
                                         in
                                             (pushBack' md' (mid1''', Interior c'), mid2''')
                                         end
                                     else
                                         (mid1'', mid2'')
                                 end
                         val mid12 = concat' md' (mid1''', mid2''')
                     in
                         mkDeep' md' (DC {fo=fo1, fi=fi1, mid=mid12, bi=bi2, bo=bo2})
                     end)
        end

    and splitByIndex md' (cs, i) =
        let val ec = ec md'
            val create = create md'
        in
            case cs of
                Shallow c =>
                let val (c1, x, c2) = C.split md' (c, S.Index i)
                in
                    (Shallow c1, x, Shallow c2)
                end
              | Deep (_, DC {fo, fi, mid, bi, bo}) =>
                let val (wfo, wfi) = (chunkWeight fo, chunkWeight fi)
                    val wm = weight mid
                    val (wbi, wbo) = (chunkWeight bi, chunkWeight bo)
                    val (cs1, x, cs2) =
                        if i < wfo then
                            let val (fo1, x, fo2) = C.split md' (fo, S.Index i)
                                val cs1 = mkDeep (DC {fo=fo1, fi=ec, mid=create, bi=ec, bo=ec})
                                val cs2 = mkDeep (DC {fo=fo2, fi=fi, mid=mid, bi=bi, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi then
                            let val j = i - wfo
                                val (fi1, x, fi2) = C.split md' (fi, S.Index j)
                                val cs1 = mkDeep (DC {fo=fo, fi=ec, mid=create, bi=ec, bo=fi1})
                                val cs2 = mkDeep (DC {fo=fi2, fi=ec, mid=mid, bi=bi, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi + wm then
                            let val j = i - wfo - wfi
                                val (mid1, n, mid2) = splitByIndex md' (mid, j)
                                val c = forceInterior n
                                val (c1, x, c2) = C.split md' (c, S.Index (j - weight mid1))
                                val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid1, bi=ec, bo=c1})
                                val cs2 = mkDeep (DC {fo=c2, fi=ec, mid=mid2, bi=bi, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi + wm + wbi then
                            let val j = i - wfo - wfi - wm
                                val (bi1, x, bi2) = C.split md' (bi, S.Index j)
                                val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=ec, bo=bi1})
                                val cs2 = mkDeep (DC {fo=bi2, fi=ec, mid=create, bi=ec, bo=bo})
                            in
                                (cs1, x, cs2)
                            end
                        else if i < wfo + wfi + wm + wbi + wbo then
                            let val j = i - wfo - wfi - wm - wbi
                                val (bo1, x, bo2) = C.split md' (bo, S.Index j)
                                val cs1 = mkDeep (DC {fo=fo, fi=fi, mid=mid, bi=bi, bo=bo1})
                                val cs2 = mkDeep (DC {fo=bo2, fi=ec, mid=create, bi=ec, bo=ec})
                            in
                                (cs1, x, cs2)
                            end
                        else
                            raise Search.Find_by
                in
                    (check md' cs1, x, check md' cs2)
                end
        end

    and splitByPredicate md' (cs, p) =
        raise Fail "todo"
              
    fun split' md' (cs, sb) =
      (case sb
        of Index i =>
           splitByIndex md' (cs, i)
         | Predicate p =>
           let fun p' (_, mv) =
                 p mv
           in
               splitByPredicate md' (cs, p')
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

    fun tabulate' md' (n, f) =
	let val n' = n - 1
      in
          if n = 0 then
              create md'
          else
              pushBack' md' (tabulate' md' (n', f), Item (f n'))
      end

    fun mkMD (MetaData {measure}) =
      C.MetaData {
          measure = (fn nd =>
                        (case nd
                          of Item x =>
                             measure x
                           | Interior c =>
                             C.measure c))
      }

    (*************************************************)
    (* Persistent *)
                 
    structure Persistent = struct

	type 'a t = 'a persistent

	val length =
	    length

	val measure =
	    measure

	fun mkMD' md = (mkMD md, TV.alwaysInvalid)

	fun concat md (cs1, cs2) =
	    concat' (mkMD' md) (cs1, cs2)

	fun find md (cs, sb) =
	  let val md = mkMD md
	  in
	      case sb
	       of Index i =>
		  forceItem (sub md (cs, i))
		| Predicate p =>
		  raise Fail "todo"
	  end

	fun take md (cs, sb) =
	  let val (cs1, _, _) = split' (mkMD' md) (cs, sb)
	  in
	      cs1
	  end

	fun drop md (cs, sb) =
	  let val md' = mkMD' md
	      val isEmpty =
		  (case sb of
		       Index i =>
		       i >= length cs 
		     | _ => raise Fail "todo")
	  in
	      if isEmpty then
		  create md'
	      else
		  let val (_, x, cs2) = split' md' (cs, sb)
		  in 
		      pushFront' md' (cs2, x)
		  end
	  end

	val foldr =
	    foldr'

	fun transient cs =
	  (cs, TV.create ())
            
    end (* Persistent *)

    (*************************************************)
    (* Transient *)
                               
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
	      val tv = TV.create ()
	  in
	      (tabulate' (md, tv) (f, n), tv)
	  end

	structure Front : END_ACCESS = struct

	  datatype metadata = datatype metadata

	  type 'a t = 'a transient

	  fun read (cs, tv) =
	    raise Fail "todo"

	  fun push md ((cs, tv), x) =
	    let val md = mkMD md
	    in
		(pushFront' (md, tv) (cs, Item x), tv)
	    end

	  fun pop md (cs, tv) =
	    let val md = mkMD md
		val (cs', n) = popFront' (md, tv) cs
	    in
		((cs', tv), forceItem n)
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
	    (pushBack' (md, tv) (cs, Item x), tv)
	    end

	  fun pop md (cs, tv) =
	    let val md = mkMD md
		val (cs', n) = popBack' (md, tv) cs
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
	      (concat' (md, tv1) (cs1, cs2), tv1)
	  end

	fun find md ((cs, _), sb) =
	  let val md = mkMD md
	  in
	      case sb
	       of Index i =>
		  forceItem (sub md (cs, i))
		| Predicate p =>
		  raise Fail "todo"
	  end

	fun split md ((cs, tv), sb) =
	  let val md = mkMD md
	      val (cs1, nd, cs2) = split' (md, tv) (cs, sb)
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
