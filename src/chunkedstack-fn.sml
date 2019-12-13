functor ChunkedstackFn (
    structure Chunk : CHUNK
) (* :> CHUNKEDSEQ where Search = Chunk.Search *) = struct

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

    structure Chunkedseq = BootstrappedChunkedseqFn(structure Chunk = C)

    structure CS = Chunkedseq

    (*************************************************)
    (* Datastructure definitions *)

    type 'a chunk
	 = 'a C.chunk

    type ('d, 'c) stack
	 = { deep : 'd, c : 'c }

    datatype ('d, 'c) t
      = Empty
      | Full of ('d, 'c) stack

    type 'a persistent
	 = ('a chunk CS.persistent, 'a chunk) t

    type 'a transient
	 = (('a chunk CS.persistent, 'a chunk) t * TV.t)

    (*************************************************)
    (* Searching *)

    datatype position
      = Deep
      | BackInner
      | BackOuter
      | None

    fun chunkEmpty c =
      (C.length c = 0)
                               
    fun chunkFull c =
      (C.length c = C.capacity)

    val weightOpt = Measure.weightOpt

    val combineMeasures =
        List.foldr A.combine A.identity

    fun measure cs =
	(case cs of
	     Empty =>
	     A.identity
	   | Full { deep, bi, bo } =>
	     combineMeasures [CS.measure deep, C.measure bi, C.measure bo])

    fun weight cs =
        (Option.valOf weightOpt) (measure cs)

    val length =
        weight

    fun empty cs =
	(length cs = 0)

    fun searchByMeasureWithWeight (cs, prefix, pred) =
	(case cs of
	     Empty =>
	     None
	   | Full { deep, bi, bo } =>
	     let fun f (posn, prefix) =
		     (case posn of
			  Deep =>
			  let val cur = A.combine (prefix, CS.measure deep)
			  in
			      if not (empty deep) andalso pred cur then
				  (Deep, prefix)
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
		 f (Deep, prefix)
	     end)

    fun searchByIndex (cs, i) =
      let fun pred m =
              (Option.valOf weightOpt) m > i
      in
          searchByMeasureWithWeight (cs, A.identity, pred)
      end

    fun searchByMeasure (cs, pred) =
      let fun pred' (_, m) =
            pred m
      in
          searchByMeasureWithWeight (cs, A.identity, pred)
      end

    fun subByIndex md (cs, i) =
	(case cs of
	     Empty =>
	     raise Subscript
	   | Full { deep, bi, bo } =>
	     let val (posn, m) = searchByIndex (cs, i)
               val j = (Option.valOf weightOpt) m
               val k = i - j
	     in
		 case posn of
		     Deep =>
		     subByIndex md (deep, k)
		   | BackInner =>
		     C.sub md (bi, S.Index k)
		   | BackOuter =>
		     C.sub md (bo, S.Index k)
		   | None =>
		     raise Subscript
	     end)


    (*************************************************)
    (* Persistent *)

    structure Persistent = struct

        type 'a t = 'a persistent

	val length =
	    weight

	val measure =
            measure

	val empty =
	    empty

    end

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

    end

end
