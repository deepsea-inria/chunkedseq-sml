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
