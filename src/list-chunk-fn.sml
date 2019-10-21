functor ListChunkFn (
    structure Search : SEARCH
    val capacity : int
  ):> CHUNK where Search = Search = struct

    structure Search = Search

    structure Algebra = Search.Measure.Algebra
                           
    type measure =
         Search.measure

    datatype 'a metadata
      = MetaData of {
          measure : 'a Search.Measure.measure_fn
      }
             
    datatype 'a chunk =
             Chunk of {
                 measure : measure,
                 items : 'a List.list
             }

    type transient_version =
         int

    type 'a metadata' =
         ('a metadata * transient_version)

    val capacity =
        capacity

    fun createWith items =
      Chunk {
          measure = Algebra.identity,
          items = items
      }

    fun create _ =
      createWith []

    fun length (Chunk {items, ...}) =
      List.length items

    fun measure (Chunk {measure, ...}) =
      measure

    fun foldr f init (Chunk {items, ...}) =
      List.foldr f init items

    fun refreshChunk (MetaData {measure, ...}) (c as Chunk {items, ...}) =
        Chunk {
            measure = let fun f (x, acc) =
                            Algebra.combine (measure x, acc)
                      in
                          List.foldr f Algebra.identity items
                      end,
            items = items
        }

    fun createWithAndRefreshChunk md items =
      refreshChunk md (createWith items)

    structure Front : END_ACCESS = struct

      type 'a metadata = 'a metadata'

      type 'a t = 'a chunk

      fun read (Chunk {items, ...}) =
        List.hd items handle _ => raise Fail "ListChunkFn.read"

      fun push (md, _) (c as Chunk {items, ...}, x) =
        createWithAndRefreshChunk md (x :: items)

      fun pop (md, _) (c as Chunk {items, ...}) =
        (createWithAndRefreshChunk md (List.tl items), List.hd items)
        handle _ => raise Fail "ListChunkFn.pop"

      fun readn md' c =
        raise Fail "todo"

      fun pushn md' (c, x) =
        raise Fail "todo"

    end

    structure Back : END_ACCESS = struct
    
      type 'a metadata = 'a metadata'

      type 'a t = 'a chunk

      fun read (Chunk {items, ...}) =
        List.nth (items, List.length items - 1)
        handle _ => raise Fail "ListChunkFn.read"

      fun push (md, _) (c as Chunk {items, ...}, x) =
        createWithAndRefreshChunk md (items @ [x])

      fun pop (md, _) (c as Chunk {items, ...}) =
        let val (items', x) =
                let val smeti = List.rev items
                in
                    (List.rev (List.tl smeti), List.hd smeti)
                    handle _ => raise Fail "ListChunkFn.pop"
                end                           
        in
            (createWithAndRefreshChunk md items', x)
        end
            
      fun readn md' c =
        raise Fail "todo"

      fun pushn md' (c, x) =
        raise Fail "todo"

    end

    fun searchByMeasure (MetaData {measure}) (items, prefix, pred) =
        let fun f (xs, sx, prefix) =
                (case xs of
                     [] =>
                     (List.rev sx, xs, prefix)
                   | x :: xs =>
                     let val cur = Algebra.combine (prefix, measure x)
                     in
                         if pred cur then
                             (List.rev sx, x :: xs, prefix)
                         else
                             f (xs, x :: sx, Algebra.combine (measure x, prefix))
                     end)
        in
            f (items, [], Algebra.identity)
        end

    fun searchByIndex md (items, i) =
        let fun pred m =
                (Option.valOf Search.Measure.weightOpt) m > i
        in
            searchByMeasure md (items, Algebra.identity, pred)
        end

    fun sub md (Chunk {items, ...}, sb) =
        (case sb of
             Search.Index i =>
             let val (items1, items2, _) = searchByIndex md (items, i)
             in
                 List.hd items2
             end
          | _ => raise Fail "todo")
        handle _ => raise Fail "ListChunkFn.sub"

    fun concat (md, _) (Chunk {items = items1, ...}, Chunk {items = items2, ...}) =
      createWithAndRefreshChunk md (items1 @ items2)


    fun split (md, _) (c as Chunk {items, ...}, sb) =
      (case sb
        of Search.Index i =>
           let val (items1, items2, _) = searchByIndex md (items, i)
               val (x, items2) = (List.hd items2, List.tl items2)
           in
               (createWithAndRefreshChunk md items1, x, createWithAndRefreshChunk md items2)
           end
         | Search.Predicate p =>
           raise Fail "todo")
      handle _ => raise Fail "ListChunkFn.split"
          
end
