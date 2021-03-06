functor StackChunkFn (
    structure Search : SEARCH
    val capacity : int
  ):> CHUNK = struct

    structure Search = Search

    structure Algebra = Search.Measure.Algebra
                           
    type measure =
         Search.measure

    datatype 'a metadata
      = MetaData of {
          measure : 'a Search.Measure.measure_fn,
          trivialItem : 'a,
          itemOverwrite : bool
      }

    type transient_version =
         int

    datatype 'a chunk =
             Chunk of {
                 transientVersion : transient_version,
                 measure : measure ref,
                 tail : int ref,
                 items : 'a Array.array
             }
                                           
    type 'a metadata' =
         ('a metadata * transient_version)

    val capacity =
        capacity

    fun createItems trivialItem =
      Array.array (capacity, trivialItem)

    fun create (MetaData {trivialItem, ...}, tv) =
      Chunk {
          transientVersion = tv,
          measure = ref Algebra.identity,
          tail = ref 0,
          items = createItems trivialItem
      }

    fun length (Chunk {tail, ... }) =
      ! tail

    fun measure (Chunk {measure, ... }) =
      ! measure

    fun foldr f init (Chunk {items, tail, ...}) =
      ArraySlice.foldr f init (ArraySlice.slice (items, 0, SOME (! tail)))
              
    fun updateMeasure (MetaData {measure = measureFn, ...}) (c as Chunk {measure, items, ...}) =
      let fun f (x, acc) =
            Algebra.combine (measureFn x, acc)
      in
          measure := foldr f Algebra.identity c
      end

    fun copyChunk (md' as (md, _), Chunk {items, tail, ...}) =
      let val c' as Chunk {tail = tail', items = items', ...} = create md'
          val t = ! tail
      in
          ArraySlice.copy {src = ArraySlice.slice (items, 0, SOME t), dst = items', di = 0};
          tail' := t;
          updateMeasure md c';
          c'
      end

    structure Front : END_ACCESS = struct

      type 'a metadata = 'a metadata'

      type 'a t = 'a chunk

      fun read (Chunk {items, ...}) =
        Array.sub (items, 0)

      fun push (md' as (md, tv)) (c as Chunk {items, tail, ...}, x) =
        let val c' as Chunk {tail = tail', items = items', ...} = create md'
            val t = ! tail + 1
        in
            ArraySlice.copy {src = ArraySlice.slice (items, 0, SOME t), dst = items', di = 1};
            Array.update (items', 0, x);
            tail' := t;
            updateMeasure md c';
            c'
        end

      fun pop (md' as (md, tv)) (c as Chunk {items, tail, ...}) =
        let val c' as Chunk {tail = tail', items = items', ...} = create md'
            val t = ! tail - 1
            val x = Array.sub (items, 0)
        in
            ArraySlice.copy {src = ArraySlice.slice (items, 1, SOME t), dst = items', di = 0};
            tail' := t;
            updateMeasure md c';
            (c', x)
        end

      fun readn md' c =
        raise Fail "todo"

      fun pushn md' (c, x) =
        raise Fail "todo"
              
    end

    structure Back : END_ACCESS = struct
    
      type 'a metadata = 'a metadata'

      type 'a t = 'a chunk

      fun read (Chunk {items, tail, ...}) =
        Array.sub (items, ! tail - 1)

      fun push (md' as (md as MetaData {measure = measureFn, ...}, tv))
               (c as Chunk {transientVersion, items, tail, measure, ...}, x) =
        if tv = transientVersion then
            let val t = ! tail
            in
                Array.update (items, t, x);
                measure := Algebra.combine (! measure, measureFn x);
                tail := t + 1;
                c
            end
        else
            push md' (copyChunk (md', c), x)


      fun pop (md' as (md as MetaData {measure = measureFn, trivialItem, itemOverwrite}, tv))
              (c as Chunk {transientVersion, measure, tail, items}) =
        if tv = transientVersion then
            let val t' = !tail - 1
                val x = Array.sub (items, t')
            in
                (case Algebra.inverseOpt
                  of NONE =>
                     let fun f (x, acc) =
                           Algebra.combine (measureFn x, acc)
                         val slice = ArraySlice.slice (items, 0, SOME t')
                     in
                         measure := ArraySlice.foldr f Algebra.identity slice
                     end
                   | SOME inverse =>
                     measure := Algebra.combine (! measure, inverse (measureFn x)));
                tail := t';
                (if itemOverwrite then
                     Array.update (items, t', trivialItem)
                 else
                     ());
                (c, x)
            end
        else
            pop md' (copyChunk (md', c))

      fun readn md' c =
        raise Fail "todo"

      fun pushn md' (c, x) =
        raise Fail "todo"

    end

    fun find md _ =
      raise Fail "todo"
                  
    fun concat (md' as (md, tv))
               (c1 as Chunk {items = items1, tail = tail1, ...},
                c2 as Chunk {items = items2, tail = tail2, ...}) =
      let val c' as Chunk {tail = tail', items = items', ...} = create md'
          val t1 = ! tail1
          val t2 = ! tail2
          val t = t1 + t2
      in
          ArraySlice.copy {src = ArraySlice.slice (items1, 0, SOME t1), dst = items', di = 0};
          ArraySlice.copy {src = ArraySlice.slice (items2, 0, SOME t2), dst = items', di = t1};
          tail' := t;
          updateMeasure md c';
          c'
      end

    fun split (md' as (md, tv)) (c as Chunk {items, tail, ...}, sb) =
      (case sb
        of Search.Index i =>
           let val t = ! tail
               val c1 as Chunk {tail = tail1, items = items1, ...} = create md'
               val c2 as Chunk {tail = tail2, items = items2, ...} = create md'
               val x = Array.sub (items, i)
           in
               ArraySlice.copy {src = ArraySlice.slice (items, 0, SOME i), dst = items1, di = 0};
               ArraySlice.copy {src = ArraySlice.slice (items, i + 1, SOME t), dst = items2, di = 0};
               updateMeasure md c1;
               updateMeasure md c2;
               (c1, x, c2)
           end
         | Search.Predicate p =>
           raise Fail "todo")
          
end
