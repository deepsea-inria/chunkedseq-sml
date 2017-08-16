functor StackChunkFn (
    
    val capacity : int
            
  ):> CHUNK = struct

    type weight =
         SequenceDescriptor.weight

    type 'a algebra = {
        combine : 'a * 'a -> 'a,
        identity : 'a,
        inverseOpt : ('a -> 'a) option
    }
                          
    datatype descr =
        datatype SequenceDescriptor.sequence_descriptor

    type transient_version =
         int
                                                                      
    datatype ('a, 'b) chunk =
             Chunk of {
                 transientVersion : transient_version,
                 weightValue : weight ref,
                 cachedValue : 'b ref,
                 tail : int ref,
                 items : 'a Array.array
             }
                                           
    val capacity =
        capacity

    fun createItems trivialItem =
      Array.array (capacity, trivialItem)

    fun create (SequenceDescriptor {trivialItem, algebra = {identity, ...}, ...}) tv =
      Chunk {
          transientVersion = tv,
          weightValue = ref 0,
          cachedValue = ref identity,
          tail = ref 0,
          items = createItems trivialItem
      }

    fun size (Chunk {tail, ... }) =
      ! tail

    fun weight (Chunk {weightValue, ...}) =
      ! weightValue

    fun cachedValue (Chunk {cachedValue, ...}) =
      ! cachedValue

    fun foldr f init (Chunk {items, tail, ...}) =
      ArraySlice.foldr f init (ArraySlice.slice (items, 0, SOME (! tail)))
              
    fun calculateWeight (SequenceDescriptor {weight, ...}) c =
      foldr (fn (x, w) => weight x + w) 0 c

    fun calculateCachedValue (SequenceDescriptor {measure, algebra = {combine, identity, ...}, ...}) c =
      foldr (fn (x, c) => combine (measure x, c)) identity c

    fun updateCachedValues sd (c as Chunk {weightValue, cachedValue, ...}) =
      (weightValue := calculateWeight sd c;
       cachedValue := calculateCachedValue sd c)

    fun copyChunk (sd as SequenceDescriptor {trivialItem, algebra={identity, ...}, ...}, Chunk {items, tail, ...}, tv) =
      let val c' as Chunk {tail = tail', items = items', ...} = create sd tv
          val t = ! tail
      in
          ArraySlice.copy {src = ArraySlice.slice (items, 0, SOME t), dst = items', di = 0};
          tail' := t;
          updateCachedValues sd c';
          c'
      end

    fun pushFront sd tv (c as Chunk {items, tail, ...}, x) =
      let val c' as Chunk {tail = tail', items = items', ...} = create sd tv
          val t = ! tail + 1
      in
          ArraySlice.copy {src = ArraySlice.slice (items, 0, SOME t), dst = items', di = 1};
          Array.update (items', 0, x);
          tail' := t;
          updateCachedValues sd c';
          c'
      end

    fun pushBack (sd as SequenceDescriptor {weight, measure, algebra = {combine, ...}, ...}) tv
                 (c as Chunk {transientVersion, items, tail, weightValue, cachedValue, ...}, x) =
      if tv = transientVersion then
          let val t = ! tail
          in
              Array.update (items, t, x);
              weightValue := ! weightValue + weight x;
              cachedValue := combine (! cachedValue, measure x);
              tail := t + 1;
              c
          end
      else
          pushBack sd tv (copyChunk (sd, c, tv), x)

    fun popFront sd tv (c as Chunk {items, tail, ...}) =
      let val c' as Chunk {tail = tail', items = items', ...} = create sd tv
          val t = ! tail - 1
          val x = Array.sub (items, 0)
      in
          ArraySlice.copy {src = ArraySlice.slice (items, 1, SOME t), dst = items', di = 0};
          tail' := t;
          updateCachedValues sd c';
          (c', x)
      end

    fun popBack (sd as SequenceDescriptor {measure, weight, algebra = {identity, combine, inverseOpt, ...}, trivialItem, itemOverwrite})
                tv
                (c as Chunk {transientVersion, items, weightValue, cachedValue, tail, ...}) =
      if tv = transientVersion then
          let val t' = !tail - 1
              val x = Array.sub (items, t')
          in
              weightValue := ! weightValue - weight x;
              (case inverseOpt
                of NONE =>
                   cachedValue := ArraySlice.foldr (fn (x, c) => combine (measure x, c)) identity (ArraySlice.slice (items, 0, SOME t'))
                 | SOME inverse =>
                   cachedValue := combine (! cachedValue, inverse (measure x)));
              tail := t';
              (if itemOverwrite then
                   Array.update (items, t', trivialItem)
               else
                   ());
              (c, x)
          end
      else
          popBack sd tv (copyChunk (sd, c, tv))

    fun concat sd tv
               (c1 as Chunk {items = items1, tail = tail1, ...},
                c2 as Chunk {items = items2, tail = tail2, ...}) =
      let val c' as Chunk {tail = tail', items = items', ...} = create sd tv
          val t1 = ! tail1
          val t2 = ! tail2
          val t = t1 + t2
      in
          ArraySlice.copy {src = ArraySlice.slice (items1, 0, SOME t1), dst = items', di = 0};
          ArraySlice.copy {src = ArraySlice.slice (items2, 0, SOME t2), dst = items', di = t1};
          tail' := t;
          updateCachedValues sd c';
          c'
      end

    fun split sd tv (c as Chunk {items, tail, ...}, i) =
      let val t = ! tail
          val c1 as Chunk {tail = tail1, items = items1, ...} = create sd tv
          val c2 as Chunk {tail = tail2, items = items2, ...} = create sd tv
          val x = Array.sub (items, i)
      in
          ArraySlice.copy {src = ArraySlice.slice (items, 0, SOME i), dst = items1, di = 0};
          ArraySlice.copy {src = ArraySlice.slice (items, i + 1, SOME t), dst = items2, di = 0};
          updateCachedValues sd c1;
          updateCachedValues sd c2;
          (c1, x, c2)
      end
          
end
