functor ListChunkFn (
    
    val capacity : int
            
  ):> CHUNK = struct

    type weight = int

    type transient_version = int

    type 'a algebra = {
        combine : 'a * 'a -> 'a,
        identity : 'a,
        inverseOpt : ('a -> 'a) option
    }
                          
    datatype ('a, 'b) sequence_descriptor =
             SequenceDescriptor of {
                 weight  : 'a -> weight,
                 measure : 'a -> 'b,
                 algebra : 'b algebra,
                 trivialItem : 'a,
                 itemOverwrite : bool
             }
                                                                      
    datatype ('a, 'b) chunk =
             Chunk of {
                 weightValue : weight,
                 cachedValue : 'b,
                 items : 'a List.list
             }

    val capacity = capacity

    fun createWith (SequenceDescriptor {algebra = {identity, ...}, ...}) items =
      Chunk {
          weightValue = 0,
          cachedValue = identity,
          items = items
      }

    fun create sd _ =
      createWith sd []

    fun size (Chunk {items, ...}) =
      List.length items

    fun empty c =
      (size c = 0)

    fun full c =
      (size c = capacity)

    fun weight (Chunk {weightValue, ...}) =
      weightValue

    fun cachedValue (Chunk {cachedValue, ...}) =
      cachedValue

    fun foldr f init (Chunk {items, ...}) =
      List.foldr f init items

    fun calculateWeight (SequenceDescriptor {weight, ...}) c =
      foldr (fn (x, w) => weight x + w) 0 c

    fun calculateCachedValue (SequenceDescriptor {measure, algebra = {combine, identity, ...}, ...}) c =
      foldr (fn (x, c) => combine (measure x, c)) identity c

    fun refreshChunk sd (c as Chunk {items, ...}) =
        Chunk {
            weightValue = calculateWeight sd c,
            cachedValue = calculateCachedValue sd c,
            items = items
        }

    val createWithAndRefreshChunk =
        fn sd =>
           refreshChunk sd o createWith sd

    fun pushFront sd _ (c as Chunk {items, ...}, x) =
      createWithAndRefreshChunk sd (x :: items)

    fun pushBack sd _ (c as Chunk {weightValue, cachedValue, items}, x) =
      createWithAndRefreshChunk sd (items @ [x])
                  
    fun popFront sd _ (c as Chunk {weightValue, cachedValue, items}) =
      (createWithAndRefreshChunk sd (List.tl items), List.hd items)

    fun popBack sd _ (c as Chunk {weightValue, cachedValue, items}) =
      let val smeti = List.rev items
          val items' = List.rev (List.tl smeti)
          val x = List.hd smeti
      in
          (createWithAndRefreshChunk sd items', x)
      end

    fun concat sd _ (Chunk {items = items1, ...}, Chunk {items = items2, ...}) =
      createWithAndRefreshChunk sd (items1 @ items2)

    fun split sd _ (c as Chunk {items, ...}, i) =
      let val items1 = List.take (items, i)
          val items2 = List.drop (items, i)
          val (x, items2) = (List.hd items2, List.tl items2)
      in
          (createWithAndRefreshChunk sd items1, x, createWithAndRefreshChunk sd items2)
      end
          
end
