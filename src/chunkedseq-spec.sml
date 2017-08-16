structure ChunkedseqSpec :> CHUNKEDSEQ = struct

    type weight =
         SequenceDescriptor.weight

    datatype descr =
        datatype SequenceDescriptor.sequence_descriptor

    type ('a, 'b) persistent =
         'a List.list
            
    type ('a, 'b) transient =
         ('a, 'b) persistent

    fun sum items =
      List.foldr (op +) 0

    fun weight (w, _, items) =
      w

    fun cachedValue (_, cv, _) =
      cv

    fun foldr f init items =
      List.foldr f init items

    fun calculateWeight (SequenceDescriptor {weight, ...}) cs =
      foldr (fn (x, w) => weight x + w) 0 cs

    fun calculateCachedValue (SequenceDescriptor {measure, algebra = {combine, identity, ...}, ...}) cs =
      foldr (fn (x, c) => combine (measure x, c)) identity cs

    fun weight _ =
      raise Fail "impossible"

    fun cachedValue _ =
      raise Fail "impossible"

    fun concat _ (items1, items2) =
      items1 @ items2

    fun take _ (items, i) =
      List.take (items, i)

    fun drop _ (items, i) =
      List.drop (items, i)

    val foldr =
        List.foldr

    structure Transient = struct

      type ('a, 'b) t = ('a, 'b) transient
                                 
      val weight =
          weight

      val cachedValue =
          cachedValue

      fun tabulate _ =
        List.tabulate

      fun pushFront _ (items, x) =
        x :: items

      fun pushBack _ (items, x) =
        items @ [x]

      fun popFront _ items =
        (List.tl items, List.hd items)

      fun popBack _ items =
        let val smeti = List.rev items
        in
            (List.rev (List.tl smeti), List.hd smeti)
        end

      val concat =
          concat

      val take =
          take

      val drop =
          drop

      val foldr =
          foldr
              
      fun persistent cs =
        cs
                               
    end
                              
    structure Persistent = struct

      type ('a, 'b) t = ('a, 'b) persistent

      val weight =
          weight

      val cachedValue =
          cachedValue
                            
      val concat =
          concat

      val take =
          take

      val drop =
          drop

      val foldr =
          foldr

      fun transient cs =
        cs

    end


end
