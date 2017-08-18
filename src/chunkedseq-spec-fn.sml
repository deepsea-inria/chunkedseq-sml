functor ChunkedseqSpecFn (
    structure Measure : MEASURE
) :> CHUNKEDSEQ = struct

    structure Measure = Measure
                        
    datatype 'a metadata
      = MetaData of {
          measure : 'a Measure.measure_fn,
          trivialItem : 'a,
          itemOverwrite : bool
      }
 
    type 'a persistent =
         (Measure.t * 'a List.list)
            
    type 'a transient =
         'a persistent

    datatype find_by = datatype Measure.find_by
                  
    fun length (_, items) =
      List.length items

    val measure' =
     fn (MetaData {measure, ...}) =>
        measure

    val identity =
        Measure.Algebra.identity

    val combine =
        Measure.Algebra.combine

    val inverseOpt =
        Measure.Algebra.inverseOpt
            
    val calculateMeasure =
     fn md =>
        let val measure = measure' md
            fun f (x, acc) =
              combine (measure x, acc)
        in
            List.foldr f identity
        end

    fun createWith md items =
      (calculateMeasure md items, items)
            
    fun concat md ((c1, xs1), (c2, xs2)) =
      createWith md (xs1 @ xs2)

    fun findBy md (items, fb) =
      let fun prefixes md items =
            let val n = List.length items
                val ps = Array.array (n, identity)
                fun f (items, p, i) =
                  (case items
                    of [] =>
                       ()
                     | x :: items' =>
                       let val p' = combine (measure' md x, p)
                       in
                           Array.update (ps, i, p');
                           f (items', p', i + 1)
                       end)
            in
                f (items, identity, 0);
                ArraySlice.full ps
            end
      in
          case fb
           of Index i =>
              i
            | Predicate p =>
              let val prefixes = prefixes md items
                  fun f i =
                    if i >= ArraySlice.length prefixes then
                        raise Measure.Find_by
                    else if p (ArraySlice.sub (prefixes, i)) then
                        i
                    else
                        f (i + 1)
              in
                  f 0
              end
            | Slice sf =>
              (case sf (prefixes md items)
                of NONE => raise Measure.Find_by
                 | SOME i => i)
      end
                 
    fun take md ((_, items), fb) =
      createWith md (List.take (items, findBy md (items, fb)))
                 
    fun drop md ((_, items), fb) =
      createWith md (List.drop (items, findBy md (items, fb)))
                 
    fun foldr f init (_, items) =
      List.foldr f init items

    structure Transient = struct

      type 'a t = 'a transient
                                 
      val length =
          length

      val measure =
       fn (c, _) => c
              
      fun tabulate md (n, f) =
        createWith md (List.tabulate (n, f))

      fun pushFront md ((c, items), x) =
        let val items' = x :: items
        in
            (combine (measure' md x, c), items')
        end

      fun pushBack md ((c, items), x) =
        let val items' = items @ [x]
        in
            (combine (measure' md x, c), items')
        end

      fun popFront md (c, items) =
        let val items' = List.tl items
            val x = List.hd items
            val c' =
                (case inverseOpt
                  of NONE =>
                     calculateMeasure md items'
                   | SOME inverse =>
                     combine (c, inverse (measure' md x)))
        in
            ((c', items'), x)
        end

      fun popBack md (c, items) =
        let val smeti = List.rev items
            val items' = List.rev (List.tl smeti)
            val x = List.hd smeti
            val c' =
                (case inverseOpt
                  of NONE =>
                     calculateMeasure md items'
                   | SOME inverse =>
                     combine (c, inverse (measure' md x)))
        in
            ((c', items'), x)
        end

      val concat =
          concat

      val take =
          take

      fun split md (cs, fb) =
        let val cs1 = take md (cs, fb)
            val (c2, items2) = drop md (cs, fb)
            val x = List.hd items2
            val items2' = List.tl items2
            val cs2 = createWith md (List.tl items2)
        in
            (cs1, x, cs2)
        end
              
      val foldr =
          foldr

      fun persistent cs =
        cs
                               
    end
                              
    structure Persistent = struct

      type 'a t = 'a persistent

      val length =
          length

      val measure =
       fn (c, _) => c
                            
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
