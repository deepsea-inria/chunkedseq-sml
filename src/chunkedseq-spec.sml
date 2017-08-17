structure ChunkedseqSpec :> CHUNKEDSEQ = struct

    datatype ('a, 'b) metadata
      = MetaData of {
          measure : ('a, 'b) Measure.t,
          trivialItem : 'a,
          itemOverwrite : bool
      }

    type ('a, 'b) persistent =
         ('b * 'a List.list)
            
    type ('a, 'b) transient =
         ('a, 'b) persistent

    datatype find_by = datatype Measure.find_by
                  
    fun length (_, items) =
      List.length items

    val measure' =
     fn (MetaData {measure = {measure, ...}, ...}) =>
        measure

    val identity =
     fn (MetaData {measure = {algebra = {identity, ...}, ...}, ...}) =>
        identity

    val combine =
     fn (MetaData {measure = {algebra = {combine, ...}, ...}, ...}) =>
        combine

    val inverseOpt =
     fn (MetaData {measure = {algebra = {inverseOpt, ...}, ...}, ...}) =>
        inverseOpt
            
    val calculateMeasure =
     fn md =>
        let val init = identity md
            val measure = measure' md
            val combine = combine md
            fun f (x, acc) =
              combine (measure x, acc)
        in
            List.foldr f init
        end

    fun createWith md items =
      (calculateMeasure md items, items)
            
    fun concat md ((c1, xs1), (c2, xs2)) =
      createWith md (xs1 @ xs2)

    fun findBy md (items, fb) =
      let fun prefixes md items =
            let val n = List.length items
                val ps = Array.array (n, identity md)
                fun f (items, p, i) =
                  (case items
                    of [] =>
                       ()
                     | x :: items' =>
                       let val p' = combine md (measure' md x, p)
                       in
                           Array.update (ps, i, p');
                           f (items', p', i + 1)
                       end)
            in
                f (items, identity md, 0);
                ArraySlice.full ps
            end
          fun find (prefixes, p) =
            let fun f i =
                  if i >= ArraySlice.length prefixes then
                      raise Measure.Find_by
                  else if p (ArraySlice.sub (prefixes, i)) then
                      i
                  else
                      f (i + 1)
            in
                f 0
            end
      in
          case fb
           of Index i =>
              i
            | Predicate p =>
              find (prefixes md items, p)
            | Slice sf =>
              sf (prefixes md items)
      end
                 
    fun take md ((_, items), fb) =
      createWith md (List.take (items, findBy md (items, fb)))
                 
    fun drop md ((_, items), fb) =
      createWith md (List.drop (items, findBy md (items, fb)))
                 
    fun foldr f init (_, items) =
      List.foldr f init items

    structure Transient = struct

      type ('a, 'b) t = ('a, 'b) transient
                                 
      val length =
          length

      val measure =
       fn (c, _) => c
              
      fun tabulate md (n, f) =
        createWith md (List.tabulate (n, f))

      fun pushFront md ((c, items), x) =
        let val items' = x :: items
        in
            (combine md (measure' md x, c), items')
        end

      fun pushBack md ((c, items), x) =
        let val items' = items @ [x]
        in
            (combine md (measure' md x, c), items')
        end

      fun popFront md (c, items) =
        let val items' = List.tl items
            val x = List.hd items
            val c' =
                (case inverseOpt md
                  of NONE =>
                     calculateMeasure md items'
                   | SOME inverse =>
                     combine md (c, inverse (measure' md x)))
        in
            ((c', items'), x)
        end

      fun popBack md (c, items) =
        let val smeti = List.rev items
            val items' = List.rev (List.tl smeti)
            val x = List.hd smeti
            val c' =
                (case inverseOpt md
                  of NONE =>
                     calculateMeasure md items'
                   | SOME inverse =>
                     combine md (c, inverse (measure' md x)))
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

      type ('a, 'b) t = ('a, 'b) persistent

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
