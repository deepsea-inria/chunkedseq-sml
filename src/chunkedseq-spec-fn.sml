functor ChunkedseqSpecFn (
    structure Search : SEARCH
) :> CHUNKEDSEQ where Search = Search = struct

    structure Search = Search

    structure Measure = Search.Measure

    structure Algebra = Measure.Algebra
                            
    type measure = Search.measure
                       
    datatype find_by = datatype Search.find_by
                                    
    datatype 'a metadata
      = MetaData of {
          measure : 'a Measure.measure_fn
      }
 
    type 'a persistent =
         (measure * 'a List.list)
            
    type 'a transient =
         'a persistent
                  
    fun length (_, items) =
      List.length items

    val measure' =
     fn (MetaData {measure, ...}) =>
        measure

    val identity =
        Algebra.identity

    val combine =
        Algebra.combine

    val inverseOpt =
        Algebra.inverseOpt
            
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
                        raise Search.Find_by
                    else if p (ArraySlice.sub (prefixes, i)) then
                        i
                    else
                        f (i + 1)
              in
                  f 0
              end
(*            | Slice sf =>
              (case sf (prefixes md items) (fn x => x)
                of NONE => raise Search.Find_by
                 | SOME i => i) *)
      end
                 
    fun sub _ ((_, cs), sb) =
      (case sb
        of Index i =>
           List.nth (cs, i)
         | _ =>
           raise Fail "todo")
          
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

      structure Front : END_ACCESS = struct

        datatype metadata = datatype metadata

        type 'a t = 'a transient

        fun read (cs, tv) =
          raise Fail "todo"

        fun push md ((c, items), x) =
          let val items' = x :: items
          in
              (combine (measure' md x, c), items')
          end
              
        fun pop md (c, items) =
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

        fun readn md {src, dst, di} =
          raise Fail "todo"
                
        fun pushn md (cs, slice) =
          raise Fail "todo"

      end

      structure Back : END_ACCESS = struct
      
        datatype metadata = datatype metadata

        type 'a t = 'a transient

        fun read (cs, tv) =
          raise Fail "todo"

        fun push md ((c, items), x) =
          let val items' = items @ [x]
          in
              (combine (measure' md x, c), items')
          end
              
        fun pop md (c, items) =
          let val (x, items') =
                  let val smeti = List.rev items
                  in
                      (List.hd smeti, List.rev (List.tl smeti))
                  end
              val c' =
                  (case inverseOpt
                    of NONE =>
                       calculateMeasure md items'
                     | SOME inverse =>
                       combine (c, inverse (measure' md x)))
          in
              ((c', items'), x)
          end
              
        fun readn md {src, dst, di} =
          raise Fail "todo"

        fun pushn md (cs, slice) =
          raise Fail "todo"
                
      end
      
      val concat =
          concat

      val take =
          take

      val sub =
          sub

      fun split md (cs, fb) =
        let val cs1 = take md (cs, fb)
            val (x, cs2) =
                let val (c2, items2) = drop md (cs, fb)
                    val x = List.hd items2
                    val items2' = List.tl items2
                in
                    (x, createWith md (List.tl items2))
                end
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

      val sub =
          sub
              
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
