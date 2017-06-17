structure ListChunk :> CHUNK = struct

    type weight = int

    type 'a weight_fn = 'a -> weight

    type 'a chunk = weight * 'a list

    val k = 2

    val create = (0, [])

    fun size (_, xs) = List.length xs

    fun empty c =
      (size c = 0)

    fun full c =
      (size c = k)

    fun weight (w, _) =
      w

    fun push_front wf ((w, xs), x) =
      (w + wf x, x :: xs)

    fun push_back wf ((w, xs), x) =
      (w + wf x, xs @ [x])

    fun pop_front wf (w, xs) =
      (case xs of
           x :: xs' =>
           ((w - wf x, xs'), x)
         | _ =>
           raise Fail "Chunk.pop_front")

    fun pop_back wf (w, xs) =
      (case rev xs of
           x :: sx' =>
           ((w - wf x, rev sx'), x)
         | _ =>
           raise Fail "Chunk.pop_back")

    fun concat _ ((w1, xs1), (w2, xs2)) =
      (w1 + w2, xs1 @ xs2)

    fun split wf ((_, xs), i) =
      let fun sigma xs =
            let val sum = foldl (fn (x, y) => x + y) 0
            in
                sum (map wf xs)
            end
          fun f (sx, xs, w) =
            (case xs of
                 x :: xs' =>
                 let val w' = w + wf x
                 in
                     if w' > i then
                         (rev sx, x, xs')
                     else
                         f (x :: sx, xs', w')
                 end
               | [] =>
                 raise Fail "Chunk.split")
          val (xs1, x, xs2) = f ([], xs, 0)
          val c1 = (sigma xs1, xs1)
          val c2 = (sigma xs2, xs2)
      in
          (c1, x, c2)
      end

    fun foldr f i (_, xs) =
      List.foldr f i xs
                   
end
