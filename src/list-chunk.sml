structure ListChunk :> CHUNK = struct

    type weight = int

    type 'a weight_fn = 'a -> weight

    type 'a chunk = weight * 'a list

    val k = 2

    val create =
        (0, [])

    fun size (_, xs) =
      length xs

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
            foldl (op +) 0 (map wf xs)
          val (xs1, xs2) = (List.take (xs, i), List.drop (xs, i))
          val (x, xs2) = (hd xs2, tl xs2)
      in
          ((sigma xs1, xs1), x, (sigma xs2, xs2))
      end

    val foldr = fn f => fn i => fn (_, xs) =>
      foldr f i xs
                   
end
