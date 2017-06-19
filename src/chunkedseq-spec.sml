structure ChunkedseqSpec :> CHUNKEDSEQ = struct

    type 'a chunkedseq = 'a list

    val create =
        []

    val size =
        length

    fun empty xs =
      size xs = 0

    fun push_front (xs, x) =
      x :: xs

    fun push_back (xs, x) =
      xs @ [x]
                                                         
    fun pop_front xs =
      (case xs of
           x :: xs' =>
           (xs', x)
         | _  =>
           raise Empty)

    fun pop_back xs =
      (case rev xs of
           x :: sx =>
           (rev sx, x)
         | _ => raise Empty)

    val concat =
        op @

    fun split (xs, i) =
      let val (xs1, xs2) = (List.take (xs, i), List.drop (xs, i))
          val (x, xs2) = (hd xs2, tl xs2)
      in
          (xs1, x, xs2)
      end

    val foldr =
        foldr

end
