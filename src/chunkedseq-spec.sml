structure ChunkedseqSpec :> CHUNKEDSEQ = struct

    type 'a chunkedseq = 'a list

    val create = []

    val size = List.length

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

    fun concat (xs1, xs2) =
      xs1 @ xs2

    fun split (xs, i) =
      let fun f (sx, xs, i) =
            (case xs of
                 x :: xs' =>
                 if i = 0 then
                     (rev sx, x, xs')
                 else
                     f (x :: sx, xs', i - 1)
               | _ =>
                 raise Empty)
      in
          f ([], xs, i)
      end

    val foldr = List.foldr

end
