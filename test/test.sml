structure Test = struct

structure CS = ChunkedseqFn(ListChunk)
structure S = ChunkedseqSpec

fun list_of cs =
  CS.foldr (fn (x, y) => x :: y) [] cs

type item = int

datatype orientation = End_front | End_back

datatype trace
  = Trace_nil
  | Trace_push of orientation * item * trace
  | Trace_pop of orientation * trace
  | Trace_split_concat of int * trace * trace

val r = Random.rand (1,1)
                    
fun random_item () =
  Random.randRange (1, 2048) r

fun random_orientation () =
  if Random.randRange (0, 1) r = 0 then
      End_front
  else
      End_back

fun gen_trace (n, d) =
  if n = 0 then
      Trace_nil
  else if n >= 1 andalso Random.randRange (0, d + 3) r = 0 then
      let val i = Random.randRange (0, n) r
          val t1 = gen_trace (i, d + 1)
          val t2 = gen_trace (n - i - 1, d + 1)
      in
          Trace_split_concat (i, t1, t2)
      end
  else if (Random.randRange (0, 2 + Word.toInt (Word.<< (0wx1, Word.fromInt n))) r) < 3 then
      let val e = random_orientation ()
          val x = random_item ()
          val t = gen_trace (n + 1, d)
      in
          Trace_push (e, x, t)
      end
  else
      let val e = random_orientation ()
          val t = gen_trace (n - 1, d)
      in
          Trace_pop (e, t)
      end

fun string_of_orientation e =
  (case e of
       End_front => "F"
     | End_back => "B")

fun print_trace (t : trace) : unit =
  let fun pt (t, p, s) = (
        if not (t = Trace_nil) then 
            print (p ^ (if s then  "└── " else "├── "))
        else
            ();
        (case t of
             Trace_nil =>
             ()
           | Trace_push (e, x, t) => (
               print ("+" ^ (string_of_orientation e) ^ Int.toString x ^ "\n");
               pt (t, p ^ (if s then  "    " else "│   "), true))
           | Trace_pop (e, t) => (
               print ("-" ^ (string_of_orientation e) ^ "[]\n");
               pt (t, p ^ (if s then  "    " else "│   "), true))
           | Trace_split_concat (i, t1, t2) => (
               print ("%" ^ Int.toString i ^ "\n");
               pt (t1, p ^ (if s then "    " else "│   "), false);
               pt (t2, p ^ (if s then "    " else "│   "), true))))
  in
      pt (t, "", true)
  end

fun print_list (sep, print_item, xs) =
  let fun pl (sep, print_item, xs) =
        (case xs of
             [] =>
             ()
           | [x] =>
             print_item x
           | x :: xs => (
               print_item x;
               print sep;
               pl (sep, print_item, xs)))
  in
      print "[";
      pl (sep, print_item, xs);
      print "]"
  end

datatype 'a list_compare_res
  = Lists_equal
  | Lists_item_mismatch of int * 'a * 'a (* position of mismatch, 
                                          * and positions in first and second lists, 
                                          * respectively *)
  | Lists_unequal_lengths of int * int   (* length of first list, length of second *)

fun compare_lists (xs, ys) =
  let val (n1, n2) = (length xs, length ys)
  in
      if n1 <> n2 then
          Lists_unequal_lengths (n1, n2)
      else
          let fun f (i, xs : item list, ys : item list) =
                (case (xs, ys) of
                     ([], []) =>
                     Lists_equal
                   | (x :: xs, y :: ys) =>
                     if x <> y then
                         Lists_item_mismatch (i, x, y)
                     else
                         f (i + 1, xs, ys)
                   | _ =>
                     raise Fail "impossible")
          in
              f (0, xs, ys)
          end
  end

fun print_chunkedseq cs = 
  print_list (",", print o Int.toString, list_of cs)

fun check t0 =
  let fun ok' (r, s) =
        (case compare_lists (r, s) of
             Lists_equal =>
             ()
           | Lists_item_mismatch (i, x, y) =>
             let val s = "item mismatch at " ^ Int.toString i ^ " with x=" ^ Int.toString x ^ " and y=" ^ Int.toString y ^ "\n"
             in
                 raise Fail s
             end
           | Lists_unequal_lengths (nr, ns) =>
             let val s = "unequal lengths |r|=" ^ Int.toString nr ^ " and |s|=" ^ Int.toString ns ^ "\n"
             in
                 raise Fail s
             end)
      fun ok (r, s) =
        ok' (S.foldr (fn (x, y) => x :: y) [] r, list_of s)
      fun chk (t, r, s) = (
          ok (r, s);
          (case t of
               Trace_nil =>
               (r, s)
             | Trace_push (End_front, x, t') =>
               let val r' = S.push_front (r, x)
                   val s' = CS.push_front (s, x)
               in
                   chk (t', r', s')
               end
             | Trace_push (End_back, x, t') =>
               let val r' = S.push_back (r, x)
                   val s' = CS.push_back (s, x)
               in
                   chk (t', r', s')
               end
             | Trace_pop (End_front, t') =>
               let val (r', _) = S.pop_front r
                   val (s', _) = CS.pop_front s
               in
                   chk (t', r', s')
               end
             | Trace_pop (End_back, t') =>
               let val (r', _) = S.pop_back r
                   val (s', _) = CS.pop_back s
               in
                   chk (t', r', s')
               end
             | Trace_split_concat (i, t1, t2) =>
               let val (r1, x, r2) = S.split (r, i)
                   val (s1, y, s2) = CS.split (s, i)
                   val _ = ok (r1, s1)
                   val _ = ok (r2, s2)
                   val _ = ok' ([x], [y])
                   val (r1', s1') = chk (t1, r1, s1)
                   val (r2', s2') = chk (t2, r2, s2)
                   val _ = ok (r1', s1')
                   val _ = ok (r2', s2')
                   val r' = S.concat (r1', r2')
                   val s' = CS.concat (s1', s2')
                   val _ = ok (r', s')
               in
                   (r', s')
               end))
  in
      chk (t0, S.create, CS.create)
  end

fun check_loop n =
  if n <= 0 then
      ()
  else
      let val t0 = Trace_push (random_orientation (), random_item (), gen_trace (1, 1))
      in
          check t0;
          check_loop (n - 1)
      end
           
end
