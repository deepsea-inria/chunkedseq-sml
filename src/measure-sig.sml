signature MEASURE = sig

    type t

    structure Algebra : ALGEBRA where type t = t

    type 'a measure_fn =
         ('a -> t)

    datatype find_by
      = Index of int
      | Predicate of (t -> bool)
      | Slice of (t ArraySlice.slice -> int)

    exception Find_by

end
