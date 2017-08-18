functor SearchFn (
    structure Measure : MEASURE
) : SEARCH = struct

    structure Measure = Measure

    type measure =
         Measure.t

    datatype find_by
      = Index of int
      | Predicate of (measure -> bool)
(*      | Slice of (measure ArraySlice.slice -> int option) *)

    exception Find_by

end
