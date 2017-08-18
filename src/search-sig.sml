signature SEARCH = sig

    structure Measure : MEASURE

    type measure =
         Measure.t

    (* The Predicate and Slice search alternatives exist to 
     * guide the search process by inspecting a sequence of
     * measure values. The measures values in this sequence
     * represent the partial sums (inclusive) of the measures 
     * of the inputs (e.g., the values stored in a chunkedseq). 
     * For example:
     *   inputs:       [a, b, c, d]  (can be anything)
     *   measures:     [1, 1, 1, 1]  (assigning unit weights)
     *   partials:     [1, 2, 3, 4]  (partials sums)
     * The function p of (Predicate p) takes as input an 
     * individual value from the array of partials and returns
     * true if the value is a target and false otherwise. The
     * search process returns the index of the first partial
     * for which the predicate returns true.
     * The function sf of (Slice sf) represents a generalization
     * of the predicate method, where the client may specify
     * a small piece (say, inside a given chunk) of the search
     * process, but this time over multiple instead of individual
     * measure values. It takes as input some slice of an array of 
     * partials and returns NONE if the target value is not 
     * found, and (SOME i) if the target value is found at 
     * position i in the slice.
     *)
    datatype find_by
      = Index of int
      | Predicate of (measure -> bool)
(*      | Slice of (measure ArraySlice.slice -> int option) *)

    exception Find_by
                  
end

                       
