signature MEASURE = sig

    type t

    structure Algebra : ALGEBRA where type t = t

    type 'a measure_fn =
         ('a -> t)

    (* The Predicate and Slice search alternatives are to 
     * assist a search process by inspecting the partial 
     * sums for their target. The partial sums represent 
     * the (inclusive) of the measures of the inputs. 
     * For example:
     *   inputs:       [a, b, c, d]  (can be anything)
     *   measures:     [1, 1, 1, 1]  (unit weight for items)
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
     * items. It takes as input some slice of an array of 
     * partials and returns NONE if the target value is not 
     * found, and (SOME i) if the target value is found at 
     * position i in the slice.
     *)
    datatype find_by
      = Index of int
      | Predicate of (t -> bool)
      | Slice of (t ArraySlice.slice -> int option)

    exception Find_by

end
