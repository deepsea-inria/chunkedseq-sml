structure Measure = struct

    type ('a, 'b) t = {
        measure : 'a -> 'b,
        algebra : 'b Algebra.t
    }

    datatype 'a find_by
      = Index of int
      | Predicate of ('a -> bool)
      | Slice of ('a ArraySlice.slice -> int)

    exception Find_by
                          
    val product : (('a, 'b) t * ('a, 'g) t) -> ('a, ('b * 'g)) t =
     fn ({measure = measureB, algebra = algebraB},
         {measure = measureC, algebra = algebraC}) =>
        let val measure = (fn x => (measureB x, measureC x))
            val algebra = Algebra.product (algebraB, algebraC)
        in
            {measure = measure, algebra = algebra}
        end

end

structure UnitMeasure = struct

    val v : ('a, unit) Measure.t = {
        measure = fn _ => (),
        algebra = UnitAlgebra.v
    }

end

structure WeightMeasure = struct

    type weight =
         WeightAlgebra.weight

    val v : ('a -> weight) -> ('a, weight) Measure.t =
     fn measure => {
         measure = measure,
         algebra = WeightAlgebra.v
     }

end
