functor MeasureFn (
    structure Algebra : ALGEBRA
) : MEASURE = struct

    structure Algebra = Algebra

    type t =
         Algebra.t

    type 'a measure_fn =
         ('a -> t)

end
