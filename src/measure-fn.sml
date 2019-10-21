functor MeasureFn (
    structure Algebra : ALGEBRA
    val weightOpt : (Algebra.t -> int) option
) : MEASURE = struct

    structure Algebra = Algebra

    type t =
         Algebra.t

    type 'a measure_fn =
         ('a -> t)

    val weightOpt = weightOpt

end
