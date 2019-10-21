signature MEASURE = sig

    type t

    structure Algebra : ALGEBRA where type t = t

    type 'a measure_fn =
         ('a -> t)

    val weightOpt : (t -> int) option

end
