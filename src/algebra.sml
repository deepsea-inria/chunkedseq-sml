structure UnitAlgebra : ALGEBRA = struct

    type t =
         unit

    val identity =
        ()

    val combine =
        (fn _ => ())

    val inverseOpt =
        SOME (fn _ => ())

end

structure WeightAlgebra : ALGEBRA = struct

    type t =
         int

    val identity =
        0

    val combine =
        (op +)

    val inverseOpt =
        SOME (fn w => ~w)

end

functor CombineAlgebrasFn (
    structure A : ALGEBRA
    structure B : ALGEBRA
) : ALGEBRA = struct

    type t =
         A.t * B.t

    val identity =
        (A.identity, B.identity)
                   
    val combine =
     fn ((xA, xB), (yA, yB)) =>
        (A.combine (xA, yA), B.combine (xB, yB))

    val inverseOpt =
        (case (A.inverseOpt, B.inverseOpt)
          of ( (NONE, _) | (_, NONE)) =>
             NONE
           | (SOME inverseA, SOME inverseB) =>
             SOME (fn (xA, xB) =>
                      (inverseA xA, inverseB xB)))

end
