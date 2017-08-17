structure Algebra = struct

    type 'a t = {
        identity : 'a,
        combine : 'a * 'a -> 'a,
        inverseOpt : ('a -> 'a) option
    }

    val product : ('a t * 'b t) -> ('a * 'b) t =
     fn ({identity = identityA, combine = combineA, inverseOpt = inverseOptA},
         {identity = identityB, combine = combineB, inverseOpt = inverseOptB}) =>
        let val identity = (identityA, identityB)
            fun combine ((xA, xB), (yA, yB)) =
              (combineA (xA, yA), combineB (xB, yB))
            val inverseOpt =
                (case (inverseOptA, inverseOptB)
                  of ( (NONE, _) | (_, NONE)) =>
                     NONE
                   | (SOME inverseA, SOME inverseB) =>
                     SOME (fn (xA, xB) =>
                              (inverseA xA, inverseB xB)))
        in
            {identity = identity, combine = combine, inverseOpt = inverseOpt} 
        end

end

structure UnitAlgebra = struct

    val v : unit Algebra.t = {
        combine = fn (_, _) => (),
        identity = (),
        inverseOpt = SOME (fn _ => ())
    }

end

structure WeightAlgebra = struct

    type weight =
         int

    val v : weight Algebra.t = {
        combine = (op +),
        identity = 0,
        inverseOpt = SOME (fn w => ~w)
    }
                      
end
