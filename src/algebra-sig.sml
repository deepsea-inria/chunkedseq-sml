signature ALGEBRA = sig

    type t

    val identity : t

    val combine : (t * t) -> t

    val inverseOpt : (t -> t) option
    
end
