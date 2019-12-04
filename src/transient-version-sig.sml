signature TRANSIENT_VERSION = sig

    type t

    val create : unit -> t

    val alwaysInvalid : t

    val same : t * t -> bool
    
end
