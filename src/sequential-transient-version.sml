structure SequentialTransientVersion
	  :> TRANSIENT_VERSION = struct

    type t = int

    val alwaysInvalid = ~1

    val r = ref 0

    fun create () =
	let val id = ! r
	in
	    r := id + 1;
	    id
	end

    fun same (t1, t2) =
	(t1 = t2)

end
