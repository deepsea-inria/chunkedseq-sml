signature ITER = sig

    type 'a t

    type 'a iter

    type 'a segment = { c : 'a t, start : int, length : int }

    datatype direction = Forward | Backward

    structure Search : SEARCH

    val create : direction * 'a t
		 -> 'a iter

    val getSegment : direction * 'a iter
		     -> 'a segment

    val jump : direction * 'a iter * Search.find_by
	       -> unit
		      
end
