structure SequenceDescriptor = struct

    type weight =
         int

    type 'a algebra = {
        combine : 'a * 'a -> 'a,
        identity : 'a,
        inverseOpt : ('a -> 'a) option
    }

    (* clients must satisfy the following:
     *   for any item x : 'a, weight x > 0
     *)
    datatype ('a, 'b) sequence_descriptor =
             SequenceDescriptor of {
                 weight  : 'a -> weight,
                 measure : 'a -> 'b,
                 algebra : 'b algebra,
                 trivialItem : 'a,
                 itemOverwrite : bool
             }

end
