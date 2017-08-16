structure ChunkedseqSpec :> CHUNKEDSEQ = struct

    structure C = ListChunkFn (val capacity = 2)

    type ('a, 'b) descr = ('a, 'b) C.sequence_descriptor

    type ('a, 'b) persistent = 
            
    type ('a, 'b) transient
                                    
    structure Persistent = struct

    end

    structure Transient = struct


    end

end
