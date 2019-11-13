with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "test-chunkedseq-sml";
  buildInputs = [
    smlnj mlton
  ];
  shellHook = ''
    export SMLNJ_PATH=${smlnj}
    export MLTON_PATH=${mlton}
    export PATH=$SMLNJ_PATH/bin:$MLTON_PATH/bin:$PATH
  '';
}
