with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "chunkedseq-sml";
  buildInputs = [
    smlnj mlton
  ];
  shellHook = ''
    export PATH=${smlnj}/bin:${mlton}/bin:$PATH
  '';
}
