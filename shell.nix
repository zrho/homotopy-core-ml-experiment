with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "env";
  buildInputs = with pkgs.ocaml-ng.ocamlPackages_4_08; [
    dune
    ocaml
    merlin
    findlib
    base
    stdio
    ppx_jane
    re
  ];
}
