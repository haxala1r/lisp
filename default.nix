{ pkgs ? import <nixpkgs> {}, ...}:

pkgs.ocamlPackages.buildDunePackage {
  pname = "ollisp";
  version = "0.0.1";
  src = pkgs.lib.cleanSource ./.;
  nativeBuildInputs = with pkgs.ocamlPackages; [findlib menhir dune_3 ocaml];
  buildInputs = with pkgs.ocamlPackages; [];
}
