{
  description = "a lisp interpreter/compiler in ocaml";
  inputs =  {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = nixpkgs.legacyPackages.${system};
    devInputs = with pkgs.ocamlPackages; [merlin];
    ocamlPkgs = with pkgs.ocamlPackages; [menhir dune_3];
    nativeInputs =  with pkgs; ocamlPkgs ++ [ocaml]; 
  in
  {
    packages.default = pkgs.ocamlPackages.buildDunePackage {
      pname = "ollisp";
      version = "0.0.1";
      src = pkgs.lib.cleanSource ./.;
      nativeBuildInputs = nativeInputs;
    };
    devShells.default = pkgs.mkShell {
      nativeBuildInputs = nativeInputs ++ devInputs;
    };
  });
}
