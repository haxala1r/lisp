{
  description = "a lisp interpreter/compiler in ocaml";
  inputs =  {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {self, nixpkgs}: 
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    devInputs = with pkgs.ocamlPackages; [merlin];
    ocamlPkgs = with pkgs.ocamlPackages; [menhir dune_3];
    nativeInputs =  with pkgs; ocamlPkgs ++ [ocaml]; 
  in
  {
    packages.x86_64-linux.default = pkgs.ocamlPackages.buildDunePackage {
      pname = "ollisp";
      version = "0.0.1";
      src = pkgs.lib.cleanSource ./.;
      nativeBuildInputs = nativeInputs;
    };
    devShells.x86_64-linux.default = pkgs.mkShell {
      nativeBuildInputs = nativeInputs ++ devInputs;
    };
  };
}
