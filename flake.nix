{
  description = "a lisp interpreter/compiler in ocaml";
  inputs =  {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {self, nixpkgs}: 
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in
  {
    packages.x86_64-linux.default = pkgs.ocamlPackages.buildDunePackage {
      pname = "ollisp";
      version = "0.0.1";
      src = pkgs.lib.cleanSource ./.;
      preBuildPhase = "ls -R";
      nativeBuildInputs = with pkgs; [
        ocamlPackages.menhir
      ];
      
    };
  };
}
