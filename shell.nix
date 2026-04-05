{pkgs ? import <nixpkgs> {}, ...}:
# I use emacs and merlin while developing

pkgs.mkShellNoCC {
  inputsFrom = [(import ./default.nix {pkgs=pkgs;})];
  packages = [pkgs.ocamlPackages.merlin];
}
