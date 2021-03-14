{ pkgs ? import <nixpkgs> { }, unstable ? import <unstable> }:

with pkgs;

mkShell { buildInputs = [ unstable.racket-minimal ]; }
