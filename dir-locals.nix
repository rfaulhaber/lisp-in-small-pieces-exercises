let
  pkgs = import <nixpkgs> { };
  unstable = import <unstable> { };
in pkgs.nixBufferBuilders.withPackages [ unstable.racket-minimal ]
