{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs }:
let 
  ghc = reflex-platform.ghc;
  drv = ghc.callPackage ./websockets-reflex.nix {
    mkDerivation = ghc.mkDerivation;
  };
in
drv


