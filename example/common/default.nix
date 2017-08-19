{ reflex-platform ? import ../../reflex-platform.nix
, compiler ? "ghc"
}:
let
  drv = reflex-platform.${compiler}.callPackage ./common.nix {};
in
  drv
