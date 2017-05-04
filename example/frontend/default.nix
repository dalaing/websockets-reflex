{ reflex-platform ? import ../../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs
, common ? import ../common }:
let 
  ghcjs = reflex-platform.ghcjs;
  drv = ghcjs.callPackage ./frontend.nix {
    mkDerivation = ghcjs.mkDerivation;
    common = common { compiler = ghcjs; };
  };
in
drv

