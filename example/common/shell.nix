{ reflex-platform ? import ../../reflex-platform.nix }:
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  drv = import ./. { inherit reflex-platform; compiler = "ghc" };
in
  if pkgs.lib.inNixShell then drv.env else drv
