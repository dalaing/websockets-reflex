{ reflex-platform ? import ../../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs
, common ? import ../common }:
let 
  ghc = reflex-platform.ghc;
  doJailbreak = reflex-platform.lib.doJailbreak;
  modified-ghc = ghc.override {
    overrides = self: super: {
      websockets-reflex = self.callPackage ../../websockets-reflex/. {};
    };
  };
  drv = modified-ghc.callPackage ./client.nix {
    mkDerivation = ghc.mkDerivation;
    common = common { compiler = modified-ghc; };
  };
in
drv

