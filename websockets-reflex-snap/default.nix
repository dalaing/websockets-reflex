{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs }:
let 
  ghc = reflex-platform.ghc;
  modified-ghc = ghc.override {
    overrides = self: super: {
      websockets-reflex = self.callPackage ../websockets-reflex/. {};
    };
  };
  drv = modified-ghc.callPackage ./websockets-reflex-snap.nix {
    mkDerivation = ghc.mkDerivation;
  };
in
drv

