{ reflex-platform ? import ../../reflex-platform }:
let 
  ghcjs = reflex-platform.ghcjs.override {
    overrides = self: super: {
      common = self.callPackage ../common { inherit reflex-platform; compiler = "ghcjs"; };
    };
  };
  drv = ghcjs.callPackage ./frontend.nix {};
in
  drv

