{ reflex-platform ? import ../../reflex-platform }:
let 
  ghc = reflex-platform.ghc.override {
    overrides = self: super: {
      websockets-reflex = self.callPackage ../../websockets-reflex { inherit reflex-platform; };
      websockets-reflex-snap = self.callPackage ../../websockets-reflex-snap { inherit reflex-platform; };
      common = self.callPackage ../common { inherit reflex-platform; compiler = "ghc"; };
    };
  };
  drv = ghc.callPackage ./backend.nix {};
in
  drv

