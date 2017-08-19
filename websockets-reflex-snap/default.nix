{ reflex-platform ? import ../reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};
  modifiedGhc = ghc.override {
    overrides = self: super: {
      websockets-reflex = self.callPackage ../websockets-reflex/. { inherit reflex-platform compiler; };
    };
  };

  drv = modifiedGhc.callPackage ./websockets-reflex-snap.nix {};
in
  drv

