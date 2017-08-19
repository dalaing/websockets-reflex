{ reflex-platform ? import ../reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};

  drv = ghc.callPackage ./websockets-reflex.nix { };
in
  drv
