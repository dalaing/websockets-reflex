{ reflex-platform ? import ../../reflex-platform.nix }:
let 
  pkgs = reflex-platform.nixpkgs.pkgs;

  sources = {
    reflex-basic-host = pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-basic-host";
      rev = "3be61ad9ce92edd9c0cdb545953cc1f8c41b6921";
      sha256 = "0g30zbz4yivg0jlwqip4qgh4bxrcwym1c7rmw9ajk8zf5x728sq7";
    };
  };

  ghc = reflex-platform.ghc.override {
    overrides = self: super: {
      reflex-basic-host = self.callPackage sources.reflex-basic-host { inherit reflex-platform; };
      websockets-reflex = self.callPackage ../../websockets-reflex { inherit reflex-platform; };
      websockets-reflex-snap = self.callPackage ../../websockets-reflex-snap { inherit reflex-platform; };
      common = self.callPackage ../common { inherit reflex-platform; compiler = "ghc"; };
    };
  };
  drv = ghc.callPackage ./backend.nix {};
in
  drv

