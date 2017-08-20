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

    reflex-selda = pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-selda";
      rev = "256275f5dd90ce3f8bb65367e0c871f5104321c8";
      sha256 = "1dap8mcavz301x0hd1zcs381rfmdfhsiicn6y3qr7c9bs0p9qsnb";
    };
  };

  ghc = reflex-platform.ghc.override {
    overrides = self: super: {
      reflex-basic-host = self.callPackage sources.reflex-basic-host { inherit reflex-platform; };
      reflex-selda = self.callPackage sources.reflex-selda { inherit reflex-platform; };
      websockets-reflex = self.callPackage ../../websockets-reflex { inherit reflex-platform; };
      websockets-reflex-snap = self.callPackage ../../websockets-reflex-snap { inherit reflex-platform; };
      common = self.callPackage ../common { inherit reflex-platform; compiler = "ghc"; };
    };
  };

  drv = pkgs.haskell.lib.addBuildTool (ghc.callPackage ./backend.nix {}) [ pkgs.sqlite ];
in
  drv

