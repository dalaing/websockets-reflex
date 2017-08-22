{ reflex-platform ? import ../../reflex-platform.nix }:
let 
  pkgs = reflex-platform.nixpkgs.pkgs;

  sources = {
    reflex-basic-host = pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-basic-host";
      rev = "921fea4de3f9393cb50e31ec5e6c6b13427bf115";
      sha256 = "0pwp3xyd73lpm6w5xfnwl90h03ybzjshzshxrc26wc6l25afd08p";
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

