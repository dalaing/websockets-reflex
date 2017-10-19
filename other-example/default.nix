{ reflex-platform ? import ../reflex-platform.nix }:
let 
  pkgs = reflex-platform.nixpkgs.pkgs;

  sources = {
    reflex-basic-host = pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-basic-host";
      rev = "921fea4de3f9393cb50e31ec5e6c6b13427bf115";
      sha256 = "0pwp3xyd73lpm6w5xfnwl90h03ybzjshzshxrc26wc6l25afd08p";
    };
  };

  ghc = reflex-platform.ghc.override {
    overrides = self: super: {
      reflex-basic-host = self.callPackage sources.reflex-basic-host { inherit reflex-platform; };
      websockets-reflex = self.callPackage ../websockets-reflex { inherit reflex-platform; };
    };
  };

  drv = ghc.callPackage ./other-example.nix {};
in
  drv

