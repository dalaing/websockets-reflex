{ reflex-platform ? import ../reflex-platform.nix
}:
let
  pkgs = reflex-platform.nixpkgs.pkgs;  
  backend = reflex-platform.ghc.callPackage ./backend { inherit reflex-platform; };
  frontend = reflex-platform.ghcjs.callPackage ./frontend { inherit reflex-platform; };
  serve = pkgs.writeScriptBin "serve" ''
    #!${pkgs.stdenv.shell}
    ${backend}/bin/backend ${frontend}/bin/frontend.jsexe/
  '';
in 
  serve
