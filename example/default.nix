{ reflex-platform ? import ../reflex-platform {}
, pkgs ? reflex-platform.nixpkgs.pkgs
, ghc ? reflex-platform.ghc
, ghcjs ? reflex-platform.ghcjs
}:
let
  backend = ghc.callPackage ./backend { inherit reflex-platform; };
  frontend = ghcjs.callPackage ./frontend { inherit reflex-platform; };
  serve = pkgs.writeScriptBin "serve" ''
    #!${pkgs.stdenv.shell}
    ${backend}/bin/backend ${frontend}/bin/frontend.jsexe/
  '';
in 
  serve
