{ mkDerivation, base, snap-core, stdenv, stm, websockets
, websockets-reflex, websockets-snap
}:
mkDerivation {
  pname = "websockets-reflex-snap";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base snap-core stm websockets websockets-reflex websockets-snap
  ];
  license = stdenv.lib.licenses.bsd3;
}
