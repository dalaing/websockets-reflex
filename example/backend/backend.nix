{ mkDerivation, base, binary, bytestring, common, containers
, dependent-map, dependent-sum, hashable, mtl, primitive, ref-tf
, reflex, snap-core, snap-server, stdenv, stm, text, ttrie
, websockets-reflex, websockets-reflex-snap
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring common containers dependent-map
    dependent-sum hashable mtl primitive reflex snap-core snap-server
    stm text ttrie websockets-reflex websockets-reflex-snap
  ];
  executableHaskellDepends = [
    base binary bytestring common containers dependent-map
    dependent-sum hashable mtl primitive ref-tf reflex snap-core
    snap-server stm text ttrie websockets-reflex websockets-reflex-snap
  ];
  license = stdenv.lib.licenses.bsd3;
}
