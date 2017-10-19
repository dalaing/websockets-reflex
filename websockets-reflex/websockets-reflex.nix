{ mkDerivation, base, bytestring, hashable, mtl, reflex, stdenv
, stm, ttrie, wai-websockets, websockets
}:
mkDerivation {
  pname = "websockets-reflex";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring hashable mtl reflex stm ttrie wai-websockets
    websockets
  ];
  license = stdenv.lib.licenses.bsd3;
}
