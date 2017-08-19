{ mkDerivation, base, bytestring, hashable, mtl, reflex, stdenv
, stm, ttrie, websockets
}:
mkDerivation {
  pname = "websockets-reflex";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring hashable mtl reflex stm ttrie websockets
  ];
  license = stdenv.lib.licenses.bsd3;
}
