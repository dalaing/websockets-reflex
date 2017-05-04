{ mkDerivation, base, bytestring, dependent-sum, hashable, mtl
, primitive, ref-tf, reflex, stdenv, stm, ttrie, websockets
}:
mkDerivation {
  pname = "websockets-reflex";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring dependent-sum hashable mtl primitive ref-tf reflex
    stm ttrie websockets
  ];
  license = stdenv.lib.licenses.bsd3;
}
