{ mkDerivation, base, binary, common, dependent-sum, mtl, primitive
, ref-tf, reflex, stdenv, websockets, websockets-reflex
}:
mkDerivation {
  pname = "client";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary common dependent-sum mtl primitive ref-tf reflex
    websockets websockets-reflex
  ];
  license = stdenv.lib.licenses.bsd3;
}
