{ mkDerivation, base, bytestring, containers, mtl, reflex
, reflex-basic-host, stdenv, stm, these, websockets
, websockets-reflex
}:
mkDerivation {
  pname = "other-example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers mtl reflex reflex-basic-host stm these
    websockets websockets-reflex
  ];
  license = stdenv.lib.licenses.bsd3;
}
