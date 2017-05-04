{ mkDerivation, base, binary, bytestring, common, containers, lens
, reflex, reflex-dom, stdenv, text
}:
mkDerivation {
  pname = "frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring common containers lens reflex reflex-dom
    text
  ];
  license = stdenv.lib.licenses.bsd3;
}
