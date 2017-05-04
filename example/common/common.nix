{ mkDerivation, base, binary, bytestring, stdenv }:
mkDerivation {
  pname = "common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base binary bytestring ];
  license = stdenv.lib.licenses.bsd3;
}
