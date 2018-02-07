{ mkDerivation, base, beam-core, opaleye, stdenv }:
mkDerivation {
  pname = "db-comp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base beam-core opaleye ];
  license = stdenv.lib.licenses.bsd3;
}
