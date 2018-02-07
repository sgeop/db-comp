{ mkDerivation, base, beam-core, beam-sqlite, mtl, opaleye
, sqlite-simple, stdenv, text
}:
mkDerivation {
  pname = "db-comp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base beam-core beam-sqlite mtl opaleye sqlite-simple text
  ];
  license = stdenv.lib.licenses.bsd3;
}
