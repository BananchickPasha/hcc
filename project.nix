{ mkDerivation, base, Cabal, containers, microlens, microlens-th
, mtl, parsec, polysemy, PyF, split, stdenv, text
}:
mkDerivation {
  pname = "hcc";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Cabal containers microlens microlens-th mtl parsec polysemy
    PyF split text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
