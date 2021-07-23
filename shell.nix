with import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz") { };
stdenv.mkDerivation {
  name = "yamlparse-test";
  buildInputs = [
    haskell.compiler.ghc8102Binary
    zlib
  ];
}
