{ haskellPackages ? (import <nixpkgs>{}).haskellPackages_ghc783_profiling }:
with haskellPackages;

let
  nanompd = cabal.mkDerivation (self: rec {
    pname = "nanompd";
    version = "0.0.0.0";
    src = ./../nanompd;
    propagatedBuildDepends = [ cabalInstall ];
    buildDepends = [
      cabalInstall attoparsec either exceptions mtl network text
      unorderedContainers
    ];
    doCheck = false;
  });

in

cabal.mkDerivation (self: rec {
  pname = "hmpc";
  version = "0.0.0.0";
  src = ./.;
  propagatedBuildDepends = [ cabalInstall ];
  buildDepends = [ cabalInstall nanompd ];
})
