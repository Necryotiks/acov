{ pkgs ? import <nixpkgs> { }, multiStdenv, lib, fetchFromGitHub, }:
let
  ghcWithPackages = pkgs.ghc.withPackages (g:
    (with g; [
      array
      base
      containers
      parsec
      transformers
      Cabal
      directory
      process
      filepath
      optparse-applicative
      parsec
      time
      HUnit
      parsec
      QuickCheck
      test-framework
      test-framework-hunit
      test-framework-quickcheck2
    ]));
in multiStdenv.mkDerivation rec {
  pname = "acov";
  version = "0.2.3";
  src = fetchFromGitHub {
    owner = "ArgonDesign";
    repo = "acov";
    rev = "be39459695de7e29dbcd229119622a0411426760";
    sha256 = "1nv7a2ls6lgc31pyrpz06941iwpyhj7bwnqlhxg16z5vk6ljj93f";
  };
  buildInputs = [ ghcWithPackages pkgs.cabal-install ];
  configurePhase = ''
    export HOME=$TMP
      echo 'active-repositories: :none' >> ${src}/acov.cabal'';
  buildPhase = ''
    make build-dpi
    cabal --enable-nix --http-transport=plain-http v2-build
    ghc -O src/acov/*.hs src/frontend/*.hs -o acov
    ghc -O src/acov-report/*.hs src/frontend/*.hs -o acov-report
  '';
  meta = with lib; {
    description = "A SystemVerilog functional coverage generator";
    license = licenses.bsd3;
  };
}
