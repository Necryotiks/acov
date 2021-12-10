{ pkgs ? import <nixpkgs> { } }:
let
  ghcWithPackages = pkgs.ghc.withPackages (g:
    (with g; [
      shake
      weeder
      #liquid-base
      #liquidhaskell
      slist
      base
      stan
      file-embed
      optparse-applicative
      array
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
      hspec
      test-framework
      test-framework-hunit
      test-framework-quickcheck2
    ]));
in pkgs.mkShell { buildInputs = with pkgs; [ ghcWithPackages gcc_multi ]; }
