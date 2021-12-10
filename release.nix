let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.callPackage ./default2.nix { }
