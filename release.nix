let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  config = {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        db-comp =
          haskellPackagesNew.callPackage ./default.nix { };
      };
    };
  };

  pkgs = import src { inherit config; };

in
  { db-comp = pkgs.haskellPackages.callPackage ./default.nix {};
  }
