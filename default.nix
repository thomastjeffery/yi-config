{ nixpkgs ? import <nixpkgs> {} }:

nixpkgs.pkgs.haskellPackages.callPackage ./yi.nix {}