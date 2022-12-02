{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            (pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
                vector
                containers
                attoparsec
                bytestring
                text
                unordered-containers
                relude
                split
            ]))
            pkgs.haskellPackages.ghcid
            pkgs.nixpkgs-fmt
          ];
          withHoogle = false;
        };
      }
    );
}
