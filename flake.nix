{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = {nixpkgs, ...}: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    devCommand = pkgs.writeScriptBin "scheme" ''
      #!${pkgs.bash}/bin/bash
      file=$1
      echo "$1" | ${pkgs.entr}/bin/entr -c ${pkgs.chicken}/bin/csi -s "$1"
    '';
  in {
    devShells.${system}.default = pkgs.mkShell {
      name = "little-schemer";
      packages = [pkgs.chicken pkgs.entr devCommand];
    };
    formatter.${system} = pkgs.alejandra;
  };
}
