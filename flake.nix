{
  # Requires nix version 2.7 or later

  # $ nix develop
  # $ nix build    [see result/bin when completed]
  # $ nix develop .#lumberjack.llvm_9.default
  # $ nix develop .#lumberjack.llvm_9.ghc98
  # $ nix develop .#lumberjack.ghc98.llvm_9
  # $ nix run
  # $ nix run .#lumberjack

  description = "The lumberjack logging library";

  nixConfig.bash-prompt-suffix = "lumberjack.env} ";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, levers }:
    rec
      {
        devShells = levers.haskellShells
          { inherit nixpkgs;
            flake = self;
            ghcvers = system: [ "ghc910" ];
            # additionalPackages = pkgs: [ pkgs.? ];
          };

        packages = levers.eachSystem (system:
          let mkHaskell = levers.mkHaskellPkg { inherit nixpkgs system; };
              pkgs = import nixpkgs { inherit system; };
          in rec
            {
              default = lumberjack;
              lumberjack = mkHaskell "lumberjack" self {};
            });
      };
}
