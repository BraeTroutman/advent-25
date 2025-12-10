{
  description = "A basic haskell setup: HLS + GHC";

  inputs = {
    # hls 2.12.0.0 with ghc 9.10.3 is broken, so we switch to older nixpkgs version
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs   = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          ghc
          haskell-language-server
        ];
        shellHook = ''
          echo "Welcome to a basic Haskell DevShell!"
          ghc --version
          haskell-language-server-wrapper --version
        '';
      };   
    };
}
