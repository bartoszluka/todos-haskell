{
  description = "todo app written in Haskell + HTMX";
  inputs.nixpkgs.url = "github.:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem
    (system: let
      pkgs =
        nixpkgs.legacyPackages.${system};
      hPkgs = pkgs.haskell.packages."ghc96";
      myDevTools = [
        hPkgs.ghc
        stack-wrapped
        pkgs.zlib
      ];
      stack-wrapped = pkgs.symlinkJoin {
        name = "stack";
        paths = [pkgs.stack];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "\
              --no-nix \
              --system-ghc \
              --no-install-ghc \
            "
        '';
      };
    in {
      devShells.default = pkgs.mkShell {
        buildInputs = myDevTools;
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
      };
    });
}
