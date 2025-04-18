{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-24.11;
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: let

    version = self.rev or "dirty";

    overlay = final: prev: {
      treesitedit = final.callPackage ./package.nix {
        inherit version;
        epkgs = final.emacsPackagesFor final.emacs;
      };
    };

    # https://github.com/NixOS/nixpkgs/issues/395169
    emacs-overlay = final: prev: {
      emacs = prev.emacs.override { withNativeCompilation = false; };
    };

    treesitter-grammars = pkgs: pkgs.tree-sitter.withPlugins (p: [
      p.tree-sitter-go
      p.tree-sitter-gomod
    ]);

    homedir = pkgs: pkgs.stdenv.mkDerivation {
      name = "homedir";
      phases = [ "installPhase" ];
      installPhase = let g = treesitter-grammars pkgs; in ''
        mkdir -p $out/.emacs.d/tree-sitter
        cp ${g}/go.so    $out/.emacs.d/tree-sitter/libtree-sitter-go.so
        cp ${g}/gomod.so $out/.emacs.d/tree-sitter/libtree-sitter-gomod.so
      '';
    };

    devsh = pkgs: let h = homedir pkgs; in pkgs.mkShell {
      buildInputs = [
        pkgs.emacs
        h
      ];
      shellHook = ''
        export HOME=${h}
      '';
    };

    epkgs = pkgs: pkgs.emacsPackagesFor pkgs.emacs;

    out = system: let
      pkgs = import nixpkgs { inherit system; overlays = [emacs-overlay]; };
    in {
      packages.default = (self.overlays.default pkgs pkgs).treesitedit;
      devShells.default = devsh pkgs;
    };

  in flake-utils.lib.eachDefaultSystem out // {
    overlays.default = overlay;
  };
}
