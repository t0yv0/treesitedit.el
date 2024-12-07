{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
    nixpkgs_darwin.url = github:NixOS/nixpkgs/nixpkgs-23.11-darwin;
  };

  outputs = { self, nixpkgs, nixpkgs_darwin }: let

    version = self.rev or "dirty";

    packages = nixpkgs: sys: emacs-flavor: let
      pkgs = import nixpkgs { system = sys; };
      epkgs = pkgs.emacsPackagesFor (emacs-flavor pkgs);

      treesitedit = epkgs.elpaBuild {
        pname = "treesitedit";
        ename = "treesitedit";
        version = version;
        src = [ ./treesitedit.el ];
        packageRequires = [];
        meta = {};
      };

    in {
      default = treesitedit;
    };

    devShells = nixpkgs: sys: emacs-flavor: let
      pkgs = import nixpkgs { system = sys; };

      emacs = emacs-flavor pkgs;

      treesitter-grammars = pkgs.tree-sitter.withPlugins (p: [
            p.tree-sitter-go
            p.tree-sitter-gomod
      ]);

      homedir = pkgs.stdenv.mkDerivation {
          name = "homedir";
          phases = [ "installPhase" ];
          installPhase = ''
            mkdir -p $out/.emacs.d/tree-sitter
            cp ${treesitter-grammars}/go.so    $out/.emacs.d/tree-sitter/libtree-sitter-go.so
            cp ${treesitter-grammars}/gomod.so $out/.emacs.d/tree-sitter/libtree-sitter-gmod.so
          '';
      };

      devShell = pkgs.mkShell {
        buildInputs = [
          emacs
          homedir
        ];
        shellHook = ''
          export HOME=${homedir}
        '';
      };

    in {
      default = devShell;
    };

  in {
    packages = {
      "x86_64-darwin" = packages nixpkgs_darwin "x86_64-darwin"  (pkgs: pkgs.emacs29-macport);
      "aarch64-darwin" = packages nixpkgs_darwin "aarch64-darwin" (pkgs: pkgs.emacs29-macport);
      "x86_64-linux" = packages nixpkgs "x86_64-linux" (pkgs: pkgs.emacs29);
      "aarch64-linux" = packages nixpkgs "aarch64-linux" (pkgs: pkgs.emacs29);
    };

    devShells = {
      "x86_64-darwin" = devShells nixpkgs_darwin "x86_64-darwin"  (pkgs: pkgs.emacs29-macport);
      "aarch64-darwin" = devShells nixpkgs_darwin "aarch64-darwin" (pkgs: pkgs.emacs29-macport);
      "x86_64-linux" = devShells nixpkgs "x86_64-linux" (pkgs: pkgs.emacs29);
      "aarch64-linux" = devShells nixpkgs "aarch64-linux" (pkgs: pkgs.emacs29);
    };
  };
}
