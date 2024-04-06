{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
    # nixpkgs_22_11.url = github:NixOS/nixpkgs/22.11;
    nixpkgs_darwin.url = github:NixOS/nixpkgs/nixpkgs-23.11-darwin;
    # copilot_flake.url = github:t0yv0/copilot.el/v20240323;
    # dape_src.url = github:svaante/dape?rev=d1a96de51cbee7c410d1f2680f860d09048e2fc5;
    # dape_src.flake = false;
  };

  outputs = { self, nixpkgs, nixpkgs_darwin }: let

    version = self.rev or "dirty";

    packages = nixpkgs: sys: emacs-flavor: let
      pkgs = import nixpkgs { system = sys; };
      epkgs = pkgs.emacsPackagesFor (emacs-flavor pkgs);
      # treesitter = pkgs.tree-sitter.withPlugins (_: pkgs.tree-sitter.allGrammars);
      # mermaid = pkgs_22_11.nodePackages.mermaid-cli;
      # copilot = (builtins.getAttr sys copilot_flake.packages).default;

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

  in {
    packages = {
      "x86_64-darwin" = packages nixpkgs_darwin "x86_64-darwin"  (pkgs: pkgs.emacs29-macport);
      "aarch64-darwin" = packages nixpkgs_darwin "aarch64-darwin" (pkgs: pkgs.emacs29-macport);
      "x86_64-linux" = packages nixpkgs "x86_64-linux" (pkgs: pkgs.emacs29);
      "aarch64-linux" = packages nixpkgs "aarch64-linux" (pkgs: pkgs.emacs29);
    };
  };
}
