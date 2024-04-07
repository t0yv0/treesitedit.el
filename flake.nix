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

  in {
    packages = {
      "x86_64-darwin" = packages nixpkgs_darwin "x86_64-darwin"  (pkgs: pkgs.emacs29-macport);
      "aarch64-darwin" = packages nixpkgs_darwin "aarch64-darwin" (pkgs: pkgs.emacs29-macport);
      "x86_64-linux" = packages nixpkgs "x86_64-linux" (pkgs: pkgs.emacs29);
      "aarch64-linux" = packages nixpkgs "aarch64-linux" (pkgs: pkgs.emacs29);
    };
  };
}
