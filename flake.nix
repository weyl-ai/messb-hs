{
  description = "MESS-B: Minimal Explanatory Stack Subtrace with Boundaries";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    weyl-std.url = "git+ssh://git@github.com/weyl-ai/weyl-std?ref=dev";
  };

  outputs = inputs@{ flake-parts, weyl-std, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ weyl-std.flakeModules.default ];
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];

      weyl-std = {
        formatter.enable = true;
        devshell.enable = false;
      };

      perSystem = { pkgs, self', ... }:
        let
          haskellPackages = pkgs.haskellPackages.override {
            overrides = final: prev: {
              mess-b = final.callCabal2nix "mess-b" ./. { };
              # Unmark diagnose as broken and jailbreak to allow text 2.1
              diagnose = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.unmarkBroken prev.diagnose);
            };
          };
        in
        {
          packages.default = haskellPackages.mess-b;

          devShells.default = haskellPackages.shellFor {
            packages = p: [ p.mess-b ];
            buildInputs = with pkgs; [
              cabal-install
              haskell-language-server
              ghc
              ghcid
              hlint
              ormolu
            ];
            withHoogle = true;
          };
          apps.default = {
            type = "app";
            program = "${self'.packages.default}/bin/mess-b";
          };
        };
    };
}
