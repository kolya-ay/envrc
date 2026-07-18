{
  description = "envrc";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs = { self, nixpkgs }:
    let
      lib = nixpkgs.lib;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          envrc = pkgs.callPackage ./package.nix { };
        in
        {
          inherit envrc;
          default = envrc;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.babashka
              pkgs.nix
            ];
          };
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          bbDeps = import ./nix/bb-deps.nix { inherit (pkgs) fetchMavenArtifact; };
        in
        {
          envrc-tests = pkgs.runCommand "envrc-tests" { nativeBuildInputs = [ pkgs.babashka ]; } ''
            export BABASHKA_CLASSPATH="${bbDeps.classpath}:${./src}:${./test}:${./plugins/default}"
            cd ${./.}
            bb -m runner
            touch "$out"
          '';
        }
      );
    };
}
