{
  description = "Envrc generated";

  nixConfig = {
    # @@nixConfig@@
  };

  inputs = {
    # @@inputs@@
  };

  outputs = inputs:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      lib = inputs.nixpkgs.lib;

      forAllSystems = f: lib.genAttrs systems (system:
        f (builtins.mapAttrs
          (_: v: v.legacyPackages.${system} or v.packages.${system} or { })
          inputs));

      mkPkg = sources: p:
        if builtins.isString p
        then sources.nixpkgs.${p}
        else sources.${builtins.elemAt p 0}.${builtins.elemAt p 1};
    in {
      devShells = forAllSystems (sources:
        let
          pkgs = sources.nixpkgs;
          mkShell = { packages ? [], shellHooks ? [], env ? {} }:
            pkgs.mkShell ({
              packages = map (mkPkg sources) packages;
              shellHook = lib.concatStringsSep "\n" shellHooks;
            } // env);
        in {
          default = mkShell {
            packages = [
              # @@packages@@
            ];
            shellHooks = [
              # @@shellHooks@@
              # @@menu@@
            ];
            env = {
              # @@env@@
            };
          };
        });
    };
}
