{
  lib,
  stdenvNoCC,
  babashka-unwrapped,
  makeWrapper,
  fetchMavenArtifact,
}:
let
  bbDeps = import ./nix/bb-deps.nix { inherit fetchMavenArtifact; };
in
stdenvNoCC.mkDerivation {
  pname = "envrc";
  version = "0.1.0";
  src = ./.;

  nativeBuildInputs = [ makeWrapper ];
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/share/envrc
    cp -r $src/src $out/share/envrc/src
    cp -r $src/plugins $out/share/envrc/plugins
    cp -r $src/resources $out/share/envrc/resources
    cp -r $src/templates $out/share/envrc/templates
    cp $src/bb.edn $out/share/envrc/bb.edn

    makeWrapper ${lib.getExe babashka-unwrapped} $out/bin/envrc \
      --run "export ENVRC_SHARE=\"\''${ENVRC_SHARE:-$out/share/envrc}\"" \
      --run "export ENVRC_DEFAULT_PLUGIN_PATH=\"\''${ENVRC_DEFAULT_PLUGIN_PATH:-$out/share/envrc/plugins/default}\"" \
      --prefix BABASHKA_CLASSPATH : "${bbDeps.classpath}:$out/share/envrc/src:$out/share/envrc/plugins/default" \
      --add-flags "-m" \
      --add-flags "envrc" \
      --add-flags "--"

    runHook postInstall
  '';

  meta = {
    description = "Babashka-powered project environment manager";
    mainProgram = "envrc";
    platforms = lib.platforms.unix;
  };
}
