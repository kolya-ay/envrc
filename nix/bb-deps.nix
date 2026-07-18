{ fetchMavenArtifact }:
let
  clojars = [ "https://repo.clojars.org" ];

  pretty = fetchMavenArtifact {
    repos = clojars;
    groupId = "org.clj-commons";
    artifactId = "pretty";
    version = "3.6.8";
    hash = "sha256-LZARa7k80gAE/ZYq94ceS3ZN897dhbCXGySq2vhYImc=";
  };

  dbus-client = fetchMavenArtifact {
    repos = clojars;
    groupId = "com.lambdaisland";
    artifactId = "dbus-client";
    version = "0.2.15";
    hash = "sha256-fJA9znxQ5m3vRNKUSEGKeUq8nENlQz6P9GFiZoCi7qM=";
  };

  cli = fetchMavenArtifact {
    repos = clojars;
    groupId = "com.lambdaisland";
    artifactId = "cli";
    version = "1.27.121";
    hash = "sha256-sXdMUq12RUi0WY3ecD70nrhKSQVhW709aAanS8EsXEA=";
  };

  dynaload = fetchMavenArtifact {
    repos = clojars;
    groupId = "borkdude";
    artifactId = "dynaload";
    version = "0.3.5";
    hash = "sha256-CIfAzbmvzs18SW6iWKMuQ6py52bz8GMuG9D1JFyowkw=";
  };

  edamame = fetchMavenArtifact {
    repos = clojars;
    groupId = "borkdude";
    artifactId = "edamame";
    version = "1.5.39";
    hash = "sha256-6Dyg+WLFgMXJyNAJZ2mLP9DQAiR+N+T0bDODucKdiFU=";
  };

  fipp = fetchMavenArtifact {
    repos = clojars;
    groupId = "fipp";
    artifactId = "fipp";
    version = "0.6.29";
    hash = "sha256-22eJoDXAE84RqZHrOBiILWjUf6ap7uRiey4NnhvvBfI=";
  };

  malli = fetchMavenArtifact {
    repos = clojars;
    groupId = "metosin";
    artifactId = "malli";
    version = "0.20.1";
    hash = "sha256-RaaMI4tsXRl2KXUPGmPMWqbQRUVrCpRcjJ1ayAZ9gd4=";
  };

  arrangement = fetchMavenArtifact {
    repos = clojars;
    groupId = "mvxcvi";
    artifactId = "arrangement";
    version = "2.1.0";
    hash = "sha256-83JROF0iDfiVHmjVJVq7UZetvL2PxrPT/KhyojOfOcg=";
  };

  jars = [ pretty dbus-client cli dynaload edamame fipp malli arrangement ];
in
{
  classpath = builtins.concatStringsSep ":" (map (j: "${j.jar}") jars);
}
