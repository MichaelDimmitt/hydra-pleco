{
  perSystem = {
    system,
    haskellProject,
    lib,
    pkgs,
    ...
  }: let
    cpExesCmd = project: let
      exes = lib.collect lib.isDerivation project.exes;
    in ''
      # Create an intermediate dir
      mkdir release

      # Copy exes to intermediate dir
      ${lib.concatMapStringsSep
        "\n"
        (exe: "cp --verbose --remove-destination --update=none ${exe}/bin/* release")
        exes}
    '';

    mkDistMusl = let
      inherit (project.exes.hydra-pleco.identifier) version;
      project = haskellProject.projectCross.musl64;
      name = "hydra-pleco-${version}-x86_64-linux";
    in
      pkgs.runCommand
      "hydra-pleco-musl64"
      {}
      ''
        mkdir -p $out

        # Copy exes to intermediate dir
        ${cpExesCmd project}

        # Package distribution
        cd release
        dist_file=${name}.tar.gz
        tar -cvzf $out/$dist_file .
      '';
  in {
    packages = lib.optionalAttrs (system == "x86_64-linux") {
      x86_64-linux-static-dist = mkDistMusl;
    };
  };
}
