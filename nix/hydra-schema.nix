{
  perSystem = {pkgs, ...}: let
    # Pull the Hydra SQL schema from the nix store
    hydraSchema = pkgs.runCommandLocal "hydra-schema.sql" {} ''
      src=${pkgs.hydra.src}/subprojects/hydra/sql/hydra.sql
      cp "$src" $out
    '';
  in {
    # Add it as an argument to `perSystem` modules
    _module.args.hydraSchema = hydraSchema;
    packages.hydra-schema = hydraSchema;
  };
}
