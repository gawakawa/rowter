{ inputs, flake-parts-lib, ... }:
{
  options.perSystem = flake-parts-lib.mkPerSystemOption (
    { lib, ... }:
    {
      options.ciPackages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
        description = "Packages for CI environment";
      };
    }
  );

  config.perSystem =
    { config, system, ... }:
    let
      pkgs = import inputs.nixpkgs { inherit system; };

      purs-nix = inputs.purs-nix { inherit system; };

      nodejs = pkgs.nodejs_24;

      ps = purs-nix.purs {
        dependencies = [
          "console"
          "effect"
          "prelude"
        ];

        test-dependencies = [
          "test-unit"
        ];

        dir = ./..;

        # Pins the interpreter in the shebang `app` generates to the same
        # Node.js the devShell and CI use.
        inherit nodejs;
      };

    in
    {
      _module.args = {
        inherit
          pkgs
          ps
          purs-nix
          ;
        ps-tools = inputs.ps-tools.legacyPackages.${system};
      };

      ciPackages = [ nodejs ];

      packages = {
        default = ps.app { name = "app"; };
        ci = pkgs.buildEnv {
          name = "ci";
          paths = config.ciPackages;
        };
      };

      apps.default = {
        type = "app";
        program = "${config.packages.default}/bin/app";
      };
    };
}
