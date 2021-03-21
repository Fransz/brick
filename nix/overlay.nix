self: super:

let
  inherit (self) pkgs;

  ghcVersion = "ghc884";

  haskellPackages = super.haskell.packages.${ghcVersion}.override {
    overrides = self: super: {};
  };
in
  {
    haskell = super.haskell // {
      inherit ghcVersion;

      packages = super.haskell.packages // {
        "${ghcVersion}" = haskellPackages;
      };
    };
  }
