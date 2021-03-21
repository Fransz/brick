let 
  pkgs = import ./pinned.nix {
    overlays = [ (import ./overlay.nix) ];
  };

  inherit (pkgs.haskell) ghcVersion;

  hspkgs = pkgs.haskell.packages.${ghcVersion};

  drv = hspkgs.callCabal2nix "brickDemo" ./.. {};
in
  drv
