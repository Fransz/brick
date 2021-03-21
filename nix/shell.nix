let
  pkgs = import ./pinned.nix {
    overlays = [ (import ./overlay.nix) ];
  };

  inherit (pkgs.haskell) ghcVersion;

  hspkgs = pkgs.haskell.packages.${ghcVersion};

  drv = import ./default.nix;
in  
  pkgs.haskellPackages.shellFor {
    withHoogle = true;
    
    packages = p: [ drv ];

    buildInputs = with hspkgs; [ hlint haskell-language-server ];
}
