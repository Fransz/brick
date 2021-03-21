let
  # release 20.09
  # Commit date is 2021-03-12 22:03:20 +0100

  hostPkgs = import <nixpkgs> {};
  pinnedVersion = hostPkgs.lib.importJSON ./nixpkgs-20.09.json;
  pinnedPkgs = hostPkgs.fetchFromGitHub {
    owner = "NixOs";
    repo ="nixpkgs";
    inherit (pinnedVersion) rev sha256;
  };
in import pinnedPkgs 


