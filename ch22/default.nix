with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
  sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
}) {});
let
  refinedSrc = pkgs.fetchFromGitHub {
    owner = "nikita-volkov";
    repo  = "refined";
    rev = "v0.4.1";
    sha256 = "1m5n9bawdm7r66da2b76rr2v8dj3kd6bpvw9jzkwzb6x5d29bikz";
  };
  dontCheck = pkgs.haskell.lib.dontCheck;
  haskellPackages = pkgs.haskell.packages.ghc865.override {
    overrides = self: super: {
      # Building `refined` from GitHub as it's marked as broken in
      # `nixpkgs` used by Miso.
      # Tests are disabled for all packages that depend on `doctest`
      # as it cannot be built with GHCJS.
      refined       = dontCheck (self.callCabal2nix "refined" refinedSrc {});
      comonad       = dontCheck super.comonad;
      lens          = dontCheck super.lens;
      QuickCheck    = dontCheck super.QuickCheck;
      parallel      = dontCheck super.parallel;
      prettyprinter = dontCheck super.prettyprinter;
      semigroupoids = dontCheck super.semigroupoids;
      # These just take forever to complete their checks
      tasty-quickcheck = dontCheck super.tasty-quickcheck;
      scientific       = dontCheck super.scientific;
    };
  };
in haskellPackages.callCabal2nix "podcatcher" ./. {}