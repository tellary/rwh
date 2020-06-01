with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
  sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
}) {});
let
  dontCheck = pkgs.haskell.lib.dontCheck;
  ghcjs = pkgs.haskell.packages.ghcjs.override {
    overrides = self: super: {
      mynetpbm = super.callCabal2nix "mynetpbm" ../ch10 {};
      # Disabling tests for build speed
      QuickCheck = dontCheck super.QuickCheck;
    };
  };
in ghcjs.callCabal2nix "ean13" ./. {}