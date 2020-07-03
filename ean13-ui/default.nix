with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
  sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
}) {});
let
  ghcjs = pkgs.haskell.packages.ghcjs.extend (
    self: super: with pkgs.haskell.lib; {
      mynetpbm = self.callCabal2nix "mynetpbm" ../ch10 {};
      ean13    = self.callCabal2nix "ean13"    ../ch12 {};
      # Disabling tests for build speed
      # QuickCheck = dontCheck super.QuickCheck;
    });
  ghc   = pkgs.haskell.packages.ghc865.extend (
    self: super: with pkgs.haskell.lib; {
      mynetpbm = self.callCabal2nix "mynetpbm" ../ch10 {};
      ean13    = self.callCabal2nix "ean13"    ../ch12 {};
      # Disabling tests for build speed
      # QuickCheck = dontCheck super.QuickCheck;
    });
in {
  dev = ghc.callCabal2nix   "ean13-ui" ./. { miso = miso-jsaddle; };
  release = ghcjs.callCabal2nix "ean13-ui" ./. {};
}