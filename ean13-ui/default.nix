with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/1.7.1.tar.gz";
  sha256 = "0q44lxzz8pp89ccaiw3iwczha8x2rxjwmgzkxj8cxm97ymsm0diy";
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
  ean13js = ghcjs.callCabal2nix "ean13-ui" ./. {};
in rec {
  dev = ghc.callCabal2nix "ean13-ui" ./. { miso = miso-jsaddle; };
  release = pkgs.haskell.lib.overrideCabal ean13js {
    postInstall = "cp html/* $exe.jsexe/";
  };
}