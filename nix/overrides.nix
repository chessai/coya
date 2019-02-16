{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  coya = (
    with rec {
      coyaSource = pkgs.lib.cleanSource ../.;
      coyaBasic  = self.callCabal2nix "coya" coyaSource { };
    };
    overrideCabal coyaBasic (old: {
    })
  );
}
