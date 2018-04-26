{ pkgs ? import <nixpkgs> {}
}:
let
  finger = pkgs.haskellPackages.callCabal2nix "finger" (pkgs.stdenv.mkDerivation rec {
  name = "finger-src";
  cabal = ../finger.cabal;
  srcs = [ ../LICENSE ../Main.hs ../Types.hs ../Web.hs ];
  phases = ["unpackPhase"];
  unpackPhase = ''
    set -Eux
    mkdir $out
    cp ${cabal} $out/finger.cabal
    ${pkgs.lib.concatStrings (map (f: "cp ${f} $out/${baseNameOf f}\n") srcs)}
    set +Eux
  '';
  }) {};
in {
  inherit finger;
}
