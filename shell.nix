{nixpkgs ? import <nixpkgs> {}}:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithHoogle
                   (haskellPackages: with haskellPackages; [
                     megaparsec
                   ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ghc] ++ (with nixpkgs; [stdenv ncurses]);
  #shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  shellHook = ''
    hogle=$(readlink -f ${ghc}/bin/hoogle | cut -d "/" -f 1-4 )
    export HIE_HOOGLE_DATABASE=$hogle/share/doc/hoogle/default.hoo
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}

