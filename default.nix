with import <nixpkgs> {};
#with import(fetchTarball https://github.com/domenkozar/hie-nix/tarball/master) {};
with import ../hie-nix {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    ghc
    cabal-install
    hie84
    binutils
    ncurses
    zlib
  ];

  shellHook = ''
    export C_INCLUDE_PATH=${ncurses.dev}/include
    export LIBRARY_PATH=${ncurses}/lib
    export LD_LIBRARY_PATH=${zlib}/lib
  '';

}
