with { pkgs = import ./nix {}; };
pkgs.mkShell
  { buildInputs = [ pkgs.niv pkgs.haskell.compiler.ghc864
  pkgs.haskellPackages.cabal-install pkgs.postgresql pkgs.zlib];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.gmp}/lib:${pkgs.zlib}/lib:${pkgs.ncurses}/lib
    '';
  }
