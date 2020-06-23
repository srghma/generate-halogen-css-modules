let
  # Import the Haskell.nix library,
  src = builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;
  nixpkgs = import (src + "/nixpkgs") (import src);
  haskell = nixpkgs.haskell-nix;

  # Instantiate a package set using the generated file.
  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };
in
  pkgSet.config.hsPkgs
