[
  (pkgs: pkgsOld: {
    nodejs       = pkgsOld.nodejs-10_x;
    nodePackages = pkgsOld.nodePackages_10_x;
  })
  (import ./pkgs/overlay.nix)
]
