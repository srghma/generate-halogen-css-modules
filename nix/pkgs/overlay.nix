pkgs: pkgsOld:
{
  hies      = pkgs.callPackage ./hie-nix {};
  gitignore = pkgs.callPackage ./nix-gitignore {};
}
