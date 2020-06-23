{
  extras = hackage:
    {
      packages = {
        generate-all-tests = ./generate-all-tests.nix;
        cases = ./cases.nix;
        };
      };
  resolver = "lts-14.18";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }