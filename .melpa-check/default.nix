{ pkgs ? import <nixpkgs> {},
  emacs ? import (builtins.fetchTarball "https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz").emacs-26-1,
  srcDir ? ../.,
  testDir ? ../.,
  packageFile ? ".melpa-check/packages.dhall"
}:
import (builtins.fetchTarball "https://github.com/akirak/emacs-package-checker/archive/v3.tar.gz") {
  inherit pkgs emacs packageFile srcDir testDir;
}
