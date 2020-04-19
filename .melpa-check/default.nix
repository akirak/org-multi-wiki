{ pkgs ? import <nixpkgs> {},
  emacs ? (import (builtins.fetchTarball "https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz")).emacs-snapshot,
  srcDir ? ../.,
  packageFile ? ".melpa-check/packages.dhall"
}:
import (builtins.fetchTarball "https://github.com/akirak/melpa-check/archive/v3.tar.gz") {
  inherit pkgs emacs packageFile srcDir;
}
