{ pkgs ? import <nixpkgs> {},
  emacs,
  srcDir ? ../.,
  testDir ? ../.,
  packageFile ? ".melpa-check/packages.dhall"
}:
import (builtins.fetchTarball "https://github.com/akirak/melpa-check/archive/v3.tar.gz") {
  inherit pkgs emacs packageFile srcDir testDir;
}
