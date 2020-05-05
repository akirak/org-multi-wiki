{
  # The default version of Emacs to use
  emacs ? "snapshot",
  # You can use niv to update Emacs
  emacs-ci ? import (import ../nix/sources.nix).nix-emacs-ci,
  # You can use niv to update melpa-check
  melpa-check ? import (import ../nix/sources.nix).melpa-check,
  # The directory containing source files
  srcDir ? ../.,
  # A configuration file which defines packages under test
  packageFile ? ".melpa-check/packages.dhall"
}:
melpa-check {
  inherit emacs emacs-ci packageFile srcDir;
}
