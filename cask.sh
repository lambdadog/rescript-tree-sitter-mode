#!/usr/bin/env nix-shell
#! nix-shell -i bash -p emacs cask

# This script is a helper for using cask on my system, since my
# "emacs" command is wrapped by a script that provides my emacs config
# and a number of packages.

exec cask "$@"
