let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
in
  import (fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    rev = "6625284c397b44bc9518a5a1567c1b5aae455c08";
    sha256 = "1w0czzv53sg35gp7sr506facbmzd33jm34p6cg23fb9kz5rf5c89";
  })
