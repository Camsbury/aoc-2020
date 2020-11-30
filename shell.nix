let
  pkgs = import ./pinned.nix {};
in
  with pkgs;
  mkShell {
    name = "vizHelper";
    buildInputs = [
      clj-kondo
      clojure
    ];
  }
