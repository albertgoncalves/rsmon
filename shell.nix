with import <nixpkgs> {};
mkShell {
    buildInputs = [
        rustc
        rustfmt
        shellcheck
        valgrind
    ];
    shellHook = ''
        . .shellhook
    '';
}
