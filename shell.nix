with import <nixpkgs> {};
mkShell {
  buildInputs = [
    (haskell.packages.ghc865.ghcWithPackages (pkgs: [
      pkgs.transformers
    ]))
  ];
}
