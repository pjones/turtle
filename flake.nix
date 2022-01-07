{
  description = "Exercise 4 from The Pragmatic Programmer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
  };

  outputs = { self, nixpkgs, ... }:
    let
      # List of supported systems:
      supportedSystems = [ "x86_64-linux" ];

      # Function to generate a set based on supported systems:
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Attribute set of nixpkgs for each system:
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in
    {
      packages = forAllSystems
        (system:
          let
            pkgs = nixpkgsFor.${system};
            haskell = pkgs.haskellPackages;
            hlib = pkgs.haskell.lib;
          in
          {
            # Full Haskell package with shared/static libraries:
            lib = haskell.callCabal2nix "turtle" self {
              relude = haskell.relude_1_0_0_1;
            };

            # Just the turtle executable:
            bin = hlib.justStaticExecutables self.packages.${system}.lib;
          });

      defaultPackage =
        forAllSystems (system: self.packages.${system}.bin);

      overlay = final: prev: {
        pjones = (prev.pjones or { }) //
          { turtle = self.packages.${prev.system}.bin; };
      };

      devShell = forAllSystems (system:
        nixpkgsFor.${system}.haskellPackages.shellFor {
          packages = _: [ self.packages.${system}.lib ];
          withHoogle = true;
          buildInputs = with nixpkgsFor.${system}; [
            haskellPackages.cabal-fmt
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
          ];
        });
    };
}
