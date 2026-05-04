{
  description = "A code formatter for Idris2";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    idris2-withpkgs.url = "github:gvnkd/flake-idris2-withPackages";
  };

  outputs = { self, nixpkgs, flake-utils, idris2-withpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        idris2 = idris2-withpkgs.inputs.idris2-src.packages.${system}.idris2;
        idris2api = idris2-withpkgs.inputs.idris2-src.packages.${system}.idris2Api;

        # Select registry packages to use as dependencies.
        # Available packages: containers, algebra, array, json, json-simple,
        # async, bytestring, hedgehog, parser, and 150+ more.
        idrisLibraries = with idris2-withpkgs.packages.${system}; [
          prettier
          parser
          idris2api
        ];

        # Wrapped idris2 with all selected packages available in devShell
        idris2Wrapped = idris2-withpkgs.lib.${system}.withPackages (p: [
          p.prettier
          p.parser
          idris2api
        ]);

        # Docs packages for dependencies (add <name>-docs here)
        docsPkgs = with idris2-withpkgs.packages.${system}; [
          # Add dependency docs here as needed
          # json-docs
        ];

        # Combine all docs into a single tree: <combined>/share/doc/<pkg>/
        combinedDocs = pkgs.symlinkJoin {
          name = "combined-idris2-docs";
          paths = docsPkgs;
        };

        # Helper script: doc list | doc show <pkg> [<module>]
        docScript = pkgs.runCommand "doc" {} ''
          mkdir -p $out/bin
          cp ${pkgs.replaceVars ./scripts/doc {
            DOCS = "${combinedDocs}/share/doc";
          }} $out/bin/doc
          chmod +x $out/bin/doc
        '';

        pkg = pkgs.idris2Packages.buildIdris {
          src = ./.;
          ipkgName = "idris2-fmt";
          version = "0.1.0";
          inherit idrisLibraries;
        };

        buildScript = pkgs.writeShellScriptBin "build" ''
          set -e
          echo "Building..."
          idris2 --build
          echo "Build complete."
        '';

        testScript = pkgs.writeShellScriptBin "run-tests" ''
          set -e
          echo "Building main library..."
          idris2 --build idris2-fmt.ipkg
          echo "Running tests..."
          ./tests/runtests.sh ./build/exec/idris2-fmt
        '';
      in
      {
        packages = {
          default = pkg.executable;
          lib = pkg.library';
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            #python3.withPackages( p: [ ] )
            idris2Packages.idris2Lsp
            python3
            gnused gnugrep gawk diffutils jq yq ripgrep
            buildScript
            testScript
          ];
          buildInputs = [
            idris2Wrapped
            pkgs.rlwrap
            idris2-withpkgs.packages.${system}.idris2-mkdoc-md
            docScript
          ];

          shellHook = ''
            export LD_LIBRARY_PATH="${idris2Wrapped}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
            export IDRIS2_LIBS="${idris2Wrapped}/lib''${IDRIS2_LIBS:+:$IDRIS2_LIBS}"
          '';
        };
      }
    );
}
