# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

{ pkgs ? import nix/nixpkgs.nix {} }:
pkgs.mkShell {
  name = "shell";
  buildInputs =
    [
      pkgs.gcc
      pkgs.pkg-config
      pkgs.sbt
      pkgs.scala
      pkgs.verilator

      # Haskell toolchain
      pkgs.cabal-install
      pkgs.haskellPackages.fourmolu

      # pkgs.haskell.compiler.ghc90

      # Clash throws a slow start warning / error on 9.4.8, so we use 9.4.7
      pkgs.haskell.compiler.ghc947
      # pkgs.haskell.compiler.ghc96

      (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)

      pkgs.openocd-riscv
      pkgs.gdb

      # For Cabal to clone git repos
      pkgs.git

      # C Tools
      pkgs.clang

      # For upgrading Nix env. To update dependencies (within bounds of the currently
      # tracking NixOS version) use:
      #
      #   niv update
      #
      # If you want to upgrade nixpkgs to another NixOS version, use:
      #
      #   niv update nixpkgs -b nixos-23.11
      #
      pkgs.niv
    ]
    ;

  shellHook = ''
    # Prevents Perl warnings
    export LC_ALL="C.UTF-8";

    # Mixing Nix Cabal and non-Nix Cabal yields some weird linking errors.
    export CABAL_DIR="$HOME/.cabal-nix";

    # Add repo utilities to path
    export PATH="$(git rev-parse --show-toplevel)/nix/bin:$PATH";
  '';
}
