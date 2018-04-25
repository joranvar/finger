{ pkgs ? import <nixpkgs> {} }:
(import ./. { inherit pkgs; }).finger.env
