#!/usr/bin/env bash

nix-shell ./workspace.nix --run 'codium --ozone-platform-hint=wayland --wait --user-data-dir ./.codium-data .'
