#!/usr/bin/env bash

if [ ! -d ~/.cask ]; then
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

pip install --upgrade --user jedi rope flake8 importmagic
