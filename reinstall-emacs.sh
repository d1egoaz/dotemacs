#!/bin/bash

brew remove emacs-plus@30
rm -rf eln-cache
rm -rf straight/build*
brew install emacs-plus@30 --with-native-comp --with-savchenkovaleriy-big-sur-curvy-3d-icon
