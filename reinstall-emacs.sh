#!/bin/bash

brew remove emacs-plus@30
rm -rf eln-cache
rm -rf straight/build*
brew install emacs-plus@30 --with-poll --with-native-comp --with-c9rgreen-sonoma-icon
rm -rf /Applications/Emacs.app && osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@31/Emacs.app" at POSIX file "/Applications" with properties {name:"Emacs.app"}'
