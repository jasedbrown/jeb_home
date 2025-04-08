#!/bin/zsh

# taken from the arch linux docs: https://wiki.archlinux.org/title/XDG_Base_Directory[
[ -d "$XDG_STATE_HOME"/zsh ] || mkdir -p "$XDG_STATE_HOME"/zsh
[ -d "$XDG_CACHE_HOME"/zsh ] || mkdir -p "$XDG_CACHE_HOME"/zsh

# Source all modular config files
for file in $ZDOTDIR/.zsh/*.zsh; do
  [ -r "$file" ] && source "$file"
done
