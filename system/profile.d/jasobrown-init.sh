# This file is a bit of a hack - if you need to set some
# envvars before anything happens at login, this seems to
# (one of the) least shitty options :shrug:

# note: this file is only read at login time.
# if you make changes, log out and log in again

# basic zsh home dir - must be set
export ZDOTDIR=$HOME/.config/zsh

# suppossedly, these have defaults, but uhhh... fml
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_STATE_HOME=$HOME/.local/state
export XDG_DATA_HOME=$HOME/.local/share

