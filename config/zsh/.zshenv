# This file is a bit of a hack - if you need to set some
# envvars before anything happens at login, this seems to
# (one of the) least shitty options :shrug:
#
# *** Make sure this file is symlinked to the $HOME dir. ***

# basic zsh home dir - must be set
export ZDOTDIR=$HOME/.config/zsh

# suppossedly, these have defaults, but uhhh... fml
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_STATE_HOME=$HOME/.local/state
export XDG_DATA_HOME=$HOME/.local/share

ulimit -n unlimited

# my good ol' source code, developer home
export SRC_HOME="/opt/dev"

# standard directory for my dotfiles repo
export DOTFILES_HOME="$HOME/jeb_home"

export EDITOR="emacs -nw"

# only explicitly set up the ssh-agent socket if we're on arch
if [ -f /etc/arch-release ]; then
    export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
fi


