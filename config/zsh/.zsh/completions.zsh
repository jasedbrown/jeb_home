# my half-baked attempt at completions in zsh.
#
# helpers include:
#  https://github.com/ohmyzsh/ohmyzsh/blob/master/oh-my-zsh.sh
#    https://github.com/ohmyzsh/ohmyzsh/blob/master/lib/completion.zsh
#  https://thevaluable.dev/zsh-completion-guide-examples/
#    https://github.com/Phantas0s/.dotfiles/blob/master/zsh/completion.zsh

export ZSH_COMPDUMP="$XDG_CACHE_HOME/zsh/zcompdump"
mkdir -p $ZSH_COMPDUMP

# Should be called before compinit
zmodload zsh/complist


# options
unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on successive tab press
setopt complete_in_word
setopt always_to_end


# zstyle pattern
# :completion:<function>:<completer>:<command>:<argument>:<tag>

zstyle ':completion:*' completer _extensions _complete _approximate

zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path "$ZSH_COMPDUMP"

# Complete . and .. special directories
zstyle ':completion:*' special-dirs true


# group the results by their description
zstyle ':completion:*' group-name ''
zstyle ':completion:*:*:-command-:*:*' group-order alias builtins functions commands

# color the desciptions
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'

zstyle ':completion:*' menu select search interactive

# this is the magic that allows partial word matching
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# _comp_options+=(globdots) # With hidden files

autoload -U compinit
compinit -d "$ZSH_COMPDUMP"
