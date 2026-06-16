# my half-baked attempt at completions in zsh.
#
# helpers include:
#  https://github.com/ohmyzsh/ohmyzsh/blob/master/oh-my-zsh.sh
#    https://github.com/ohmyzsh/ohmyzsh/blob/master/lib/completion.zsh
#  https://thevaluable.dev/zsh-completion-guide-examples/
#    https://github.com/Phantas0s/.dotfiles/blob/master/zsh/completion.zsh

export ZSH_CACHE_DIR="$XDG_CACHE_HOME/zsh"
export ZSH_COMPDUMP="$ZSH_CACHE_DIR/.zcompdump"
export JJ_ZSH_COMPLETION="$ZSH_CACHE_DIR/_jj"
mkdir -p "$ZSH_CACHE_DIR"

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
zstyle ':completion:*' cache-path "$ZSH_CACHE_DIR"

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
# Reuse the existing compdump when possible. This skips compaudit and dump
# regeneration on normal shell startup; remove $ZSH_COMPDUMP to force a rebuild.
if [[ -s "$ZSH_COMPDUMP" ]]; then
    compinit -C -d "$ZSH_COMPDUMP"
else
    compinit -d "$ZSH_COMPDUMP"
fi

# jj completions (must be after compinit)
if [[ -s "$JJ_ZSH_COMPLETION" ]]; then
    source "$JJ_ZSH_COMPLETION"
elif command -v jj >/dev/null 2>&1; then
    if [[ -w "$ZSH_CACHE_DIR" ]]; then
        # Generating jj completions shells out, so cache the static result after
        # the first run instead of using process substitution every startup.
        jj util completion zsh >| "$JJ_ZSH_COMPLETION" 2>/dev/null && source "$JJ_ZSH_COMPLETION"
    else
        source <(jj util completion zsh)
    fi
fi
