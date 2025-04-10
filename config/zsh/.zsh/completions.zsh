# TODO: this is busted - revisit me
# zstyle ':completion:*' cache-path "$XDG_CACHE_HOME"/zsh/zcompcache
zstyle ':completion:*' menu select
autoload -U compinit


# _comp_options+=(globdots) # With hidden files

compinit
#compinit -d "$XDG_CACHE_HOME"/zsh/zcompdump-$ZSH_VERSION

