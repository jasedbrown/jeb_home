# Ensure history is enabled
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

# Append history instead of overwriting
setopt APPEND_HISTORY
# jfc, i hate shared history
unsetopt share_history

# Write history immediately, rather than waiting
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS

