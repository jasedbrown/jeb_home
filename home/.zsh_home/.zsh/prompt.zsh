autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats '%b '
setopt prompt_subst
PROMPT=' %F{blue}%c%f %F{red}${vcs_info_msg_0_}%f%F{yellow}→%f '
