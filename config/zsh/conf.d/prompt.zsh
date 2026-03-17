autoload -Uz vcs_info
zstyle ':vcs_info:git:*' formats '%b '

_vcs_branch_info() {
    local jj_bookmark
    jj_bookmark=$(jj log --no-graph -r "@ & bookmarks()" -T 'bookmarks.join(", ")' 2>/dev/null)
    if [[ -n "$jj_bookmark" ]]; then
        _branch_info="${jj_bookmark} "
    else
        vcs_info
        _branch_info="${vcs_info_msg_0_}"
    fi
}

precmd() { _vcs_branch_info }
setopt prompt_subst
PROMPT=' %F{blue}%c%f %F{red}${_branch_info}%f%F{yellow}→%f '
