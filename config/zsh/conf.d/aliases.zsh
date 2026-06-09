# always print colors with 'ls'
if command -v gls >/dev/null 2>&1; then
    alias ls='gls --color=auto'
elif [ "$(uname -s)" = "Darwin" ]; then
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi

alias cd.='cd ..'
alias cd..='cd ../../'
alias cd...='cd ../../../'
alias cd....='cd ../../../../'
alias cd.....='cd ../../../../../'
alias cdh='cd $DOTFILES_HOME'
alias cds='cd $SRC_HOME'

alias ll='ls -la'
alias llr='ls -lart'
alias mkdir='mkdir -p'
alias findname='find -L . -name'
alias shippo='tail -200f'
alias gs='git status'
alias ethereal='wireshark'
alias ff='firefox'

# emacs magick.
alias emacs="emacs -nw"
alias e='emacs -nw'
alias ec='emacsclient -nw'

# handy terminal "reset", without executing `reset`
# this will: Clear scrollback buffer, Move cursor to home position, Clear visible screen
alias clearall='echo -e "\033[3J\033[H\033[2J"'
