# always print colors with 'ls'
alias ls='ls --color=auto'

alias cd.='cd ..'
alias cd..='cd ../../'
alias cd...='cd ../../../'
alias cd....='cd ../../../../'
alias cd.....='cd ../../../../../'
alias cds='cd $SRC_DIR'
alias ll='ls -la'
alias llr='ls -lart'
alias mkdir='mkdir -p'
alias findname='find -L . -name'
alias ec='emacsclient -nw'
alias shippo='tail -200f'
alias gs='git status'
alias ethereal='wireshark'

# emacs magick. explicitly set a primary build directory for cargo
# when executed via emacs, so it doesn't conflict with the regular <project>/target dir.
# Note: It'd probably be better to have a per-project subjectory, but, meh, here we are ...
EMACS_CARGO_DIR=${XDG_CACHE_HOME}/emacs/cargo-builds
alias emacs="CARGO_TARGET_DIR=${EMACS_CARGO_DIR} emacs -nw"

# handy terminal "reset", without executing `reset`
# this will: Clear scrollback buffer, Move cursor to home position, Clear visible screen
alias clearall='echo -e "\033[3J\033[H\033[2J"'
