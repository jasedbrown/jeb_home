#################
## oh-my-zsh stuffs
export ZSH="$HOME/.oh-my-zsh"

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"
plugins=(git)
source $ZSH/oh-my-zsh.sh


#################
## jasobrown configuration
export JAVA_HOME=/opt/dev/java/cur_jdk
export PATH=$PATH:~/bin:/home/jasobrown/.local/bin:$JAVA_HOME/bin

alias cd.='cd ..'
alias cd..='cd ../../'
alias cd...='cd ../../../'
alias cd....='cd ../../../../'
alias cd.....='cd ../../../../../'
alias cds='cd /opt/dev'
alias cdr='cd /opt/dev/readyset'
alias ll='ls -la'
alias llr='ls -lart'
alias mkdir='mkdir -p'
alias findname='find -L . -name'
alias ec='emacsclient'
alias shippo='tail -200f'
alias gs='git status'
alias ethereal='wireshark'
alias gviz='xclip -out -selection clipboard | dot -Tpng | feh -'

# emacs magick
EMACS_CONFIG_HOME=/opt/dev/jeb_home/home/.emacs.d/jasobrown
alias emacs="/usr/local/bin/emacs -nw -q --load ${EMACS_CONFIG_HOME}/standalone.el"
alias rmacs="EMACS_LANG=rust emacs"
alias jmacs="EMACS_LANG=java emacs"

# use plists in lsp-mode (https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization)
# to avoid annoying "Error running timer ‘lsp-lens-refresh’: (wrong-type-argument hash-table-p ..."
export LSP_USE_PLISTS=true

# jfc, i hate shared history
unsetopt share_history

# disable readyset telemetry
export RS_API_KEY="readyset_internal"

ulimit -n unlimited
