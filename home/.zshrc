#################
## oh-my-zsh stuffs
export ZSH="$HOME/.oh-my-zsh"

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"
plugins=(git)
source $ZSH/oh-my-zsh.sh


#################
## jasobrown configuration
export PATH=$PATH:~/bin:/home/jasobrown/.local/bin

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
alias ec='emacsclient -nw'
alias shippo='tail -200f'
alias gs='git status'
alias ethereal='wireshark'

# rust flags to make use of sccache
export RUSTC_WRAPPER=/home/jasobrown/.cargo/bin/sccache
export CARGO_INCREMENTAL=0

# emacs magick. explicitly set a primary build directory for cargo
# when executed via emacs, so it doesn't conflict with the regular <project>/target dir.
# Note: It'd probably be better to have a per-project subjectory, but, meh, here we are ...
EMACS_CARGO_DIR=~/.emacs.d/cargo-builds
alias emacs="CARGO_TARGET_DIR=${EMACS_CARGO_DIR} /usr/local/bin/emacs -nw"

export EDITOR="emacs -nw"

# use plists in lsp-mode (https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization)
# to avoid annoying "Error running timer ‘lsp-lens-refresh’: (wrong-type-argument hash-table-p ..."
export LSP_USE_PLISTS=true

# sdkman (for java jdk management)
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

#pyenv (for python sdk and virtualenv management)
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init - zsh)"

# jfc, i hate shared history
unsetopt share_history

# disable readyset telemetry
export DISABLE_TELEMETRY="true"

ulimit -n unlimited
