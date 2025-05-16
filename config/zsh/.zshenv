ulimit -n unlimited

# my good ol' source code, developer home
export SRC_HOME="/opt/dev"

# rust flags to make use of sccache
export RUSTC_WRAPPER=/home/jasobrown/.cargo/bin/sccache
export CARGO_INCREMENTAL=0

export EDITOR="emacs -nw"

# emacs: use plists in lsp-mode (https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization)
# to avoid annoying "Error running timer ‘lsp-lens-refresh’: (wrong-type-argument hash-table-p ..."
export LSP_USE_PLISTS=true

# only explicitly set up the ssh-agent socket if we're on arch
if [ -f /etc/arch-release ]; then
    export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
fi

# for custom ripgrep behavior, need to set an env var pointing to the conf file.
# https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#configuration-file
export RIPGREP_CONFIG_PATH=$XDG_CONFIG_HOME/ripgrep/rg.conf



############
# PL sdk/env managers

# this adds the cargo bin dir to the path
source "$HOME/.cargo/env"

# sdkman (for java jdk management)
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# pyenv (for python sdk and virtualenv management)
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init - zsh)"

# rbenv (for ruby sdk and env management)
# export PATH="$HOME/.rbenv/bin:$PATH"
# eval "$(rbenv init - --no-rehash bash)"

# golang (<sigh>)
## GOROOT -location where your Go installation (compiler, tools, standard library) lives
## GOPATH - root of your Go workspace - where Go code, downloaded dependencies, and compiled binaries are stored
# export GOROOT=/usr/local/go/current
# export GOPATH=$HOME/.local/share/gopath
# export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

. "$HOME/.cargo/env"
