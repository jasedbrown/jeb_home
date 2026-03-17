# a place to dump all random env settings.
# primarily for PLs/SKDs.

# this is to avoid a nag message from tmux
export DISABLE_AUTO_TITLE=true

# emacs: use plists in lsp-mode (https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization)
# to avoid annoying "Error running timer ‘lsp-lens-refresh’: (wrong-type-argument hash-table-p ..."
export LSP_USE_PLISTS=true

# for custom ripgrep behavior, need to set an env var pointing to the conf file.
# https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#configuration-file
export RIPGREP_CONFIG_PATH=$XDG_CONFIG_HOME/ripgrep/rg.conf

# this is a hack to get rocksdb to compile on arch,
# which uses gcc-15 now.
export CXXFLAGS="$CXXFLAGS -include cstdint"


#######################
# PL sdk/env managers

# this adds the cargo bin dir to the path
source "$HOME/.cargo/env"

# sdkman (for java jdk management) - lazy loaded
export SDKMAN_DIR="$HOME/.sdkman"
function sdk() {
    unfunction sdk java javac gradle mvn
    [[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]] && source "$SDKMAN_DIR/bin/sdkman-init.sh"
    sdk "$@"
}
function java javac gradle mvn() {
    unfunction sdk java javac gradle mvn
    [[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]] && source "$SDKMAN_DIR/bin/sdkman-init.sh"
    command "$0" "$@"
}

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

# nvm/nodejs - lazy loaded (claude code just needs node on PATH)
export NVM_DIR="$HOME/.config/nvm"
export PATH="$PATH:${XDG_DATA_HOME:-$HOME/.local/share}/npm/bin"
function nvm() {
    unfunction nvm
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    nvm "$@"
}

# bun - because apparently there are not enough fucking node.js package managers ...
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

