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

if [ -f /etc/arch-release ]; then
    # This is a hack to get rocksdb to compile on Arch, which uses gcc-15 now.
    export CXXFLAGS="$CXXFLAGS -include cstdint"
fi


#######################
# PL sdk/env managers

# this adds the cargo bin dir to the path
if [ -r "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

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

# pyenv (for python sdk and virtualenv management) - lazy loaded
export PYENV_ROOT="$HOME/.pyenv"
if [ -d "$PYENV_ROOT" ]; then
    # Keep pyenv-managed Python binaries available without paying for pyenv init
    # during every shell startup.
    export PATH="$PYENV_ROOT/bin:$PYENV_ROOT/shims:$PATH"
fi
if (( $+commands[pyenv] )); then
    function _load_pyenv() {
        unfunction pyenv 2>/dev/null
        # --no-rehash avoids startup-time shim regeneration; run `pyenv rehash`
        # manually after installing new Python executables.
        eval "$(command pyenv init - --no-rehash --no-push-path zsh)"
        if command pyenv commands 2>/dev/null | command grep -q '^virtualenv-init$'; then
            eval "$(command pyenv virtualenv-init - zsh)"
        fi
    }
    function pyenv() {
        _load_pyenv
        pyenv "$@"
    }
fi

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
