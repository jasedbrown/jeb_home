# a place to dump all random env settings.
# primarily for PLs/SKDs.



# emacs: use plists in lsp-mode (https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization)
# to avoid annoying "Error running timer ‘lsp-lens-refresh’: (wrong-type-argument hash-table-p ..."
export LSP_USE_PLISTS=true

# for custom ripgrep behavior, need to set an env var pointing to the conf file.
# https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#configuration-file
export RIPGREP_CONFIG_PATH=$XDG_CONFIG_HOME/ripgrep/rg.conf

# set the ollama API address - this is the default, but tools that want to
# use ollama (like aider) want the value explicitly declared.
export OLLAMA_API_BASE=http://127.0.0.1:11434

# this is a hack to get rocksdb to compile on arch,
# which uses gcc-15 now.
export CXXFLAGS="$CXXFLAGS -include cstdint"


#######################
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

# nvm/nodejs - really only using for claude code (atm)
export NVM_DIR="$HOME/.config/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
