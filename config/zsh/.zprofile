ulimit -n unlimited

############
# PL sdk/env managers
# chatgpt recommends putting these into the .zprofile file
# as they change the session

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

source "$HOME/.cargo/env"
