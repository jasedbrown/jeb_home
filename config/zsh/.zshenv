# my good ol' source code, developer home
export SRC_DIR="/opt/dev"

# a place to persist data from docker instances (like databases)
export DOCKER_DATA_DIR="/var/lib/docker"

# rust flags to make use of sccache
export RUSTC_WRAPPER=/home/jasobrown/.cargo/bin/sccache
export CARGO_INCREMENTAL=0

export EDITOR="emacs -nw"

# emacs: use plists in lsp-mode (https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization)
# to avoid annoying "Error running timer ‘lsp-lens-refresh’: (wrong-type-argument hash-table-p ..."
export LSP_USE_PLISTS=true

# this is for the systemd ssh-agent
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket

# for custom ripgrep behavior, need to set an env var pointing to the conf file.
# https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#configuration-file
export RIPGREP_CONFIG_PATH=$XDG_CONFIG_HOME/ripgrep/rg.conf
