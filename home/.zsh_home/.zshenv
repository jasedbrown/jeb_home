# rust flags to make use of sccache
export RUSTC_WRAPPER=/home/jasobrown/.cargo/bin/sccache
export CARGO_INCREMENTAL=0

export PATH=$PATH:~/bin:/home/jasobrown/.local/bin

export EDITOR="emacs -nw"

# use plists in lsp-mode (https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization)
# to avoid annoying "Error running timer ‘lsp-lens-refresh’: (wrong-type-argument hash-table-p ..."
export LSP_USE_PLISTS=true

# disable readyset telemetry
export DISABLE_TELEMETRY="true"
