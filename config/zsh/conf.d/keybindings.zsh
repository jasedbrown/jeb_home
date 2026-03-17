# https://zsh.sourceforge.io/Doc/Release/Zsh-Line-Editor.html

# force the emacs key ZLE bindings
bindkey -e

# a more emacs-like keybinding for backward-kill-word
WORDCHARS=''
bindkey '^[^?' backward-kill-word

# force the Delete key to delete the next char, and not print a `~`.
bindkey '^[[3~' delete-char
