# https://zsh.sourceforge.io/Doc/Release/Zsh-Line-Editor.html

# force the emacs key ZLE bindings
bindkey -e

# a more emacs-like keybinding for backward-kill-word
WORDCHARS=''
bindkey '^[^?' backward-kill-word
