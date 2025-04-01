# Source all modular config files
for file in ~/.zsh/*.zsh; do
  [ -r "$file" ] && source "$file"
done
