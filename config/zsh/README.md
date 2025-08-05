# jasobrown's zsh config

Here's what I came up with when tag teaming with chatgpt:

```
./.zsh
├── .zshenv                # Minimal, global settings (always sourced)
├── .zprofile              # Login shell settings (env vars, PATH), not read in interactive shells
├── .zshrc                 # Interactive shell settings (aliases, prompt, completion)
├── .zlogout               # Cleanup tasks on logout (optional)
└── .zsh/                  # Modular config directory
    ├── aliases.zsh        # Alias definitions
    ├── environment.zsh    # Extra environment variables
    ├── functions.zsh      # Custom shell functions
    ├── prompt.zsh         # Prompt configuration
    ├── completions.zsh    # Completion tweaks
    ├── keybindings.zsh    # Custom keybindings
    ├── history.zsh        # History settings
    ├── options.zsh        # Shell options (setopt, unsetopt)
    └── plugins.zsh        # Plugin management (if using zsh plugins)
```

Some of these I'm not using, but i like the recommendations, so I'll try to follow as much as I like :P

The real thing to know is that:
* .zshenv is read first for all ZSH shells
* .zprofile is read for SSH _interactive_ shells
* .zshrc is read next

The trick is we need to tell ZSH where the $ZDOTDIR is located before zsh starts loading. It does not automatically check the `$XDG_CONFIG_HOME`.
Thus, we need a bit of a hack: we symlink the local `.zshenv` to the `$HOME` dir, which sets up the `$ZDOTDIR`. Enverything else can work
as-expected after that. This should ensure both local shells and interactive ssh shells work as I want them.
