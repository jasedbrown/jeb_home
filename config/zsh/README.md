# jasobrown's zsh config

Here's what I came up with when tag teaming with chatgpt:

```
./.zsh
├── .zshenv                # Minimal, global settings (always sourced)
├── .zprofile              # Login shell settings (env vars, PATH)
├── .zshrc                 # Interactive shell settings (aliases, prompt, completion)
├── .zlogout               # Cleanup tasks on logout (optional)
└── .zsh/                  # Modular config directory
    ├── aliases.zsh        # Alias definitions
    ├── environment.zsh    # Extra environment variables (loaded from .zprofile)
    ├── functions.zsh      # Custom shell functions
    ├── prompt.zsh         # Prompt configuration
    ├── completions.zsh    # Completion tweaks
    ├── keybindings.zsh    # Custom keybindings
    ├── history.zsh        # History settings
    ├── options.zsh        # Shell options (setopt, unsetopt)
    └── plugins.zsh        # Plugin management (if using zsh plugins)
```

Some of these I'm not using, but i like the recommendations, so I'll try to follow as much as I like :P
