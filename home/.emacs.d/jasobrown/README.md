# emacs_config
yet another misguided attempt at upping my emacs config game ...

based on the wonderful [emacs-rust-config](https://github.com/rksm/emacs-rust-config) project.

# using

start emacs with the custom init files:

```
$ emacs -q --load <path_to_this_repo>/standalone.el
```

set a shell var when starting emacs, list this:

```
$ EMACS_LANG=java <emacs command from above>
```

for easiest results, create a shell alias:

```
alias rmacs="EMACS_LANG=rust emacs -q --load <path_to_this_repo>/standalone.el"
```

# hacking

ordering of config file loading is:

- `standalone.el`
- `jeb.el`
- programming language-specific config (like `rust.el`)
- `post-lang.el`
