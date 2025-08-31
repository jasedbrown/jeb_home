# jasobrown dotfiles 

Welcome to the next reincarnation of my home files/configuration management.
If I'm writing this, it means I've hit linux config bankrupcy once again ...
This time, I'm basing things on the new-fangled "dotfiles" trend - it seems sane
and reproducable, and is predicated on the [XDG base directory spec](https://specifications.freedesktop.org/basedir-spec/latest/)

I'm also trying out arch linux, cuz clearly I have a lot of free time :shrug:
Thus, this repo has install scripts for both pop!_os and arch linux. It is
hoped the scripts are idempotent to be rerun after each update (I'm not sure
how achievable that is in practice, but that's the hope).

## arch linux install

1. new system - [helpful steps](https://linuxiac.com/arch-linux-install/)
instead of kde, `pacman -S sddm hyprland wofi firefox git networkmanager kitty`
just install kitty as hyprland expects it by default (it seems _fine_ ...)

2. install [paru](https://github.com/Morganamilo/paru) - it helps instal AUR packages
requires a pacman-installed rust, so your user-level rustup install won't work :facepalm:

3. system76 drivers - [docs](https://support.system76.com/articles/system76-driver/#arch---using-an-aur-helper)
after paru is installed, this is reasonably easy (just downloads more rust :shrug:)

## pop!_os install

Apparently I couldn't give up on this, so I asked Claude to help translate the arch stuffs for pop!,
as well as incorporating my old setup scripts.

## random notes/links
useful stuff

* taking [screenshots](https://itsfoss.com/taking-screenshots-hyprland) via [grimblast](https://github.com/hyprwm/contrib/blob/main/grimblast/grimblast)

* wifi: networkmanager's `nmcli`
** `nmcli device wifi list` - to list all wifi networks
** `nmcli device wifi connect _SSID_ password _pw_` to connect to a network

NOTE: you can also use `impala` TUI; need `iwd` running as systemd service

* printing (cups/avavhi)
** to discover network things (including printers): `avahi-discover`

* bluetooth
** use `bluetoothctl` to discover and pair other devices.

* Japanese language input
** IME: input method editor. Use `mozc`
** IMF: input method framework, allows you to switch IMEs. Use `fcitx5`
*** use `fcitx5-configtool` to install a Japaneses keyboard
** Add env config - I've added it to the `hyprland` config so it gets loaded with UI, but not dependent on a shell (like zsh).
** `fcitx5-remote -t` will turn it on and off.
