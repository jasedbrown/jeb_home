# jasobrown dotfiles 

Welcome to the next reincarnation of my home files/configuration management.
If I'm writing this, it means I've hit linux config bankrupcy once again ...
This time, I'm basing things on the new-fangled "dotfiles" trend - it seems sane
and reproducable.

I'm also trying out arch linux, cuz clearly I have a lot of free time :shrug:

## arch linux install

1. new system - [helpful steps](https://linuxiac.com/arch-linux-install/)
instead of kde, `pacman -S sddm hyprland wofi firefox git networkmanager kitty`
just install kitty as hyprland expects it by default (it seems _fine_ ...)

2. install [paru](https://github.com/Morganamilo/paru) - it helps instal AUR packages
requires a pacman-installed rust, so your user-level rustup install won't work :facepalm:

3. system76 drivers - [docs](https://support.system76.com/articles/system76-driver/#arch---using-an-aur-helper)
after paru is installed, this is reasonably easy (just downloads more rust :shrug:)

## pop!_os install

Apparently I couldnt give up on this, so I asked Claude to help translate the arch stuffs for pop!,
as well as incorporating my old setup scripts.

## random links
useful stuff

* taking [screenshots](https://itsfoss.com/taking-screenshots-hyprland) via [grimblast](https://github.com/hyprwm/contrib/blob/main/grimblast/grimblast)
