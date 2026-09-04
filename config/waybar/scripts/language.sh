#!/bin/bash
# Report the active fcitx5 input method to waybar.
#
# Shows A / あ rather than EN / JP: that is the idiom fcitx5's own tray and
# every desktop IME uses, and unlike a flag emoji it is monochrome, so it
# inherits the chip's colours. (Nothing installed here covers regional
# indicators anyway -- flags would render as tofu.)
#
# Emits JSON so the module carries a CSS class per language, and so a dead
# fcitx5 shows as such instead of silently claiming English forever -- which is
# what the previous version did.

im=$(fcitx5-remote -n 2>/dev/null)

if [[ -z $im ]]; then
    printf '{"text":"×","class":"off","tooltip":"fcitx5 is not running"}\n'
    exit 0
fi

case "$im" in
    mozc*)
        printf '{"text":"あ","class":"jp","tooltip":"Japanese (Mozc)"}\n'
        ;;
    keyboard-us)
        printf '{"text":"A","class":"en","tooltip":"English (US)"}\n'
        ;;
    *)
        # some other input method got added; show it as non-Japanese
        printf '{"text":"A","class":"en","tooltip":"%s"}\n' "$im"
        ;;
esac
