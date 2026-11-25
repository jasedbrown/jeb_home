# Always do a full sync + upgrade first; Arch doesn't support partial upgrades.
# Every later install uses `-S` (no `y`) so the package db isn't re-synced mid-run.
# No --noconfirm here so replacement/conflict prompts and pacman warnings are visible.
sudo pacman -Syu

# Install the packages listed in a file that aren't installed yet, using the
# given command. pacman and paru print an "is up to date -- skipping" line for
# every already-installed package (even with --needed and -q), so `pacman -T`
# filters those out first. It also sees AUR packages once they're installed.
#   usage: install_missing <list-file> <install command...>
install_missing() {
    local list="$1"
    shift
    local missing
    missing=$(grep -Ev '^[[:space:]]*(#|$)' "$list" | xargs -r pacman -T)
    if [ -n "$missing" ]; then
        # word splitting is intended: one argument per package
        "$@" $missing
    fi
}

install_missing ./arch/packages.txt sudo pacman -S --needed --noconfirm

# System76 components should never be installed on unrelated hardware.
IS_SYSTEM76=false
if grep -qi "System76" /sys/class/dmi/id/sys_vendor /sys/class/dmi/id/product_name 2>/dev/null; then
    IS_SYSTEM76=true
fi

if ! command -v paru &> /dev/null || ! paru --version &> /dev/null; then
    echo "Installing paru..."
    rm -rf /tmp/paru
    git clone https://aur.archlinux.org/paru.git /tmp/paru
    cd /tmp/paru
    makepkg -si --noconfirm
    cd - > /dev/null
fi

paru -Sua

install_missing ./arch/aur-packages.txt paru -S --needed --noconfirm

if [ "$IS_SYSTEM76" = true ]; then
    install_missing ./arch/packages-system76.txt sudo pacman -S --needed --noconfirm
    install_missing ./arch/aur-packages-system76.txt paru -S --needed --noconfirm
fi

# A real system battery (as opposed to a peripheral's, e.g. a mouse or
# keyboard) means this is a laptop. Desktops/mini-PCs (e.g. Meerkat, Thelio)
# have no BAT* entry here.
IS_LAPTOP=false
if compgen -G "/sys/class/power_supply/BAT*" &> /dev/null; then
    IS_LAPTOP=true
fi

if [ "$IS_SYSTEM76" = true ] && [ "$IS_LAPTOP" = true ]; then
    install_missing ./arch/aur-packages-laptop.txt paru -S --needed --noconfirm
fi

######
# Network services

if command -v NetworkManager &> /dev/null; then
    sudo systemctl enable --now NetworkManager.service
fi


# System76 drivers
if [ "$IS_SYSTEM76" = true ] && pacman -Qi system76-driver &> /dev/null; then
    sudo systemctl enable --now system76
fi

if [ "$IS_SYSTEM76" = true ] && pacman -Qi system76-firmware-daemon &> /dev/null; then
    sudo systemctl enable --now system76-firmware-daemon
    # gpasswd announces the add even when the user is already a member
    if ! id -nG "$USER" | grep -qw adm; then
        sudo gpasswd -a "$USER" adm
    fi
fi

if [ "$IS_SYSTEM76" = true ] && pacman -Qi system76-scheduler &> /dev/null; then
    sudo systemctl enable --now com.system76.Scheduler.service
fi

# Bluetooth (only if hardware exists)
if [ -d /sys/class/bluetooth ]; then
    sudo systemctl enable --now bluetooth.service
fi

# Printing (CUPS)
if command -v cups &> /dev/null; then
    sudo systemctl enable --now cups.service
fi

# Avahi (mDNS/DNS-SD)
if command -v avahi-daemon &> /dev/null; then
    sudo systemctl enable --now avahi-daemon.service
fi

# SSH agent (user service)
if command -v ssh-agent &> /dev/null; then
    systemctl --user enable --now ssh-agent
fi

# enable pipewire for audio
systemctl --user enable --now pipewire.service

# set up NTP time sync
sudo timedatectl set-ntp true

# weekly TRIM for SSDs/NVMe (avoids write-performance degradation over time)
sudo systemctl enable --now fstrim.timer

# plymouth is used to display a splash screen during boot time.
# we need to be careful about adding it to the initramfs script.
#
# Note: any time you change the theme, the initrd must be rebuilt.
# If using a default theme:
#     plymouth-set-default-theme -R theme
# 
# https://wiki.ubuntu.com/Plymouth
# https://wiki.archlinux.org/title/Plymouth
HOOKS_LINE=$(grep '^HOOKS=' /etc/mkinitcpio.conf)

# If "plymouth" is already there, skip
if [[ "$HOOKS_LINE" != *"plymouth"* ]]; then
  echo "Installing plymouth ..."
  # Insert 'plymouth' after 'udev'
  NEW_HOOKS_LINE=$(echo "$HOOKS_LINE" | sed -E 's/(udev)([^)]*)/\1 plymouth\2/')
  sudo sed -i "s|^HOOKS=.*|$NEW_HOOKS_LINE|" /etc/mkinitcpio.conf

  # not sure if this rebuild is necessary, as we'll do it in the next step 
  # (the `-R` flag to `plymouth-set-default-theme` will do it)
  sudo mkinitcpio -P

  # arch-logo should have been installed with the AUR packages.txt
  sudo plymouth-set-default-theme -R arch-logo
fi
