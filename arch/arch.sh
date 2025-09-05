echo "Installing core packages..."
grep -v "^#" ./arch/packages.txt | xargs sudo pacman -Sy --needed --noconfirm


echo "Installing AUR helper..."
if ! command -v paru &> /dev/null; then
    echo "Installing paru..."
    git clone https://aur.archlinux.org/paru.git
    cd paru
    makepkg -si --noconfirm
    cd ..
    rm -rf paru
fi

echo "Installing AUR packages..."
grep -v "^#" ./arch/aur-packages.txt | xargs paru -Sy --needed --noconfirm

echo "Installing system76 drivers..."
./arch/system76.sh

echo "Installing systemctl stuffs..."

# Network services
if command -v NetworkManager &> /dev/null; then
    sudo systemctl enable --now NetworkManager.service
fi

# Network services
if command -v iwctl &> /dev/null; then
    sudo systemctl enable --now iwd.service
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

# D-Bus (required for many services)
sudo systemctl enable --now dbus.service
sudo systemctl enable --now dbus-broker.service

# SSH agent (user service)
if command -v ssh-agent &> /dev/null; then
    systemctl --user enable --now ssh-agent
fi

# enable pipewire for audio
sudo systemctl enable --now pipewire.service

# set up NTP time sync
sudo timedatectl set-ntp true

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

