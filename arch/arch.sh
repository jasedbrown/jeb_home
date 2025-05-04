echo "Installing core packages..."
sudo pacman -S --needed --noconfirm - < arch/packages.txt

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
paru -S --needed --noconfirm - < arch/aur-packages.txt

echo "Installing system76 drivers..."
./arch/system76.sh

echo "Installing systemctl stuffs..."
sudo systemctl enable bluetooth.service
sudo systemctl enable NetworkManager.service
systemctl --user enable ssh-agent


# plymouth is used to display a splash screen during boot time.
# we need to be careful about adding it to the initramfs script.
#
# Note: any time you change the theme, the initrd must be rebuilt.
# If using a default theme:
#     plymouth-set-default-theme -R theme
# 
# https://wiki.ubuntu.com/Plymouth
# https://wiki.archlinux.org/title/Plymouth
echo "Installing plymouth ..."
HOOKS_LINE=$(grep '^HOOKS=' /etc/mkinitcpio.conf)

# If "plymouth" is already there, skip
if [[ "$HOOKS_LINE" != *"plymouth"* ]]; then
  # Insert 'plymouth' after 'udev'
  NEW_HOOKS_LINE=$(echo "$HOOKS_LINE" | sed -E 's/(udev)([^)]*)/\1 plymouth\2/')
  sudo sed -i "s|^HOOKS=.*|$NEW_HOOKS_LINE|" /etc/mkinitcpio.conf
fi

sudo mkinitcpio -P
# arch-logo should have been installed with the AUR packages.txt
sudo plymouth-set-default-theme -R arch-logo
