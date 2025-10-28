# note: .zprofile is read specifically during login shells, but not
# in interactive non-login shells like most terminal emulator sessions.
# Add user-local bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
