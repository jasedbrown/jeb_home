# A collection of my simple functions to make zsh life easier
#
# tip: when you need to `cd` and don't want to change your cwd,
# use parentheses to create a subshell as it will just change
# directories for the duration of the function execution.
# you need to include all relevant functionality in those parens.

# Function to start docker compose
dockerup() {
  (cd $SRC_DIR/jeb-configs/docker && docker compose up -d)
}

# Function to shut down docker compose
dockerdown() {
  (cd $SRC_DIR/jeb-configs/docker && docker compose down)
}
