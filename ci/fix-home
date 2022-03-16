#!/usr/bin/env sh

# CI Steps on Linux (in the container) are run as root, while on macOS and Windows, they are not.
# And on GitHub Actions, environment variables from the host machine has a higher priority than those from a container,
# including user-specific variables like `USER`, `HOME`, etc.
#
# The following fixes the `HOME` value for CLI tools (primarily Stack) that expects a properly configured `HOME` value.
if [ "$(whoami)" = root ]; then
  HOME=/root "$@"
else
  "$@"
fi
