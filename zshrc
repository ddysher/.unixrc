#!/usr/bin/env zsh
##------------------------------------------------------------------------------
## ~/.unixrc/.zshrc - Main ZSH Configuration Entry Point
##
## This file serves as the main entry point for ZSH configuration.
## It loads modular configuration files from the config directory.
##
## Structure:
##   config/core/         - Core zsh settings (options, completion, keybindings)
##   config/environment/  - Environment variables and PATH management
##   config/aliases/      - Aliases and custom functions
##   config/development/  - Development environment setups
##   config/platform/     - Platform-specific configurations
##   config/hosts/        - Host-specific configurations
##------------------------------------------------------------------------------

# Configuration directory
export ZSHRC_CONFIG_DIR="$HOME/.unixrc/zshrc.d"

# Global switches - can be overridden in environment
export CLOUD_ENV=${CLOUD_ENV:-"false"}  # Enable/disable cloud tools

##------------------------------------------------------------------------------
## Load configuration modules in order
##------------------------------------------------------------------------------

# Helper function to safely source files
_source_if_exists() {
  [[ -r "$1" ]] && source "$1"
}

# 1. Core ZSH settings (must be loaded first)
_source_if_exists "$ZSHRC_CONFIG_DIR/core/options.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/core/completion.zsh"

# 2. Environment setup (PATH, exports, etc.)
_source_if_exists "$ZSHRC_CONFIG_DIR/environment/homebrew.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/environment/path.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/environment/exports.zsh"

# 3. Oh My Zsh setup
_source_if_exists "$ZSHRC_CONFIG_DIR/core/ohmyzsh.zsh"

# 4. Platform-specific configurations
case "$(uname)" in
  Darwin) _source_if_exists "$ZSHRC_CONFIG_DIR/platform/macos.zsh" ;;
  Linux)  _source_if_exists "$ZSHRC_CONFIG_DIR/platform/linux.zsh" ;;
esac

# 5. Host-specific configurations
case "$(hostname)" in
  "Deyuans-Macbook-Air.local")                   _source_if_exists "$ZSHRC_CONFIG_DIR/hosts/macbook-air.zsh" ;;
  "Deyuans-MacBook-Pro.local")                   _source_if_exists "$ZSHRC_CONFIG_DIR/hosts/macbook-pro.zsh" ;;
  "Deyuans-MacBook-M1")                          _source_if_exists "$ZSHRC_CONFIG_DIR/hosts/macbook-m1.zsh" ;;
  "Deyuans-MacBook-M3")                          _source_if_exists "$ZSHRC_CONFIG_DIR/hosts/macbook-m3.zsh" ;;
  "deyuan.pit.corp.google.com")                  _source_if_exists "$ZSHRC_CONFIG_DIR/hosts/google-pit.zsh" ;;
  "deyuan-macbookpro.roam.corp.google.com")      _source_if_exists "$ZSHRC_CONFIG_DIR/hosts/google-roam.zsh" ;;
  "watermelon")                                  _source_if_exists "$ZSHRC_CONFIG_DIR/hosts/watermelon.zsh" ;;
esac

# 6. Development environments
_source_if_exists "$ZSHRC_CONFIG_DIR/development/go.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/development/python.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/development/node.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/development/ruby.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/development/java.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/development/cloud.zsh"

# 7. Aliases and functions
_source_if_exists "$ZSHRC_CONFIG_DIR/aliases/general.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/aliases/docker.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/aliases/functions.zsh"

# 8. Key bindings (must be loaded after oh-my-zsh)
_source_if_exists "$ZSHRC_CONFIG_DIR/core/keybindings.zsh"

# 9. Additional tools and completions
_source_if_exists "$HOME/.unixrc/tools/z/z.sh"  # Enable z.sh

# 10. Local customizations (not tracked in git)
_source_if_exists "$ZSHRC_CONFIG_DIR/local.zsh"

# Clean up helper function
unfunction _source_if_exists
