#!/usr/bin/env zsh
##------------------------------------------------------------------------------
## ~/.unixrc/.zshrc - Main ZSH Configuration Entry Point
##
## This file serves as the main entry point for ZSH configuration.
## It loads modular configuration files from the config directory.
##
## Structure:
##   config/core/         - Core zsh settings, e.g. options, completion, etc.
##   config/environment/  - Environment variables, path, alias, etc
##   config/development/  - Development environment setups
##   config/platform/     - Platform-specific configurations
##   config/hosts/        - Host-specific configurations
##------------------------------------------------------------------------------

# Configuration directory
export ZSHRC_CONFIG_DIR="$HOME/.unixrc/zshrc.d"


##------------------------------------------------------------------------------
## Load general configuration modules in order.
##------------------------------------------------------------------------------

# Helper function to safely source files
_source_if_exists() {
  [[ -r "$1" ]] && source "$1"
}

# ZSH settings, must load first
_source_if_exists "$ZSHRC_CONFIG_DIR/core/options.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/core/completion.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/core/ohmyzsh.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/core/keybindings.zsh" # must load after oh-my-zsh

# Environment setup, e.g. expoerts, aliases.
_source_if_exists "$ZSHRC_CONFIG_DIR/environment/exports.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/environment/docker.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/environment/general.zsh"

# Development environments.
_source_if_exists "$ZSHRC_CONFIG_DIR/development/go.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/development/python.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/development/node.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/development/ruby.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/development/java.zsh"
_source_if_exists "$ZSHRC_CONFIG_DIR/development/cloud.zsh"

# Additional tools and completions
_source_if_exists "$HOME/.unixrc/tools/z/z.sh"  # Enable z.sh


##------------------------------------------------------------------------------
## Load specific configuration override in order.
##------------------------------------------------------------------------------

# Platform-specific configurations
case "$(uname)" in
  Darwin) _source_if_exists "$ZSHRC_CONFIG_DIR/platform/macos.zsh" ;;
  Linux)  _source_if_exists "$ZSHRC_CONFIG_DIR/platform/linux.zsh" ;;
esac

# Host-specific configurations (removed obsolete hosts)
case "$(hostname)" in
  "Deyuans-MacBook-M1") _source_if_exists "$ZSHRC_CONFIG_DIR/hosts/macbook-m1.zsh" ;;
  "Deyuans-MacBook-M3") _source_if_exists "$ZSHRC_CONFIG_DIR/hosts/macbook-m3.zsh" ;;
  "watermelon")         _source_if_exists "$ZSHRC_CONFIG_DIR/hosts/watermelon.zsh" ;;
esac

# Local customizations (not tracked in git)
_source_if_exists "$ZSHRC_CONFIG_DIR/local.zsh"

# Clean up helper function
unfunction _source_if_exists
