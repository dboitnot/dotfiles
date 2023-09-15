{ config, pkgs ? import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz";
    sha256 = "0vwsc34lfr0xn92azqg6pdf8hhksxbdw82kmfrs1nf0sks6l5hyx";
}) {}, ... }:

let
  confDPath = ~/.config/home-manager/conf.d;
  confDFiles = builtins.filter (f: builtins.match ".*\\.nix" f != null) (builtins.attrNames (builtins.readDir confDPath));
  importedConfs = map (f: import (confDPath + "/${f}")) confDFiles;
in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "dboitnot";
  home.homeDirectory = "/home/dboitnot";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  fonts.fontconfig.enable = true;

  # atuin provides a fuzzy finder for command line history.
  programs.atuin = {
    enable = true;
    settings = {
      search_mode = "skim";
      show_preview = true;
    };
    flags = ["--disable-up-arrow"];
  };

  programs.bash = {
    enable = true;
  };

  # bashmount is a tool to mount and unmount removable media from the CLI
  programs.bashmount.enable = true;

  # direnv is a tool to load/unload environment variables based on the current
  # directory.
  programs.direnv.enable = true;

  # eza is a modern replacement for ls.
  # programs.eza = {
  #   enable = true;
  #   enableAliases = true;
  #   git = true;
  #   icons = true;
  # };

  programs.firefox = {
    enable = true;
  };

  # tealdeer is a command line client for tldr-pages. To use it, run:
  #   $ tldr <command>
  programs.tealdeer.enable = true;

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')


    # General

    pkgs.git pkgs.ripgrep pkgs.fd pkgs.fira-code pkgs.powerline-symbols
    pkgs.stow pkgs.dnsutils pkgs.jq pkgs.hunspellDicts.en_US-large
    pkgs.autorandr pkgs._1password pkgs._1password-gui pkgs.zoom-us

    # Development tools for emacs
    pkgs.cmake pkgs.clang pkgs.gnumake pkgs.terraform pkgs.irony-server
    pkgs.rustup pkgs.ansible pkgs.black

    # AWS
    pkgs.awscli2 pkgs.cw
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/dboitnot/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    EDITOR = "vim";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  imports = importedConfs;
}
