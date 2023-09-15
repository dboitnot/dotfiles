{ config, pkgs, lib, ... }:

let myEmacsPkg = pkgs.emacs28; in
{
  programs.emacs = {
    enable = true;
    package = myEmacsPkg;
  };

  services.emacs = {
    enable = true;
    package = myEmacsPkg;
    client.enable = true;
    socketActivation.enable = true;
  };

  # home.file.".config/emacs".source = builtins.fetchGit {
  #   url = "https://github.com/doomemacs/doomemacs";
  #   # rev = "branch or ref";
  # };

  home.sessionVariablesExtra = lib.mkAfter ''
    export PATH="$PATH:$HOME/.config/emacs/bin";
  '';
}
