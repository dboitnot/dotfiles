{
  programs.i3status = {
    enable = true;
  };

  services.picom = {
    enable = true;
  };

  xsession = {
    enable = true;
    windowManager.i3 = {
      enable = true;
      config = null;
    };
  };
}
