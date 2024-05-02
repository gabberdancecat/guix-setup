# This is configured in Desktop.org
# To update: nix-env -iA nixpkgs.myPackages
# with import <nixpkgs> {}; {
{
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        # librewolf
        # signal-desktop
        # rust-analyzer-unwrapped
        # rust-analyzer
        # clippy
        # rustfmt # do i need this?
        # mullvad-vpn
        # gh # github-cli
        # ghidra-bin # hacking
        # mysql-workbench # python backend
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" ];
      extraOutputsToInstall = [ "man" "doc" ];
    };
  };
  programs.firejail = pkgs: with pkgs; {
    enable = true;
    wrappedBinaries = {
      librewolf = {
        executable = "${pkgs.librewolf}/bin/librewolf";
        profile = "${pkgs.firejail}/etc/firejail/librewolf.profile";
        extraArgs = [
          # Required for U2F USB stick
          "--ignore=private-dev"
          # Enforce dark mode
          "--env=GTK_THEME=Adwaita:dark"
          # Enable system notifications
          "--dbus-user.talk=org.freedesktop.Notifications"
        ];
      };
      signal-desktop = {
        executable = "${pkgs.signal-desktop}/bin/signal-desktop";
      profile = "${pkgs.firejail}/etc/firejail/signal-desktop.profile";
      extraArgs = [
        # FIXME
        "--env=LC_ALL=C"
      ];
      };
    };
  };
  fonts.fonts = pkgs: with pkgs; [
    liberation_ttf
    fira-code
    fira-code-symbols
  ];
}
