# This is configured in Desktop.org
# To update: nix-env -iA nixpkgs.myPackages
# with import <nixpkgs> {}; {
let
  #
  # Define an overlay to create the new package variant
  overlays = [
    (self: super: {
      renoise344 = super.renoise.override {
        releasePath = /home/nya/Music/Prod/misc/rns_344_linux_x86_64.tar.gz;  # Replace with your correct path
      };
    })
  ];
  # Import Nixpkgs with the overlay applied
  myPkgs = import <nixpkgs> { overlays = overlays; };
  riverWrapper = myPkgs.writeShellScriptBin "river" ''
    export LD_LIBRARY_PATH="${myPkgs.addOpenGLRunpath}/lib:$LD_LIBRARY_PATH"
    exec ${myPkgs.river}/bin/river "$@"
  '';
in {
  allowUnfree = true;
  packageOverrides = pkgs: with myPkgs; {
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
	vesktop
	# metasploit
	# exploitdb
	# villain
	
	# (cutter.withPlugins (ps: with ps; [ rz-ghidra ])) # temporary remove
	# (rizin.withPlugins (ps: with ps; [ rz-ghidra ])) # temporary remove
	
	pwndbg
	gef
	redis
	gobuster
	openssl
	renoise344
	# riverWrapper
	hyprland
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/bin" ];
      extraOutputsToInstall = [ "man" "doc" ];
    };
  };
  # Overlay to modify only the `releasePath` of the Renoise package
  # nixpkgs.overlays = [
  #   (self: super: {
  #     renoise344 = super.renoise.overrideAttrs (old: {
  #       releasePath = "/home/nya/Downloads/rns_344_linux_x86_64.tar.gz";  # Replace with correct path
  #     });
  #   })
  # ];
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
