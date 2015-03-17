{
  machines = {
    heinrich   = import ./machines/heinrich.nix;
    labtop     = import ./machines/labtop.nix;
    mailserver = import ./machines/mailserver.nix;
  };
}
