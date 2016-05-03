rec {
  callMachine = import ./call-machine.nix;
  callNetwork = import ./call-network.nix;
  getVuizvuiTests = import ./get-tests.nix;
}
