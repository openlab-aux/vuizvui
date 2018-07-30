{ buildUnity, fetchGog }:

buildUnity {
  name = "wizard-of-legend";
  fullName = "WizardOfLegend";
  saveDir = "Contingent99/Wizard of Legend";
  version = "1.033b";

  src = fetchGog {
    productId = 2061814323;
    downloadName = "en3installer0";
    sha256 = "192fhway7ij5f4fh0vb1204f3yg3fxz08fvqlg03gskjs9krcbcz";
  };
}
