{ buildUnity, fetchGog, unzip }:

buildUnity {
  name = "wizard-of-legend";
  fullName = "WizardOfLegend";
  saveDir = "Contingent99/Wizard of Legend";
  version = "1.033";

  src = fetchGog {
    productId = 2061814323;
    downloadName = "en3installer0";
    sha256 = "1gbj0znfw87n2kq76vbchb3r2pyrl0wmdcjrvyr11b9igvqakv48";
  };

  unpackCmd = "${unzip}/bin/unzip -qq \"$curSrc\" 'data/noarch/game/*' || :";
}
