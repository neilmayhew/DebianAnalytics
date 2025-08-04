{ mkDerivation, lib,
    attoparsec, base, blaze-html, bytestring, containers, criterion, debian,
    deepseq, filepath, hashable, HTTP, HUnit, iproute, MissingH, network,
    network-uri, terminal-size, time, time-locale-compat, unordered-containers
}:
mkDerivation {
  pname = "DebianAnalytics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec  base  blaze-html  bytestring  containers  criterion  debian
    deepseq  filepath  hashable  HTTP  HUnit  iproute  MissingH  network
    network-uri  terminal-size  time  time-locale-compat  unordered-containers
  ];
  description = "Analyze apache log files from a Debian repo";
  license = lib.licenses.gpl3;
}
