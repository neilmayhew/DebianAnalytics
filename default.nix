{ stdenv, mkDerivation,
    attoparsec, base, blaze-html, bytestring, debian, filepath, hashable,
    HUnit, HTTP, iproute, MissingH, network, network-uri, time, time-locale-compat,
    unordered-containers
}:
mkDerivation {
  pname = "DebianAnalytics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec  base  blaze-html  bytestring  debian  filepath  hashable
    HUnit  HTTP  iproute  MissingH  network  network-uri  time  time-locale-compat
    unordered-containers
  ];
  description = "Analyze apache log files from a Debian repo";
  license = stdenv.lib.licenses.gpl3;
}
