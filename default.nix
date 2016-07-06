{ mkDerivation, attoparsec, base, blaze-html, bytestring, filepath
, hashable, HTTP, MissingH, network, network-uri, stdenv, unordered-containers
}:
mkDerivation {
  pname = "DebianAnalytics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base blaze-html bytestring filepath hashable HTTP
    MissingH network network-uri unordered-containers
  ];
  description = "Analyze apache log files from a Debian repo";
  license = stdenv.lib.licenses.gpl3;
}
