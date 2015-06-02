{ mkDerivation, base, containers, directory, filepath, fsnotify
, stdenv
}:
mkDerivation {
  pname = "mySortMaildir";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ base containers directory filepath fsnotify ];
  description = "A small programm to sort maildirs";
  license = stdenv.lib.licenses.bsd3;
}
