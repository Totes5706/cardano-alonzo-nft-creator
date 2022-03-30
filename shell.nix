{ pure ? false
, source-repo-override ? { } }:
let
  packages = import ./. { inherit source-repo-override; };
  inherit (packages) pkgs plutus-apps plutus-starter;
  inherit (plutus-starter) haskell;
  
  cardano-node = import
   (pkgs.fetchgit {
     url = "https://github.com/input-output-hk/cardano-node";
     # A standard release compatible with the cardano-wallet commit above is always preferred.
     rev = "1.34.1";
     sha256 = "1hh53whcj5y9kw4qpkiza7rmkniz18r493vv4dzl1a8r5fy3b2bv";
   })
   { };

in
  haskell.project.shellFor {
    withHoogle = false;
    
    
    nativeBuildInputs = with plutus-starter; [
      hlint
      cabal-install
      cardano-node.cardano-cli
      cardano-node.cardano-node
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ] ++ (pkgs.lib.optionals pure [
      pkgs.git
      pkgs.cacert
      pkgs.curl
      pkgs.jq
    ]);
  }
