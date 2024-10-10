(use-modules (guix)
             (guix utils)
             (guix gexp)
             (gnu packages haskell-web)
             (gnu packages haskell-xyz)
             (guix build-system haskell)
             ((guix licenses) #:prefix license:))

(define-public ghc-lens-aeson
  (package
    (name "ghc-lens-aeson")
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lens-aeson" version))
       (sha256
        (base32 "00ac8anw6a3alwlqqvbr1vp7brajrdp66ximl7ylvj28wbznmg3v"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lens-aeson")))
    (inputs (list ghc-lens
                  ghc-text-short
                  ghc-vector
                  ghc-unordered-containers
                  ghc-aeson
                  ghc-scientific))
    (arguments
     `(#:cabal-revision ("2"
                         "1mw2ijrdkkpazgnfb1msahgf1dlygrcl2i3hi4g7vqf8b95knwss")))
    (home-page "http://github.com/lens/lens-aeson/")
    (synopsis "Law-abiding lenses for aeson")
    (description "Law-abiding lenses for aeson.")
    (license license:expat)))

(package
 (name "ast2hsc")
 (version "0.1.0.0")
 (source (local-file "." "root"
                     #:recursive? #t))
 (native-inputs (list ghc-aeson
                      ghc-lens
                      ghc-lens-aeson
		      ghc-pretty-simple))
 (build-system haskell-build-system)
 (home-page "nil")
 (synopsis "nil")
 (description "nil")
 (license license:asl2.0))
