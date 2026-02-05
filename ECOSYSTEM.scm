;; SPDX-License-Identifier: MPL-2.0-or-later
;; ECOSYSTEM.scm - Project ecosystem positioning

(ecosystem
  ((version . "1.0.0")
   (name . "must")
   (type . "implementation")
   (purpose . "Implementation of the Mustfile specification - the engine that executes deployment state transitions")
   (position-in-ecosystem . "execution-layer")
   (language . "Ada 2022")
   (status . "v0.1.0-alpha")
   (related-projects
     ((mustfile . "specification-implemented")
      (just . "sibling-tool")
      (nickel . "configuration-parser")
      (_pathroot . "integration-consumer")
      (rhodium-standard . "sibling-standard")
      (git-hud . "infrastructure")))
   (what-this-is . ("The must binary (Ada implementation)"
                    "CLI engine for executing Mustfile contracts"
                    "Deployment orchestration runtime"
                    "Physical state enforcement engine"))
   (what-this-is-not . ("The Mustfile format specification (see hyperpolymath/mustfile)"
                        "A configuration language"
                        "A build system"))
   (relationship-to-mustfile . "This repo implements HOW to execute Mustfiles; the mustfile repo defines WHAT they are")
   (analogy . "must:Mustfile :: just:Justfile")))
