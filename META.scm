;; SPDX-License-Identifier: MPL-2.0-or-later
;; META.scm - Project metadata and architectural decisions

(define project-meta
  `((version . "1.0.0")
    (project-type . "implementation")
    (architecture-decisions
      ((adr-001
         (title . "Ada 2022 for implementation language")
         (status . "accepted")
         (date . "2025-12-27")
         (rationale . "Safety-critical deployment, formal verification, strong typing")
         (alternatives . "Rust, OCaml"))
       (adr-002
         (title . "Separate specification from implementation")
         (status . "accepted")
         (date . "2026-02-05")
         (rationale . "Following just/Justfile pattern: specification defines WHAT, implementation defines HOW")
         (consequences . "Implementation repo (must) contains Ada code, specification repo (mustfile) contains docs/examples"))))
    (development-practices
      ((code-style . "ada2022")
       (security . "openssf-scorecard")
       (testing . "property-based")
       (versioning . "semver")
       (documentation . "asciidoc")
       (branching . "trunk-based")))
    (design-rationale
      ((ada-choice
         (description . "Ada provides safety guarantees for deployment orchestration")
         (benefits . "Strong typing, proven reliability, formal verification"))
       (just-analogy
         (description . "must:Mustfile :: just:Justfile relationship")
         (benefits . "Clear separation of concerns, familiar mental model"))))))
