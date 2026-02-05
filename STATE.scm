;; STATE.scm - RSR State File
;; SPDX-License-Identifier: MPL-2.0-or-later
;; Copyright (C) 2025 Jonathan D.A. Jewell
;;
;; This file tracks the current state of the project using S-expressions.
;; It is machine-readable and used by RSR tooling for validation.

(state
  (version . "0.1.0")
  (phase . "alpha")
  (updated . "2026-02-05")

  (project
    (name . "must")
    (tier . "infrastructure")
    (license . "MPL-2.0")
    (license-note . "PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem")
    (language . "ada"))

  (compliance
    (rsr . #t)
    (security-hardened . #t)
    (ci-cd . #t)
    (guix-primary . #f)
    (nix-fallback . #f))

  (artifacts
    (binary . "bin/must")
    (container . "ghcr.io/hyperpolymath/must:latest"))

  (dependencies
    (build
      ("gnat" . ">=12")
      ("gprbuild" . ">=22"))
    (runtime))

  (milestones
    (v0.1.0
      (status . "released")
      (date . "2025-12-27")
      (features
        "Task runner"
        "Template engine"
        "Requirements enforcer"
        "Deploy command"))
    (v0.2.0
      (status . "planned")
      (features
        "Mustache partials"
        "Content requirement checks"
        "TOML variable loading")))

  (spark-conversion
    (status . "complete")
    (completion-date . "2026-02-05")
    (modules-converted
      "must_types"
      "cli_parser"
      "must.adb"
      "task_runner"
      "requirement_checker"
      "mustfile_loader"
      "mustache_engine"
      "toml_parser"
      "deployer")
    (result
      (compilation-errors . 0)
      (compilation-warnings . 0)
      (memory-safety . "all strings bounded, stack-only allocation")
      (type-safety . "explicit conversions, type predicates enforced")
      (spark-ready . #t)))

  (session-history
    ((date . "2026-02-05")
     (milestone . "SPARK Conversion Complete")
     (actions
       ("Converted all 9 modules to bounded strings for memory safety"
        "Removed all heap allocations - stack-only memory"
        "Added explicit type conversions for Path/Command/Description/String"
        "Fixed license headers (AGPL → MPL-2.0) across all files"
        "Implemented helper functions for String→Bounded_String key lookups"
        "Added auto-truncating error messages"
        "Fixed predicate issue in Mustfile_Config"
        "Removed build artifacts from git tracking"
        "Full build success with zero errors, zero warnings"
        "Runtime verification: must --help and --version working"
        "Created comprehensive documentation: SPARK-CONVERSION-COMPLETE.md")))))
