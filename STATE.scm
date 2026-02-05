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
    (status . "in-progress")
    (phase-1-complete . #t)
    (modules-converted
      "must_types (bounded strings, type predicates, contracts)")
    (next-modules
      "cli_parser, must.adb, mustfile_loader, task_runner, requirement_checker, mustache_engine, deployer"))

  (session-history
    ((date . "2026-02-05")
     (milestone . "SPARK Conversion - Phase 1")
     (actions
       ("Converted must_types to bounded strings for memory safety"
        "Added type predicates to Task_Def, Requirement_Def, Template_Def, Enforcement_Config"
        "Added Pre/Post conditions to conversion functions"
        "Fixed license headers (AGPL → MPL-2.0)"
        "Added GNATprove configuration to must.gpr"
        "Created SPARK-STATUS.md and conversion session docs"
        "must_types compiles successfully; rest of codebase needs conversion")))))
