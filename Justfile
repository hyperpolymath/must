# SPDX-License-Identifier: AGPL-3.0-or-later
# must - Authority-First State Orchestrator
# https://gitlab.com/hyperpolymath/must

set shell := ["bash", "-uc"]
set dotenv-load := true
set positional-arguments := true

# Project metadata
project := "must"
version := "0.1.0"
tier := "infrastructure"

# --- Default & Help ---
default:
    @just --list --unsorted

help recipe="":
    #!/usr/bin/env bash
    if [ -z "{{recipe}}" ]; then
        just --list --unsorted
        echo ""
        echo "Usage: just help <recipe>"
        echo "       just cookbook     # Generate docs/just-cookbook.adoc"
    else
        just --show "{{recipe}}" 2>/dev/null || echo "Recipe '{{recipe}}' not found"
    fi

info:
    @echo "Project: {{project}} (Rust)"
    @echo "Version: {{version}}"
    @echo "Tier: {{tier}}"
    @echo "Recipes: $(just --summary | wc -w)"

# --- Build & Compile (Rust) ---
build *args:
    @echo "🔧 Building must (debug)..."
    cargo build {{args}}

build-release *args:
    @echo "🔧 Building must (release)..."
    cargo build --release {{args}}

build-watch:
    @echo "👀 Watching for changes..."
    cargo watch -x build

clean:
    @echo "🧹 Cleaning..."
    cargo clean
    rm -rf target/.rustc_info.json target/debug/incremental target/release/incremental

clean-all: clean
    @echo "🧹 Deep cleaning..."
    rm -rf target/*

# --- Test & Quality (Rust) ---
test *args:
    @echo "🧪 Running tests..."
    cargo test {{args}}

test-verbose:
    @echo "🧪 Running tests (verbose)..."
    cargo test -- --nocapture {{args}}

test-coverage:
    @echo "📊 Generating coverage..."
    cargo llvm-cov --open

fmt:
    @echo "🎨 Formatting..."
    cargo fmt

fmt-check:
    @echo "🔍 Checking format..."
    cargo fmt --check

lint:
    @echo "🕵️ Linting..."
    cargo clippy -- -D warnings

quality: fmt-check lint test
    @echo "✅ All quality checks passed!"

# --- Run & Execute ---
run *args:
    @echo "▶️ Running must..."
    cargo run {{args}}

dev:
    @echo "🔄 Starting dev mode..."
    cargo watch -x run

# --- Dependencies (Rust/Cargo) ---
deps:
    @echo "📦 Dependencies managed by cargo"

deps-audit:
    @echo "🔒 Auditing dependencies..."
    cargo audit

# --- Must-Specific Recipes ---
validate-state:
    @echo "🔍 Validating state..."
    cargo run -- validate

validate-nickel:
    @echo "🔍 Validating mustfile.ncl with Nickel..."
    nickel check mustfile.ncl

gen-mustfile-ncl:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📜 Generating mustfile.ncl from Mustfile..."
    grep -A 20 "== Files Required" Mustfile | awk '/^-/{print "    { path = \""$$2"\", hash = \"TODO\" },"}' > mustfile.ncl.tmp
    grep -A 20 "== Files Forbidden" Mustfile | awk '/^-/{print "    \""$$2"\","}' >> mustfile.ncl.tmp
    cat >> mustfile.ncl.tmp << 'EOF'
  ],
  tasks = {
    build = { steps = ["cargo build"], must_have = [{ path = "target/debug/must" }] },
    test = { steps = ["cargo test"], must_have = [] }
  }
}
EOF
    mv mustfile.ncl.tmp mustfile.ncl
    echo "✅ Generated mustfile.ncl (edit hashes manually)"

# --- Containers (Podman) ---
container-build tag="latest":
    #!/usr/bin/env bash
    podman build -t {{project}}:{{tag}} -f Containerfile .

container-run tag="latest" *args:
    #!/usr/bin/env bash
    podman run --rm -it {{project}}:{{tag}} {{args}}

# --- CI & Automation ---
ci: deps quality validate-state validate-nickel
    @echo "🏗️ CI pipeline complete!"

# --- Documentation ---
cookbook:
    #!/usr/bin/env bash
    mkdir -p docs
    just --list | awk '/^[a-z-]+/{print "=== " $1 " ===\n\n```bash\njust " \$1 "\n```\n"}' > docs/just-cookbook.adoc
    echo "Generated: docs/just-cookbook.adoc"

# --- Security ---
security: deps-audit
    @echo "🛡️ Running security checks..."
    cargo deny check 2>/dev/null || echo "Install cargo-deny for advanced checks"

# --- State Management ---
state-touch:
    @echo "📅 Updating STATE.scm timestamp..."
    sed -i "s/(updated . \".*\")/(updated . \"$(date -Iseconds)\")/" STATE.scm

# --- Hybrid Automation ---
automate task="all":
    #!/usr/bin/env bash
    case "{{task}}" in
        all) just fmt && just lint && just test && just validate-state ;;
        state) just validate-state ;;
        nickel) just validate-nickel ;;
        *) echo "Unknown task: {{task}}. Use: all, state, nickel" && exit 1 ;;
    esac
