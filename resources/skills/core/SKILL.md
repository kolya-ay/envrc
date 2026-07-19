---
name: core
description: |
  How to drive a project with `envrc`: run and observe services, isolate work
  in worktrees, inspect status/ports, and use Konsole panes for long-running
  processes. Use whenever working inside an envrc project (a repo with `.envrc`
  running `use rc`)
allowed-tools: Bash(envrc:*)
---

# envrc core

`envrc` layers a per-project dev environment on direnv. Config lives in
`envrc.edn`; commands below work from anywhere inside the project.

## Services (process-compose backend)

Services are `:tasks` entries with `:service true` (optionally `:process-compose`
for depends-on/readiness). They do NOT live under a top-level `:services` key.

```bash
envrc services up               # start the stack (process-compose)
envrc services status           # per-service state
envrc services logs <name>      # tail one service; add 2>&1 | tail -n 40
envrc services restart <name>   # bounce one service
envrc services down             # stop the stack
envrc services attach           # process-compose TUI
```

Restart idiom: `envrc services down && envrc services up`. If "already up",
the stack is running — use `status`/`logs`, don't re-`up`.

## Workspaces (git worktrees)

```bash
envrc ws branch <name>   # new isolated worktree + branch, direnv-allowed
envrc ws list            # worktrees for this repo
envrc ws go <name>       # print a worktree's path
envrc ws rm <name>       # remove the worktree and its branch
```

- Run `ws` from **inside** the project — it resolves the repo from your cwd.
  `direnv exec <dir>` does NOT chdir, so it won't retarget `ws`.
- Dest is `<state>/envrc/worktrees/<scope>/<slug>/<name>`.
- Creation options via `:use {:worktree {…}}`: `:link`/`:copy` propagate files
  (default `:link` = `.envrc`/`envrc.edn`); `:dirty true`/`#{:staged …}` carries
  uncommitted changes; `:submodules true` runs `git submodule update --init`.
  Off unless configured.
- Never run bare `envrc ws` (no verb).

## Status & ports

```bash
envrc status              # overview
envrc status services     # service health
envrc status ports        # allocated ports for this workspace
```

Ports are configured under `:use {:ports {:base … :stride … :vars […]}}`. Each
worktree gets a distinct `PORT_OFFSET`, so parallel stacks never collide.

## Panes (Konsole side panes)

Only inside Konsole (`KONSOLE_DBUS_SESSION` set). A pane is a background process
you can talk to; plain Bash runs once and returns.

```bash
envrc pane spawn <name> -- <cmd...>   # split current tab, run, register
envrc pane list [--json]
envrc pane logs   <name> [-n N] [--since-last]
envrc pane send   <name> -- <text>    # raw keystrokes, no implicit newline
envrc pane signal <name> SIGINT|SIGTERM|SIGHUP|SIGKILL
envrc pane focus  <name>
envrc pane kill   <name>
```

- Wait for readiness by polling, not sleeping:
  `until envrc pane logs web -n 100 | grep -q 'Local:'; do sleep 0.3; done`.
- Iterate with `--since-last` after edits instead of re-reading the buffer.
- Use `signal` (not `send`) for Ctrl-C: `envrc pane signal web SIGINT`.
- Reuse before respawn: check `envrc pane list` first.
- Don't spawn a pane for a one-shot command — use plain Bash.

## Files & directories

- **`.envrc`** — `use rc`; regenerates a flake from `envrc.edn` (or `use flake`)
  and registers the project.
- **`envrc.edn`** — project config. Top-level keys: `:tasks :files :env
  :packages :use :config :title`. No top-level `:services`/`:ports`/`:commands`.
- **`envrc.json`** — Nix-emitted config layer; precedence `edn > yaml > json`.
  The same non-`:config` key in two formats is a hard "appears in both" error.
- **`:files` labels** — `:local` (append to `.git/info/exclude`), `:ref` (mirror
  into `.ref/`), `:link`/`:copy` (into worktrees). Keywords, not URI schemes.
- **`.ref/`** — framework scratch (plans/specs/tasks), kept out of git; per-worktree
  by default, shared across worktrees after `envrc apply ref` in the main checkout.
- **State/cache/runtime** — `~/.local/state/envrc/{worktrees,ref,<scope>/<slug>}`,
  `~/.cache/direnv/layouts/<hash>` (generated flake + service state),
  `$XDG_RUNTIME_DIR/envrc/.../process-compose.sock`. Keyed by scope/slug/workspace.
