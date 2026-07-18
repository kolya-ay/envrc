direnv_layout_dir() {
  local hash path
  hash="$(echo -n "$PWD" | sha1sum | cut -c1-10)"
  path="${PWD//\//-}"
  echo "${XDG_CACHE_HOME:-$HOME/.cache}/direnv/layouts/${hash}${path}"
}

use_edn() {
  local edn="${1:-}"

  # If no path specified, search for any envrc.* format
  if [[ -z "$edn" ]]; then
    for f in envrc.edn envrc.yml envrc.yaml envrc.json; do
      if [[ -f "$f" ]]; then
        edn="$f"
        break
      fi
    done
  fi

  [[ -f "$edn" ]] || {
    log_error "edn: no envrc.{edn,yml,yaml,json} found"
    return 1
  }

  local layout_dir flake_path new_content
  layout_dir="$(direnv_layout_dir)"
  mkdir -p "$layout_dir"
  flake_path="$layout_dir/flake.nix"

  new_content="$(envrc gen flake --stdout)"
  if [[ ! -f "$flake_path" ]] || [[ "$new_content" != "$(cat "$flake_path")" ]]; then
    echo "$new_content" >"$flake_path"
    log_status "edn: regenerated flake.nix"
  fi

  # Watch every envrc.* file and any external :config pointer so edits trigger reload
  while IFS= read -r f; do
    [[ -n "$f" ]] && watch_file "$f"
  done < <(envrc status watch)

  export ENVRC_ROOT="$PWD"
  local _err_file
  _err_file=$(mktemp)
  if ! ENVRC_CONFIG_JSON="$(envrc config 2>"$_err_file")"; then
    log_error "envrc config failed: $(cat "$_err_file")"
    rm -f "$_err_file"
    return 1
  fi
  rm -f "$_err_file"
  export ENVRC_CONFIG_JSON

  eval "$(envrc gen project --stdout)"
  eval "$(envrc gen ports --stdout)"
  eval "$(envrc gen services-env --stdout)"
  eval "$(envrc gen enter --stdout)"

  use flake "$layout_dir"

  envrc status 1>&2 || true
}

# rc [slug] — register the current dir as a project, then bring up whatever
# environment it has. edn project → use_edn; flake-only → use flake; bare dir →
# registration only. Worktree checkouts (under <state>/**/worktrees/**) bring up
# the env but never register; we already track those separately.
use_rc() {
  local slug="${1:-}"

  if [[ -f envrc.edn || -f envrc.yml || -f envrc.yaml || -f envrc.json ]]; then
    use_edn || return $?
  elif [[ -f flake.nix ]]; then
    use flake . || return $?
  fi

  local state_root="${XDG_STATE_HOME:-$HOME/.local/state}"
  if [[ "$PWD" == "$state_root/"* && "$PWD" == *"/worktrees/"* ]]; then
    return 0
  fi

  local args=(--dir "$PWD")
  [[ -n "$slug" ]] && args+=(--slug "$slug")
  envrc project register "${args[@]}" || log_error "rc: project register failed"
}
