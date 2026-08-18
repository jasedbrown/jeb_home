#!/usr/bin/env zsh
# Claude Code status line: model, effort level, context remaining, git branch.

input=$(cat)

cwd=$(print -r -- "$input" | jq -r '.workspace.current_dir // empty')
[[ -z "$cwd" ]] && cwd="$PWD"
cd "$cwd" 2>/dev/null

model=$(print -r -- "$input" | jq -r '.model.display_name // "?"')
effort=$(print -r -- "$input" | jq -r '.effort.level // empty')
fast=$(print -r -- "$input" | jq -r '.fast_mode // false')
ctxused=$(print -r -- "$input" | jq -r '.context_window.used_percentage // empty')
rate5h=$(print -r -- "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
resets5h=$(print -r -- "$input" | jq -r '.rate_limits.five_hour.resets_at // empty')

badge="$model"
[[ -n "$effort" ]] && badge="${badge}:${effort}"
[[ "$fast" == "true" ]] && badge="${badge}⚡"

ctx=""
if [[ -n "$ctxused" ]]; then
  pct=${ctxused%%.*}
  if (( pct >= 80 )); then
    ctx_color="red"
  elif (( pct >= 50 )); then
    ctx_color="yellow"
  else
    ctx_color="green"
  fi
  ctx=$(printf '%s%%%% ctx' "$pct")
fi

rate=""
if [[ -n "$rate5h" ]]; then
  rpct=${rate5h%%.*}
  if (( rpct >= 80 )); then
    rate_color="red"
  elif (( rpct >= 50 )); then
    rate_color="yellow"
  else
    rate_color="green"
  fi
  timeleft=""
  if [[ -n "$resets5h" ]]; then
    now=$(date +%s)
    secs=$(( resets5h - now ))
    if (( secs > 0 )); then
      hrs=$(( secs / 3600 ))
      mins=$(( (secs % 3600) / 60 ))
      if (( hrs > 0 )); then
        timeleft=$(printf ', %dh%dm left' "$hrs" "$mins")
      else
        timeleft=$(printf ', %dm left' "$mins")
      fi
    fi
  fi
  rate=$(printf '(%s%%%% 5h%s)' "$rpct" "$timeleft")
fi

branch=""
if command -v jj >/dev/null 2>&1; then
  branch=$(jj log --no-graph -r '@ & bookmarks()' -T 'bookmarks.join(", ")' 2>/dev/null)
fi
if [[ -n "$branch" ]]; then
  : # keep jj bookmark
else
  autoload -Uz vcs_info
  zstyle ':vcs_info:git:*' formats '%b'
  vcs_info
  branch="${vcs_info_msg_0_}"
fi

out=" %F{cyan}${badge}%f"
[[ -n "$ctx" ]] && out="${out} %F{${ctx_color}}${ctx}%f"
[[ -n "$rate" ]] && out="${out} %F{${rate_color}}${rate}%f"
[[ -n "$branch" ]] && out="${out} %F{red}${branch}%f"
out="${out} "

print -P "$out"
