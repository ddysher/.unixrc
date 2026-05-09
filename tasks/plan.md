# agent-tool extension plan

Extend `emacs.d/inits/init-agent-tool.el` from a single `agent-tool-start` command into a transient-driven, multi-session manager with a dirvish-style sidebar, per-agent native resume, flexible directory selection, and a kill-buffer guard.

## Goals

1. Launch any registered agent in any directory, with sane defaults.
2. Resume a previous session of an agent using each tool's native resume flag — let the tool's own picker handle selection.
3. See and jump between live agents from a persistent side window (no need to dig through ghostel buffers).
4. Avoid accidental kill: prompt before killing an agent buffer.
5. Drive everything from one transient menu, with completing-read fallbacks for muscle memory.

## Non-goals

- **No Emacs-side session listing.** `claude` and `codex` don't expose `--list`; their `--resume` flag launches an interactive picker. We delegate to that picker rather than scraping `~/.claude/projects/*.jsonl` (private file format, brittle).
- No new terminal backend — stay on `ghostel`.
- No project-tree/file preview in the sidebar — just live sessions.
- No tabspaces / perspective integration in the first cut.

## Architecture

```
init-agent-tool.el
├── data: agent-tool-agents (rich registry)
│     each entry plist:
│       :program           "claude"
│       :resume-flag       "--resume"   (or nil if unsupported)
│       :continue-flag     "--continue" (or nil)
│       :extra-args        nil          (optional list)
│
├── identity (NOT buffer name)
│   └── buffer-local `agent-tool--session` plist:
│         :agent :dir :resume-mode :started-at
│       Buffer name is owned by ghostel (OSC 2). We never read it for identity.
│
├── core
│   ├── agent-tool--resolve-dir         (default | prompt | recent | arg)
│   ├── agent-tool--launch              (agent dir &key resume)
│   │     resume ∈ nil | 'pick (--resume) | 'continue (--continue)
│   └── agent-tool--sessions            list of live buffers (pruned on kill)
│
├── sidebar
│   ├── agent-tool-sidebar-mode         tabulated-list-mode
│   ├── agent-tool-sidebar              toggle side window (left, width ≈ 36)
│   ├── columns: [Status | Agent | Dir | Buffer]
│   │     Status from (process-live-p (get-buffer-process buf))
│   │     Agent / Dir from buffer-local agent-tool--session
│   │     Buffer = whatever ghostel set (cosmetic)
│   └── keys: RET visit, o other-window-visit, k kill (with confirm),
│             g refresh, n/p nav, q quit window
│
├── kill-guard
│   └── kill-buffer-query-functions hook → y/n prompt for buffers with
│       non-nil agent-tool--session AND live process
│
└── ui
    ├── agent-tool-dispatch             transient menu (entry point)
    │     infix:   -d directory mode (project | prompt)
    │              -r resume mode    (off | --continue | --resume)
    │     suffix:  one per agent + sidebar toggle + jump-to-session
    └── agent-tool-start (kept; thin wrapper, back-compatible)
```

### Why no buffer-name identity scheme

`ghostel-exec` triggers OSC 2 inside the TUI; ghostel rewrites the buffer name from terminal escape sequences seconds after launch. Any name we set will be overwritten. So we set whatever ghostel wants (`generate-new-buffer ghostel-buffer-name`, like the existing code) and store identity in a buffer-local variable. This also avoids buffer-name collisions across instances entirely — `generate-new-buffer` already uniquifies.

### Resume model

| Flag user picks            | What we run                          | Picker shown by                   |
|----------------------------|--------------------------------------|-----------------------------------|
| off (default)              | `claude` / `codex`                   | n/a                               |
| `-r continue`              | `claude --continue` / `codex --continue` | n/a — replays last session   |
| `-r resume`                | `claude --resume` / `codex --resume` | the tool's own interactive TUI    |

If an agent declares `:resume-flag nil`, the resume infix is grayed out / skipped for it. cursor-agent's flag set is unconfirmed → start with `:resume-flag nil`, add later when verified.

## Dependency graph

```
registry (agents data)
      │
      ├──► dir resolution ──► launch ──► kill guard
      │                          │
      │                          └──► session tracking ──► sidebar
      │
      └──► resume flag wiring ──► launch (with resume args)
                                    │
                                    └──► transient menu (top-level UI)
```

## Vertical slices

### Phase 1 — Foundation

**T1. Rich agent registry**
Replace `agent-tool-commands` (alist symbol→string) with `agent-tool-agents` (alist symbol→plist with `:program :resume-flag :continue-flag :extra-args`). Refactor `agent-tool-start` to read the registry. Bare command always launches at project root — directory override lives in the transient (T7), to keep one knob per behavior.

- AC: `M-x agent-tool-start` works exactly as before for all 5 agents.
- AC: Old `agent-tool-commands` removed (one user, no shim needed).

**T2. Buffer-local session identity + tracking**
Add buffer-local `agent-tool--session` plist on launch. Maintain `agent-tool--sessions` (list of buffers). Hook `kill-buffer-hook` to prune. Buffer name stays as `generate-new-buffer ghostel-buffer-name` (ghostel will rename via OSC 2; we don't fight it).

- AC: Buffer-local `agent-tool--session` is set immediately after launch and survives ghostel's rename.
- AC: `agent-tool--sessions` reflects live count; killing a buffer prunes it.
- AC: Two claude instances in the same dir produce two independent entries (no collision logic needed — `generate-new-buffer` uniquifies the name; identity comes from the plist).

> **Checkpoint A — User testing guide (Phase 1)**
> 1. Restart Emacs (or eval the file).
> 2. `M-x agent-tool-start RET claude RET` — claude starts at project root, exactly as today.
> 3. `M-x agent-tool-start RET codex RET` — codex starts in a second buffer at project root.
> 4. `M-: agent-tool--sessions RET` — should list both buffers.
> 5. In one buffer: `M-: agent-tool--session RET` — should show plist with `:agent claude :dir <expected>`.
> 6. Kill one buffer (`C-x k`); re-eval `agent-tool--sessions` — list shrinks.
> 7. Confirm ghostel still renames the buffer to whatever it sees in OSC 2 (visual check in mode line).

---

### Phase 2 — Visibility & safety

**T3. Kill-buffer prompt**
Add a `kill-buffer-query-functions` predicate that prompts `y/n` before killing any buffer with a live process and non-nil `agent-tool--session`. Defcustom `agent-tool-confirm-kill` (default `t`).

- AC: `C-x k` on a live agent buffer prompts `Kill running <agent> session in <dir>? (y or n)`.
- AC: After agent exits cleanly, `C-x k` does not prompt.
- AC: `(setq agent-tool-confirm-kill nil)` disables the prompt.

**T4. Sidebar (`agent-tool-sidebar`)**
Implement using `tabulated-list-mode` in a left side window via `display-buffer-in-side-window`, width via `agent-tool-sidebar-width` (default 36, matches `dirvish-side-width`). Columns: Status (●/○), Agent, Dir, Buffer name. Keys: `RET` visit, `o` other-window visit, `k` kill (triggers T3 prompt), `g` revert, `n`/`p` nav, `q` quit. Refresh on `kill-buffer-hook` + manual `g` for first cut.

- AC: `M-x agent-tool-sidebar` toggles a left side window of width 36.
- AC: With 2 live sessions, `RET` on a row switches main window to that buffer.
- AC: `k` invokes the same kill flow as `C-x k` (Phase 2 T3).

> **Checkpoint B — User testing guide (Phase 2)**
> 1. Launch claude in `~/.unixrc/`, then claude-w in `~/tmp/`.
> 2. `M-x agent-tool-sidebar` — left side window opens, two rows visible.
> 3. `RET` on the first row — main window switches to that buffer.
> 4. `o` on the second row — opens it in another window.
> 5. `k` on row 1 — y/n prompt appears; `n` keeps it, `y` kills.
> 6. `g` — sidebar refreshes; killed row gone.
> 7. From a session buffer: `C-x k` — prompt appears for live, suppressed for dead.
> 8. `(setq agent-tool-confirm-kill nil)` then `C-x k` — no prompt; reset to `t`.

---

### Phase 3 — Resume

**T5. Resume flag wiring**
Add `:resume-flag` and `:continue-flag` per agent. Extend `agent-tool--launch` to accept `:resume 'pick|'continue` and append the right flag. No listing code, no filesystem scraping.

- AC: claude entry has `:resume-flag "--resume" :continue-flag "--continue"`.
- AC: codex entry has `:resume-flag "--resume" :continue-flag "--continue"`.
- AC: cursor-agent starts with `:resume-flag nil` until verified.

**T6. `agent-tool-resume` and `agent-tool-continue` commands**
- `agent-tool-resume`: prompts agent, launches with `--resume` (delegates to tool's TUI picker).
- `agent-tool-continue`: prompts agent, launches with `--continue` (last session).
- Both honor prefix-arg for directory override (same as T1).
- Agents without a `:resume-flag` are filtered out of the prompt.

- AC: `M-x agent-tool-resume RET claude RET` spawns `claude --resume` and the claude TUI immediately shows its session picker.
- AC: `M-x agent-tool-continue RET claude RET` spawns `claude --continue` and resumes last conversation.
- AC: cursor-agent does not appear in the resume/continue prompt (until its flags are wired in T5).

> **Checkpoint C — User testing guide (Phase 3)**
> 1. Have ≥1 prior claude session in the project (run claude once, ask it something, exit).
> 2. `M-x agent-tool-resume RET claude RET` — buffer opens; inside it, claude's own picker UI appears; pick a session; conversation context is restored.
> 3. `M-x agent-tool-continue RET claude RET` — last session restored without picker.
> 4. Repeat both with codex.
> 5. `M-x agent-tool-resume RET` — cursor-agent should be absent from the candidate list.
> 6. Sidebar shows the new resumed session like any other.

---

### Phase 4 — Polish

**T7. Transient dispatch (`agent-tool-dispatch`)**
Top-level transient with:
- Infix `-d` directory mode: `project` (default) | `prompt`.
- Infix `-r` resume mode: `off` (default) | `continue` | `resume`.
- Suffix per agent: `c` claude, `x` codex, `w` claude-w, `W` codex-w, `u` cursor-agent.
- Suffix `s` toggle sidebar; `j` jump-to-session (completing-read over `agent-tool--sessions`).
Bind `agent-tool-dispatch` once a free key is found in `init-global-keys.el`.

- AC: `M-x agent-tool-dispatch` opens the transient.
- AC: With `-r resume`, hitting `c` runs `claude --resume` (delegates to TUI picker).
- AC: With `-d prompt`, hitting `c` prompts for directory first.
- AC: cursor-agent suffix is grayed out when `-r` ≠ `off` (no resume flag).

**T8. Smoke checklist in file header**
Add an 8-step manual smoke list to the commentary block at the top of the file.

- AC: Header lists the steps; running them all passes on a clean Emacs.

> **Checkpoint D — User testing guide (Phase 4)**
> 1. `M-x agent-tool-dispatch` — transient opens.
> 2. Press `c` — claude launches at project root (defaults).
> 3. Reopen transient, press `-d`, choose `prompt`, press `c` — directory prompt appears, then claude launches there.
> 4. Reopen transient, press `-r`, choose `resume`, press `c` — claude opens its native picker.
> 5. Press `s` — sidebar toggles.
> 6. Press `j` — completing-read of live sessions; pick one to jump.
> 7. Try `u` (cursor-agent) with `-r resume` — should be inactive.
> 8. Bound key (e.g. `C-c a`) opens dispatch.

## Risks / open questions

- **cursor-agent flags**: unverified. Phase 3 starts with cursor-agent `:resume-flag nil` and `:continue-flag nil`; user adds them later if/when they confirm the CLI shape.
- **Sidebar refresh**: post-command-hook would be wasteful; rely on `kill-buffer-hook` + manual `g`. If state drift becomes annoying, add a 2-second idle timer later.
- **Process liveness**: `process-live-p (get-buffer-process buf)` is correct for ghostel; verify `○` shows for a buffer whose agent has exited but the buffer is still around.
- **Transient prefix collision**: pick `C-c a` after grepping `init-global-keys.el`; fall back to `M-x` only if collision.

## File layout

- All changes in `emacs.d/inits/init-agent-tool.el`. No new files.
- One added line in `emacs.d/inits/init-global-keys.el` for the dispatch keybinding (T7).

## Follow-up ideas (post-Phase 4)

These are tracked in detail in `tasks/todo.md` under "Future / nice-to-have." Listed here so the architectural shape stays visible:

1. **Run/wait status glyph** — needs a hook into ghostel output filtering or an idle-time sentinel; the data flow extends `agent-tool--session` with a `:status` field updated by a process-output advice or timer.
2. **TAB-peek / RET-commit split** — pure sidebar UX; no model change. Two keys mapped to one buffer-display function differing only in `(select-window . t/nil)`.
3. **Dirvish-style modeline** — cosmetic; one `mode-line-format` rewrite. Surfaces sort direction, filter, position/total.
4. **Custom session label** — extends `agent-tool--session` with `:label`, surfaces a `-n` infix on the dispatch transient, alters sidebar render only. Decoupled from buffer name (ghostel still owns).
5. **`/`-search filter** — sidebar-local filter predicate; re-renders against a substring match across the card text. Not isearch.
6. **Rebind sidebar to `C-c A`** — sidebar is the higher-frequency action; transient moves to a fresh key. Two-line change to `init-global-keys.el` plus header smoke list.
7. **Status-first default sort** — depends on (1). Adds `agent-tool-sidebar-sort` defcustom; sidebar render groups + sorts before iterating.
8. **Directory picker for the bare commands** — `agent-tool-start` / `-resume` / `-continue` get a tabspaces-style three-section picker (default project root shown, `... (choose a dir)` sentinel, known projects from `project--list`). Closes the asymmetry where only the transient supports directory choice. New helper `agent-tool--read-dir`; existing `agent-tool--launch` already takes `dir`.

Dependency: (7) blocks on (1). The others are independent and can land in any order.

## Acceptance for the whole feature

1. Launch any agent at any directory in ≤3 keystrokes from `agent-tool-dispatch`.
2. Sidebar lists live agents; RET / kill / refresh all work.
3. `agent-tool-resume` / `agent-tool-continue` delegate to each tool's native picker.
4. Killing a buffer of a live agent prompts y/n.
5. Existing `M-x agent-tool-start` still works.
