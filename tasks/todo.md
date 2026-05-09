# agent-tool todo

Vertical slices, ordered. Each phase ends with a hands-on testing guide you can walk through.

## Phase 1 — Foundation

- [ ] **T1. Rich agent registry + directory override**
  - Replace `agent-tool-commands` with `agent-tool-agents` (alist symbol → plist: `:program :resume-flag :continue-flag :extra-args`).
  - Refactor `agent-tool-start` to read the registry.
  - Honor prefix-arg: `C-u` prompts for directory (default = project root).

- [ ] **T2. Buffer-local session identity + tracking**
  - Buffer-local `agent-tool--session` plist (`:agent :dir :resume-mode :started-at`); set immediately after launch.
  - Global `agent-tool--sessions` list, pruned on `kill-buffer-hook`.
  - **Do not** set the buffer name ourselves — keep `(generate-new-buffer ghostel-buffer-name)`. Ghostel renames via OSC 2; we never read the name for identity.

### Phase 1 user test guide

1. Restart Emacs (or eval the file).
2. `M-x agent-tool-start RET claude RET` — claude starts at project root (current behavior).
3. `C-u M-x agent-tool-start RET claude RET ~/tmp/ RET` — claude starts in `~/tmp/`.
4. `M-: agent-tool--sessions RET` — both buffers listed.
5. In a session buffer: `M-: agent-tool--session RET` — plist with the right `:agent` and `:dir`.
6. `C-x k` one buffer; re-eval `agent-tool--sessions` — list shrinks.
7. Watch ghostel rename the buffer post-launch — confirm our session plist is unaffected.

---

## Phase 2 — Visibility & safety

- [ ] **T3. Kill-buffer prompt**
  - Add predicate to `kill-buffer-query-functions`.
  - Prompt only when buffer has `agent-tool--session` AND `(process-live-p (get-buffer-process buf))`.
  - Defcustom `agent-tool-confirm-kill` (default `t`).

- [ ] **T4. Sidebar (`agent-tool-sidebar`)**
  - `tabulated-list-mode` derived `agent-tool-sidebar-mode`.
  - Side window: left, width `agent-tool-sidebar-width` (default 36).
  - Columns: Status (●/○) | Agent | Dir | Buffer.
  - Keys: `RET` visit, `o` other-window visit, `k` kill (uses T3), `g` refresh, `n`/`p` nav, `q` quit.
  - Refresh on `kill-buffer-hook` + manual `g`.

### Phase 2 user test guide

1. Launch claude in `~/.unixrc/`, then claude-w in `~/tmp/`.
2. `M-x agent-tool-sidebar` — left side window of width 36 opens; two rows.
3. `RET` on row 1 — main window switches to that buffer.
4. `o` on row 2 — opens it in another window without leaving sidebar.
5. `k` on row 1 — y/n prompt; `n` keeps it; `y` kills.
6. `g` — refreshed; killed row gone.
7. From a session buffer: `C-x k` — prompts for live, no prompt for dead.
8. `(setq agent-tool-confirm-kill nil)` then `C-x k` — no prompt; reset to `t`.

---

## Phase 3 — Resume (delegated to each tool's native picker)

- [ ] **T5. Resume flag wiring**
  - Set `:resume-flag "--resume"` and `:continue-flag "--continue"` for `claude`, `codex`, `claude-w`, `codex-w`.
  - Leave `cursor-agent` flags as `nil` until verified.
  - **No filesystem scraping.** No listing code. The tool's own picker handles selection.

- [ ] **T6. `agent-tool-resume` and `agent-tool-continue` commands**
  - `agent-tool-resume`: prompts agent (filters out those without `:resume-flag`), launches with `--resume` — tool's own picker UI takes over.
  - `agent-tool-continue`: same, but `--continue` (last session, no picker).
  - Both honor `C-u` for directory override.

### Phase 3 user test guide

1. Generate prior claude history: run claude once in a project, chat, exit.
2. `M-x agent-tool-resume RET claude RET` — buffer opens; claude's native picker UI appears; pick session → restored.
3. `M-x agent-tool-continue RET claude RET` — last session restored, no picker.
4. Repeat both for codex.
5. `M-x agent-tool-resume RET` — cursor-agent absent from completion candidates.
6. Sidebar shows the resumed session like any other.

---

## Phase 4 — Polish

- [ ] **T7. Transient dispatch (`agent-tool-dispatch`)**
  - Infix `-d`: `project` | `prompt` (directory mode).
  - Infix `-r`: `off` | `continue` | `resume`.
  - Suffix per agent: `c` claude, `x` codex, `w` claude-w, `W` codex-w, `u` cursor-agent.
  - Suffix `s` sidebar toggle, `j` jump-to-session.
  - Gray out an agent's suffix when `-r` ≠ `off` and that agent has no resume flag.
  - Bind in `init-global-keys.el` (default attempt: `C-c a`).

- [ ] **T8. Smoke checklist in file header**
  - 8-step list in commentary at top of `init-agent-tool.el`.

### Phase 4 user test guide

1. `M-x agent-tool-dispatch` — transient opens.
2. Press `c` — claude launches at project root (defaults).
3. Reopen, `-d` → `prompt`, then `c` — directory prompt → claude launches in chosen dir.
4. Reopen, `-r` → `resume`, then `c` — claude's native session picker UI appears.
5. Reopen, `-r` → `continue`, then `c` — last session restored.
6. `s` — sidebar toggles.
7. `j` — completing-read of live sessions; pick one to jump.
8. With `-r resume`, the `u` (cursor-agent) suffix is inactive.
9. Confirm bound key (e.g. `C-c a`) opens dispatch.

---

## Done criteria (whole feature)

- [ ] Launch any agent at any directory from `agent-tool-dispatch` in ≤3 keystrokes.
- [ ] Sidebar shows all live agents; RET / kill / refresh work.
- [ ] `agent-tool-resume` / `agent-tool-continue` delegate to each tool's own picker.
- [ ] Kill prompts before destroying a live session.
- [ ] Existing `M-x agent-tool-start` still works.
