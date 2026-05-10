# agent-tool todo

Vertical slices, ordered. Each phase ends with a hands-on testing guide you can walk through.

## Phase 1 — Foundation

- [x] **T1. Rich agent registry + directory override**
  - Replace `agent-tool-commands` with `agent-tool-agents` (alist symbol → plist: `:program :resume-flag :continue-flag :extra-args`).
  - Refactor `agent-tool-start` to read the registry.
  - Bare `agent-tool-start` always uses project root (no prefix-arg). Directory override lives in the transient (T7).

- [x] **T2. Buffer-local session identity + tracking** *(landed with T1 — they were inseparable; kill-buffer prompt and sidebar still pending in Phase 2)*
  - Buffer-local `agent-tool--session` plist (`:agent :dir :resume-mode :started-at`); set immediately after launch.
  - Global `agent-tool--sessions` list, pruned on `kill-buffer-hook`.
  - **Do not** set the buffer name ourselves — keep `(generate-new-buffer ghostel-buffer-name)`. Ghostel renames via OSC 2; we never read the name for identity.

### Phase 1 user test guide

1. Restart Emacs (or eval the file).
2. `M-x agent-tool-start RET claude RET` — claude starts at project root (current behavior).
3. `M-x agent-tool-start RET codex RET` — codex starts in another buffer at project root.
4. `M-: agent-tool--sessions RET` — both buffers listed.
5. In a session buffer: `M-: agent-tool--session RET` — plist with the right `:agent` and `:dir`.
6. `C-x k` one buffer; re-eval `agent-tool--sessions` — list shrinks.
7. Watch ghostel rename the buffer post-launch — confirm our session plist is unaffected.

---

## Phase 2 — Visibility & safety

- [x] **T3. Kill-buffer prompt**
  - Add predicate to `kill-buffer-query-functions`.
  - Prompt only when buffer has `agent-tool--session` AND `(process-live-p (get-buffer-process buf))`.
  - Defcustom `agent-tool-confirm-kill` (default `t`).

- [x] **T4. Sidebar (`agent-tool-sidebar`)**
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

- [x] **T5. Resume flag wiring** *(landed with T1; flags were declared in the registry from the start)*
  - Set `:resume-flag "--resume"` and `:continue-flag "--continue"` for `claude`, `codex`, `claude-w`, `codex-w`.
  - Leave `cursor-agent` flags as `nil` until verified.
  - **No filesystem scraping.** No listing code. The tool's own picker handles selection.

- [x] **T6. `agent-tool-resume` and `agent-tool-continue` commands**
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

- [x] **T7. Transient dispatch (`agent-tool-dispatch`)**
  - Infix `-d`: `project` | `prompt` (directory mode).
  - Infix `-r`: `off` | `continue` | `resume`.
  - Suffix per agent: `c` claude, `x` codex, `w` claude-w, `W` codex-w, `u` cursor-agent.
  - Suffix `s` sidebar toggle, `j` jump-to-session.
  - Gray out an agent's suffix when `-r` ≠ `off` and that agent has no resume flag.
  - Bind in `init-global-keys.el` (default attempt: `C-c a`).

- [x] **T8. Smoke checklist in file header**
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

## Phase 5 — Sidebar UX

Pure rendering / interaction layer changes. No model changes. Sized roughly: (S) ~30 min, (M) afternoon.

- [x] **T9. TAB peek / RET commit.** (S) `TAB` displays the card's buffer in another window without taking focus; `RET` unchanged. Reuses an existing window first (`display-buffer-reuse-window`).

- [x] **T10. Dirvish-style modeline.** (S) Replace `Agents [N]` with a richer line: sort field with arrow direction (`↑ status|mtime`), filter indicator if `/` filter is active, and `position / total` on the right.

- [x] **T11. `/` filter.** (M) `/` in the sidebar reads a substring query and filters cards by matching across the card text (agent / dir / buffer / label). Press `/` again with empty input to clear; `g` clears too. Re-render with a filter predicate on `agent-tool--sessions`. No isearch.

### Phase 5 user test guide

1. Restart Emacs. Launch claude in `~/.unixrc/`, codex in `~/tmp/`.
2. `M-x agent-tool-sidebar` — two cards.
3. On card 1, press `TAB` — buffer 1 appears in the right-hand window; sidebar still has focus.
4. Move to card 2, press `TAB` — buffer 2 appears in the same window (reused).
5. Press `RET` — switch to that buffer with focus.
6. Sidebar modeline shows e.g. `↑ status|mtime  2 / 2`.
7. `/` then type `claude` then `RET` — only the claude card visible; modeline shows filter active.
8. `g` — filter cleared, both cards back.

---

## Phase 6 — Launch flow polish

- [x] **T12. Rebind global keys.** (S) `C-c a` → `agent-tool-dispatch`, `C-c A` → `agent-tool-sidebar`; smoke list updated.

- [x] **T13. Directory picker on bare commands.** (M) `agent-tool-start` / `-resume` / `-continue` get a tabspaces-style three-section picker after the agent prompt:
  1. Default project root, shown with its path (one-keystroke pick).
  2. `... (choose a dir)` sentinel → `read-directory-name`. Same convention as `tabspaces-prompt-project-dir` (`tabspaces.el:694–712`).
  3. Known projects from `project--list`.

  New helper `agent-tool--read-dir` using `project--ensure-read-project-list` + `project--file-completion-table` for the right completion category. Closes the asymmetry where only the transient supports directory choice.

- [x] **T14. `-n` session label infix.** (S) Add a `-n` infix to `agent-tool-dispatch` for a custom session label. Stored in `agent-tool--session :label`; sidebar shows `· label` after the agent name on the first card line when present (e.g. `● claude · review-pr`). Decoupled from the buffer name (ghostel still owns that via OSC 2).

### Phase 6 user test guide

1. After T12: `C-c A` opens the sidebar; `C-c a` opens the transient.
2. After T13: `M-x agent-tool-start RET claude RET` — agent prompt, then a directory picker showing `Project root (~/foo/)`, `... (choose a dir)`, and your known projects. Pick each path.
3. After T13: same for `agent-tool-resume` and `agent-tool-continue`.
4. After T14: in the transient, set `-n review-pr`, press `c` — claude launches; sidebar card shows `● claude · review-pr`.

---

## Phase 7 — Status awareness

- [x] **T15. Status glyph: running vs. waiting for input.** (M) Today the sidebar shows ● (live) or ○ (dead). Add a third state for "agent generating output" vs. "idle at prompt." Approaches to evaluate:
  1. Advise `ghostel--filter` (or equivalent output hook) — record last-output timestamp + look for prompt sentinel.
  2. Idle-time heuristic alone (~500ms of no output ⇒ waiting).
  3. OSC 9;4 progress sequences if the tool emits them.

  Extend `agent-tool--session` with `:status` (one of `running`, `waiting`, `dead`). Render `◐ / ● / ○`. Don't block on perfection — a 1s-idle heuristic beats nothing.

- [x] **T16. Default sort: status first, then created-at.** (S, blocks on T15) Default sidebar order: running → waiting → dead, each group by `:started-at` ascending. Add `agent-tool-sidebar-sort` defcustom (`status`, `mtime`, `agent`) so users can override.

### Phase 7 user test guide

1. Launch claude. Sidebar shows `◐` while it's still booting / generating; flips to `●` when prompt is visible and idle; flips to `○` when killed.
2. Launch a second agent and trigger output in one — running session sorts above the waiting one.
3. `(setq agent-tool-sidebar-sort 'mtime)` — order changes to pure created-at.

## Done criteria (whole feature)

- [x] Launch any agent at any directory from `agent-tool-dispatch` in ≤3 keystrokes.
- [x] Sidebar shows all live agents; RET / kill / refresh work.
- [x] `agent-tool-resume` / `agent-tool-continue` delegate to each tool's own picker.
- [x] Kill prompts before destroying a live session.
- [x] Existing `M-x agent-tool-start` still works.
