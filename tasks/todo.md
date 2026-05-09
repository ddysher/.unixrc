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

## Future / nice-to-have

Sized roughly: (S) ~30 min, (M) afternoon, (L) day+.

- [ ] **Status glyph: running vs. waiting for input.** (M) Today the sidebar shows ● (process live) or ○ (dead). Add a third state distinguishing "agent is generating output" from "prompt visible, idle." Approaches to evaluate:
  1. Watch ghostel filter output for each tool's prompt sentinel (claude `> `, codex `▌`) and toggle a buffer-local flag.
  2. Idle-time heuristic on last-output timestamp (~500ms == waiting).
  3. OSC 9;4 progress sequences if the tool emits them.
  Render as `◐ running / ● waiting / ○ dead`. Don't block on perfection; a 1s-idle heuristic beats nothing.

- [ ] **TAB peeks, RET commits.** (S) In the sidebar, `TAB` previews the card's buffer in the right-hand window without leaving the sidebar (focus stays). `RET` keeps current behavior — switch to the buffer and take focus. Mirrors dirvish-side preview style.

- [ ] **Sidebar modeline parity with dirvish.** (S) Replace the simple `Agents [N]` modeline with a richer line modeled on the dirvish-side example: sort field with arrow direction (`↑ name|mtime`), filter indicator (`Omit`), and `position / total` on the right (e.g. `2 / 5`). Reuse the same faces dirvish uses where they're loaded, fall back gracefully where they aren't.

- [ ] **Optional session label via transient.** (S) Add a `-n` infix to `agent-tool-dispatch` for a custom session label. Stored in `agent-tool--session :label`; sidebar shows it on the agent name line when present (e.g. `● claude · review-pr`). Decoupled from the buffer name (ghostel still owns that via OSC 2).

- [ ] **`/` to filter cards.** (M) In the sidebar, `/` reads a query and filters cards to those whose card text matches (agent / dir / buffer / label). Press `/` again to clear, or `g` to refresh. Implementation: re-render with a filter predicate on `agent-tool--sessions`. No isearch inside the card text needed; substring match is enough.

- [ ] **Rebind: `C-c A` → sidebar; transient gets a different key.** (S) Sidebar is the more frequent action. Reassign `C-c A` to `agent-tool-sidebar`; pick a fresh key for `agent-tool-dispatch` (candidates: `C-c a` lowercase, or `C-c C-a`). Update `init-global-keys.el` and the smoke checklist in the file header.

- [ ] **Default sort: status first, then created-at.** (S, blocked on status glyph) Once status exists, default sidebar order becomes: running → waiting → dead, with each group sorted by `:started-at` ascending. Add `agent-tool-sidebar-sort` defcustom (`status`, `mtime`, `agent`) so users can override. Until status lands, current behavior (newest-first) is fine.

- [ ] **Directory picker for `agent-tool-start` / `-resume` / `-continue`.** (M) Today these three commands always launch at the project root with no override (override only available via the transient's `-d` infix). After picking the agent, also prompt for a directory using a tabspaces-style picker. Layout:
    1. **Default project root**, shown with its actual path so it's pickable in one keystroke (e.g. `Project root (~/.unixrc/)`).
    2. **`... (choose a dir)`** sentinel — falls through to `read-directory-name`. Same convention as `tabspaces-prompt-project-dir` (see `tabspaces.el:694–712`).
    3. **Known projects** from `project--list` (Emacs's built-in known-projects list, populated automatically as you visit projects).

    Implementation: extract a helper `agent-tool--read-dir` mirroring `tabspaces-prompt-project-dir`. Use `project--ensure-read-project-list` + `project--file-completion-table` so `completing-read` gets the right category for substring/orderless filtering. Call it from `agent-tool-start`, `agent-tool-resume`, `agent-tool-continue` between agent prompt and `agent-tool--launch`.

    Resolves the asymmetry where the transient supports directory choice but the M-x commands don't. Also makes the bare commands more useful as standalone entry points without the transient.

## Done criteria (whole feature)

- [x] Launch any agent at any directory from `agent-tool-dispatch` in ≤3 keystrokes.
- [x] Sidebar shows all live agents; RET / kill / refresh work.
- [x] `agent-tool-resume` / `agent-tool-continue` delegate to each tool's own picker.
- [x] Kill prompts before destroying a live session.
- [x] Existing `M-x agent-tool-start` still works.
