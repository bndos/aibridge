# AI Bridge (Emacs)

A lightweight Emacs chat UI that talks to a Codex MCP server, rendered in an Org-mode buffer. It feels like a terminal chat, with streaming assistant output, per-buffer sessions, and inline approvals for commands and patches.

## What it is

- Minimal Emacs Lisp client for Codex MCP (`aibridge-mcp.el`).
- Codex-specific glue and event handling (`aibridge-codex.el`).
- An Org-based chat experience (`aibridge.el`) with:
  - New/resume conversations, buffer renamed to include a short convo id.
  - Streaming `agent_message_delta` into `#+begin_src` blocks.
  - Inline approvals for command execution and patch application.
  - Per-buffer MCP process, so multiple chats can run concurrently.

## Requirements

- Emacs 28+ (tested with recent builds).
- A Codex MCP server on your PATH (default command: `codex mcp`).

If your Codex binary isn’t called `codex`, customize `aibridge-codex-command`.

## Quick start

1. Add this repo to your `load-path` and load the files, or `(require 'aibridge)`.
2. Run `M-x aibridge-org`.
3. Pick “Create new conversation” (or select an existing one to resume).
4. Type your prompt and press `RET` (or `C-c C-c`) to send. Use `S-RET` for a newline.

The buffer will rename to `*AI Bridge Org: <short-id>*` once the session id is known.

## Useful keys (in `aibridge-org-mode`)

- Send: `RET` or `C-c C-c`
- Newline: `S-RET`
- Change model: `C-c m`
- Change sandbox policy: `C-c s`
- Change approval policy: `C-c p`
- Open link at point: `C-c C-o`

Approvals (in approval buffers):
- Approve: `C-c C-c`
- Deny: `C-c C-k`

## Troubleshooting

- Enable simple logging for MCP traffic: `M-: (aibridge-mcp-install-logger)`
- By default, the MCP server command is `codex mcp`. Customize `aibridge-codex-command` if different.
- The Org chat buffer renames after receiving a session/conversation id. If it doesn’t, capture the first few MCP lines (request/response + first notification) and file an issue.

## Files

- `aibridge-mcp.el` – transport (JSON-RPC over stdio, line-delimited)
- `aibridge-codex.el` – Codex-specific API + event/approval handling
- `aibridge.el` – Org-mode chat UI and per-buffer client lifecycle

More documentation and examples to come.

