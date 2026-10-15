# ABL language server

Run `oxabl lsp` to use diagnostics, incremental document sync, and whole-document formatting.

Database effect code lenses are off by default. Enable the access group in `oxabl.toml`:

```toml
[workspace.lint]
enable = ["strict.access"]

[workspace.schema]
files = ["schema.df"]
```

The transaction rule TX004 also enables lenses. File overrides apply to each request.
The client must advertise `textDocument.codeLens` support.

Each main block, procedure, function, method, or trigger with database effects or calls gets one lens.
For example: `reads customer · writes order-line · 2 calls (2 unknown) · partial`.
Reads and writes include effects from called units. `partial` means some effects can be missing.
Units with no database effects and no calls have no lens. Include-only units have no lens.
Positions refer to header lines in the open document, including unsaved text.
Titles use the same analysis as `oxabl explain`. They have no click action.

Requests return ready lenses immediately. A background worker reads the daemon cache or builds the reachable graph.
The first request can return an empty list. Clients with `workspace.codeLens.refreshSupport` receive a refresh request.
Other clients see ready lenses on their next request. Document edits and watched-file changes clear old results.

Clients with dynamic registration receive file-specific registrations when enabled files open.
This also supports file overrides and sessions without a declared workspace root.
Static clients receive the capability at initialization when the declared workspace enables an effect rule.
After changing this setting, restart a static client to update its capabilities.

The daemon protocol and the existing check and analyze JSON documents are unchanged.
