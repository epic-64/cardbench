# Legacy game JSON drop directory

Drop exported game JSON files here (one `GameDefinition` per file, e.g. a file
produced by the editor's "export" / download button) to assert they still load
into the **current** model.

`LegacyGameJsonSpec` reads every `*.json` file in this directory and fails if any
of them no longer deserializes into `engine.GameDefinition`. This is the
backward-compatibility guard: run it before and after a model/JSON change to see
whether older saves still load.

Notes:
- Files are matched by the `.json` extension; this `README.md` is ignored.
- An empty directory makes the guard pass trivially (nothing to check).
- This directory is on the test classpath, so files are picked up automatically —
  no code change needed when you add one.
