# AGENTS.md

Guidance for anyone, human or coding agent, changing bookmark2. Usage lives in
`bin/bookmark2 help`, and the overview in [README.md](README.md).

bookmark2 keeps every link in one GNU recutils file: `Link` records with a
`Url`, optional `Title` and `Note`, and any number of `Tag` fields. A `/` nests
a tag. A Clojure + Swing + FlatLaf window and a command line read and write
that file. Nobody's personal data lives in this repo; only
[`examples/data.rec`](examples/data.rec) holds sample links.

## Layout

- [`src/bookmark2/db.clj`](src/bookmark2/db.clj) - finding the database (`--data`, `BOOKMARK2_DATA`, `~/.config/bookmark2/config.edn`, then `~/.local/share/bookmark2/data.rec`), reading it, and planning changes: add, edit, tags, read toggle, rename a tag subtree. No Swing.
- [`src/bookmark2/store.clj`](src/bookmark2/store.clj) - `commit!`, which refuses when the file changed on disk, plus undo and url normalizing.
- [`src/bookmark2/rec.clj`](src/bookmark2/rec.clj) - GNU librec, called through Java's FFM API with no binding library, loaded at run time.
- [`src/bookmark2/ui.clj`](src/bookmark2/ui.clj) - the window: the tag tree on the left, links folded by tag on the right, and themes. All state is in the `state` atom.
- [`src/bookmark2/main.clj`](src/bookmark2/main.clj) - the command line (`add`, `list`, `read`). The options table drives the help.
- [`src/bookmark2/web.clj`](src/bookmark2/web.clj), [`os.clj`](src/bookmark2/os.clj), [`json.clj`](src/bookmark2/json.clj), [`log.clj`](src/bookmark2/log.clj) - page titles and opening links, per-OS calls, a small JSON writer, the JSON-lines log.
- [`script/`](script/) - `install` (puts the command on the PATH, writes the config), `build_app` (the macOS app), `make_icon`, and `post-commit` (an optional maintainer hook).

## Rules

- Libraries: FlatLaf and its theme pack (`flatlaf-intellij-themes`, the same version), nothing else. Everything else comes from the JDK or is written here. Open an issue before adding a dependency.
- Writes are librec's bytes, the same ones `recins` writes. [`test/bookmark2/db_test.clj`](test/bookmark2/db_test.clj) checks that against the real tool; keep it passing.
- librec is GPL-3.0. It is loaded at run time from the user's system, never bundled into the app.
- Every librec call holds `rec/lock`, because librec is not thread-safe. A C `int` argument needs a Java `int`: `downcall` coerces by layout, so add every new signature to its `signatures` table.
- Every write is a plan first, `[[path old new] ...]`, and goes through `store/commit!`.
- Dark themes only. `themes` in `ui.clj` lists the dark ones, and the colours the app draws itself (`accent`, `dim`, `band`, `tag-color`) come from the theme's `UIManager` keys, so they follow a theme switch.
- Keyboard first: every action needs a key. `pane-keys` in `ui.clj` binds the single keys and also generates the help, so a new action goes into that table or into the menu actions, never as an unlisted binding.
- Per-OS calls (text editor, reveal in file manager) live in [`os.clj`](src/bookmark2/os.clj).
- Never commit a personal database. `/data.rec` is gitignored, and tests use temp files or `examples/data.rec`.

## The macOS app

[`script/build_app`](script/build_app) builds the app with the JDK's own `jar`
and `jpackage`, no build library. It updates `Bookmark2.app` in place in the
app folder: `BOOKMARK2_APP_DIR`, else `:app-dir` in the config, else
`~/Applications`. `main` needs `(:gen-class)`, because `jpackage` starts it as a
Java class.

The icon's source is [`assets/icon/bookmark2.svg`](assets/icon/bookmark2.svg).
After editing it, run [`script/make_icon`](script/make_icon) and commit the
`.icns` and `.png` it writes. The build reads those files and never renders
the SVG itself.

Optional for maintainers: [`script/post-commit`](script/post-commit) rebuilds
and reinstalls the app after each commit that touches `src/`, `assets/`,
`deps.edn` or `script/build_app`. `script/post-commit help` shows how to
install it as a git hook. `BOOKMARK2_NO_INSTALL=1 git commit ...` skips it once.

## Check

```bash
clojure -M:test
recfix examples/data.rec
```

## Debug the window

`cp examples/data.rec try.rec && bin/bookmark2 --repl 5555 --data try.rec`
opens a socket REPL on 127.0.0.1:5555. `(bookmark2.ui/snapshot)` returns what
the window shows. Widget calls go through
`javax.swing.SwingUtilities/invokeLater`; use `invokeLater`, not
`invokeAndWait`, for anything that opens a modal dialog. Test writes on a copy.
The log is at `~/.local/state/bookmark2/log.jsonl`.
