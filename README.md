# tall website (gh-pages)

Static site served by GitHub Pages at <https://massimoaria.github.io/tall/>.

This orphan branch holds only the website; the R package lives on `master`/`develop`.

- `*.html`, `css/`, `js/`: site pages (no build step: edit and push).
- `tutorial/index.html`: the self-contained interactive tutorial
  (source: `TALL_tutorial.html`). Deep links use `tutorial/#/<step>`.
- `images/tutorial/`: slides taken from the tutorial for the chapter page.

Preview locally with `python3 -m http.server` from this folder.
