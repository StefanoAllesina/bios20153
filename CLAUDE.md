# CLAUDE.md
 
Instructions for Claude Code working in this repository.
 
## What this project is
 
Lecture notes for `BIOS 20153 Fundamentals of Ecology and Evolutionary Biology`, written as a Quarto book in R.
Published as static HTML to GitHub Pages, with a downloadable PDF built from the
same sources.
 
Audience: First-year biology students. Good grasp of calculus, not much linear algebra; dynamical systems need to start from the basics.
 
The notes are the primary teaching artifact. Correctness of the mathematics and
consistency of notation outrank concision, style, and cleverness.
 
## Source of truth
 
**Editable source:**
 
- `*.qmd` — chapter text and chunks
- `R/` — helper and plotting functions
- `data/` — input data
- `_quarto.yml`, `renv.lock`, `Makefile`, `_extensions/`
**Never edit, never commit fixes into:**
 
- `_book/`, `docs/`, `_site/` — build output
- `*_files/` — chunk-generated assets
- `_freeze/` — cached chunk results (regenerate by rendering, don't hand-edit)
If output looks wrong, fix the `.qmd` or the R function and re-render. Editing
generated files is always the wrong fix and the change will be silently lost.
 
## Commands
 
````bash
quarto preview              # live HTML preview while writing
quarto render               # build ALL formats (html + pdf) — use before pushing
quarto render --to pdf      # PDF only, when checking fallbacks
quarto publish gh-pages     # build and publish to the gh-pages branch
````
 
Do not run `quarto publish` unless explicitly asked.
 
## Layout
 
````
_quarto.yml          book config, chapter order
index.qmd            preface / syllabus
NN-topic.qmd         one chapter per file, zero-padded, ordered
R/figures.R          one function per figure
R/utils.R            distributions, estimators, shared math helpers
data/
_freeze/             committed; lets CI build without R
````
 
New chapters must be added to the `chapters:` list in `_quarto.yml` — a `.qmd`
that isn't listed there is silently excluded from the build.
 
## Notation conventions
 
Consistency matters more than any individual choice here. Do not change
notation, even to something more standard, without being asked.
 
| Concept | Use | Not |
|---|---|---|
| Vectors | `\mathbf{x}` | `\vec{x}`, `x` |
| Matrices | `\mathbf{X}` | `X` |
| Expectation | `\mathbb{E}[\cdot]` | `E(\cdot)`, `\mathrm{E}` |
| Variance | `\operatorname{Var}(\cdot)` | `V(\cdot)`, `var` |
| Estimators | `\hat{\theta}` | `\theta^*`, `\tilde\theta` |
| True parameter | `\theta_0` | `\theta^{true}` |
| Sample size | `n` | `N` |
| Independence | `\perp` | `\independent` |
 
Other rules:
 
- Display math uses `$$ ... $$`. Number and label equations that are referenced:
  `$$ ... $$ {#eq-mle-score}`, cited as `@eq-mle-score`.
- Theorems, definitions, and proofs use Quarto's callout/theorem environments,
  not bold-text-and-indent by hand.
- Cross-reference labels: `#sec-`, `#fig-`, `#tbl-`, `#eq-`, `#thm-`. Always
  reference with `@`, never "see the figure above" — the PDF and HTML order can
  differ.

## Figures
 
- One function per figure in `R/figures.R`, named `plot_<thing>()`. Inline chunk
  code should be a single call: `plot_clt_demo(n = 30)`.
- Anything stochastic is seeded: `set.seed(42)` inside the function, not at
  chunk level.
- Every figure chunk gets `#| label: fig-...` and `#| fig-cap:`.
- Themes and palettes are set once globally, not per figure. Don't add ad hoc
  `theme()` calls to individual plots.
- Do not tune `fig-width` / `fig-height` per figure unless a figure is
  genuinely broken; defaults come from `_quarto.yml`.


## Interactive widgets — the contract
 
Widgets are Observable JS (`ojs` chunks). Students do not write or run code;
widgets exist to build intuition by moving a parameter and watching something
change.
 
**All numerical logic lives in R.** Densities, estimators, likelihoods,
simulations — computed in an R chunk, passed to the browser with
`ojs_define()`. Never reimplement a distribution, estimator, or random number
generator in JavaScript. Two implementations of the course's math will
eventually disagree and the notes will be wrong in a way nobody notices.
 
**Every widget is paired with a static PDF fallback.** The required shape:
 
````markdown
```{r}
#| label: <name>-grid
#| include: false
# precompute the FULL parameter grid the slider can reach
ojs_define(grid = grid)
```
 
::: {.content-visible when-format="html"}
```{ojs}
viewof i = Inputs.range([1, 19], {step: 1, value: 7, label: "..."})
rows = transpose(grid).filter(d => d.i === i)
Plot.plot({ ... })
```
:::
 
::: {.content-visible when-format="pdf"}
```{r}
#| label: fig-<name>-static
#| echo: false
#| fig-cap: "... Interactive version: <<PUBLISHED URL>>#sec-<section>"
# faceted plot at 3 representative parameter values, from the SAME grid
```
:::
````
 
Rules:
 
- Never add an `ojs` chunk without the paired `when-format="pdf"` block.
- The fallback plots from the same precomputed object, so the numbers in the PDF
  are literally the numbers the widget shows.
- The fallback shows **comparative statics** — typically a 3-panel facet at the
  low, middle, and high end of the parameter range. A single frozen snapshot
  does not convey what the widget conveys.
- The fallback caption points to the live URL.
- `transpose()` is required in OJS: `ojs_define()` hands over column-oriented
  data.
- Index slider positions by integer (`i`), never by floating-point equality
  against the parameter value.
- No `htmlwidgets` (plotly, leaflet, DT) inside the main text flow — they don't
  render to PDF. Static ggplot or OJS only.
## Copyediting passes
 
When asked to fix typos, grammar, or phrasing:
 
- Work on named files only, one at a time. Never sweep the whole book.
- **Do not modify anything inside** `$...$`, `$$...$$`, R chunks, `ojs` chunks,
  YAML front matter, or chunk options.
- Do not rename variables, change subscripts, or "correct" notation.
- Do not restructure: no reordering sections, no splitting or merging
  paragraphs, no new headings, no converting prose to bullet lists.
- Do not reword theorem or definition statements. Flag them instead.
- Leave deliberate repetition alone — restating a result is often pedagogical.
- Report anything mathematically suspect rather than fixing it silently.
This is the highest-risk task in the repo: a mangled subscript still reads
fluently, so review is hard. Keep diffs small enough to check with
`git diff --word-diff`.
 
## Rendering and CI
 
`execute: freeze: auto` is set, and `_freeze/` is committed. Consequences:
 
- Prose or notation edits do not re-run R. Don't force a full re-render for a
  typo fix.
- Any change to an R chunk or to `R/` invalidates the freeze for affected
  chapters — re-render and commit the updated `_freeze/`.
- `.nojekyll` must exist in the published output. Without it GitHub Pages
  discards `_`-prefixed directories and the site breaks. `quarto publish
  gh-pages` handles this; don't remove it.
## Before finishing any task
 
1. `quarto render` — **both** formats, not just HTML. It is easy to break the
   PDF for weeks while previewing HTML all day, and the failure is usually
   silent: an empty figure, not an error.
2. Check no generated directory is staged.
3. For new widgets, confirm the PDF fallback actually renders a figure.
## Ask before
 
- Changing notation, or any convention in this file
- Adding a package dependency (`renv::install` + commit `renv.lock`)
- Adding, deleting, splitting, or reordering chapters
- Rewriting a theorem, proof, or definition
- Running `quarto publish`
- Deleting or regenerating `_freeze/` wholesale
## Preferences
 
- Prefer editing existing files over creating new ones.
- Don't add explanatory comments to `.qmd` prose.