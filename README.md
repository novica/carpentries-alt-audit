# Carpentries alt text audit

Finds figures without alt text in Carpentries lessons. The results are in [`alt-audit.csv`](alt-audit.csv).

A figure counts as missing alt text when:

- an R chunk that draws a plot (ggplot2, base graphics, a saved ggplot object,
  `knitr::include_graphics()`) has no `fig.alt`,
- a Python chunk (`{python}` in `.Rmd`/`.qmd`) that draws a plot (matplotlib,
  pandas `.plot`, seaborn, plotnine, plotly express, altair) has no `fig.alt`,
  or
- a Markdown image (`![...](...)`, `<img>`) has no alt attribute.
