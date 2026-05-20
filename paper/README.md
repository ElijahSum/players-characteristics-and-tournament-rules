# Master Thesis HSE

LaTeX project for:

**Player Characteristics and Adaptability to Temporal Constraints: Evidence from Titled Tuesday Chess Tournaments**

## Structure

- `main.tex`: main thesis file.
- `sections/`: thesis sections.
- `references.bib`: bibliography.
- `figures/`: generated thesis figures.
- `tables/`: stargazer-generated table outputs.

## Build

Compile with:

```bash
pdflatex main.tex
bibtex main
pdflatex main.tex
pdflatex main.tex
```

The empirical analysis scripts are stored in the replication package under `code/03_analysis/`. Table and figure generators are stored under `code/04_reporting/`.

Advanced thesis visuals can be regenerated locally with:

```bash
MPLCONFIGDIR=/private/tmp/mpl ../.venv/bin/python ../code/04_reporting/figure_scripts/create_advanced_thesis_visuals.py
```

The interpretable style-feature PCA figure can be regenerated with:

```bash
MPLCONFIGDIR=/private/tmp/mpl ../.venv/bin/python ../code/04_reporting/figure_scripts/create_style_pca_figure.py
```
