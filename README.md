# RoBDEMAT Visualizer

Upload-and-visualize tool for risk-of-bias assessments already made
with RoBDEMAT (Delgado et al., *J Dent* 2022;127:104350,
https://doi.org/10.1016/j.jdent.2022.104350).

## Features

- **Upload & edit data**: upload CSV/Excel, or start from the
  downloadable template. Study names are free text (vertically
  centered); every judgement cell is a **dropdown** for both
  uploaded and manually added rows. Right-click a row for more
  options (insert/remove row).
- **Traffic light plot**: table-style grid with "RoBDEMAT domains"
  as a spanning title above the item columns, and "Study" as a
  rotated label to the left of the table. Grey header row (item
  numbers 1.1–4.2), grey study-name column sized to the longest
  name, gridlines throughout, and a footnote with each domain title
  on its own line followed by its items on the line below. Each
  circle carries a black +/-/x symbol. Customize font, font size,
  circle size, per-category colour and legend text, with "Default"
  and "Colour-blind friendly" resets. Export at custom width/
  height/DPI as PNG or TIFF.
- **Summary plot**: original stacked-bar logic (plain `count()`, no
  gridline reworking) reordered so item 1.1 is at the top through
  4.2 at the bottom, with the same customization layered on top as
  the traffic light plot (font, font size, per-category colour,
  legend text, Default / Colour-blind-friendly buttons, PNG/TIFF
  export at custom size/DPI).

## Expected input format

| Study        | 1.1 | 1.2 | 1.3 | 2.1 | 2.2 | 3.1 | 3.2 | 4.1 | 4.2 |
|--------------|-----|-----|-----|-----|-----|-----|-----|-----|-----|
| Author 2020  | ... | ... | ... | ... | ... | ... | ... | ... | ... |

Cell values on upload: full judgement text, or these codes —
`S`/`Sufficiently reported`/`Adequate`, `I`, `N`/`Not reported`/
`Not adequate`, `NA`/`N/A`/`Not applicable`. Once in the app, edits
go through the dropdown only.

## Run it locally

```r
install.packages(c(
  "shiny", "ggplot2", "dplyr", "tidyr", "magrittr",
  "readxl", "openxlsx", "colourpicker", "rhandsontable", "patchwork"
))
shiny::runApp("app.R")
```

## Deploy for free on shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name='YOUR_ACCOUNT', token='YOUR_TOKEN', secret='YOUR_SECRET')
rsconnect::deployApp(appDir = "path/to/robdemat_app")
```

Install every package above locally *before* deploying — shinyapps.io
only bundles packages it detects in your local library at deploy
time. If the app fails to start there, run
`rsconnect::showLogs(appName = "your-app-name")` for the real error.

## Changelog (this revision)

- Home tab: the "How to use" steps now sit on the left with an
  **Example figure** on the right showing what the final product looks
  like. The image is `www/example_figure.png` (a `www` folder next to
  `app.R`; Shiny serves it automatically). The bundled image is a
  stand-in illustration — replace it with a real export from the app
  (same file name) and the Home page picks it up. If the file is
  missing, a dashed placeholder is shown instead. Remember to include
  the `www` folder when deploying.

## Notes / possible extensions

- The editable table only appears after a file is uploaded — say
  the word if you'd like a "start from blank" option instead.
- Multi-file upload to compare two reviewers' judgements and flag
  disagreements.
- A combined PDF/report export bundling both plots and the data
  table.
