# AGENTS.md

This project is an R Shiny app for exploring a heart attack dataset.

## Project layout
- `app.R` is the main Shiny app.
- `R/helpers.R` contains small helper functions.
- `R/mod_download_plot.R` contains a reusable Shiny module for plot downloads.
- `data/heart.rds` is the dataset used by the app.
- `www/` holds static assets (e.g., `logo.png`).

## How to run
- Open `heart.Rproj` in RStudio and click **Run App**, or run:
  - `shiny::runApp()` from the project root.

## Conventions
- Use `rg` for searching files/content when possible.
- Keep edits minimal and focused; avoid reformatting unless necessary.
- Prefer `apply_patch` for single-file edits.

## Notes
- The app filters the dataset by outcome, diagnosis, DRG, and age range.
- Some outputs sample to 1,000 rows for performance.
