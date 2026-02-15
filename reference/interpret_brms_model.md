# Generate a diagnostic report for a fitted brms model

Builds a lightweight R Markdown summary for a Mosquito Alert modelling
model fitted with `brms`, including convergence diagnostics, parameter
tables, and a posterior predictive check. The function accepts either a
previously saved model file (RDS) or an in-memory `brmsfit` object and
can leverage metadata stored on the fitted object (for example the
`location_slug` attribute set by
[`run_brms_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/run_brms_model.md))
to drive naming and reporting.

## Usage

``` r
interpret_brms_model(
  model,
  out_file = NULL,
  format = c("html_document", "pdf_document", "word_document"),
  title = NULL,
  metadata = NULL
)
```

## Arguments

- model:

  Either a path to an RDS file containing a `brmsfit` object or an
  in-memory `brmsfit` object.

- out_file:

  Optional output file path or directory. If a directory (or `NULL`), a
  filename of the form `model_<slug>_brms_report.<ext>` is created
  inside it, where `<slug>` is taken from the model metadata when
  available.

- format:

  Document format passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
  Defaults to HTML but PDF and Word are supported (`"html_document"`,
  `"pdf_document"`, `"word_document"`).

- title:

  Optional report title. If omitted, a sensible default based on the
  location slug or file name is used.

- metadata:

  Optional named list of additional metadata fields to include in the
  report's metadata table. These entries augment (and override where
  keys collide) the metadata discovered on the model object.

## Value

Absolute path to the rendered report.
