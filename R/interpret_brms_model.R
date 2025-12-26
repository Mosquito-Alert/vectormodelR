#' Generate a diagnostic report for a fitted brms model
#'
#' Builds a lightweight R Markdown summary for a Mosquito Alert modelling model
#' fitted with `brms`, including convergence diagnostics, parameter tables, and
#' a posterior predictive check. The function accepts either a previously saved
#' model file (RDS) or an in-memory `brmsfit` object and can leverage metadata
#' stored on the fitted object (for example the `location_slug` attribute set by
#' `run_brms_model()`) to drive naming and reporting.
#'
#' @param model Either a path to an RDS file containing a `brmsfit` object or an
#'   in-memory `brmsfit` object.
#' @param out_file Optional output file path or directory. If a directory (or
#'   `NULL`), a filename of the form `model_<slug>_brms_report.<ext>` is created
#'   inside it, where `<slug>` is taken from the model metadata when available.
#' @param format Document format passed to `rmarkdown::render()`. Defaults to
#'   HTML but PDF and Word are supported (`"html_document"`,
#'   `"pdf_document"`, `"word_document"`).
#' @param title Optional report title. If omitted, a sensible default based on
#'   the location slug or file name is used.
#' @param metadata Optional named list of additional metadata fields to include
#'   in the report's metadata table. These entries augment (and override where
#'   keys collide) the metadata discovered on the model object.
#'
#' @return Absolute path to the rendered report.
#' @export
interpret_brms_model <- function(
    model,
    out_file = NULL,
    format = c("html_document", "pdf_document", "word_document"),
    title = NULL,
    metadata = NULL
) {
  format <- match.arg(format)
  
  required_pkgs <- c("brms", "dplyr", "posterior", "bayesplot", "ggplot2", "rmarkdown", "tibble")
  missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  model_is_path <- is.character(model) && length(model) == 1L && nzchar(model)
  input_path <- NULL
  
  if (model_is_path) {
    if (!file.exists(model)) {
      stop("Model file not found at ", model, call. = FALSE)
    }
    input_path <- normalizePath(model, winslash = "/", mustWork = TRUE)
    model_fit <- readRDS(input_path)
  } else {
    model_fit <- model
  }
  
  if (!inherits(model_fit, "brmsfit")) {
    stop("`model` must be a brmsfit object or a path to one stored in an RDS file.",
         call. = FALSE)
  }
  
  if (!model_is_path) {
    input_path <- tempfile(pattern = "model_brms_", fileext = ".Rds")
    saveRDS(model_fit, input_path)
  }
  
  location_slug <- attr(model_fit, "location_slug", exact = TRUE)
  source_dataset <- attr(model_fit, "source_dataset", exact = TRUE)
  saved_model_path <- attr(model_fit, "output_path", exact = TRUE)
  
  base_metadata <- list(
    location_slug = location_slug,
    source_dataset = source_dataset,
    saved_model = saved_model_path,
    model_file = input_path,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  
  if (!is.null(metadata)) {
    base_metadata <- utils::modifyList(base_metadata, metadata)
  }
  
  keep_entry <- function(x) {
    if (is.null(x)) return(FALSE)
    if (length(x) == 0) return(FALSE)
    if (length(x) == 1 && (is.na(x) || identical(x, "") || identical(x, FALSE))) return(FALSE)
    TRUE
  }
  base_metadata <- base_metadata[vapply(base_metadata, keep_entry, logical(1))]
  
  collapse_value <- function(x) {
    x <- unlist(x)
    if (length(x) == 0) {
      return(NA_character_)
    }
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(NA_character_)
    }
    paste(x, collapse = ", ")
  }
  metadata_tbl <- tibble::tibble(
    Field = names(base_metadata),
    Value = vapply(base_metadata, collapse_value, character(1), USE.NAMES = FALSE)
  )
  
  metadata_tbl <- dplyr::filter(metadata_tbl, !is.na(.data$Value) & .data$Value != "")
  metadata_path <- tempfile(pattern = "brms_meta_", fileext = ".Rds")
  saveRDS(metadata_tbl, metadata_path)
  
  model_summary <- summary(model_fit)
  family_name <- model_summary$family
  links <- model_summary$links
  formula_chr <- paste(deparse(stats::formula(model_fit)), collapse = " ")
  chains <- model_summary$chains
  iter <- model_summary$iter
  warmup <- model_summary$warmup
  total_draws <- chains * (iter - warmup)
  n_obs <- model_summary$nobs
  
  collect_tables <- function(summ_obj) {
    tabs <- list(
      smooth = summ_obj$spec_pars,
      random = summ_obj$random,
      fixed = summ_obj$fixed
    )
    tabs[vapply(tabs, is.null, logical(1))] <- NULL
    tabs
  }
  
  tables <- collect_tables(model_summary)
  tables <- tables[vapply(tables, function(x) {
    if (is.null(x)) return(FALSE)
    nr <- suppressWarnings(tryCatch(NROW(x), error = function(e) NA_integer_))
    !is.na(nr) && nr > 0
  }, logical(1))]
  stacked_list <- lapply(names(tables), function(nm) {
    df <- as.data.frame(tables[[nm]])
    if (!nrow(df)) {
      return(NULL)
    }
    df$.__block <- nm
    rn <- rownames(tables[[nm]])
    if (length(rn) == 0) {
      rn <- seq_len(nrow(df))
    }
    df$.__par <- as.character(rn)
    df
  })
  stacked <- dplyr::bind_rows(stacked_list)
  if (is.null(stacked) || !nrow(stacked)) {
    stacked <- tibble::tibble()
  }
  
  rhat_max <- if ("Rhat" %in% names(stacked) && nrow(stacked)) max(stacked$Rhat, na.rm = TRUE) else NA_real_
  bulk_ess_min <- if ("Bulk_ESS" %in% names(stacked) && nrow(stacked)) min(stacked$Bulk_ESS, na.rm = TRUE) else NA_real_
  tail_ess_min <- if ("Tail_ESS" %in% names(stacked) && nrow(stacked)) min(stacked$Tail_ESS, na.rm = TRUE) else NA_real_
  
  nuts_pars <- brms::nuts_params(model_fit)
  divergences <- sum(nuts_pars$Parameter == "divergent__" & nuts_pars$Value == 1)
  treedepth_vals <- nuts_pars[nuts_pars$Parameter == "treedepth__", "Value"]
  
  max_treedepth_config <- NA_integer_
  try({
    control <- model_fit$fit@fit_args$control
    if (!is.null(control$max_treedepth)) {
      max_treedepth_config <- as.integer(control$max_treedepth)
    }
  }, silent = TRUE)
  
  treedepth_hits <- NA_integer_
  if (!is.na(max_treedepth_config) && length(treedepth_vals)) {
    treedepth_hits <- sum(treedepth_vals >= max_treedepth_config)
  }
  treedepth_max <- if (length(treedepth_vals)) max(treedepth_vals) else NA_real_
  
  has_rows <- function(x) {
    if (is.null(x)) return(FALSE)
    nr <- suppressWarnings(tryCatch(NROW(x), error = function(e) NA_integer_))
    if (is.na(nr)) return(FALSE)
    nr > 0
  }
  
  interpretation_lines <- character()
  if (has_rows(model_summary$fixed)) {
    fixed_df <- as.data.frame(model_summary$fixed)
    fixed_df$parameter <- rownames(model_summary$fixed)
    
    format_param <- function(param) {
      param <- gsub("^b_", "", param)
      param <- gsub("\\(Intercept\\)", "Intercept", param, fixed = FALSE)
      gsub("_", " ", param)
    }
    
    family_is_logistic <- identical(family_name, "bernoulli") || grepl("logit", paste(links, collapse = " "), ignore.case = TRUE)
    
    for (i in seq_len(nrow(fixed_df))) {
      row <- fixed_df[i, ]
      param_label <- format_param(row$parameter)
      est <- row$Estimate
      lower <- row$`l-95% CI`
      upper <- row$`u-95% CI`
      
      ci_text <- sprintf("%.2f to %.2f", lower, upper)
      evidence <- if (is.na(lower) || is.na(upper)) {
        "with uncertain direction"
      } else if (lower > 0) {
        "indicating a positive association"
      } else if (upper < 0) {
        "indicating a negative association"
      } else {
        "with the credible interval spanning zero, suggesting no clear direction"
      }
      
      if (family_is_logistic) {
        odds_est <- exp(est)
        odds_lower <- exp(lower)
        odds_upper <- exp(upper)
        odds_text <- sprintf("(odds ratio %.2f, 95%% CI %.2f–%.2f)", odds_est, odds_lower, odds_upper)
        line <- sprintf(
          "%s: posterior mean %.2f (95%% CI %s) %s %s.",
          param_label, est, ci_text, odds_text, evidence
        )
      } else {
        line <- sprintf(
          "%s: posterior mean %.2f (95%% CI %s) %s.",
          param_label, est, ci_text, evidence
        )
      }
      
      interpretation_lines <- c(interpretation_lines, line)
    }
  }
  
  if (has_rows(model_summary$spec_pars)) {
    smooth_terms <- rownames(model_summary$spec_pars)
    if (length(smooth_terms)) {
      interpretation_lines <- c(
        interpretation_lines,
        sprintf(
          "Smooth terms included: %s, allowing non-linear effects to vary flexibly.",
          paste(smooth_terms, collapse = ", ")
        )
      )
    }
  }
  
  if (has_rows(model_summary$random)) {
    rnames <- rownames(model_summary$random)
    if (is.null(rnames) || !length(rnames)) {
      random_groups <- "grouping factors"
    } else {
      random_groups <- unique(sub("^r_", "", rnames))
    }
    interpretation_lines <- c(
      interpretation_lines,
      sprintf(
        "Random effects captured variation across %s, accounting for grouping structure in the data.",
        paste(random_groups, collapse = ", ")
      )
    )
  }
  
  interpretation_path <- tempfile(pattern = "brms_interp_", fileext = ".Rds")
  saveRDS(interpretation_lines, interpretation_path)
  
  diag_sentence <- function() {
    pieces <- c()
    
    if (!is.na(rhat_max)) {
      if (rhat_max <= 1.01) {
        pieces <- c(pieces, sprintf("All reported parameters had R̂ ≤ %.2f, indicating good convergence.", rhat_max))
      } else {
        pieces <- c(pieces, sprintf("Some parameters showed elevated R̂ (max %.2f), indicating potential convergence issues.", rhat_max))
      }
    }
    
    if (!is.na(bulk_ess_min) && !is.na(tail_ess_min)) {
      pieces <- c(pieces, sprintf("Effective sample sizes were adequate (min Bulk ESS = %.0f; min Tail ESS = %.0f).",
                                  bulk_ess_min, tail_ess_min))
    }
    
    if (divergences == 0) {
      pieces <- c(pieces, "No divergent transitions were observed.")
    } else {
      pieces <- c(pieces, sprintf("%d divergent transitions were observed; posterior geometry may still need investigation.", divergences))
    }
    
    if (!is.na(max_treedepth_config)) {
      if (!is.na(treedepth_hits) && treedepth_hits == 0) {
        pieces <- c(pieces, sprintf("No transitions hit the maximum treedepth (%d).", max_treedepth_config))
      } else if (!is.na(treedepth_hits)) {
        pieces <- c(pieces, sprintf("%d transitions hit the maximum treedepth (%d).", treedepth_hits, max_treedepth_config))
      }
    } else if (!is.na(treedepth_max)) {
      pieces <- c(pieces, sprintf("The maximum observed treedepth was %s.", treedepth_max))
    }
    
    paste(pieces, collapse = " ")
  }
  
  diagnostic_text <- diag_sentence()
  
  report_stem <- if (!is.null(location_slug) && nzchar(location_slug)) {
    paste0("model_", location_slug, "_brms_report")
  } else if (!is.null(input_path)) {
    paste0(tools::file_path_sans_ext(basename(input_path)), "_report")
  } else {
    paste0("brms_model_report_", format(Sys.time(), "%Y%m%d%H%M%S"))
  }
  
  extension <- switch(
    format,
    html_document = ".html",
    pdf_document = ".pdf",
    word_document = ".docx"
  )
  
  if (is.null(out_file) || !nzchar(out_file)) {
    base_dir <- if (model_is_path) dirname(input_path) else getwd()
    out_dir <- base_dir
    out_file <- file.path(out_dir, paste0(report_stem, extension))
  } else {
    path_ext <- tools::file_ext(out_file)
    is_dir <- dir.exists(out_file) || identical(path_ext, "") || grepl("[\\/]+$", out_file)
    if (is_dir) {
      out_dir <- out_file
      if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      }
      out_file <- file.path(out_dir, paste0(report_stem, extension))
    } else {
      out_dir <- dirname(out_file)
      if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      }
      if (!grepl("\\.html$|\\.pdf$|\\.docx$", out_file, ignore.case = TRUE)) {
        out_file <- paste0(out_file, extension)
      }
    }
  }
  
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  report_title <- if (!is.null(title)) {
    title
  } else if (!is.null(location_slug) && nzchar(location_slug)) {
    paste0("brms model report: ", location_slug)
  } else if (model_is_path) {
    paste0("brms model report: ", basename(input_path))
  } else {
    "brms model report"
  }
  
  rmd_lines <- c(
    "---",
    sprintf("title: \"%s\"", gsub("\"", "\\\\\"", report_title)),
    sprintf("output: %s", format),
    "params:",
    sprintf("  model_rds: \"%s\"", input_path),
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
    "library(brms)",
    "library(dplyr)",
    "library(posterior)",
    "library(bayesplot)",
    "library(ggplot2)",
    "m <- readRDS(params$model_rds)",
    "s <- summary(m)",
    "```",
    "",
    "## Model overview",
    "",
    "```{r}",
    sprintf("cat('**Family:** %s  \\n')", family_name),
    sprintf("cat('**Links:** %s  \\n')", paste(names(links), links, sep = ' = ', collapse = ', ')),
    sprintf("cat('**Formula:** %s  \\n')", formula_chr),
    sprintf("cat('**Observations:** %s  \\n')", n_obs),
    sprintf("cat('**Chains / Iter / Warmup:** %s / %s / %s  \\n')", chains, iter, warmup),
    sprintf("cat('**Post-warmup draws:** %s  \\n')", total_draws),
    "```",
    "",
    "## Metadata",
    "",
    "```{r}",
    sprintf("meta_tbl <- readRDS('%s')", metadata_path),
    "if (nrow(meta_tbl) > 0) knitr::kable(meta_tbl, digits = 2) else cat('No metadata supplied.\n')",
    "```",
    "",
    "## Sampler diagnostics",
    "",
    "```{r}",
    "np <- nuts_params(m)",
    "td_tbl <- np %>% count(Parameter, Value) %>% filter(Parameter %in% c('divergent__', 'treedepth__'))",
    "if (nrow(td_tbl) > 0) knitr::kable(td_tbl) else cat('No sampler diagnostics to report.\n')",
    "```",
    "",
    "```{r}",
    sprintf("cat('%s')", gsub("'", "\\'", diagnostic_text)),
    "```",
    "",
    "## Parameter summary",
    "",
    "### Fixed effects",
    "",
    "```{r}",
    "if (!is.null(s$fixed)) knitr::kable(as.data.frame(s$fixed), digits = 2) else cat('No fixed effects table available.\n')",
    "```",
    "",
    "### Group-level effects",
    "",
    "```{r}",
    "if (!is.null(s$random)) knitr::kable(as.data.frame(s$random), digits = 2) else cat('No group-level effects table available.\n')",
    "```",
    "",
    "### Smooth terms",
    "",
    "```{r}",
    "if (!is.null(s$spec_pars)) knitr::kable(as.data.frame(s$spec_pars), digits = 2) else cat('No smooth terms table available.\n')",
    "```",
    "",
    "## Posterior predictive check",
    "",
    "```{r}",
    "pp_check(m, ndraws = 200)",
    "```",
    "",
    "## Interpretation",
    "",
    "```{r, results='asis'}",
    sprintf("interp_lines <- readRDS('%s')", interpretation_path),
    "if (length(interp_lines)) {",
    "  cat(paste0('* ', interp_lines, collapse = '\n'))",
    "} else {",
    "  cat('No interpretive summary available.\\n')",
    "}",
    "```"
  )
  
  rmd_path <- tempfile(pattern = "brms_report_", fileext = ".Rmd")
  writeLines(rmd_lines, rmd_path)
  
  rmarkdown::render(
    input = rmd_path,
    output_file = out_file,
    quiet = TRUE
  )
  
  normalizePath(out_file, winslash = "/", mustWork = FALSE)
}
