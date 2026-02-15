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
  chains <- model_summary$chains
  iter <- model_summary$iter
  warmup <- model_summary$warmup
  total_draws <- chains * (iter - warmup)
  n_obs <- model_summary$nobs
  family_is_logistic <- identical(family_name, "bernoulli") ||
    grepl("logit", paste(links, collapse = " "), ignore.case = TRUE)
  
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
  
  format_value <- function(value, logistic = FALSE) {
    if (is.na(value)) return("NA")
    if (isTRUE(logistic)) {
      return(sprintf("%.1f%%", round(value * 100, 1)))
    }
    sprintf("%.2f", value)
  }
  
  format_ci <- function(lower, upper, logistic = FALSE) {
    if (is.na(lower) || is.na(upper)) return("")
    sprintf("(95%% CI %s to %s)", format_value(lower, logistic), format_value(upper, logistic))
  }
  
  conditional_specs <- list(
    pop_z = list(
      effect = "pop_z",
      label = "Population density (scaled)",
      axis_label = "population density (z score)"
    ),
    ppt_z = list(
      effect = "ppt_z",
      label = "Rainfall in the last day (scaled)",
      axis_label = "rainfall (log1p z score)"
    ),
    ndvi_z = list(
      effect = "ndvi_z",
      label = "Green-space proximity (NDVI)",
      axis_label = "NDVI proximity (z score)"
    ),
    elev_z = list(
      effect = "elev_z",
      label = "Elevation (scaled)",
      axis_label = "elevation (z score)"
    ),
    sea_days = list(
      effect = "sea_days",
      label = "Day of the year",
      axis_label = "Days per biweek"
    ),
    maxTM_z = list(
      effect = "maxTM_z",
      label = "Maximum temperature (scaled)",
      axis_label = "maximum temperature (z score)"
    )
  )
  
  conditional_plot_specs <- list()
  conditional_text_lines <- character()
  landcover_df <- tibble::tibble()
  landcover_text_lines <- character()
  
  interpretation_lines <- character()
  if (has_rows(model_summary$fixed)) {
    fixed_df <- as.data.frame(model_summary$fixed)
    fixed_df$parameter <- rownames(model_summary$fixed)
    
    format_param <- function(param) {
      param <- gsub("^b_", "", param)
      param <- gsub("\\(Intercept\\)", "Intercept", param, fixed = FALSE)
      gsub("_", " ", param)
    }
    
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
      is_intercept <- identical(param_label, "Intercept")
      if (is_intercept) {
        line <- sprintf(
          "Intercept: posterior mean %.2f (95%% CI %s). Baseline log-odds of presence at mean covariate values (and average group effects).",
          est, ci_text
        )
        interpretation_lines <- c(interpretation_lines, line)
        next
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
  
  for (nm in names(conditional_specs)) {
    spec <- conditional_specs[[nm]]
    ce_obj <- tryCatch(
      brms::conditional_effects(model_fit, effects = spec$effect, re_formula = NA),
      error = function(e) NULL
    )
    ce_data <- NULL
    if (!is.null(ce_obj) && length(ce_obj)) {
      ce_data <- tryCatch(as.data.frame(ce_obj[[1]]), error = function(e) NULL)
    }
    if (!is.null(ce_data) && spec$effect %in% names(ce_data) && "estimate__" %in% names(ce_data)) {
      plot_data <- ce_data[, c(spec$effect, "estimate__", "lower__", "upper__"), drop = FALSE]
      plot_path <- tempfile(pattern = paste0("brms_conditional_plot_", spec$effect, "_"), fileext = ".Rds")
      saveRDS(plot_data, plot_path)
      resp_label <- attr(ce_obj[[1]], "resp")
      if (is.null(resp_label) || !nzchar(resp_label)) {
        resp_label <- attr(ce_obj[[1]], "response")
      }
      if (is.null(resp_label) || !nzchar(resp_label)) {
        resp_label <- if (family_is_logistic) "Detection probability" else "Fitted mean"
      }
      conditional_plot_specs[[length(conditional_plot_specs) + 1L]] <- list(
        effect = spec$effect,
        label = spec$label,
        axis_label = spec$axis_label,
        x_label = spec$effect,
        data_path = plot_path,
        y_label = resp_label
      )
      predictor <- ce_data[[spec$effect]]
      if (!is.numeric(predictor)) {
        suppressWarnings(predictor <- as.numeric(predictor))
      }
      idx_min <- which.min(predictor)
      idx_max <- which.max(predictor)
      if (length(idx_min) && length(idx_max)) {
        min_val <- predictor[idx_min[1]]
        max_val <- predictor[idx_max[1]]
        min_est <- ce_data$estimate__[idx_min[1]]
        max_est <- ce_data$estimate__[idx_max[1]]
        min_lower <- if ("lower__" %in% names(ce_data)) ce_data$lower__[idx_min[1]] else NA_real_
        min_upper <- if ("upper__" %in% names(ce_data)) ce_data$upper__[idx_min[1]] else NA_real_
        max_lower <- if ("lower__" %in% names(ce_data)) ce_data$lower__[idx_max[1]] else NA_real_
        max_upper <- if ("upper__" %in% names(ce_data)) ce_data$upper__[idx_max[1]] else NA_real_
        min_descr <- format_value(min_est, family_is_logistic)
        max_descr <- format_value(max_est, family_is_logistic)
        min_ci <- format_ci(min_lower, min_upper, family_is_logistic)
        max_ci <- format_ci(max_lower, max_upper, family_is_logistic)
        if (nzchar(min_ci)) min_descr <- paste(min_descr, min_ci)
        if (nzchar(max_ci)) max_descr <- paste(max_descr, max_ci)
        delta <- max_est - min_est
        change_clause <- ""
        if (!is.na(delta)) {
          if (family_is_logistic) {
            change_clause <- sprintf(
              "Approximate change of %.1f percentage points across the observed range.",
              abs(delta * 100)
            )
          } else {
            change_clause <- sprintf(
              "Approximate change of %.2f on the response scale across the observed range.",
              abs(delta)
            )
          }
        }
        direction_clause <- ""
        if (!is.na(delta)) {
          assoc_term <- if (family_is_logistic) "detection probability" else "the response"
          direction_clause <- if (delta > 0) {
            sprintf("indicating a positive association with %s across the observed range.", assoc_term)
          } else if (delta < 0) {
            sprintf("indicating a negative association with %s across the observed range.", assoc_term)
          } else {
            "suggesting little variation across the observed range."
          }
        }
        line <- sprintf(
          "%s: fitted %s shifts from %s to %s as %s ranges from %.2f to %.2f.",
          spec$label,
          if (family_is_logistic) "detection probability" else "outcome mean",
          min_descr,
          max_descr,
          spec$axis_label,
          min_val,
          max_val
        )
        line <- gsub("\\s+", " ", trimws(paste(line, change_clause, direction_clause)))
        conditional_text_lines <- c(conditional_text_lines, line)
      }
    }
  }
  
  if (length(conditional_text_lines)) {
    interpretation_lines <- c(interpretation_lines, conditional_text_lines)
  }
  
  lc_ranef <- tryCatch(brms::ranef(model_fit)$landcover_code, error = function(e) NULL)
  if (!is.null(lc_ranef) && length(dim(lc_ranef)) == 3L) {
    dn <- dimnames(lc_ranef)
    stat_labels <- c("Estimate", "Est.Error", "Q2.5", "Q97.5")
    stat_axis <- which(vapply(dn, function(nms) {
      if (is.null(nms)) return(FALSE)
      sum(nms %in% stat_labels) >= 3
    }, logical(1)))
    stat_axis <- if (length(stat_axis)) stat_axis[1] else 3L
    if (stat_axis > length(dim(lc_ranef))) {
      stat_axis <- length(dim(lc_ranef))
    }
    term_axes <- setdiff(seq_len(length(dim(lc_ranef))), c(1L, stat_axis))
    term_axis <- if (length(term_axes)) term_axes[1] else 2L
    perm <- c(1L, term_axis, stat_axis)
    lc_aligned <- aperm(lc_ranef, perm = perm)
    if (length(dim(lc_aligned)) >= 3) {
      aligned_dn <- dimnames(lc_aligned)
      groups <- aligned_dn[[1]]
      terms <- aligned_dn[[2]]
      stats <- aligned_dn[[3]]
      if (is.null(groups) || !length(groups)) {
        groups <- as.character(seq_len(dim(lc_aligned)[1]))
      }
      if (is.null(terms) || !length(terms)) {
        terms <- paste0("term", seq_len(dim(lc_aligned)[2]))
      }
      if (is.null(stats) || !length(stats)) {
        stats <- stat_labels[seq_len(dim(lc_aligned)[3])]
      }
      rows <- vector("list", length(groups) * length(terms))
      idx <- 1L
      for (g_idx in seq_along(groups)) {
        for (t_idx in seq_along(terms)) {
          slice <- tryCatch(lc_aligned[g_idx, t_idx, , drop = TRUE], error = function(e) NULL)
          if (is.null(slice)) next
          names(slice) <- if (!is.null(names(slice))) names(slice) else stats
          get_stat <- function(name) {
            if (name %in% names(slice)) return(unname(slice[[name]]))
            NA_real_
          }
          group_label <- as.character(groups[g_idx])
          term_label <- as.character(terms[t_idx])
          rows[[idx]] <- tibble::tibble(
            landcover = group_label,
            term = term_label,
            estimate = get_stat("Estimate"),
            est_error = get_stat("Est.Error"),
            lower = get_stat("Q2.5"),
            upper = get_stat("Q97.5")
          )
          idx <- idx + 1L
        }
      }
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows)) {
        landcover_df <- dplyr::bind_rows(rows)
      }
    }
  }
  
  if (nrow(landcover_df)) {
    landcover_df <- dplyr::mutate(landcover_df, term = gsub("^b_", "", .data$term))
    landcover_df <- dplyr::arrange(landcover_df, dplyr::desc(.data$estimate))
    landcover_orderable <- dplyr::filter(landcover_df, is.finite(.data$estimate))
    
    if (nrow(landcover_orderable)) {
      top_row <- landcover_orderable[1, ]
      bottom_row <- landcover_orderable[nrow(landcover_orderable), ]
      top_ci <- format_ci(top_row$lower, top_row$upper, FALSE)
      bottom_ci <- format_ci(bottom_row$lower, bottom_row$upper, FALSE)
      pos_line <- sprintf(
        "Land-cover '%s' shows a higher baseline log-odds (estimate %.2f %s), suggesting more reports than the average cell.",
        top_row$landcover,
        top_row$estimate,
        top_ci
      )
      landcover_text_lines <- c(landcover_text_lines, gsub("\\s+", " ", trimws(pos_line)))
      if (nrow(landcover_orderable) > 1) {
        neg_line <- sprintf(
          "Land-cover '%s' sits lower on the log-odds scale (estimate %.2f %s), pointing to fewer reports than the baseline class.",
          bottom_row$landcover,
          bottom_row$estimate,
          bottom_ci
        )
        landcover_text_lines <- c(landcover_text_lines, gsub("\\s+", " ", trimws(neg_line)))
        range_line <- sprintf(
          "Land-cover random effects span %.2f to %.2f on the log-odds scale.",
          min(landcover_orderable$estimate, na.rm = TRUE),
          max(landcover_orderable$estimate, na.rm = TRUE)
        )
        landcover_text_lines <- c(landcover_text_lines, range_line)
      }
    }
  }
  
  landcover_path <- tempfile(pattern = "brms_landcover_", fileext = ".Rds")
  saveRDS(landcover_df, landcover_path)
  
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
  
  if (length(landcover_text_lines)) {
    interpretation_lines <- c(interpretation_lines, landcover_text_lines)
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
    "cat('**Formula:** ', paste(deparse(formula(m)), collapse = ' '), '  \\n', sep = '')",
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
    "```"
  )
  
  if (length(conditional_plot_specs)) {
    conditional_section <- c("## Conditional effects", "")
    for (spec in conditional_plot_specs) {
      conditional_section <- c(
        conditional_section,
        sprintf("### %s", spec$label),
        "",
        "```{r}",
        sprintf("ce_df <- tryCatch(readRDS('%s'), error = function(e) NULL)", spec$data_path),
        "if (!is.null(ce_df) && nrow(ce_df)) {",
        sprintf("  gg <- ggplot(ce_df, aes(x = %s, y = estimate__)) +\n    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = 'grey80', alpha = 0.6) +\n    geom_line(color = '#1b6ec2', size = 0.9) +\n    labs(x = '%s', y = '%s') +\n    theme_minimal()",
                spec$effect,
                gsub("'", "\\'", spec$x_label),
                gsub("'", "\\'", spec$y_label)),
        "  print(gg)",
        "} else {",
        "  cat('Conditional effect unavailable.\\n')",
        "}",
        "```",
        ""
      )
    }
  } else {
    conditional_section <- c(
      "## Conditional effects",
      "",
      "No conditional effects available.",
      ""
    )
  }
  
  landcover_section <- c(
    "## Land-cover random effects",
    "",
    "```{r}",
    sprintf("landcover_df <- readRDS('%s')", landcover_path),
    "if (nrow(landcover_df)) {",
    "  knitr::kable(landcover_df, digits = 2)",
    "} else {",
    "  cat('No land-cover random effects available.\\n')",
    "}",
    "```",
    ""
  )
  
  rmd_lines <- c(
    rmd_lines,
    conditional_section,
    landcover_section,
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
    "  cat(paste0('* ', interp_lines, collapse = '\\n'))",
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
