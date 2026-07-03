# -----------------------------------------------------------------------------
# Internal shared helpers for the INLA model-fitting functions.
#
# These helpers are used by both `run_inla_model()` and
# `run_inla_bym2_model()` so the two functions load prepared data, coerce
# formulae, and write output in a consistent way.
# -----------------------------------------------------------------------------

# Coerce a supplied formula (character vector, single string, or formula
# object) into a single formula text string.
.inla_formula_text <- function(formula) {
  if (is.character(formula) && length(formula) > 1L) {
    paste(formula, collapse = " + ")
  } else if (inherits(formula, "formula")) {
    paste(deparse(formula), collapse = " ")
  } else if (is.character(formula) && length(formula) == 1L) {
    formula
  } else {
    stop("`formula` must be a string or formula object.", call. = FALSE)
  }
}

# Load a prepared (non-spatial) modelling dataset for INLA fitting.
#
# Accepts a prepared data object, a data frame, a path to an RDS file, or
# `NULL` (in which case the file is located from the location identifiers).
.inla_load_model_data <- function(
    dataset,
    temporal_resolution,
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    input_dir = "data/proc",
    verbose = TRUE
) {
  dataset_path <- NULL
  location_slug <- NULL

  if (is.null(dataset)) {
    if (is.null(iso3) || is.null(admin_level) || is.null(admin_name)) {
      stop(
        "If `dataset` is NULL, you must provide `iso3`, `admin_level`, and ",
        "`admin_name` to locate the prepared data.",
        call. = FALSE
      )
    }

    ids <- build_location_identifiers(iso3, admin_level, admin_name)
    location_slug <- ids$slug

    resolution_suffix <- paste0("_", temporal_resolution)
    target_file <- file.path(
      input_dir,
      sprintf("model_prep_%s%s_data.rds", location_slug, resolution_suffix)
    )

    if (!file.exists(target_file)) {
      stop(
        "Prepared dataset not found at: ", target_file,
        "\nRun `prepare_model_data(..., temporal_resolution = \"",
        temporal_resolution, "\", write = TRUE)` first.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message("Loading prepared dataset from: ", target_file)
    }

    dataset <- readRDS(target_file)
    dataset_path <- target_file
  } else if (is.character(dataset) && length(dataset) == 1L) {
    if (!file.exists(dataset)) {
      stop("Dataset file not found: ", dataset, call. = FALSE)
    }

    if (isTRUE(verbose)) {
      message("Loading dataset from: ", dataset)
    }

    dataset_path <- dataset
    dataset <- readRDS(dataset)
  }

  if (inherits(
    dataset,
    c(
      "brms_data_prep",
      "bym2_data_prep",
      "inla_bym2_data_prep",
      "model_data_prep"
    )
  )) {
    model_data <- dataset$model_data

    if (is.null(location_slug)) {
      location_slug <- dataset$meta$slug
    }
  } else if (is.data.frame(dataset)) {
    model_data <- dataset

    if (is.null(location_slug)) {
      location_slug <- attr(dataset, "location_slug", exact = TRUE)
    }
  } else {
    stop(
      "`dataset` must be NULL, a path, a data frame, or a prepared ",
      "data object.",
      call. = FALSE
    )
  }

  if (!is.data.frame(model_data)) {
    stop(
      "The prepared object's `model_data` component must be a data frame.",
      call. = FALSE
    )
  }

  list(
    model_data = model_data,
    location_slug = location_slug,
    dataset_path = dataset_path
  )
}

# Write a fitted INLA model to disk, mirroring the output-path conventions used
# by the brms model-fitting functions. Returns the file path written.
.inla_write_output <- function(
    fit,
    output_path,
    dataset_path,
    location_slug,
    temporal_resolution,
    model_kind,
    verbose = TRUE
) {
  final_output_path <- output_path

  if (is.null(final_output_path) || !nzchar(final_output_path)) {
    if (!is.null(dataset_path)) {
      final_output_path <- dirname(dataset_path)
    } else {
      stop(
        "`write_output = TRUE` requires `dataset` be a path or ",
        "`output_path` be supplied.",
        call. = FALSE
      )
    }
  }

  path_ext <- tools::file_ext(final_output_path)

  is_dir_target <- dir.exists(final_output_path) ||
    identical(path_ext, "") ||
    grepl("[\\/]+$", final_output_path)

  stem_base <- if (!is.null(location_slug) && nzchar(location_slug)) {
    paste0("model_", location_slug, "_", model_kind, "_", temporal_resolution)
  } else if (!is.null(dataset_path)) {
    paste0(
      tools::file_path_sans_ext(basename(dataset_path)),
      "_", model_kind, "_", temporal_resolution
    )
  } else {
    paste0(
      model_kind, "_model_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      "_", temporal_resolution
    )
  }

  if (is_dir_target) {
    if (!dir.exists(final_output_path)) {
      dir.create(final_output_path, recursive = TRUE, showWarnings = FALSE)
    }

    final_file <- file.path(final_output_path, paste0(stem_base, ".rds"))
  } else {
    parent_dir <- dirname(final_output_path)

    if (!dir.exists(parent_dir)) {
      dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
    }

    final_file <- final_output_path
  }

  saveRDS(fit, final_file)

  if (isTRUE(verbose)) {
    message("INLA model written to: ", final_file)
  }

  final_file
}
