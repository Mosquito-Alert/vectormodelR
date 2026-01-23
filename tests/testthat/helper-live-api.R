skip_if_no_live_senscape <- function() {
  testthat::skip_on_cran()

  if (!nzchar(Sys.getenv("SENSCAPE_API_KEY"))) {
    testthat::skip("SENSCAPE_API_KEY not set")
  }

  if (!identical(Sys.getenv("RUN_LIVE_API_TESTS"), "true")) {
    testthat::skip("Live Senscape API tests disabled; set RUN_LIVE_API_TESTS=true")
  }
}
