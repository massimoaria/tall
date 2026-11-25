# The Shiny app's helpers live in inst/tall/ and are sourced by server.R, not
# exported from the package. Tests source the file they need into a fresh
# environment, from the source tree when available, else from the install.
app_env <- function(file) {
  path <- testthat::test_path("..", "..", "inst", "tall", file)
  if (!file.exists(path)) {
    path <- system.file("tall", file, package = "tall")
  }
  testthat::skip_if(!nzchar(path) || !file.exists(path), paste(file, "not found"))
  env <- new.env(parent = asNamespace("tall"))
  sys.source(path, envir = env)
  env
}
