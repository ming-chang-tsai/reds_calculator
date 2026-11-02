# Export only the public app files. Posit continues to use app.R on master.
# Usage: Rscript scripts/build-browser.R [output-directory]
args <- commandArgs(trailingOnly = TRUE)
destination <- if (length(args)) args[[1]] else ".browser-site"
if (!file.exists("app.R") || !dir.exists("www")) {
  stop("Run this script from the repository root.")
}
if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop('Install the exporter first: install.packages("shinylive")')
}

staging <- tempfile("reds-browser-app-")
dir.create(staging)
stopifnot(file.copy("app.R", staging))
dir.create(file.path(staging, "www"))
stopifnot(file.copy("www/ioc.svg", file.path(staging, "www")))

shinylive::export(
  staging,
  destination,
  template_params = list(title = "IOC REDs CAT2 Calculator")
)
file.create(file.path(destination, ".nojekyll"))
unlink(staging, recursive = TRUE)
message("Browser site exported to: ", normalizePath(destination))
