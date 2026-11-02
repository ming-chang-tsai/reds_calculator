# Verify the export contains exactly the existing public app and logo.
# Usage: Rscript scripts/check-browser.R [export-directory]
args <- commandArgs(trailingOnly = TRUE)
site <- if (length(args)) args[[1]] else ".browser-site"
bundle <- jsonlite::fromJSON(file.path(site, "app.json"))
stopifnot(setequal(bundle$name, c("app.R", "www/ioc.svg")))
read_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
for (name in bundle$name) {
  stopifnot(identical(
    sub("\n$", "", bundle$content[bundle$name == name]),
    read_text(name)
  ))
}
stopifnot(file.exists(file.path(site, ".nojekyll")))
stopifnot(!grepl("http-equiv=\"refresh\"", read_text(file.path(site, "index.html"))))

# Check all possible yes/no combinations allowed by the app's exclusions.
# This verifies mapping coverage and the severe-primary weight, without
# changing the established scoring rules.
app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env)
checked <- 0L
for (sex in c("Male", "Female")) {
  nsecondary <- length(app_env$secondary_labels[[sex]])
  combinations <- expand.grid(rep(list(0:1), 7L + nsecondary))
  for (i in seq_len(nrow(combinations))) {
    values <- as.integer(combinations[i, ])
    p <- values[1:7]
    s <- values[8:length(values)]
    if (p[4] + s[2] > 1) next
    if (sex == "Male" && p[1] + p[2] > 1) next
    if (sex == "Female" && p[1] + p[2] + s[4] > 1) next
    primary <- 2L * p[1] + sum(p[-1])
    secondary <- sum(s)
    mapping <- app_env$mapping
    hit <- mapping[mapping$sex == sex & mapping$primary == primary &
                     mapping$secondary == secondary, ]
    stopifnot(nrow(hit) == 1L)
    checked <- checked + 1L
  }
}
message("Verified unchanged exported source and ", checked, " reachable input combinations.")
