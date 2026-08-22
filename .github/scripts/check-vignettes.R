vignettes <- list.files(
  "vignettes",
  pattern = "\\.(Rnw|Rmd)$",
  full.names = TRUE,
  recursive = FALSE
)

if (!length(vignettes)) {
  stop("No vignette source files found in vignettes/")
}

message("Building vignettes:")
print(vignettes)

status <- tools::buildVignettes(
  dir = ".",
  tangle = FALSE,
  check = TRUE,
  quiet = FALSE
)

if (!isTRUE(status)) {
  stop("Vignette build/check failed")
}
