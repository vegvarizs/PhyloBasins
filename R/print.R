#' @export
print.pb_project <- function(x, ...) {

  cat("<PhyloBasins project>\n")

  cat("\nTree:\n")

  if (is.null(x$tree)) {
    cat("  not loaded\n")
  } else {
    cat("  loaded\n")
  }

  invisible(x)
}
