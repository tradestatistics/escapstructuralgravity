#' @rdname available
#' @noRd
available_logicals <- function() {
  c("Yes" = "yes", "No" = "no")
}

#' @rdname available
#' @noRd
available_models <- function() {
  c(
    "Poisson Pseudo Maximum Likelihood (PPML)" = "ppml",
    "Ordinary Least Squares (OLS) [Not Recommended]" = "ols"
  )
}

#' Create an url
#' @param url the URL
#' @param text the text to display
#' @return an a tag
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' @importFrom shiny tags
#' @export
enurl <- function(url, text) {
  tags$a(href = url, text)
}

#' Columns wrappers
#' @param ... additional parameters for a column division
#' @importFrom shiny column
#' @rdname cols
#' @export
col_12 <- function(...) {
  column(12, ...)
}

#' @rdname cols
#' @export
col_10 <- function(...) {
  column(10, ...)
}

#' @rdname cols
#' @export
col_9 <- function(...) {
  column(9, ...)
}

#' @rdname cols
#' @export
col_8 <- function(...) {
  column(8, ...)
}

#' @rdname cols
#' @export
col_6 <- function(...) {
  column(6, ...)
}

#' @rdname cols
#' @export
col_4 <- function(...) {
  column(4, ...)
}

#' @rdname cols
#' @export
col_3 <- function(...) {
  column(3, ...)
}

#' @rdname cols
#' @export
col_2 <- function(...) {
  column(2, ...)
}

#' @rdname cols
#' @export
col_1 <- function(...) {
  column(1, ...)
}
