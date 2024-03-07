#' @keywords internal
#' @importFrom cachem cache_disk
"_PACKAGE"

#' params
#'
#' Internal dataset for section/commodity codes.
#'
#' @docType data
#' @keywords datasets
#' @name params
NULL

#' agtpa_applications
#'
#' Internal dataset for GF estimation.
#'
#' @docType data
#' @keywords datasets
#' @name agtpa_applications
NULL

styles <- list(
  skin_color = "blue",
  css_files = "custom.css"
)

shiny::shinyOptions(
  cache = cache_disk(
    dir = "/tradestatistics/escapstructuralgravity_cache"
    # logfile = "/tradestatistics/log/escapstructuralgravity_cache.log"
  )
)
