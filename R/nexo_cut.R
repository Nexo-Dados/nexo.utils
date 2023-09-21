#' Title
#'
#' @param values
#' @param breaks
#' @param starting
#' @param binding
#' @param spacing
#' @param under_text
#' @param over_text
#'
#' @return
#' @export
#'
#' @examples
nexo_cut <- function(values, breaks,
                     starting = "De", binding="a", spacing=" ",
                     under_text="Menos de", over_text="Mais de") {
  # based on pretty_cut
  require(scales)
  labels <- map_chr(seq(1, length(breaks)-1), function(i){

    value1 = breaks[i]
    value2 = breaks[i+1]
    if (is.infinite(value1)) {
      paste0(under_text, spacing,
             number(value2, accuracy = 1, decimal.mark = ",", big.mark = "."))
    } else if (is.infinite(value2)) {
      paste0(over_text, spacing,
             number(value1, accuracy = 1, decimal.mark = ",", big.mark = "."))
    }  else {
      paste0(starting, spacing,
             number(value1, accuracy = 1, decimal.mark = ",", big.mark = "."),
             spacing, binding, spacing,
             number(value2, accuracy = 1, decimal.mark = ",", big.mark = "."))
    }
  })
  cut(values, breaks=breaks, labels=labels)
}
