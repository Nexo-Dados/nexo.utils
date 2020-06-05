#' Quantile plot
#'
#' @param x numerical or integer vector
#' @param quantiles
#' @param by
#' @param range
#'
#' @return
#' @export
#'
#' @examples
quantile.plot <- function(x, q=100) {
  require(ggplot2)
  require(scales)
  quantiles <- seq(c(0.01:1), by=1/q)
  y <- quantile(x, quantiles, na.rm=T)
  qplot(quantiles, y) +
    scale_x_continuous() +
    scale_y_continuous(labels=comma) +
    labs(x="Quantiles", y="Values")
}
