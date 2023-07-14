#' Title
#'
#' @param filename
#' @param plot
#' @param display
#' @param w
#' @param h
#' @param theme
#' @param extensions
#'
#' @return
#' @export
#'
#' @examples
plot2 <- function (filename, plot = last_plot(),
                         display = FALSE, w = 15, h = 10, theme = theme_minimal(),
                         map = FALSE, extensions = c("pdf", "png", "svg"))
{
  require(ggplot2)
  require(svglite)
  # change to theme_map if map
  if (map) { theme = ggthemes::theme_map()}

  # plot's temporary setting
  temp <- plot + theme

  # extensions
  map(extensions, function(ext) {
    # if pdf
    if (ext == "pdf") {
      dir.create(paste("./", ext, sep = ""))
      ggplot2::ggsave(file = paste0("./", ext, "/", filename, ".", ext), plot = temp,
                      width = w, height = h, useDingbats = FALSE)
    }
    # others
    else {
      dir.create(paste("./", ext, sep = ""))
      ggplot2::ggsave(file = paste0("./", ext, "/", filename, ".", ext), plot = temp,
                      width = w, height = h)
    }
  })
  # display only if true
  if (display) temp
}
