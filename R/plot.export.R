#' Title
#'
#' @param title
#' @param id
#' @param project
#' @param subtitle
#' @param plot
#' @param display
#' @param w
#' @param h
#' @param caption
#' @param theme
#' @param extensions
#'
#' @return
#' @export
#'
#' @examples
plot.export <- function (title, id, project, subtitle = "", plot = last_plot(),
                      display = TRUE, w = 15, h = 10, caption = Sys.time(), theme = theme_minimal(),
                      map = FALSE, extensions = c("pdf", "png"))
{
  # change to theme_map if map
  if (map) { theme = ggthemes::theme_map()}

  # plot's temporary setting
  temp <- plot + theme + labs(title = title, subtitle = subtitle,
                              caption = caption)
  # extensions
  map(extensions, function(ext) {
    # if pdf
    if (extensions[ext] == "pdf") {
      dir.create(paste("./", extensions[ext], sep = ""))
      ggplot2::ggsave(file = paste("./", extensions[ext],
                                   "/", extensions[ext], "_", id, "_", project,
                                   ".", extensions[ext], sep = ""), plot = temp,
                      width = w, height = h, useDingbats = FALSE)
    }
    # others
    else {
      dir.create(paste("./", extensions[ext], sep = ""))
      ggplot2::ggsave(file = paste("./", extensions[ext],
                                   "/", extensions[ext], "_", id, "_", project,
                                   ".", extensions[ext], sep = ""), plot = temp,
                      width = w, height = h)
    }
  })
  # display only if true
  if (display) temp
}
