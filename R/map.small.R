#' Title
#'
#' @param df
#' @param fill
#' @param region
#' @param factor
#' @param ncolors
#'
#' @return
#' @export
#'
#' @examples
map.small <- function(df,
                      fill,
                      region,
                      factor = T,
                      ncolors = 7) {

  # filter if small
  df %>% filter(is_small == "Yes") -> smol

  if (factor) {
    map_data("world") %>%
      mutate(region = ifelse(
        region == "French Guiana",
        "France",
        ifelse(region == "Greenland", "Denmark", region)
      )) %>%
      left_join(df, by = c("region")) %>%  filter(is.na(is_small)) %>%
      ggplot() +
      geom_polygon(aes(
        x = long,
        y = lat,
        group = group,
        fill = n,
        label = region
      ), col = "gray14") +
      geom_point(
        data = smol,
        aes(
          x = as.numeric(ponto_lon),
          y = as.numeric(ponto_lat),
          fill = n
        ),
        color = "gray12",
        shape = 21,
        position = "dodge"
      ) +
      ggalt::coord_proj() +
      scale_fill_manual(values =  (colorRampPalette(
        RColorBrewer::brewer.pal(4, "YlOrRd")
      )(ncolors)), drop = F)
    ext.functions::extplot("Total recebido", "Total recebido", 553)
  } else{
    map_data("world") %>%
      mutate(region = ifelse(
        region == "French Guiana",
        "France",
        ifelse(region == "Greenland", "Denmark", region)
      )) %>%
      left_join(df, by = c("region")) %>%  filter(is.na(is_small)) %>%
      ggplot() +
      geom_polygon(aes(
        x = long,
        y = lat,
        group = group,
        fill = n,
        label = region
      ), col = "gray14") +
      geom_point(
        data = smol,
        aes(
          x = as.numeric(ponto_lon),
          y = as.numeric(ponto_lat),
          fill = n
        ),
        color = "gray12",
        shape = 21,
        position = "dodge"
      ) +
      ggalt::coord_proj() +
      scale_fill_continuous()
  }
}
