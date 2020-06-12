#' Title
#'
#' @param file
#' @param df
#'
#' @return
#' @export
#'
#' @examples
readxl.all <- function(file, df=FALSE) {
  sheets <- readxl::excel_sheets(file)
  if (df) {
  list <- purrr::map_df(sheets, function(x) readxl::read_excel(file, sheet=x))
  } else {
  list <- purrr::map(sheets, function(x) readxl::read_excel(file, sheet=x))
  }
  return(list)
}
