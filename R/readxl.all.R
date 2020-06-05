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
  sheets <- excel_sheets(file)
  if (df) {
  list <- map_df(sheets, function(x) read_excel(file, sheet=x))
  } else {
  list <- map(sheets, function(x) read_excel(file, sheet=x))
  }
  return(list)
}
