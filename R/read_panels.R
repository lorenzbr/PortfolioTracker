#' Read panels
#'
#' @usage read_panels(path_panels)
#' @param path_panels A single character string. Path of panels.
#'
#' @return A list containing data frames with panels.
#'
#' @export
read_panels <- function(path_panels) {

  if (length(list.files(path_panels)) > 0) {

    files <- file.path(path_panels, list.files(path_panels))
    list_panels <- lapply(files, data.table::fread)

  }

  return(list_panels)

}
