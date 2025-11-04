#' Launch Water Quality Dashboard
#'
#' Launches an interactive Shiny dashboard to explore the water quality dataset
#'
#' @export
#' @examples
#' if (interactive()) {
#' yarra_view()
#' }
yarra_view <- function() {
  appDir <- system.file("shiny", package = "yarraView")
  if (appDir == "") {
    stop("Could not find dashboard directory. Try re-installing the package.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}