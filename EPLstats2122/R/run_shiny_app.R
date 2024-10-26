#' Run Shiny App
#' @description Launches the Shiny app in the package.
#' @export
run_shiny_app <- function() {
  shiny::runApp(system.file("shiny", package = "EPLstats2122"))
}
