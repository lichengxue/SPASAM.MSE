#' Launch the Shiny App
#'
#' This function launches the Shiny app included in the package.
#' @export
Build_move <- function() {
  appDir <- system.file("shinyApp/app3.R", package = "SPASAM.MSE")
  if (appDir == "") {
    stop("Could not find the Shiny app directory. Try re-installing `SPASAM.MSE`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

