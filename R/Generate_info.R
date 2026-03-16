#' Launch the Shiny App
#'
#' This function launches the Shiny app included in the package.
#' @export
Generate_info <- function() {
  appDir <- system.file("shinyApp/app4.R", package = "SPASAM.MSE")
  if (appDir == "") {
    stop("Could not find the Shiny app directory. Try re-installing `SPASAM.MSE`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

