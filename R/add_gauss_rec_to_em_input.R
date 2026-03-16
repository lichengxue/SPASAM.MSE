#' Add Gaussian environmental recruitment settings to EM input
#'
#' Adds an optional Gaussian environmental effect on recruitment to a WHAM-style
#' EM input object. This function modifies both \code{data}, \code{par}, and
#' \code{map} components so the resulting object can be passed to model fitting.
#'
#' If \code{gauss_rec_em$use = FALSE}, placeholder values are inserted and the
#' corresponding parameters are fixed via \code{map}.
#'
#' @param em_input List. Assessment model input object.
#' @param gauss_rec_em List specifying the Gaussian recruitment effect.
#'
#' @return Modified \code{em_input} object.
#'
#' @export
add_gauss_rec_to_em_input <- function(em_input, gauss_rec_em) {
  
  if (is.null(gauss_rec_em)) return(em_input)
  if (is.null(gauss_rec_em$use)) gauss_rec_em$use <- FALSE
  
  n_stocks <- em_input$data$n_stocks
  n_Ecov <- em_input$data$n_Ecov
  
  if (!isTRUE(gauss_rec_em$use)) {
    em_input$data$use_gauss_T_rec <- 0L
    em_input$data$Ecov_rec_T_col <- 0L
    
    em_input$par$Topt_rec <- 0
    em_input$par$log_width_rec <- 0
    em_input$par$beta_T_rec <- rep(0, n_stocks)
    
    if (is.null(em_input$map)) em_input$map <- list()
    em_input$map$Topt_rec <- factor(NA)
    em_input$map$log_width_rec <- factor(NA)
    em_input$map$beta_T_rec <- factor(rep(NA, n_stocks))
    
    return(em_input)
  }
  
  if (is.null(gauss_rec_em$Ecov_rec_T_col)) {
    stop("gauss_rec_em$Ecov_rec_T_col must be provided when gauss_rec_em$use = TRUE.")
  }
  
  Ecov_col_R <- gauss_rec_em$Ecov_rec_T_col
  
  if (Ecov_col_R < 1 || Ecov_col_R > n_Ecov) {
    stop("gauss_rec_em$Ecov_rec_T_col is out of range for em_input$data$n_Ecov.")
  }
  
  Ecov_col_TMB <- as.integer(Ecov_col_R - 1L)
  
  if (is.null(gauss_rec_em$Topt_rec)) {
    stop("gauss_rec_em$Topt_rec must be provided when gauss_rec_em$use = TRUE.")
  }
  
  if (is.null(gauss_rec_em$width_rec)) {
    stop("gauss_rec_em$width_rec must be provided when gauss_rec_em$use = TRUE.")
  }
  
  if (gauss_rec_em$width_rec <= 0) {
    stop("gauss_rec_em$width_rec must be > 0.")
  }
  
  if (is.null(gauss_rec_em$beta_T_rec)) {
    gauss_rec_em$beta_T_rec <- rep(0, n_stocks)
  }
  
  if (length(gauss_rec_em$beta_T_rec) == 1) {
    gauss_rec_em$beta_T_rec <- rep(gauss_rec_em$beta_T_rec, n_stocks)
  }
  
  if (length(gauss_rec_em$beta_T_rec) != n_stocks) {
    stop("gauss_rec_em$beta_T_rec must have length 1 or n_stocks.")
  }
  
  em_input$data$use_gauss_T_rec <- 1L
  em_input$data$Ecov_rec_T_col <- Ecov_col_TMB
  
  em_input$par$Topt_rec <- gauss_rec_em$Topt_rec
  em_input$par$log_width_rec <- log(gauss_rec_em$width_rec)
  em_input$par$beta_T_rec <- as.numeric(gauss_rec_em$beta_T_rec)
  
  if (is.null(em_input$map)) em_input$map <- list()
  
  if (isTRUE(gauss_rec_em$estimate)) {
    if (is.null(em_input$map$Topt_rec)) {
      em_input$map$Topt_rec <- factor(1)
    }
    if (is.null(em_input$map$log_width_rec)) {
      em_input$map$log_width_rec <- factor(1)
    }
    if (is.null(em_input$map$beta_T_rec)) {
      em_input$map$beta_T_rec <- factor(seq_len(n_stocks))
    }
  } else {
    em_input$map$Topt_rec <- factor(NA)
    em_input$map$log_width_rec <- factor(NA)
    em_input$map$beta_T_rec <- factor(rep(NA, n_stocks))
  }
  
  return(em_input)
}