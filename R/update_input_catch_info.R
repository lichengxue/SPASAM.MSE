#' Update Catch CV and Neff in Input List (with Optional Row Subsetting)
#'
#' Updates \code{input$data$agg_catch_sigma} and \code{input$data$catch_Neff} using the provided CV and Neff matrices.
#' Optionally removes catch data (aggregated or PAA) for specified fleets and years, and updates internal flags accordingly.
#'
#' @param input List. A WHAM input list that must contain \code{input$data}.
#' @param agg_catch_sigma Numeric matrix. Aggregated catch standard deviations (CVs) to update. 
#' Can be full size (\code{n_years × n_fleets}) or a subset of rows (\code{length(ind_em) × n_fleets}).
#' @param catch_Neff Numeric matrix. Effective sample size for catch data. Must match the dimensions of \code{agg_catch_sigma}.
#' @param remove_agg Logical. Whether to remove aggregated catch observations for specific fleets/years.
#' @param remove_agg_pointer Integer vector. Fleet pointers (columns) for which aggregated catch data should be removed.
#' @param remove_agg_years Integer vector or matrix. Years (rows) to remove aggregated catch data. Can be a vector (applies to all fleets) or a matrix with dimensions [years × fleets].
#' @param remove_paa Logical. Whether to remove catch PAA observations for specific fleets/years.
#' @param remove_paa_pointer Integer vector. Fleet pointers (columns) for which catch PAA data should be removed.
#' @param remove_paa_years Integer vector or matrix. Years (rows) to remove catch PAA data. Can be a vector (applies to all fleets) or a matrix with dimensions [years × fleets].
#' @param ind_em Integer vector or NULL. Row indices (years) to update. If NULL, updates all rows.
#'
#' @return The modified \code{input} list with updated \code{agg_catch_sigma}, \code{catch_Neff}, and updated catch flags.
#' @export
#'
#' @details
#' If \code{ind_em} is specified, only those rows are updated in the input matrices.
#' Removal of aggregated catch or PAA observations is handled by setting corresponding flags and array elements to zero.
#'
#' @examples
#' # Example (assuming input is a WHAM input list)
#' # input <- update_input_catch_info(input, agg_catch_sigma, catch_Neff)
#' # input <- update_input_catch_info(input, agg_catch_sigma, catch_Neff, remove_agg = TRUE, remove_agg_pointer = 1, remove_agg_years = 5:10)
update_input_catch_info <- function(input, agg_catch_sigma, catch_Neff, 
                                    remove_agg = FALSE, remove_agg_pointer = NULL, remove_agg_years = NULL,
                                    remove_paa = FALSE, remove_paa_pointer = NULL, remove_paa_years = NULL,
                                    ind_em = NULL) {
  # Validate dimensions
  if (!all(dim(agg_catch_sigma) == dim(catch_Neff))) {
    stop("agg_catch_sigma and catch_Neff must have the same dimensions.")
  }
  
  if (is.null(ind_em)) {
    # Update full matrices
    input$data$agg_catch_sigma <- agg_catch_sigma
    input$data$catch_Neff <- catch_Neff
    
    if (remove_agg) {
      if (is.null(remove_agg_pointer)) stop("remove_agg_pointer must be specified!")
      if (is.null(remove_agg_years)) stop("remove_agg_years must be specified!")
      
      remove_agg_years_mat <- if (is.matrix(remove_agg_years)) {
        remove_agg_years
      } else {
        matrix(remove_agg_years, ncol = length(remove_agg_pointer), nrow = length(remove_agg_years))
      }
      
      for (i in seq_along(remove_agg_pointer)) {
        ptr <- remove_agg_pointer[i]
        y <- remove_agg_years_mat[, i]
        input$data$use_catch[y, ptr] <- 0
        input$data$agg_catch[y, ptr] <- 0
      }
    }
    
    if (remove_paa) {
      if (is.null(remove_paa_pointer)) stop("remove_paa_pointer must be specified!")
      if (is.null(remove_paa_years)) stop("remove_paa_years must be specified!")
      
      remove_paa_years_mat <- if (is.matrix(remove_paa_years)) {
        remove_paa_years
      } else {
        matrix(remove_paa_years, ncol = length(remove_paa_pointer), nrow = length(remove_paa_years))
      }
      
      for (j in seq_along(remove_paa_pointer)) {
        ptr <- remove_paa_pointer[j]
        y <- remove_paa_years_mat[, j]
        input$data$use_catch_paa[y, ptr] <- 0
        input$data$catch_paa[ptr, y, ] <- 0
      }
    }
    
    input <- set_osa_obs(input)
    
  } else {
    # Update only selected rows
    input$data$agg_catch_sigma[ind_em, ] <- agg_catch_sigma[ind_em, , drop = FALSE]
    input$data$catch_Neff[ind_em, ] <- catch_Neff[ind_em, , drop = FALSE]
    
    # Disable PAA flags if fully zero
    all_zero_paa <- apply(input$data$catch_paa, c(1, 2), function(x) all(x == 0))
    if (any(all_zero_paa)) {
      id <- which(all_zero_paa, arr.ind = TRUE)
      for (i in seq_len(nrow(id))) {
        input$data$use_catch_paa[id[i, 2], id[i, 1]] <- 0
      }
    }
    
    # Disable both agg and PAA flags if fully zero
    all_zero_agg <- apply(input$data$agg_catch, c(1, 2), function(x) all(x == 0))
    if (any(all_zero_agg)) {
      id <- which(all_zero_agg, arr.ind = TRUE)
      for (i in seq_len(nrow(id))) {
        input$data$use_catch[id[i, 1], id[i, 2]] <- 0
        input$data$use_catch_paa[id[i, 1], id[i, 2]] <- 0
      }
    }
    
    input <- set_osa_obs(input)
  }
  
  return(input)
}