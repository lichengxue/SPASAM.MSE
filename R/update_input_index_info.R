#' Update Index Information in Input List (with Optional Row Subsetting)
#'
#' Updates \code{input$data$agg_index_sigma} and \code{input$data$index_Neff} using the provided CV and Neff matrices.
#' Optionally removes index data (aggregated or PAA) for specified indices (pointers) and years, and updates internal flags accordingly.
#'
#' @param input List. A WHAM input list that must contain \code{input$data}.
#' @param agg_index_sigma Numeric matrix. Aggregated index standard deviations (CVs) to update. 
#' Can be full size (\code{n_years × n_indices}) or a subset of rows (\code{length(ind_em) × n_indices}).
#' @param index_Neff Numeric matrix. Effective sample size for index data. Must match the dimensions of \code{agg_index_sigma}.
#' @param remove_agg Logical. Whether to remove aggregated index observations for specific indices/years.
#' @param remove_agg_pointer Integer vector. Index pointers (columns) for which aggregated index data should be removed.
#' @param remove_agg_years Integer vector or matrix. Years (rows) to remove aggregated index data. Can be a vector (applies to all pointers) or a matrix with dimensions [years × pointers].
#' @param remove_paa Logical. Whether to remove index PAA observations for specific indices/years.
#' @param remove_paa_pointer Integer vector. Index pointers (columns) for which index PAA data should be removed.
#' @param remove_paa_years Integer vector or matrix. Years (rows) to remove index PAA data. Can be a vector (applies to all pointers) or a matrix with dimensions [years × pointers].
#' @param ind_em Integer vector or NULL. If provided, only updates rows corresponding to these indices. If NULL, updates all rows.
#'
#' @return The modified \code{input} list with updated \code{agg_index_sigma}, \code{index_Neff}, and updated index flags.
#'
#' @details
#' If \code{ind_em} is specified, only those rows are updated in the input matrices.
#' Removal of aggregated index or PAA observations is handled by setting corresponding flags and array elements to zero.
#' The function automatically calls \code{set_osa_obs()} at the end to refresh OSA flags.
#'
#' @export
update_input_index_info <- function(input, agg_index_sigma, index_Neff, 
                                    remove_agg = FALSE, remove_agg_pointer = NULL, remove_agg_years = NULL,
                                    remove_paa = FALSE, remove_paa_pointer = NULL, remove_paa_years = NULL,
                                    ind_em = NULL) {
  # Validate dimensions
  if (!all(dim(agg_index_sigma) == dim(index_Neff))) {
    stop("agg_index_sigma and index_Neff must have the same dimensions.")
  }
  
  if (is.null(ind_em)) {
    # Update full matrices
    input$data$agg_index_sigma <- agg_index_sigma
    input$data$index_Neff <- index_Neff
    
    if (remove_agg) {
      if (is.null(remove_agg_pointer)) stop("remove_agg_pointer must be specified!")
      if (is.null(remove_agg_years)) stop("remove_agg_years must be specified!")
      
      # Coerce to matrix for easy looping
      remove_agg_years_mat <- if (is.matrix(remove_agg_years)) {
        remove_agg_years
      } else {
        matrix(remove_agg_years, ncol = length(remove_agg_pointer), nrow = length(remove_agg_years))
      }
      
      for (i in seq_along(remove_agg_pointer)) {
        ptr <- remove_agg_pointer[i]
        y <- remove_agg_years_mat[, i]
        input$data$use_indices[y, ptr] <- 0
        input$data$agg_indices[y, ptr] <- 0
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
        input$data$use_index_paa[y, ptr] <- 0
        input$data$index_paa[ptr, y, ] <- 0
      }
    }
    
    # Refresh OSA observations
    input <- set_osa_obs(input)
    
  } else {
    # Update only selected rows (ind_em)
    input$data$agg_index_sigma <- agg_index_sigma[ind_em, , drop = FALSE]
    input$data$index_Neff <- index_Neff[ind_em, , drop = FALSE]
    
    # Check and disable PAA flags if fully zero
    all_zero_paa <- apply(input$data$index_paa, c(1, 2), function(x) all(x == 0))
    if (any(all_zero_paa)) {
      id <- which(all_zero_paa, arr.ind = TRUE)
      for (i in seq_len(nrow(id))) {
        input$data$use_index_paa[id[i, 2], id[i, 1]] <- 0
      }
    }
    
    # Check and disable both agg and PAA flags if fully zero
    all_zero_agg <- apply(input$data$agg_indices, c(1, 2), function(x) all(x == 0))
    if (any(all_zero_agg)) {
      id <- which(all_zero_agg, arr.ind = TRUE)
      for (i in seq_len(nrow(id))) {
        input$data$use_indices[id[i, 1], id[i, 2]] <- 0
        input$data$use_index_paa[id[i, 1], id[i, 2]] <- 0
      }
    }
    
    input <- set_osa_obs(input)
  }
  
  return(input)
}