#' Update Estimation Model Inputs with Aggregated Data
#'
#' Update an estimation-model information object by replacing fleet- and
#' index-related inputs with aggregated versions derived from user-specified
#' fleet and index mappings. This function is primarily intended for
#' panmictic/spatially aggregated assessment configurations, where multiple
#' fleets or indices from the operating model are combined into a reduced set
#' of fleets and indices for the estimation model.
#'
#' The function updates:
#' \itemize{
#'   \item \code{par_inputs} entries related to catch and index observations,
#'   \item numbers of fleets and indices in \code{par_inputs} and
#'     \code{basic_info},
#'   \item fleet and index region pointers,
#'   \item user-defined weight-at-age (\code{user_waa}) after aggregation, and
#'   \item \code{basic_info$waa} so that the EM uses the aggregated WAA values.
#' }
#'
#' Fleet and index mappings are specified using pointer vectors:
#' \itemize{
#'   \item \code{fleet_pointer}: maps original fleets to aggregated fleets,
#'   \item \code{index_pointer}: maps original indices to aggregated indices.
#' }
#'
#' Pointer values of \code{0} exclude the corresponding fleet or index from the
#' aggregated EM.
#'
#' @param om List. Operating model object. Expected to contain
#'   \code{om$input$data}, including \code{agg_catch} and \code{agg_indices}.
#' @param em_info List. Estimation model information object containing at least:
#'   \itemize{
#'     \item \code{par_inputs}: model parameter inputs,
#'     \item \code{basic_info}: basic model dimensions and WAA arrays,
#'     \item optionally \code{par_inputs$user_waa}: user-defined WAA matrix.
#'   }
#' @param aggregate_catch_info List. Aggregated fleet settings. Expected
#'   components may include:
#'   \itemize{
#'     \item \code{$n_fleets}: Integer. Number of aggregated fleets.
#'     \item \code{$fleet_pointer}: Integer vector of length
#'       \code{om$input$data$n_fleets}; maps original fleets to aggregated fleets
#'       (\code{0} = exclude).
#'     \item \code{$catch_cv}: Numeric vector of length \code{n_fleets}.
#'     \item \code{$catch_Neff}: Numeric vector of length \code{n_fleets}.
#'     \item \code{$use_agg_catch}: Integer vector of length \code{n_fleets}.
#'     \item \code{$use_catch_paa}: Integer vector of length \code{n_fleets}.
#'     \item \code{$use_catch_weighted_waa}: Logical. If \code{TRUE}, fleet WAA
#'       are weighted by mean aggregate catch over \code{ind_em}; otherwise they
#'       are averaged equally within each aggregated fleet.
#'   }
#' @param aggregate_index_info List. Aggregated index settings. Expected
#'   components may include:
#'   \itemize{
#'     \item \code{$n_indices}: Integer. Number of aggregated indices.
#'     \item \code{$index_pointer}: Integer vector of length
#'       \code{om$input$data$n_indices}; maps original indices to aggregated
#'       indices (\code{0} = exclude).
#'     \item \code{$index_cv}: Numeric vector of length \code{n_indices}.
#'     \item \code{$index_Neff}: Numeric vector of length \code{n_indices}.
#'     \item \code{$fracyr_indices}: Numeric vector of length \code{n_indices}.
#'     \item \code{$q}: Numeric vector of length \code{n_indices}.
#'     \item \code{$use_indices}: Integer vector of length \code{n_indices}.
#'     \item \code{$use_index_paa}: Integer vector of length \code{n_indices}.
#'     \item \code{$units_indices}: Integer vector of length \code{n_indices}.
#'     \item \code{$units_index_paa}: Integer vector of length \code{n_indices}.
#'     \item \code{$use_index_weighted_waa}: Logical. If \code{TRUE}, index WAA
#'       are weighted by mean aggregate index values over \code{ind_em};
#'       otherwise they are averaged equally within each aggregated index.
#'   }
#' @param ind_em Integer vector. Indices specifying which years from OM data are
#'   used in the estimation model.
#'
#' @details
#' This function assumes the aggregated EM is panmictic, so the output uses:
#' \itemize{
#'   \item \code{n_regions = 1}
#'   \item \code{n_stocks = 1}
#' }
#'
#' The aggregated \code{user_waa} matrix is rebuilt in the standard WHAM order:
#' \enumerate{
#'   \item fleets,
#'   \item regions,
#'   \item indices,
#'   \item stocks.
#' }
#'
#' If \code{par_inputs$user_waa} is available, it is used as the source WAA.
#' Otherwise, \code{basic_info$waa} is used.
#'
#' @return Updated \code{em_info} list with:
#'   \itemize{
#'     \item updated \code{par_inputs},
#'     \item updated \code{basic_info},
#'     \item aggregated \code{par_inputs$user_waa},
#'     \item synchronized \code{basic_info$waa}.
#'   }
#'
#' @examples
#' \dontrun{
#' updated_em_info <- update_em_with_basic_info(
#'   om = some_operating_model,
#'   em_info = some_em_info,
#'   aggregate_catch_info = list(
#'     n_fleets = 2,
#'     fleet_pointer = c(1, 1, 2, 0),
#'     use_catch_weighted_waa = TRUE
#'   ),
#'   aggregate_index_info = list(
#'     n_indices = 2,
#'     index_pointer = c(1, 2, 0),
#'     use_index_weighted_waa = TRUE
#'   ),
#'   ind_em = 1:10
#' )
#' }
#'
#' @export
update_em_with_basic_info <- function(om,
                                      em_info,
                                      aggregate_catch_info,
                                      aggregate_index_info,
                                      ind_em) {
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  data <- om$input$data
  
  if (is.null(data$agg_catch)) {
    stop("om$input$data$agg_catch is required.", call. = FALSE)
  }
  if (is.null(data$agg_indices)) {
    stop("om$input$data$agg_indices is required.", call. = FALSE)
  }
  
  if (is.null(em_info$par_inputs)) {
    stop("em_info$par_inputs is required.", call. = FALSE)
  }
  if (is.null(em_info$basic_info)) {
    stop("em_info$basic_info is required.", call. = FALSE)
  }
  
  n_ages <- em_info$par_inputs$n_ages
  if (is.null(n_ages)) {
    stop("em_info$par_inputs$n_ages is required.", call. = FALSE)
  }
  
  n_fleets_orig  <- data$n_fleets
  n_indices_orig <- data$n_indices
  
  if (is.null(n_fleets_orig) || is.null(n_indices_orig)) {
    stop("om$input$data$n_fleets and om$input$data$n_indices are required.", call. = FALSE)
  }
  
  ## --------------------------------------------------------------------------
  ## Helper functions
  ## --------------------------------------------------------------------------
  get_source_waa <- function(em_info) {
    if (!is.null(em_info$par_inputs$user_waa)) {
      src <- em_info$par_inputs$user_waa
      if (!is.matrix(src)) {
        src <- as.matrix(src)
      }
      return(src)
    }
    
    if (!is.null(em_info$basic_info$waa)) {
      waa <- em_info$basic_info$waa
      if (length(dim(waa)) == 3) {
        ## typical layout: source x year x age
        return(waa[, 1, , drop = FALSE][, 1, drop = FALSE])
      }
      if (is.matrix(waa)) {
        return(waa)
      }
    }
    
    stop("Could not find a valid WAA source in em_info$par_inputs$user_waa or em_info$basic_info$waa.",
         call. = FALSE)
  }
  
  get_default_vec <- function(x, n, fallback = 1) {
    if (is.null(x)) {
      return(rep(fallback, n))
    }
    if (length(x) == 1L) {
      return(rep(x, n))
    }
    if (length(x) != n) {
      stop("Provided vector has incorrect length.", call. = FALSE)
    }
    x
  }
  
  make_weights <- function(values_mat, use_weighted) {
    if (!isTRUE(use_weighted)) {
      return(rep(1 / ncol(values_mat), ncol(values_mat)))
    }
    
    avg_vals <- if (is.matrix(values_mat)) {
      colMeans(values_mat, na.rm = TRUE)
    } else {
      as.numeric(values_mat)
    }
    
    if (all(is.na(avg_vals)) || sum(avg_vals, na.rm = TRUE) <= 0) {
      return(rep(1 / length(avg_vals), length(avg_vals)))
    }
    
    as.numeric(avg_vals / sum(avg_vals, na.rm = TRUE))
  }
  
  aggregate_rows <- function(mat, rows, weights = NULL, fun = c("weighted", "mean")) {
    fun <- match.arg(fun)
    
    out <- mat[rows, , drop = FALSE]
    if (nrow(out) == 1) return(as.numeric(out[1, ]))
    
    if (fun == "mean") {
      return(colMeans(out, na.rm = TRUE))
    }
    
    if (is.null(weights)) {
      stop("weights must be provided when fun = 'weighted'.", call. = FALSE)
    }
    
    if (length(weights) != nrow(out)) {
      stop("Length of weights does not match number of rows being aggregated.", call. = FALSE)
    }
    
    as.numeric(colSums(out * weights))
  }
  
  ## --------------------------------------------------------------------------
  ## Aggregated dimensions and pointers
  ## --------------------------------------------------------------------------
  fleet_pointer <- aggregate_catch_info$fleet_pointer
  if (is.null(fleet_pointer)) {
    warning("aggregate_catch_info$fleet_pointer is not specified; using all fleets in one group.")
    fleet_pointer <- rep(1L, n_fleets_orig)
  }
  if (length(fleet_pointer) != n_fleets_orig) {
    stop("Length of aggregate_catch_info$fleet_pointer must equal om$input$data$n_fleets.",
         call. = FALSE)
  }
  
  index_pointer <- aggregate_index_info$index_pointer
  if (is.null(index_pointer)) {
    warning("aggregate_index_info$index_pointer is not specified; using all indices in one group.")
    index_pointer <- rep(1L, n_indices_orig)
  }
  if (length(index_pointer) != n_indices_orig) {
    stop("Length of aggregate_index_info$index_pointer must equal om$input$data$n_indices.",
         call. = FALSE)
  }
  
  valid_fleets  <- sort(unique(fleet_pointer[fleet_pointer > 0]))
  valid_indices <- sort(unique(index_pointer[index_pointer > 0]))
  
  if (length(valid_fleets) == 0) {
    stop("No fleets remain after applying aggregate_catch_info$fleet_pointer.", call. = FALSE)
  }
  if (length(valid_indices) == 0) {
    stop("No indices remain after applying aggregate_index_info$index_pointer.", call. = FALSE)
  }
  
  n_fleets  <- aggregate_catch_info$n_fleets  %||% length(valid_fleets)
  n_indices <- aggregate_index_info$n_indices %||% length(valid_indices)
  
  if (!setequal(valid_fleets, seq_len(n_fleets))) {
    stop("Positive values in fleet_pointer must define aggregated fleets as 1:n_fleets with no gaps.",
         call. = FALSE)
  }
  if (!setequal(valid_indices, seq_len(n_indices))) {
    stop("Positive values in index_pointer must define aggregated indices as 1:n_indices with no gaps.",
         call. = FALSE)
  }
  
  ## panmictic aggregated EM
  n_regions <- 1L
  n_stocks  <- 1L
  
  ## --------------------------------------------------------------------------
  ## Update par_inputs scalars/vectors
  ## --------------------------------------------------------------------------
  em_info$par_inputs$catch_cv <- get_default_vec(
    aggregate_catch_info$catch_cv,
    n_fleets,
    em_info$par_inputs$catch_cv[1]
  )
  em_info$par_inputs$catch_Neff <- get_default_vec(
    aggregate_catch_info$catch_Neff,
    n_fleets,
    em_info$par_inputs$catch_Neff[1]
  )
  em_info$par_inputs$use_agg_catch <- get_default_vec(
    aggregate_catch_info$use_agg_catch,
    n_fleets,
    em_info$par_inputs$use_agg_catch[1]
  )
  em_info$par_inputs$use_catch_paa <- get_default_vec(
    aggregate_catch_info$use_catch_paa,
    n_fleets,
    em_info$par_inputs$use_catch_paa[1]
  )
  
  em_info$par_inputs$index_cv <- get_default_vec(
    aggregate_index_info$index_cv,
    n_indices,
    em_info$par_inputs$index_cv[1]
  )
  em_info$par_inputs$index_Neff <- get_default_vec(
    aggregate_index_info$index_Neff,
    n_indices,
    em_info$par_inputs$index_Neff[1]
  )
  em_info$par_inputs$fracyr_indices <- get_default_vec(
    aggregate_index_info$fracyr_indices,
    n_indices,
    em_info$par_inputs$fracyr_indices[1]
  )
  em_info$par_inputs$q <- get_default_vec(
    aggregate_index_info$q,
    n_indices,
    em_info$par_inputs$q[1]
  )
  em_info$par_inputs$use_indices <- get_default_vec(
    aggregate_index_info$use_indices,
    n_indices,
    em_info$par_inputs$use_indices[1]
  )
  em_info$par_inputs$use_index_paa <- get_default_vec(
    aggregate_index_info$use_index_paa,
    n_indices,
    em_info$par_inputs$use_index_paa[1]
  )
  em_info$par_inputs$units_indices <- get_default_vec(
    aggregate_index_info$units_indices,
    n_indices,
    em_info$par_inputs$units_indices[1]
  )
  em_info$par_inputs$units_index_paa <- get_default_vec(
    aggregate_index_info$units_index_paa,
    n_indices,
    em_info$par_inputs$units_index_paa[1]
  )
  
  em_info$par_inputs$n_fleets <- n_fleets
  em_info$par_inputs$n_indices <- n_indices
  em_info$basic_info$n_fleets <- n_fleets
  em_info$basic_info$n_indices <- n_indices
  
  em_info$par_inputs$n_regions <- n_regions
  em_info$par_inputs$n_stocks <- n_stocks
  em_info$basic_info$n_regions <- n_regions
  em_info$basic_info$n_stocks <- n_stocks
  
  em_info$par_inputs$fleet_regions <- rep(1L, n_fleets)
  em_info$par_inputs$index_regions <- rep(1L, n_indices)
  
  ## --------------------------------------------------------------------------
  ## Source WAA and row layout
  ## --------------------------------------------------------------------------
  source_waa <- get_source_waa(em_info)
  
  n_regions_orig <- em_info$basic_info$n_regions %||% em_info$par_inputs$n_regions
  n_stocks_orig  <- em_info$basic_info$n_stocks  %||% em_info$par_inputs$n_stocks
  
  if (nrow(source_waa) < (n_fleets_orig + n_regions_orig + n_indices_orig + n_stocks_orig)) {
    stop("Source WAA has fewer rows than expected from original fleet/region/index/stock dimensions.",
         call. = FALSE)
  }
  
  fleet_rows_orig  <- seq_len(n_fleets_orig)
  region_rows_orig <- n_fleets_orig + seq_len(n_regions_orig)
  index_rows_orig  <- n_fleets_orig + n_regions_orig + seq_len(n_indices_orig)
  stock_rows_orig  <- n_fleets_orig + n_regions_orig + n_indices_orig + seq_len(n_stocks_orig)
  
  ## --------------------------------------------------------------------------
  ## Compute weights for aggregated fleets
  ## --------------------------------------------------------------------------
  fleet_weights <- vector("list", n_fleets)
  for (f in valid_fleets) {
    rows_f <- which(fleet_pointer == f)
    agg_catch_filtered <- data$agg_catch[ind_em, rows_f, drop = FALSE]
    fleet_weights[[f]] <- make_weights(
      agg_catch_filtered,
      use_weighted = isTRUE(aggregate_catch_info$use_catch_weighted_waa)
    )
  }
  
  ## --------------------------------------------------------------------------
  ## Compute weights for aggregated indices
  ## --------------------------------------------------------------------------
  index_weights <- vector("list", n_indices)
  for (i in valid_indices) {
    rows_i <- which(index_pointer == i)
    agg_indices_filtered <- data$agg_indices[ind_em, rows_i, drop = FALSE]
    index_weights[[i]] <- make_weights(
      agg_indices_filtered,
      use_weighted = isTRUE(aggregate_index_info$use_index_weighted_waa)
    )
  }
  
  ## --------------------------------------------------------------------------
  ## Aggregate fleet WAA
  ## --------------------------------------------------------------------------
  aggregated_fleet_waa <- vector("list", n_fleets)
  for (f in valid_fleets) {
    rows_f <- fleet_rows_orig[which(fleet_pointer == f)]
    aggregated_fleet_waa[[f]] <- aggregate_rows(
      source_waa,
      rows = rows_f,
      weights = fleet_weights[[f]],
      fun = "weighted"
    )
  }
  
  ## --------------------------------------------------------------------------
  ## Aggregate region WAA (panmictic => single region)
  ## --------------------------------------------------------------------------
  aggregated_region_waa <- vector("list", n_regions)
  aggregated_region_waa[[1]] <- aggregate_rows(
    source_waa,
    rows = region_rows_orig,
    fun = "mean"
  )
  
  ## --------------------------------------------------------------------------
  ## Aggregate index WAA
  ## --------------------------------------------------------------------------
  aggregated_index_waa <- vector("list", n_indices)
  for (i in valid_indices) {
    rows_i <- index_rows_orig[which(index_pointer == i)]
    aggregated_index_waa[[i]] <- aggregate_rows(
      source_waa,
      rows = rows_i,
      weights = index_weights[[i]],
      fun = "weighted"
    )
  }
  
  ## --------------------------------------------------------------------------
  ## Aggregate stock WAA (panmictic => single stock)
  ## --------------------------------------------------------------------------
  aggregated_stock_waa <- vector("list", n_stocks)
  aggregated_stock_waa[[1]] <- aggregate_rows(
    source_waa,
    rows = stock_rows_orig,
    fun = "mean"
  )
  
  ## --------------------------------------------------------------------------
  ## Rebuild user_waa in WHAM order: fleets, regions, indices, stocks
  ## --------------------------------------------------------------------------
  new_user_waa <- do.call(
    rbind,
    c(aggregated_fleet_waa,
      aggregated_region_waa,
      aggregated_index_waa,
      aggregated_stock_waa)
  )
  
  if (!is.matrix(new_user_waa)) {
    new_user_waa <- matrix(new_user_waa, ncol = n_ages, byrow = TRUE)
  }
  
  colnames(new_user_waa) <- colnames(source_waa)
  
  em_info$par_inputs$user_waa <- new_user_waa
  em_info$basic_info$waa <- new_user_waa
  
  return(em_info)
}