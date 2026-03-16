#' Aggregate Operating-Model Data for a Reduced Estimation Model
#'
#' Aggregate catch, survey index, proportions-at-age, weight-at-age, and
#' maturity-at-age information from an operating model (`om`) into a reduced
#' estimation-model data structure stored in `em_info`.
#'
#' This function is primarily intended for simplified or panmictic estimation
#' model configurations, especially when multiple operating-model fleets and/or
#' indices are combined into fewer groups for the estimation model. Aggregation
#' is controlled through user-supplied pointer vectors that define which
#' original fleets or indices belong to each aggregated group.
#'
#' In addition to aggregating observations, the function updates the associated
#' estimation-model inputs stored in `em_info$par_inputs`, including:
#' \itemize{
#'   \item aggregated catch and index observations,
#'   \item aggregated proportions-at-age,
#'   \item aggregated fleet- and index-level parameter vectors,
#'   \item aggregated weight-at-age (`user_waa`), and
#'   \item aggregated maturity-at-age (`user_maturity`).
#' }
#'
#' Fleets or indices assigned a pointer value of `0` are excluded from the
#' aggregated estimation-model data.
#'
#' @param om List. Operating-model object containing input data and simulated
#'   observations. This function expects `om$input$data` to contain objects such
#'   as `agg_catch`, `catch_paa`, `agg_indices`, and `index_paa`.
#'
#' @param em_info List. Estimation-model information object. The function
#'   updates and returns this object. At minimum, `em_info$par_inputs` should
#'   contain the parameter vectors and biological inputs needed for aggregation,
#'   and `em_info$basic_info` should contain the corresponding WAA pointer
#'   structure.
#'
#' @param ind_em Integer vector. Indices of years to retain from the operating
#'   model when constructing the reduced estimation-model data.
#'
#' @param aggregate_catch_info List or `NULL`. Catch aggregation settings used
#'   when combining multiple operating-model fleets into fewer estimation-model
#'   fleets.
#'
#'   Expected components may include:
#'   \itemize{
#'     \item `n_fleets`: Integer. Number of aggregated fleets in the estimation
#'       model. In practice, this should match the number of unique positive
#'       values in `fleet_pointer`.
#'     \item `catch_cv`: Numeric vector. Catch coefficients of variation.
#'       Can be supplied either at the original fleet level or at the aggregated
#'       fleet-group level.
#'     \item `catch_Neff`: Numeric vector. Effective sample sizes for catch
#'       compositions. Can be supplied either at the original or aggregated
#'       fleet-group level.
#'     \item `use_agg_catch`: Integer vector of 0/1 values. Indicator for
#'       whether each aggregated fleet uses total catch observations.
#'     \item `use_catch_paa`: Integer vector of 0/1 values. Indicator for
#'       whether each aggregated fleet uses catch proportions-at-age.
#'     \item `fleet_pointer`: Integer vector of length equal to the number of
#'       original operating-model fleets. Values indicate fleet membership in
#'       aggregated groups. A value of `0` excludes that fleet.
#'     \item `use_catch_weighted_waa`: Logical. If `TRUE`, aggregated
#'       fleet-level weight-at-age is weighted by annual catch within each
#'       aggregated fleet group. If `FALSE`, equal weights are used across
#'       fleets within each group.
#'   }
#'
#'   \strong{Important:} `use_catch_weighted_waa` directly affects the
#'   aggregated fleet WAA blocks used by panmictic/type-1 estimation models.
#'
#' @param aggregate_index_info List or `NULL`. Index aggregation settings used
#'   when combining multiple operating-model indices into fewer estimation-model
#'   indices.
#'
#'   Expected components may include:
#'   \itemize{
#'     \item `n_indices`: Integer. Number of aggregated indices in the
#'       estimation model. In practice, this should match the number of unique
#'       positive values in `index_pointer`.
#'     \item `index_cv`: Numeric vector. Coefficients of variation for index
#'       observations. Can be supplied either at the original index level or at
#'       the aggregated index-group level.
#'     \item `index_Neff`: Numeric vector. Effective sample sizes for index
#'       compositions. Can be supplied either at the original or aggregated
#'       index-group level.
#'     \item `fracyr_indices`: Numeric vector. Fraction of the year associated
#'       with each index observation.
#'     \item `q`: Numeric vector. Initial survey catchability values.
#'     \item `use_indices`: Integer vector of 0/1 values. Indicator for whether
#'       each aggregated index is used.
#'     \item `use_index_paa`: Integer vector of 0/1 values. Indicator for
#'       whether each aggregated index uses index proportions-at-age.
#'     \item `units_indices`: Integer vector. Observation units for total index
#'       values, typically biomass (`1`) or numbers (`2`).
#'     \item `units_index_paa`: Integer vector. Observation units for index
#'       composition values, typically biomass (`1`) or numbers (`2`).
#'     \item `index_pointer`: Integer vector of length equal to the number of
#'       original operating-model indices. Values indicate index membership in
#'       aggregated groups. A value of `0` excludes that index.
#'     \item `use_catch_weighted_waa`: Logical. If `TRUE`, aggregated
#'       index-level weight-at-age is weighted by annual index magnitude within
#'       each aggregated index group. If `FALSE`, equal weights are used across
#'       indices within each group.
#'   }
#'
#'   \strong{Note:} for backward compatibility with earlier code, index-level
#'   WAA weighting uses `aggregate_index_info$use_catch_weighted_waa`.
#'
#' @param aggregate_weights_info List or `NULL`. Optional settings for
#'   computing weighted averages for spawning weight-at-age and maturity-at-age
#'   after aggregation.
#'
#'   Supported components may include:
#'   \itemize{
#'     \item `ssb_waa_weights`: List controlling how aggregated spawning
#'       weight-at-age is weighted.
#'     \item `maturity_weights`: List controlling how aggregated maturity-at-age
#'       is weighted.
#'   }
#'
#'   Each of these may contain:
#'   \itemize{
#'     \item `fleet`: Logical. If `TRUE`, use an aggregated fleet-weight matrix.
#'     \item `index`: Logical. If `TRUE`, use an aggregated index-weight matrix.
#'     \item `pointer`: Integer. Aggregated fleet or index group to use when
#'       borrowing weights.
#'   }
#'
#' @return A modified version of `em_info` with aggregated data and updated
#'   estimation-model inputs. The returned object typically includes updates to:
#'   \itemize{
#'     \item `em_info$par_inputs$agg_catch`
#'     \item `em_info$par_inputs$catch_paa`
#'     \item `em_info$par_inputs$agg_indices`
#'     \item `em_info$par_inputs$index_paa`
#'     \item `em_info$par_inputs$user_waa$waa`
#'     \item `em_info$par_inputs$user_maturity`
#'     \item `em_info$par_inputs$n_regions`
#'     \item `em_info$par_inputs$n_stocks`
#'     \item `em_info$par_inputs$n_fleets`
#'     \item `em_info$par_inputs$n_indices`
#'     \item `em_info$basic_info$waa`
#'     \item the associated `waa_pointer_*` objects in both `basic_info` and
#'       `par_inputs$user_waa`
#'   }
#'
#' @details
#' The function performs the following steps:
#'
#' \strong{1. Aggregate fleet- and index-level parameter vectors}
#'
#' Parameter vectors such as `catch_cv`, `catch_Neff`, `index_cv`, `index_Neff`,
#' `q`, and other indicators are aggregated according to the supplied pointer
#' vectors. If supplied at the original fleet or index level, values are
#' collapsed to aggregated groups by taking the mean within each group. If
#' already supplied at the aggregated-group level, the values are used directly.
#'
#' \strong{2. Aggregate total catch and index observations}
#'
#' For each aggregated fleet or index group, annual observations are summed
#' across all original fleets or indices assigned to that group.
#'
#' \strong{3. Aggregate proportions-at-age}
#'
#' Catch and index proportions-at-age are aggregated using annual weighted
#' averages, where the weights are the corresponding annual total catches or
#' annual total index values for each original fleet or index. The resulting
#' proportions-at-age are normalized to sum to 1 within year when the weighted
#' total is positive.
#'
#' \strong{4. Aggregate fleet- and index-level WAA}
#'
#' Fleet-level and index-level weight-at-age are aggregated within each pointer
#' group using either:
#' \itemize{
#'   \item annual catch- or index-based weights, if
#'     `use_catch_weighted_waa = TRUE`, or
#'   \item equal weights across members of the group otherwise.
#' }
#'
#' \strong{5. Aggregate spawning WAA and maturity}
#'
#' Spawning weight-at-age and maturity-at-age are collapsed to a single
#' aggregated block. By default, equal weights are used across available
#' source blocks. Alternatively, weights may be borrowed from a chosen
#' aggregated fleet or index group via `aggregate_weights_info`.
#'
#' \strong{6. Reset EM dimensions}
#'
#' After aggregation, the estimation-model dimensions are reset to:
#' \itemize{
#'   \item `n_regions = 1`
#'   \item `n_stocks = 1`
#'   \item `n_fleets =` number of valid aggregated fleet groups
#'   \item `n_indices =` number of valid aggregated index groups
#' }
#'
#' and all WAA pointers are updated accordingly.
#'
#' @section Assumptions about input dimensions:
#' This function assumes:
#' \itemize{
#'   \item `om$input$data$agg_catch` is indexed by year and fleet.
#'   \item `om$input$data$catch_paa` is indexed by fleet, year, and age.
#'   \item `om$input$data$agg_indices` is indexed by year and index.
#'   \item `om$input$data$index_paa` is indexed by index, year, and age.
#'   \item `em_info$par_inputs$user_waa$waa` is indexed by weight block,
#'     year, and age.
#'   \item `em_info$par_inputs$user_maturity` is indexed by maturity block,
#'     year, and age.
#'   \item `length(em_info$par_inputs$user_waa$waa_pointer_fleets)` matches the
#'     number of original fleets before aggregation.
#'   \item `length(em_info$par_inputs$user_waa$waa_pointer_indices)` matches the
#'     number of original indices before aggregation.
#' }
#'
#' @section Handling of excluded fleets and indices:
#' Fleets or indices assigned pointer value `0` are excluded from all
#' aggregation steps.
#'
#' @section Default behavior:
#' \itemize{
#'   \item If a parameter vector in `aggregate_catch_info` or
#'     `aggregate_index_info` is `NULL`, the corresponding default values from
#'     `em_info$par_inputs` are used.
#'   \item If no weighting rule is provided in `aggregate_weights_info`,
#'     equal weights are used for aggregated spawning WAA and maturity.
#'   \item If `aggregate_catch_info$fleet_pointer` or
#'     `aggregate_index_info$index_pointer` is `NULL`, all fleets or indices are
#'     placed in a single aggregated group by default.
#' }
#'
#' @examples
#' \dontrun{
#' aggregate_catch_info <- list(
#'   fleet_pointer = c(1, 1, 2, 2),
#'   use_catch_weighted_waa = TRUE
#' )
#'
#' aggregate_index_info <- list(
#'   index_pointer = c(1, 1, 0),
#'   use_catch_weighted_waa = TRUE
#' )
#'
#' em_info <- make_aggregate_data(
#'   om = om,
#'   em_info = em_info,
#'   ind_em = 1:20,
#'   aggregate_catch_info = aggregate_catch_info,
#'   aggregate_index_info = aggregate_index_info,
#'   aggregate_weights_info = list()
#' )
#' }
#'
#' @export
make_aggregate_data <- function(om,
                                em_info,
                                ind_em,
                                aggregate_catch_info = NULL,
                                aggregate_index_info = NULL,
                                aggregate_weights_info = NULL) {
  
  data <- om$input$data
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  #------------------------------------
  # helper: validate pointer
  #------------------------------------
  validate_pointer <- function(pointer, n_expected, name) {
    if (is.null(pointer)) {
      stop(name, " is not specified.")
    }
    if (length(pointer) != n_expected) {
      stop("Length of ", name, " must equal ", n_expected, ".")
    }
    if (any(!is.finite(pointer))) {
      stop(name, " contains non-finite values.")
    }
    if (any(pointer < 0)) {
      stop(name, " cannot contain negative values. Use 0 for exclusion.")
    }
    as.integer(pointer)
  }
  
  #------------------------------------
  # helper: aggregate scalar/vector parameters by pointer
  #------------------------------------
  aggregate_parameters <- function(values, pointers, default_values) {
    valid_groups <- sort(unique(pointers[pointers > 0]))
    
    if (length(valid_groups) == 0) {
      return(numeric(0))
    }
    
    if (is.null(values)) {
      if (length(default_values) == length(valid_groups)) {
        return(default_values)
      }
      values <- rep(default_values[1], length(pointers))
    }
    
    if (length(values) == length(valid_groups)) {
      return(values)
    }
    
    if (length(values) != length(pointers)) {
      stop("Length of `values` must match either the full pointer length or the number of aggregated groups.")
    }
    
    out <- numeric(length(valid_groups))
    for (i in seq_along(valid_groups)) {
      idx <- which(pointers == valid_groups[i])
      out[i] <- mean(values[idx], na.rm = TRUE)
    }
    out
  }
  
  #------------------------------------
  # helper: create row-normalized weights
  #------------------------------------
  make_group_weights <- function(x, weighted = TRUE) {
    if (!is.matrix(x)) {
      x <- as.matrix(x)
    }
    
    n_years <- nrow(x)
    n_members <- ncol(x)
    
    if (n_members == 0) {
      return(matrix(numeric(0), nrow = n_years, ncol = 0))
    }
    
    if (n_members == 1) {
      return(matrix(1, nrow = n_years, ncol = 1))
    }
    
    if (!weighted) {
      return(matrix(1 / n_members, nrow = n_years, ncol = n_members))
    }
    
    rs <- rowSums(x, na.rm = TRUE)
    out <- matrix(0, nrow = n_years, ncol = n_members)
    
    positive <- rs > 0
    if (any(positive)) {
      out[positive, ] <- x[positive, , drop = FALSE] / rs[positive]
    }
    if (any(!positive)) {
      out[!positive, ] <- 1 / n_members
    }
    
    out
  }
  
  #------------------------------------
  # helper: aggregate proportions-at-age
  #------------------------------------
  aggregate_paa <- function(paa_array, obs_matrix) {
    n_members <- dim(paa_array)[1]
    n_years <- dim(paa_array)[2]
    n_ages <- dim(paa_array)[3]
    
    out <- matrix(0, nrow = n_years, ncol = n_ages)
    
    for (i in seq_len(n_members)) {
      member_obs <- obs_matrix[, i]
      for (y in seq_len(n_years)) {
        if (!is.na(member_obs[y]) && member_obs[y] > 0) {
          out[y, ] <- out[y, ] + paa_array[i, y, ] * member_obs[y]
        }
      }
    }
    
    for (y in seq_len(n_years)) {
      s <- sum(out[y, ], na.rm = TRUE)
      if (s > 0) {
        out[y, ] <- out[y, ] / s
      } else {
        out[y, ] <- rep(0, n_ages)
      }
    }
    
    out
  }
  
  #------------------------------------
  # helper: aggregate a WAA-like 3D array
  #------------------------------------
  aggregate_waa_block <- function(waa_mat, pointers, valid_groups, weights_list, ind_em) {
    out_list <- vector("list", length(valid_groups))
    
    for (g in seq_along(valid_groups)) {
      grp <- valid_groups[g]
      grp_idx <- which(pointers == grp)
      
      tmp <- waa_mat[grp_idx, ind_em, , drop = FALSE]
      if (length(dim(tmp)) == 2) {
        tmp <- array(tmp, dim = c(1, dim(tmp)))
      }
      
      weights <- weights_list[[as.character(grp)]]
      if (is.null(weights)) {
        stop("Weights not found for aggregated group ", grp, ".")
      }
      
      weighted_tmp <- tmp * array(weights, dim = dim(tmp))
      agg <- apply(weighted_tmp, c(2, 3), sum)
      out_list[[g]] <- array(agg, dim = c(1, dim(agg)))
    }
    
    if (length(out_list) == 1) {
      out_list[[1]]
    } else {
      abind::abind(out_list, along = 1)
    }
  }
  
  #------------------------------------
  # defaults and validation
  #------------------------------------
  aggregate_catch_info <- aggregate_catch_info %||% list()
  aggregate_index_info <- aggregate_index_info %||% list()
  aggregate_weights_info <- aggregate_weights_info %||% list()
  
  fleet_pointer <- validate_pointer(
    aggregate_catch_info$fleet_pointer %||% rep(1L, data$n_fleets),
    data$n_fleets,
    "aggregate_catch_info$fleet_pointer"
  )
  
  index_pointer <- validate_pointer(
    aggregate_index_info$index_pointer %||% rep(1L, data$n_indices),
    data$n_indices,
    "aggregate_index_info$index_pointer"
  )
  
  valid_fleets <- sort(unique(fleet_pointer[fleet_pointer > 0]))
  valid_indices <- sort(unique(index_pointer[index_pointer > 0]))
  
  n_fleets <- length(valid_fleets)
  n_indices <- length(valid_indices)
  n_ages <- em_info$par_inputs$n_ages
  
  if (n_fleets == 0) {
    warning("No valid fleet groups found. All fleets were excluded by `fleet_pointer`.")
  }
  if (n_indices == 0) {
    warning("No valid index groups found. All indices were excluded by `index_pointer`.")
  }
  
  #------------------------------------
  # aggregate fleet- and index-level parameters
  #------------------------------------
  em_info$par_inputs$catch_cv <- aggregate_parameters(
    aggregate_catch_info$catch_cv,
    fleet_pointer,
    em_info$par_inputs$catch_cv
  )
  em_info$par_inputs$catch_Neff <- aggregate_parameters(
    aggregate_catch_info$catch_Neff,
    fleet_pointer,
    em_info$par_inputs$catch_Neff
  )
  em_info$par_inputs$use_agg_catch <- as.integer(round(aggregate_parameters(
    aggregate_catch_info$use_agg_catch,
    fleet_pointer,
    em_info$par_inputs$use_agg_catch
  )))
  em_info$par_inputs$use_catch_paa <- as.integer(round(aggregate_parameters(
    aggregate_catch_info$use_catch_paa,
    fleet_pointer,
    em_info$par_inputs$use_catch_paa
  )))
  
  em_info$par_inputs$index_cv <- aggregate_parameters(
    aggregate_index_info$index_cv,
    index_pointer,
    em_info$par_inputs$index_cv
  )
  em_info$par_inputs$index_Neff <- aggregate_parameters(
    aggregate_index_info$index_Neff,
    index_pointer,
    em_info$par_inputs$index_Neff
  )
  em_info$par_inputs$q <- aggregate_parameters(
    aggregate_index_info$q,
    index_pointer,
    em_info$par_inputs$q
  )
  em_info$par_inputs$use_indices <- as.integer(round(aggregate_parameters(
    aggregate_index_info$use_indices,
    index_pointer,
    em_info$par_inputs$use_indices
  )))
  em_info$par_inputs$use_index_paa <- as.integer(round(aggregate_parameters(
    aggregate_index_info$use_index_paa,
    index_pointer,
    em_info$par_inputs$use_index_paa
  )))
  em_info$par_inputs$units_indices <- as.integer(round(aggregate_parameters(
    aggregate_index_info$units_indices,
    index_pointer,
    em_info$par_inputs$units_indices
  )))
  em_info$par_inputs$fracyr_indices <- aggregate_parameters(
    aggregate_index_info$fracyr_indices,
    index_pointer,
    em_info$par_inputs$fracyr_indices
  )
  em_info$par_inputs$units_index_paa <- as.integer(round(aggregate_parameters(
    aggregate_index_info$units_index_paa,
    index_pointer,
    em_info$par_inputs$units_index_paa
  )))
  
  #------------------------------------
  # aggregate catch observations
  #------------------------------------
  agg_catch <- matrix(NA_real_, nrow = length(ind_em), ncol = n_fleets)
  agg_catch_paa <- array(0, dim = c(n_fleets, length(ind_em), n_ages))
  
  if (n_fleets > 0) {
    use_catch_paa <- em_info$par_inputs$use_catch_paa
    
    for (g in seq_along(valid_fleets)) {
      grp <- valid_fleets[g]
      grp_idx <- which(fleet_pointer == grp)
      
      fleet_catch <- data$agg_catch[ind_em, grp_idx, drop = FALSE]
      agg_catch[, g] <- rowSums(fleet_catch, na.rm = TRUE)
      
      if (length(use_catch_paa) >= g && isTRUE(use_catch_paa[g] == 1)) {
        catch_paa <- data$catch_paa[grp_idx, ind_em, , drop = FALSE]
        agg_catch_paa[g, , ] <- aggregate_paa(catch_paa, fleet_catch)
      }
    }
  }
  
  #------------------------------------
  # aggregate index observations
  #------------------------------------
  agg_indices <- matrix(NA_real_, nrow = length(ind_em), ncol = n_indices)
  agg_index_paa <- array(0, dim = c(n_indices, length(ind_em), n_ages))
  
  if (n_indices > 0) {
    use_index_paa <- em_info$par_inputs$use_index_paa
    
    for (g in seq_along(valid_indices)) {
      grp <- valid_indices[g]
      grp_idx <- which(index_pointer == grp)
      
      index_obs <- data$agg_indices[ind_em, grp_idx, drop = FALSE]
      agg_indices[, g] <- rowSums(index_obs, na.rm = TRUE)
      
      if (length(use_index_paa) >= g && isTRUE(use_index_paa[g] == 1)) {
        index_paa <- data$index_paa[grp_idx, ind_em, , drop = FALSE]
        agg_index_paa[g, , ] <- aggregate_paa(index_paa, index_obs)
      }
    }
  }
  
  #------------------------------------
  # update EM dimensions and aggregated data
  #------------------------------------
  em_info$par_inputs$n_regions <- 1
  em_info$par_inputs$n_stocks <- 1
  em_info$par_inputs$n_fleets <- n_fleets
  em_info$par_inputs$n_indices <- n_indices
  
  em_info$par_inputs$fleet_regions <- rep(1, n_fleets)
  em_info$par_inputs$index_regions <- rep(1, n_indices)
  
  em_info$par_inputs$agg_catch <- agg_catch
  em_info$par_inputs$catch_paa <- agg_catch_paa
  em_info$par_inputs$agg_indices <- agg_indices
  em_info$par_inputs$index_paa <- agg_index_paa
  
  #------------------------------------
  # compute group weights for WAA aggregation
  #------------------------------------
  fleet_weights <- list()
  if (n_fleets > 0) {
    use_catch_weighted_waa <- isTRUE(aggregate_catch_info$use_catch_weighted_waa %||% FALSE)
    
    for (grp in valid_fleets) {
      grp_idx <- which(fleet_pointer == grp)
      tmp <- data$agg_catch[ind_em, grp_idx, drop = FALSE]
      fleet_weights[[as.character(grp)]] <- make_group_weights(
        tmp,
        weighted = use_catch_weighted_waa
      )
    }
  }
  
  index_weights <- list()
  if (n_indices > 0) {
    use_index_catch_weighted_waa <- isTRUE(aggregate_index_info$use_catch_weighted_waa %||% FALSE)
    
    for (grp in valid_indices) {
      grp_idx <- which(index_pointer == grp)
      tmp <- data$agg_indices[ind_em, grp_idx, drop = FALSE]
      index_weights[[as.character(grp)]] <- make_group_weights(
        tmp,
        weighted = use_index_catch_weighted_waa
      )
    }
  }
  
  #------------------------------------
  # aggregate fleet WAA
  #------------------------------------
  aggregated_fleet_waa <- NULL
  if (n_fleets > 0) {
    waa_pointer_fleets <- em_info$par_inputs$user_waa$waa_pointer_fleets
    
    if (length(waa_pointer_fleets) != length(fleet_pointer)) {
      stop("Length of `em_info$par_inputs$user_waa$waa_pointer_fleets` must match the number of original fleets.")
    }
    
    waa_fleet_mat <- em_info$par_inputs$user_waa$waa[waa_pointer_fleets, , , drop = FALSE]
    
    aggregated_fleet_waa <- aggregate_waa_block(
      waa_mat = waa_fleet_mat,
      pointers = fleet_pointer,
      valid_groups = valid_fleets,
      weights_list = fleet_weights,
      ind_em = ind_em
    )
  }
  
  #------------------------------------
  # aggregate index WAA
  #------------------------------------
  aggregated_index_waa <- NULL
  if (n_indices > 0) {
    waa_pointer_indices <- em_info$par_inputs$user_waa$waa_pointer_indices
    
    if (length(waa_pointer_indices) != length(index_pointer)) {
      stop("Length of `em_info$par_inputs$user_waa$waa_pointer_indices` must match the number of original indices.")
    }
    
    waa_index_mat <- em_info$par_inputs$user_waa$waa[waa_pointer_indices, , , drop = FALSE]
    
    aggregated_index_waa <- aggregate_waa_block(
      waa_mat = waa_index_mat,
      pointers = index_pointer,
      valid_groups = valid_indices,
      weights_list = index_weights,
      ind_em = ind_em
    )
  }
  
  #------------------------------------
  # aggregate spawning-stock WAA
  #------------------------------------
  waa_pointer_ssb <- em_info$par_inputs$user_waa$waa_pointer_ssb
  waa_ssb_mat <- em_info$par_inputs$user_waa$waa[waa_pointer_ssb, ind_em, , drop = FALSE]
  
  if (length(dim(waa_ssb_mat)) == 2) {
    waa_ssb_mat <- array(waa_ssb_mat, dim = c(1, dim(waa_ssb_mat)))
  }
  
  ssb_waa_weights <- aggregate_weights_info$ssb_waa_weights
  
  if (is.null(ssb_waa_weights)) {
    ssb_weights <- matrix(
      1 / dim(waa_ssb_mat)[1],
      nrow = dim(waa_ssb_mat)[1],
      ncol = dim(waa_ssb_mat)[2]
    )
  } else if (isTRUE(ssb_waa_weights$fleet)) {
    pointer <- ssb_waa_weights$pointer
    ssb_weights <- fleet_weights[[as.character(pointer)]]
    if (is.null(ssb_weights)) {
      stop("Requested fleet weight pointer for `ssb_waa_weights` not found.")
    }
    ssb_weights <- t(ssb_weights)
  } else if (isTRUE(ssb_waa_weights$index)) {
    pointer <- ssb_waa_weights$pointer
    ssb_weights <- index_weights[[as.character(pointer)]]
    if (is.null(ssb_weights)) {
      stop("Requested index weight pointer for `ssb_waa_weights` not found.")
    }
    ssb_weights <- t(ssb_weights)
  } else {
    ssb_weights <- matrix(
      1 / dim(waa_ssb_mat)[1],
      nrow = dim(waa_ssb_mat)[1],
      ncol = dim(waa_ssb_mat)[2]
    )
  }
  
  weighted_ssb_waa <- waa_ssb_mat * array(ssb_weights, dim = dim(waa_ssb_mat))
  aggregated_stock_waa <- array(
    apply(weighted_ssb_waa, c(2, 3), sum),
    dim = c(1, dim(weighted_ssb_waa)[2], dim(weighted_ssb_waa)[3])
  )
  
  waa_list <- list()
  if (!is.null(aggregated_fleet_waa)) waa_list <- c(waa_list, list(aggregated_fleet_waa))
  if (!is.null(aggregated_index_waa)) waa_list <- c(waa_list, list(aggregated_index_waa))
  waa_list <- c(waa_list, list(aggregated_stock_waa))
  
  em_info$par_inputs$user_waa$waa <- if (length(waa_list) == 1) {
    waa_list[[1]]
  } else {
    abind::abind(waa_list, along = 1)
  }
  
  em_info$basic_info$waa <- em_info$par_inputs$user_waa$waa
  
  #------------------------------------
  # aggregate maturity
  #------------------------------------
  maturity <- em_info$par_inputs$user_maturity[, ind_em, , drop = FALSE]
  if (length(dim(maturity)) == 2) {
    maturity <- array(maturity, dim = c(1, dim(maturity)))
  }
  
  maturity_weights <- aggregate_weights_info$maturity_weights
  
  if (is.null(maturity_weights)) {
    mat_weights <- matrix(
      1 / dim(maturity)[1],
      nrow = dim(maturity)[1],
      ncol = dim(maturity)[2]
    )
  } else if (isTRUE(maturity_weights$fleet)) {
    pointer <- maturity_weights$pointer
    mat_weights <- fleet_weights[[as.character(pointer)]]
    if (is.null(mat_weights)) {
      stop("Requested fleet weight pointer for `maturity_weights` not found.")
    }
    mat_weights <- t(mat_weights)
  } else if (isTRUE(maturity_weights$index)) {
    pointer <- maturity_weights$pointer
    mat_weights <- index_weights[[as.character(pointer)]]
    if (is.null(mat_weights)) {
      stop("Requested index weight pointer for `maturity_weights` not found.")
    }
    mat_weights <- t(mat_weights)
  } else {
    mat_weights <- matrix(
      1 / dim(maturity)[1],
      nrow = dim(maturity)[1],
      ncol = dim(maturity)[2]
    )
  }
  
  weighted_maturity <- maturity * array(mat_weights, dim = dim(maturity))
  aggregated_maturity <- array(
    apply(weighted_maturity, c(2, 3), sum),
    dim = c(1, dim(weighted_maturity)[2], dim(weighted_maturity)[3])
  )
  
  em_info$par_inputs$user_maturity <- aggregated_maturity
  em_info$basic_info$maturity <- aggregated_maturity
  
  #------------------------------------
  # reset WAA pointers
  #------------------------------------
  em_info$basic_info$waa_pointer_fleets <- if (n_fleets > 0) {
    seq_len(n_fleets)
  } else {
    integer(0)
  }
  
  em_info$basic_info$waa_pointer_indices <- if (n_indices > 0) {
    seq.int(from = n_fleets + 1, length.out = n_indices)
  } else {
    integer(0)
  }
  
  em_info$basic_info$waa_pointer_ssb <- n_fleets + n_indices + 1
  em_info$basic_info$waa_pointer_M <- em_info$basic_info$waa_pointer_ssb
  
  em_info$par_inputs$user_waa$waa_pointer_fleets <- em_info$basic_info$waa_pointer_fleets
  em_info$par_inputs$user_waa$waa_pointer_indices <- em_info$basic_info$waa_pointer_indices
  em_info$par_inputs$user_waa$waa_pointer_ssb <- em_info$basic_info$waa_pointer_ssb
  em_info$par_inputs$user_waa$waa_pointer_M <- em_info$basic_info$waa_pointer_M
  
  return(em_info)
}