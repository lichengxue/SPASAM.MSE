#' Aggregate Catch and Index Data for an Estimation Model
#'
#' Aggregate catch, catch proportions-at-age, survey indices, and index
#' proportions-at-age from operating-model data into the estimation-model
#' structure stored in `info`.
#'
#' This function is intended for cases where multiple operating-model fleets
#' and/or indices are collapsed into a smaller number of estimation-model
#' fleets or indices. Aggregation is controlled using pointer vectors in
#' `aggregate_catch_info` and `aggregate_index_info`, where identical positive
#' pointer values define membership in the same aggregated group and `0`
#' indicates exclusion.
#'
#' For each aggregated fleet or index group, total observations are summed
#' across all members of the group, and proportions-at-age are combined using
#' observation-weighted averages based on annual catch or annual index values.
#'
#' @param data A list containing operating-model data used by the estimation
#'   model. Expected components include:
#'   \itemize{
#'     \item `agg_catch`: Matrix or array indexed by year and fleet, containing
#'       aggregate catch observations.
#'     \item `catch_paa`: Array indexed by fleet, year, and age, containing
#'       catch proportions-at-age.
#'     \item `agg_indices`: Matrix or array indexed by year and index,
#'       containing aggregate survey/index observations.
#'     \item `index_paa`: Array indexed by index, year, and age, containing
#'       index proportions-at-age.
#'     \item `n_fleets`: Integer. Number of original fleets in the
#'       operating-model data. Used when a default fleet pointer is needed.
#'     \item `n_indices`: Integer. Number of original indices in the
#'       operating-model data. Used when a default index pointer is needed.
#'   }
#'
#' @param info A list containing estimation-model inputs and metadata. This
#'   function updates and returns this object. It is expected to contain at
#'   least:
#'   \itemize{
#'     \item `catch_info`: A list where aggregated catch outputs will be stored.
#'     \item `index_info`: A list where aggregated index outputs will be stored.
#'     \item `basic_info$n_ages`: Integer. Number of ages in the estimation
#'       model.
#'   }
#'
#' @param aggregate_catch_info A list describing how operating-model fleets are
#'   aggregated into estimation-model fleets. Supported element:
#'   \itemize{
#'     \item `fleet_pointer`: Integer vector of length equal to the number of
#'       original fleets. Positive integers identify aggregated fleet groups,
#'       and `0` excludes a fleet from the aggregated estimation-model data.
#'   }
#'   If `fleet_pointer` is `NULL`, all fleets are assigned to a single group.
#'
#' @param aggregate_index_info A list describing how operating-model indices are
#'   aggregated into estimation-model indices. Supported element:
#'   \itemize{
#'     \item `index_pointer`: Integer vector of length equal to the number of
#'       original indices. Positive integers identify aggregated index groups,
#'       and `0` excludes an index from the aggregated estimation-model data.
#'   }
#'   If `index_pointer` is `NULL`, all indices are assigned to a single group.
#'
#' @param ind_em Integer vector giving the year indices or row positions from
#'   the operating-model data to be used by the estimation model.
#'
#' @param n_fleets Integer. Number of fleets in the estimation model after
#'   aggregation. In practice, this should match the number of unique positive
#'   values in `aggregate_catch_info$fleet_pointer`, or be `1` when all fleets
#'   are collapsed into a single estimation-model fleet.
#'
#' @param n_indices Integer. Number of indices in the estimation model after
#'   aggregation. In practice, this should match the number of unique positive
#'   values in `aggregate_index_info$index_pointer`, or be `1` when all indices
#'   are collapsed into a single estimation-model index.
#'
#' @return A modified `info` list with aggregated observation data stored in:
#'   \itemize{
#'     \item `info$catch_info$agg_catch`: Matrix of aggregated catch
#'       observations with dimensions `[length(ind_em), n_fleets]`.
#'     \item `info$catch_info$catch_paa`: Array of aggregated catch
#'       proportions-at-age with dimensions
#'       `[n_fleets, length(ind_em), n_ages]`.
#'     \item `info$index_info$agg_indices`: Matrix of aggregated index
#'       observations with dimensions `[length(ind_em), n_indices]`.
#'     \item `info$index_info$index_paa`: Array of aggregated index
#'       proportions-at-age with dimensions
#'       `[n_indices, length(ind_em), n_ages]`.
#'   }
#'
#' @details
#' Aggregation proceeds separately for catch and index data.
#'
#' \strong{Catch aggregation}
#'
#' If `n_fleets == 1`, all fleets are summed into a single catch time series.
#' Catch proportions-at-age are then aggregated across fleets using annual
#' catch-weighted averages. If there is only one original fleet, its
#' proportions-at-age are carried through directly.
#'
#' If `n_fleets > 1`, fleets are grouped according to
#' `aggregate_catch_info$fleet_pointer`. For each positive pointer group:
#' \itemize{
#'   \item annual catch is summed across all fleets in the group, and
#'   \item annual proportions-at-age are computed as catch-weighted averages
#'     across the fleets in the group, then normalized within year.
#' }
#'
#' \strong{Index aggregation}
#'
#' If `n_indices == 1`, all indices are summed into a single index time series.
#' Index proportions-at-age are then aggregated across indices using annual
#' index-weighted averages. If there is only one original index, its
#' proportions-at-age are carried through directly.
#'
#' If `n_indices > 1`, indices are grouped according to
#' `aggregate_index_info$index_pointer`. For each positive pointer group:
#' \itemize{
#'   \item annual index values are summed across all indices in the group, and
#'   \item annual proportions-at-age are computed as index-weighted averages
#'     across the indices in the group, then normalized within year.
#' }
#'
#' Fleets or indices assigned pointer value `0` are excluded entirely.
#'
#' @section Assumptions about input dimensions:
#' The function assumes:
#' \itemize{
#'   \item `data$agg_catch[ind_em, ]` is indexed by year and fleet,
#'   \item `data$catch_paa[, ind_em, ]` is indexed by fleet, year, and age,
#'   \item `data$agg_indices[ind_em, ]` is indexed by year and index, and
#'   \item `data$index_paa[, ind_em, ]` is indexed by index, year, and age.
#' }
#'
#' If these dimensions differ in the underlying model objects, the function
#' should be revised accordingly.
#'
#' @section Weighting and normalization:
#' Proportions-at-age are aggregated using annual weighted sums, where the
#' weight for each fleet or index is its total observation value in that year.
#' After summing across fleets or indices, the resulting age vector is
#' normalized to sum to 1 within each year whenever the total weighted
#' contribution is positive. If the total contribution in a year is zero, the
#' aggregated proportions-at-age for that year are set to zero.
#'
#' @section Default behavior:
#' If `aggregate_catch_info$fleet_pointer` is `NULL`, the function assigns all
#' original fleets to a single fleet group. If `aggregate_index_info$index_pointer`
#' is `NULL`, the function assigns all original indices to a single index group.
#'
#' @section Error handling:
#' The function stops with an error when:
#' \itemize{
#'   \item the supplied fleet pointer length does not match the number of
#'     original fleets,
#'   \item the supplied index pointer length does not match the number of
#'     original indices,
#'   \item `n_fleets` or `n_indices` is inconsistent with the requested
#'     aggregation structure.
#' }
#'
#' @examples
#' \dontrun{
#' # Example: collapse 4 original fleets into 2 estimation-model fleets
#' aggregate_catch_info <- list(
#'   fleet_pointer = c(1, 1, 2, 2)
#' )
#'
#' # Example: collapse 3 original indices into 2 estimation-model indices,
#' # excluding the third original index
#' aggregate_index_info <- list(
#'   index_pointer = c(1, 2, 0)
#' )
#'
#' info <- aggregate_em_data(
#'   data = data,
#'   info = info,
#'   aggregate_catch_info = aggregate_catch_info,
#'   aggregate_index_info = aggregate_index_info,
#'   ind_em = 1:20,
#'   n_fleets = 2,
#'   n_indices = 2
#' )
#' }
#'
#' @export
aggregate_em_data <- function(data,
                              info,
                              aggregate_catch_info,
                              aggregate_index_info,
                              ind_em,
                              n_fleets,
                              n_indices) {
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  #------------------------------------
  # helper: validate pointer
  #------------------------------------
  validate_pointer <- function(pointer, n_expected, name) {
    if (is.null(pointer)) {
      return(rep(1L, n_expected))
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
  # helper: aggregate paa
  #------------------------------------
  aggregate_paa <- function(paa_array, obs_matrix, n_ages) {
    # paa_array: [member, year, age]
    # obs_matrix: [year, member]
    n_members <- dim(paa_array)[1]
    n_years <- dim(paa_array)[2]
    
    out <- matrix(0, nrow = n_years, ncol = n_ages)
    
    for (i in seq_len(n_members)) {
      for (y in seq_len(n_years)) {
        w <- obs_matrix[y, i]
        if (!is.na(w) && w > 0) {
          out[y, ] <- out[y, ] + paa_array[i, y, ] * w
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
    
    array(out, dim = c(1, nrow(out), ncol(out)))
  }
  
  #------------------------------------
  # setup
  #------------------------------------
  aggregate_catch_info <- aggregate_catch_info %||% list()
  aggregate_index_info <- aggregate_index_info %||% list()
  
  n_ages <- info$basic_info$n_ages
  
  fleet_pointer <- validate_pointer(
    aggregate_catch_info$fleet_pointer,
    data$n_fleets,
    "aggregate_catch_info$fleet_pointer"
  )
  
  index_pointer <- validate_pointer(
    aggregate_index_info$index_pointer,
    data$n_indices,
    "aggregate_index_info$index_pointer"
  )
  
  valid_fleet_groups <- sort(unique(fleet_pointer[fleet_pointer > 0]))
  valid_index_groups <- sort(unique(index_pointer[index_pointer > 0]))
  
  if (n_fleets != length(valid_fleet_groups) && n_fleets != 1) {
    stop("`n_fleets` does not match the number of valid aggregated fleet groups.")
  }
  
  if (n_indices != length(valid_index_groups) && n_indices != 1) {
    stop("`n_indices` does not match the number of valid aggregated index groups.")
  }
  
  #------------------------------------
  # aggregate catch data
  #------------------------------------
  if (n_fleets == 1) {
    
    catch_mat <- data$agg_catch[ind_em, , drop = FALSE]
    info$catch_info$agg_catch <- matrix(rowSums(catch_mat, na.rm = TRUE), ncol = 1)
    
    catch_paa_arr <- data$catch_paa[, ind_em, , drop = FALSE]
    
    if (ncol(catch_mat) == 1) {
      info$catch_info$catch_paa <- catch_paa_arr
    } else {
      info$catch_info$catch_paa <- aggregate_paa(
        paa_array = catch_paa_arr,
        obs_matrix = catch_mat,
        n_ages = n_ages
      )
    }
    
  } else {
    
    info$catch_info$agg_catch <- matrix(
      NA_real_,
      nrow = length(ind_em),
      ncol = length(valid_fleet_groups)
    )
    
    info$catch_info$catch_paa <- array(
      0,
      dim = c(length(valid_fleet_groups), length(ind_em), n_ages)
    )
    
    for (g in seq_along(valid_fleet_groups)) {
      grp <- valid_fleet_groups[g]
      grp_idx <- which(fleet_pointer == grp)
      
      catch_mat <- data$agg_catch[ind_em, grp_idx, drop = FALSE]
      info$catch_info$agg_catch[, g] <- rowSums(catch_mat, na.rm = TRUE)
      
      catch_paa_arr <- data$catch_paa[grp_idx, ind_em, , drop = FALSE]
      
      if (ncol(catch_mat) == 1) {
        info$catch_info$catch_paa[g, , ] <- catch_paa_arr[1, , ]
      } else {
        info$catch_info$catch_paa[g, , ] <- aggregate_paa(
          paa_array = catch_paa_arr,
          obs_matrix = catch_mat,
          n_ages = n_ages
        )[1, , ]
      }
    }
  }
  
  #------------------------------------
  # aggregate index data
  #------------------------------------
  if (n_indices == 1) {
    
    index_mat <- data$agg_indices[ind_em, , drop = FALSE]
    info$index_info$agg_indices <- matrix(rowSums(index_mat, na.rm = TRUE), ncol = 1)
    
    index_paa_arr <- data$index_paa[, ind_em, , drop = FALSE]
    
    if (ncol(index_mat) == 1) {
      info$index_info$index_paa <- index_paa_arr
    } else {
      info$index_info$index_paa <- aggregate_paa(
        paa_array = index_paa_arr,
        obs_matrix = index_mat,
        n_ages = n_ages
      )
    }
    
  } else {
    
    info$index_info$agg_indices <- matrix(
      NA_real_,
      nrow = length(ind_em),
      ncol = length(valid_index_groups)
    )
    
    info$index_info$index_paa <- array(
      0,
      dim = c(length(valid_index_groups), length(ind_em), n_ages)
    )
    
    for (g in seq_along(valid_index_groups)) {
      grp <- valid_index_groups[g]
      grp_idx <- which(index_pointer == grp)
      
      index_mat <- data$agg_indices[ind_em, grp_idx, drop = FALSE]
      info$index_info$agg_indices[, g] <- rowSums(index_mat, na.rm = TRUE)
      
      index_paa_arr <- data$index_paa[grp_idx, ind_em, , drop = FALSE]
      
      if (ncol(index_mat) == 1) {
        info$index_info$index_paa[g, , ] <- index_paa_arr[1, , ]
      } else {
        info$index_info$index_paa[g, , ] <- aggregate_paa(
          paa_array = index_paa_arr,
          obs_matrix = index_mat,
          n_ages = n_ages
        )[1, , ]
      }
    }
  }
  
  return(info)
}