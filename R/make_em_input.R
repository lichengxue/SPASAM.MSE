#' Generate Input Data for the Estimation Model
#'
#' Construct a \code{wham} input object for the estimation model (EM) used in
#' management strategy evaluation (MSE). This function prepares EM inputs from
#' the operating model (OM) outputs under a range of assessment configurations,
#' including:
#' \itemize{
#'   \item panmictic / spatially aggregated models,
#'   \item fleets-as-areas models,
#'   \item separate regional assessment models,
#'   \item fully spatially explicit models with or without movement,
#'   \item reduced-region assessment models, and
#'   \item optional updates to catch, index, and environmental covariate inputs.
#' }
#'
#' The function subsets the requested assessment years, builds the corresponding
#' EM information objects, fills catch and index observations from the OM, and
#' passes the resulting configuration to \code{\link{prepare_wham_input}}.
#'
#' @param om List. Operating model object containing input data and simulated
#'   outputs. Expected to include \code{om$input$data}.
#' @param em_info List. Estimation-model configuration object used to define the
#'   EM structure.
#' @param M_em List. Natural mortality configuration for the EM.
#' @param sel_em List. Selectivity configuration for the EM.
#' @param NAA_re_em List. Numbers-at-age random effects configuration for the EM.
#' @param move_em List. Movement configuration for the EM.
#' @param catchability_em List. Catchability configuration for the EM.
#' @param ecov_em List or \code{NULL}. Environmental covariate configuration for
#'   the EM.
#' @param em.opt List. Assessment model options. Expected components include:
#'   \itemize{
#'     \item \code{$separate.em}: Logical. If \code{TRUE}, fit non-spatial or
#'       spatially implicit EMs without global SPR. If \code{FALSE}, fit a
#'       spatially explicit EM.
#'     \item \code{$separate.em.type}: Integer (1--3), used when
#'       \code{separate.em = TRUE}:
#'       \itemize{
#'         \item \code{1}: Panmictic (spatially aggregated).
#'         \item \code{2}: Fleets-as-areas.
#'         \item \code{3}: Separate model for each region.
#'       }
#'     \item \code{$do.move}: Logical. If \code{TRUE}, include movement in the
#'       spatially explicit EM.
#'     \item \code{$est.move}: Logical. If \code{TRUE}, estimate movement rates
#'       in the spatially explicit EM.
#'   }
#' @param em_years Numeric or integer vector. Years available to the EM before
#'   optional subsetting by \code{year.use}.
#' @param year.use Integer or \code{NULL}. Number of terminal years from
#'   \code{em_years} to include in the EM. If \code{NULL}, all \code{em_years}
#'   are used.
#' @param age_comp_em Character. Likelihood for age-composition observations.
#'   Common options include:
#'   \itemize{
#'     \item \code{"multinomial"}
#'     \item \code{"dir-mult"}
#'     \item \code{"dirichlet-miss0"}
#'     \item \code{"dirichlet-pool0"}
#'     \item \code{"logistic-normal-miss0"}
#'     \item \code{"logistic-normal-ar1-miss0"}
#'     \item \code{"logistic-normal-pool0"}
#'     \item \code{"logistic-normal-01-infl"}
#'     \item \code{"logistic-normal-01-infl-2par"}
#'     \item \code{"mvtweedie"}
#'     \item \code{"dir-mult-linear"}
#'   }
#' @param aggregate_catch_info List or \code{NULL}. User-defined settings for
#'   aggregating catch observations in panmictic EMs. Expected components may
#'   include:
#'   \itemize{
#'     \item \code{$n_fleets}: Integer number of aggregated fleets.
#'     \item \code{$catch_cv}: Numeric vector of fleet-specific catch CVs.
#'     \item \code{$catch_Neff}: Numeric vector of effective sample sizes.
#'     \item \code{$use_agg_catch}: Integer vector of 0/1 flags for using
#'       aggregate catches.
#'     \item \code{$use_catch_paa}: Integer vector of 0/1 flags for using catch
#'       proportions-at-age.
#'     \item \code{$fleet_pointer}: Integer vector mapping original fleets into
#'       aggregated fleet groups.
#'     \item \code{$use_catch_weighted_waa}: Logical. Whether to use
#'       catch-weighted WAA.
#'   }
#' @param aggregate_index_info List or \code{NULL}. User-defined settings for
#'   aggregating survey/index observations in panmictic EMs. Expected components
#'   may include:
#'   \itemize{
#'     \item \code{$n_indices}: Integer number of aggregated indices.
#'     \item \code{$index_cv}: Numeric vector of index-specific CVs.
#'     \item \code{$index_Neff}: Numeric vector of effective sample sizes.
#'     \item \code{$fracyr_indices}: Numeric vector of within-year timing for
#'       each index.
#'     \item \code{$q}: Numeric vector of initial catchabilities.
#'     \item \code{$use_indices}: Integer vector of 0/1 flags for using
#'       aggregate indices.
#'     \item \code{$use_index_paa}: Integer vector of 0/1 flags for using
#'       index proportions-at-age.
#'     \item \code{$units_indices}: Integer vector defining biomass (1) versus
#'       numbers (2) for aggregate observations.
#'     \item \code{$units_index_paa}: Integer vector defining biomass (1) versus
#'       numbers (2) for composition observations.
#'     \item \code{$index_pointer}: Integer vector mapping original indices into
#'       aggregated index groups.
#'     \item \code{$use_catch_weighted_waa}: Logical. Whether to use
#'       index-magnitude-weighted WAA. This naming is kept for consistency with
#'       \code{\link{make_aggregate_data}}.
#'   }
#' @param aggregate_weights_info List or \code{NULL}. Optional settings used to
#'   compute weighted averages of maturity-at-age and weight-at-age when
#'   aggregating data (primarily for panmictic and fleets-as-areas EMs).
#'   Expected components may include:
#'   \itemize{
#'     \item \code{$ssb_waa_weights}: List describing weighting rules for SSB
#'       WAA.
#'     \item \code{$maturity_weights}: List describing weighting rules for
#'       maturity-at-age.
#'   }
#' @param filter_indices Integer vector of 0/1 values or \code{NULL}. Optional
#'   indicator vector specifying which indices are retained in the EM
#'   (\code{1} = keep, \code{0} = exclude).
#' @param reduce_region_info List or \code{NULL}. Optional settings for fitting
#'   a reduced-region EM. Expected components may include:
#'   \itemize{
#'     \item \code{$remove_regions}: Integer vector of 0/1 flags indicating
#'       regions to retain/remove.
#'     \item \code{$reassign}: Numeric reassignment rule for surveys from
#'       removed regions.
#'     \item \code{$NAA_where_em}: Integer array defining stock-age-region
#'       occupancy in the reduced EM.
#'     \item \code{$sel_em}, \code{$M_em}, \code{$NAA_re_em},
#'       \code{$catchability_em}, \code{$move_em}, \code{$ecov_em}: reduced-model
#'       configuration objects.
#'     \item \code{$onto_move_list}: List of ontogenetic movement settings for
#'       the reduced EM.
#'   }
#' @param update_catch_info List or \code{NULL}. Optional updates to catch input
#'   information after building the EM input object. Expected components may
#'   include:
#'   \itemize{
#'     \item \code{$agg_catch_sigma}: Matrix of catch standard deviations or CVs.
#'     \item \code{$catch_Neff}: Matrix of effective sample sizes.
#'     \item \code{$remove_agg}, \code{$remove_agg_pointer},
#'       \code{$remove_agg_years}: settings for dropping aggregate observations.
#'     \item \code{$remove_paa}, \code{$remove_paa_pointer},
#'       \code{$remove_paa_years}: settings for dropping composition observations.
#'   }
#' @param update_index_info List or \code{NULL}. Optional updates to index input
#'   information after building the EM input object. Expected components may
#'   include:
#'   \itemize{
#'     \item \code{$agg_index_sigma}: Matrix of index standard deviations or CVs.
#'     \item \code{$index_Neff}: Matrix of effective sample sizes.
#'     \item \code{$remove_agg}, \code{$remove_agg_pointer},
#'       \code{$remove_agg_years}: settings for dropping aggregate observations.
#'     \item \code{$remove_paa}, \code{$remove_paa_pointer},
#'       \code{$remove_paa_years}: settings for dropping composition observations.
#'   }
#' @param ecov_em_opts List or \code{NULL}. Optional settings for overriding EM
#'   environmental covariate inputs. Expected components may include:
#'   \itemize{
#'     \item \code{$use_ecov_em}: Logical. If \code{TRUE}, use EM-specified Ecov
#'       mean values rather than inheriting the OM observed series directly.
#'     \item \code{$period}: Optional integer vector of rows to replace. If
#'       supplied, only those rows are replaced.
#'     \item \code{$mean}: Optional replacement Ecov matrix/vector. If missing,
#'       the function attempts to use \code{ecov_em$mean}.
#'   }
#'   If \code{$period} is \code{NULL}, the full Ecov series is replaced.
#'
#' @details
#' The function proceeds as follows:
#' \enumerate{
#'   \item Determines the EM years to use from \code{em_years} and
#'     \code{year.use}.
#'   \item Builds or filters the EM information object according to the chosen
#'     assessment structure.
#'   \item Fills catch and index observations from the OM data for the selected
#'     years.
#'   \item Optionally replaces environmental covariate means in the EM using
#'     \code{ecov_em_opts}.
#'   \item Calls \code{\link{prepare_wham_input}} to construct a \code{wham}
#'     input object.
#'   \item Optionally updates WAA, catch information, and index information.
#' }
#'
#' For spatially implicit models (\code{separate.em = TRUE}), movement and trend
#' structures are disabled. For spatially explicit models
#' (\code{separate.em = FALSE}), movement can either be retained or removed
#' depending on \code{em.opt$do.move}. Reduced-region models can also be formed
#' through \code{reduce_region_info}.
#'
#' @return A \code{wham} input object, or a list of \code{wham} input objects
#'   when \code{em.opt$separate.em.type = 3}.
#'
#' @seealso \code{\link{loop_through_fn}}, \code{\link{prepare_wham_input}},
#'   \code{\link{update_waa}}
#'
#' @export
make_em_input <- function(om,
                          em_info,
                          M_em,
                          sel_em,
                          NAA_re_em,
                          move_em,
                          catchability_em,
                          ecov_em,
                          em.opt,
                          em_years,
                          year.use,
                          age_comp_em,
                          aggregate_catch_info = NULL,
                          aggregate_index_info = NULL,
                          aggregate_weights_info = NULL,
                          filter_indices = NULL,
                          reduce_region_info = NULL,
                          update_catch_info = NULL,
                          update_index_info = NULL,
                          ecov_em_opts = NULL) {
  
  if (is.null(em.opt)) stop("em.opt must be specified!")
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  ## ------------------------------------------------------------
  ## Internal helper: build Ecov input for EM
  ## ------------------------------------------------------------
  build_ecov_em_input <- function(ecov_em, om, ecov_em_opts = NULL) {
    
    if (is.null(ecov_em)) return(NULL)
    
    ecov_em_new <- ecov_em
    ecov_em_new$year <- ecov_em$year
    ecov_em_new$mean <- om$input$data$Ecov_obs
    ecov_em_new$use_obs <- ecov_em$use_obs
    ecov_em_new$logsigma <- ecov_em$logsigma
    
    if (!is.null(ecov_em_opts) && isTRUE(ecov_em_opts$use_ecov_em)) {
      
      replacement_mean <- NULL
      if (!is.null(ecov_em_opts$mean)) {
        replacement_mean <- ecov_em_opts$mean
      } else if (!is.null(ecov_em$mean)) {
        replacement_mean <- ecov_em$mean
      }
      
      if (is.null(replacement_mean)) {
        stop("ecov_em_opts$use_ecov_em = TRUE requires ecov_em_opts$mean or ecov_em$mean.",
             call. = FALSE)
      }
      
      base_mean <- ecov_em_new$mean
      new_mean  <- replacement_mean
      
      if (is.vector(base_mean)) base_mean <- matrix(base_mean, ncol = 1)
      if (is.vector(new_mean))  new_mean  <- matrix(new_mean,  ncol = 1)
      
      if (ncol(base_mean) != ncol(new_mean)) {
        stop("Replacement Ecov mean must have the same number of columns as om$input$data$Ecov_obs.",
             call. = FALSE)
      }
      
      if (is.null(ecov_em_opts$period)) {
        if (!all(dim(base_mean) == dim(new_mean))) {
          stop("Full Ecov replacement requires replacement mean to have the same dimensions as om$input$data$Ecov_obs.",
               call. = FALSE)
        }
        ecov_em_new$mean <- new_mean
      } else {
        period <- ecov_em_opts$period
        
        if (any(period < 1 | period > nrow(base_mean))) {
          stop("ecov_em_opts$period contains rows outside the range of om$input$data$Ecov_obs.",
               call. = FALSE)
        }
        
        if (nrow(new_mean) == length(period)) {
          base_mean[period, ] <- new_mean
        } else if (nrow(new_mean) >= max(period)) {
          base_mean[period, ] <- new_mean[period, , drop = FALSE]
        } else {
          stop("Replacement Ecov mean must either have length(period) rows or enough rows to subset by ecov_em_opts$period.",
               call. = FALSE)
        }
        
        ecov_em_new$mean <- base_mean
      }
    }
    
    return(ecov_em_new)
  }
  
  ## ------------------------------------------------------------
  ## Internal helper: update catch input information
  ## ------------------------------------------------------------
  apply_update_catch_info <- function(em_input, update_catch_info, ind_em) {
    if (is.null(update_catch_info)) return(em_input)
    
    agg_catch_sigma    <- update_catch_info$agg_catch_sigma
    catch_Neff         <- update_catch_info$catch_Neff
    remove_agg         <- update_catch_info$remove_agg
    remove_agg_pointer <- update_catch_info$remove_agg_pointer
    remove_agg_years   <- update_catch_info$remove_agg_years
    remove_paa         <- update_catch_info$remove_paa
    remove_paa_pointer <- update_catch_info$remove_paa_pointer
    remove_paa_years   <- update_catch_info$remove_paa_years
    
    update_input_catch_info(
      input = em_input,
      agg_catch_sigma = agg_catch_sigma,
      catch_Neff = catch_Neff,
      remove_agg = remove_agg,
      remove_agg_pointer = remove_agg_pointer,
      remove_agg_years = remove_agg_years,
      remove_paa = remove_paa,
      remove_paa_pointer = remove_paa_pointer,
      remove_paa_years = remove_paa_years,
      ind_em = ind_em
    )
  }
  
  ## ------------------------------------------------------------
  ## Internal helper: update index input information
  ## ------------------------------------------------------------
  apply_update_index_info <- function(em_input, update_index_info, ind_em) {
    if (is.null(update_index_info)) return(em_input)
    
    agg_index_sigma    <- update_index_info$agg_index_sigma
    index_Neff         <- update_index_info$index_Neff
    remove_agg         <- update_index_info$remove_agg
    remove_agg_pointer <- update_index_info$remove_agg_pointer
    remove_agg_years   <- update_index_info$remove_agg_years
    remove_paa         <- update_index_info$remove_paa
    remove_paa_pointer <- update_index_info$remove_paa_pointer
    remove_paa_years   <- update_index_info$remove_paa_years
    
    update_input_index_info(
      input = em_input,
      agg_index_sigma = agg_index_sigma,
      index_Neff = index_Neff,
      remove_agg = remove_agg,
      remove_agg_pointer = remove_agg_pointer,
      remove_agg_years = remove_agg_years,
      remove_paa = remove_paa,
      remove_paa_pointer = remove_paa_pointer,
      remove_paa_years = remove_paa_years,
      ind_em = ind_em
    )
  }
  
  ## ------------------------------------------------------------
  ## Determine movement type based on options
  ## ------------------------------------------------------------
  if (isTRUE(em.opt$separate.em)) {
    em.opt$do.move <- FALSE
    move.type <- NULL
  } else if (!isTRUE(em.opt$do.move)) {
    move.type <- 3
  } else if (all(move_em$stock_move)) {
    move.type <- 2
  } else {
    move.type <- 1
  }
  
  data <- om$input$data
  
  ## ------------------------------------------------------------
  ## Set defaults for aggregation flags to match make_aggregate_data()
  ## ------------------------------------------------------------
  if (is.null(aggregate_catch_info)) aggregate_catch_info <- list()
  if (is.null(aggregate_catch_info$use_catch_weighted_waa)) {
    aggregate_catch_info$use_catch_weighted_waa <- FALSE
  }
  
  if (is.null(aggregate_index_info)) aggregate_index_info <- list()
  if (is.null(aggregate_index_info$use_catch_weighted_waa)) {
    aggregate_index_info$use_catch_weighted_waa <- FALSE
  }
  
  ## ------------------------------------------------------------
  ## Determine EM years to use
  ## ------------------------------------------------------------
  if (!is.null(year.use)) {
    if (year.use > length(em_years)) {
      warning("year.use must be <= length(em_years)! Setting year.use to length(em_years).")
      year.use <- length(em_years)
    }
    ind_em <- (length(em_years) - year.use + 1):length(em_years)
    em_years <- tail(em_years, year.use)
  } else {
    year.use <- length(em_years)
    ind_em <- (length(em_years) - year.use + 1):length(em_years)
  }
  
  ## ============================================================
  ## Non-spatial or spatially implicit assessment models
  ## ============================================================
  if (isTRUE(em.opt$separate.em)) {
    
    ## ----------------------------------------------------------
    ## Type 1: Panmictic / spatially aggregated
    ## ----------------------------------------------------------
    if (em.opt$separate.em.type == 1) {
      
      n_fleets  <- aggregate_catch_info$n_fleets %||% 1
      n_indices <- aggregate_index_info$n_indices %||% 1
      n_stocks <- n_regions <- 1
      
      em_info <- make_aggregate_data(
        om = om,
        em_info = em_info,
        ind_em = ind_em,
        aggregate_catch_info = aggregate_catch_info,
        aggregate_index_info = aggregate_index_info,
        aggregate_weights_info = aggregate_weights_info
      )
      
      agg_catch   <- em_info$par_inputs$agg_catch
      catch_paa   <- em_info$par_inputs$catch_paa
      agg_indices <- em_info$par_inputs$agg_indices
      index_paa   <- em_info$par_inputs$index_paa
      
      em_info$par_inputs$move_dyn <- 0
      em_info$par_inputs$onto_move <- matrix(0)
      em_info$par_inputs$apply_re_trend <- 0
      em_info$par_inputs$apply_mu_trend <- 0
      
      info <- generate_basic_info_em(
        em_info = em_info,
        em_years = em_years,
        n_stocks = 1,
        n_regions = 1,
        n_fleets = n_fleets,
        n_indices = n_indices
      )
      
      basic_info <- info$basic_info
      catch_info <- info$catch_info
      index_info <- info$index_info
      F_info     <- info$F
      
      catch_info$agg_catch   <- agg_catch
      catch_info$catch_paa   <- catch_paa
      index_info$agg_indices <- agg_indices
      index_info$index_paa   <- index_paa
      
      ecov_em_new <- build_ecov_em_input(ecov_em, om, ecov_em_opts)
      
      em_input <- prepare_wham_input(
        basic_info   = basic_info,
        selectivity  = sel_em,
        M            = M_em,
        NAA_re       = NAA_re_em,
        move         = NULL,
        catchability = catchability_em,
        ecov         = ecov_em_new,
        age_comp     = age_comp_em,
        catch_info   = catch_info,
        index_info   = index_info,
        F            = F_info
      )
      
      waa_info <- info$par_inputs$user_waa
      em_input <- update_waa(em_input, waa_info = waa_info)
    }
    
    ## ----------------------------------------------------------
    ## Type 2: Fleets-as-areas
    ## ----------------------------------------------------------
    if (em.opt$separate.em.type == 2) {
      
      n_fleets  <- data$n_fleets
      n_indices <- data$n_indices
      
      if (!is.null(filter_indices) && length(filter_indices) != n_indices) {
        stop("Length of filter_indices must equal n_indices!")
      }
      
      fleet_regions <- em_info$catch_info$fleet_regions
      index_regions <- em_info$index_info$index_regions
      
      em_info <- filter_and_generate_em_info(
        em_info = em_info,
        em.opt = em.opt,
        ind_em = ind_em,
        fleet_regions = fleet_regions,
        index_regions = index_regions,
        filter_indices = filter_indices,
        aggregate_weights_info = aggregate_weights_info
      )
      
      idx <- NULL
      if (!is.null(filter_indices) && any(filter_indices == 0)) {
        n_indices <- sum(filter_indices != 0)
        idx <- which(filter_indices != 0)
      }
      
      info <- generate_basic_info_em(
        em_info = em_info,
        em_years = em_years,
        n_stocks = 1,
        n_regions = 1,
        n_fleets = n_fleets,
        n_indices = n_indices,
        filter_indices = filter_indices
      )
      
      basic_info <- info$basic_info
      
      info$catch_info$fleet_regions[] <- 1
      info$index_info$index_regions[] <- 1
      
      info$catch_info$agg_catch     <- data$agg_catch[ind_em, , drop = FALSE]
      info$catch_info$catch_paa     <- data$catch_paa[, ind_em, , drop = FALSE]
      info$catch_info$use_agg_catch <- data$use_agg_catch[ind_em, , drop = FALSE]
      info$catch_info$use_catch_paa <- data$use_catch_paa[ind_em, , drop = FALSE]
      
      if (!is.null(filter_indices) && any(filter_indices == 0)) {
        info$index_info$agg_indices   <- data$agg_indices[ind_em, idx, drop = FALSE]
        info$index_info$index_paa     <- data$index_paa[idx, ind_em, , drop = FALSE]
        info$index_info$use_indices   <- data$use_indices[ind_em, idx, drop = FALSE]
        info$index_info$use_index_paa <- data$use_index_paa[ind_em, idx, drop = FALSE]
      } else {
        info$index_info$agg_indices   <- data$agg_indices[ind_em, , drop = FALSE]
        info$index_info$index_paa     <- data$index_paa[, ind_em, , drop = FALSE]
        info$index_info$use_indices   <- data$use_indices[ind_em, , drop = FALSE]
        info$index_info$use_index_paa <- data$use_index_paa[ind_em, , drop = FALSE]
      }
      
      basic_info$move_dyn <- 0
      basic_info$onto_move <- matrix(0)
      basic_info$apply_re_trend <- 0
      basic_info$apply_mu_trend <- 0
      
      if (is.null(age_comp_em)) age_comp_em <- "multinomial"
      
      ecov_em_new <- build_ecov_em_input(ecov_em, om, ecov_em_opts)
      
      em_input <- prepare_wham_input(
        basic_info   = basic_info,
        selectivity  = sel_em,
        M            = M_em,
        NAA_re       = NAA_re_em,
        move         = NULL,
        catchability = catchability_em,
        ecov         = ecov_em_new,
        age_comp     = age_comp_em,
        catch_info   = info$catch_info,
        index_info   = info$index_info,
        F            = info$F
      )
      
      waa_info <- info$par_inputs$user_waa
      em_input <- update_waa(em_input, waa_info = waa_info)
    }
    
    ## ----------------------------------------------------------
    ## Type 3: Separate assessment models by region
    ## ----------------------------------------------------------
    if (em.opt$separate.em.type == 3) {
      
      fleet_regions <- data$fleet_regions
      index_regions <- data$index_regions
      
      em_input <- list()
      
      em_info_new <- filter_and_generate_em_info(
        em_info = em_info,
        em.opt = em.opt,
        ind_em = ind_em,
        fleet_regions = fleet_regions,
        index_regions = index_regions,
        filter_indices = filter_indices
      )
      
      for (r in seq_len(data$n_regions)) {
        
        info <- generate_basic_info_em(
          em_info = em_info_new[[r]],
          em_years = em_years,
          n_stocks = 1,
          n_regions = 1,
          n_fleets = em_info_new[[r]]$par_inputs$n_fleets,
          n_indices = em_info_new[[r]]$par_inputs$n_indices,
          filter_indices = filter_indices
        )
        
        basic_info <- info$basic_info
        
        basic_info$move_dyn <- 0
        basic_info$onto_move <- matrix(0)
        basic_info$apply_re_trend <- 0
        basic_info$apply_mu_trend <- 0
        
        relevant_fleets <- which(fleet_regions == r)
        
        info$catch_info$agg_catch     <- data$agg_catch[ind_em, relevant_fleets, drop = FALSE]
        info$catch_info$catch_paa     <- data$catch_paa[relevant_fleets, ind_em, , drop = FALSE]
        info$catch_info$use_agg_catch <- data$use_agg_catch[ind_em, relevant_fleets, drop = FALSE]
        info$catch_info$use_catch_paa <- data$use_catch_paa[ind_em, relevant_fleets, drop = FALSE]
        
        relevant_indices <- which(index_regions == r)
        
        if (!is.null(filter_indices)) {
          relevant_indices <- relevant_indices[filter_indices[relevant_indices] != 0]
        }
        
        info$index_info$agg_indices   <- data$agg_indices[ind_em, relevant_indices, drop = FALSE]
        info$index_info$index_paa     <- data$index_paa[relevant_indices, ind_em, , drop = FALSE]
        info$index_info$use_indices   <- data$use_indices[ind_em, relevant_indices, drop = FALSE]
        info$index_info$use_index_paa <- data$use_index_paa[ind_em, relevant_indices, drop = FALSE]
        
        ecov_em_new <- build_ecov_em_input(ecov_em, om, ecov_em_opts)
        
        em_input[[r]] <- prepare_wham_input(
          basic_info   = basic_info,
          selectivity  = sel_em,
          M            = M_em,
          NAA_re       = NAA_re_em,
          move         = NULL,
          catchability = catchability_em,
          ecov         = ecov_em_new,
          age_comp     = age_comp_em,
          catch_info   = info$catch_info,
          index_info   = info$index_info,
          F            = info$F
        )
        
        waa_info <- info$par_inputs$user_waa
        em_input[[r]] <- update_waa(em_input[[r]], waa_info = waa_info)
      }
    }
  }
  
  ## ============================================================
  ## Spatially explicit assessment model
  ## ============================================================
  if (!isTRUE(em.opt$separate.em)) {
    
    n_fleets  <- data$n_fleets
    n_indices <- data$n_indices
    fleet_regions <- em_info$catch_info$fleet_regions
    index_regions <- em_info$index_info$index_regions
    
    remove_regions <- reduce_region_info$remove_regions %||% NULL
    
    em_info <- filter_and_generate_em_info(
      em_info = em_info,
      em.opt = em.opt,
      ind_em = ind_em,
      fleet_regions = fleet_regions,
      index_regions = index_regions,
      filter_indices = filter_indices,
      reduce_region_info = reduce_region_info
    )
    
    idx <- NULL
    if (!is.null(filter_indices) && any(filter_indices == 0)) {
      n_indices <- sum(filter_indices != 0)
      idx <- which(filter_indices != 0)
    }
    
    ## ----------------------------------------------------------
    ## No regions removed
    ## ----------------------------------------------------------
    if (is.null(remove_regions)) {
      
      n_stocks  <- om$input$data$n_stocks
      n_regions <- om$input$data$n_regions
      
      info <- generate_basic_info_em(
        em_info = em_info,
        em_years = em_years,
        n_stocks = n_stocks,
        n_regions = n_regions,
        n_fleets = n_fleets,
        n_indices = n_indices,
        filter_indices = filter_indices
      )
      
      basic_info <- info$basic_info
      
      info$catch_info$agg_catch     <- data$agg_catch[ind_em, , drop = FALSE]
      info$catch_info$catch_paa     <- data$catch_paa[, ind_em, , drop = FALSE]
      info$catch_info$use_agg_catch <- data$use_agg_catch[ind_em, , drop = FALSE]
      info$catch_info$use_catch_paa <- data$use_catch_paa[ind_em, , drop = FALSE]
      
      if (!is.null(filter_indices) && any(filter_indices == 0)) {
        info$index_info$agg_indices   <- data$agg_indices[ind_em, idx, drop = FALSE]
        info$index_info$index_paa     <- data$index_paa[idx, ind_em, , drop = FALSE]
        info$index_info$use_indices   <- data$use_indices[ind_em, idx, drop = FALSE]
        info$index_info$use_index_paa <- data$use_index_paa[ind_em, idx, drop = FALSE]
      } else {
        info$index_info$agg_indices   <- data$agg_indices[ind_em, , drop = FALSE]
        info$index_info$index_paa     <- data$index_paa[, ind_em, , drop = FALSE]
        info$index_info$use_indices   <- data$use_indices[ind_em, , drop = FALSE]
        info$index_info$use_index_paa <- data$use_index_paa[ind_em, , drop = FALSE]
      }
      
      ecov_em_new <- build_ecov_em_input(ecov_em, om, ecov_em_opts)
      
      if (isTRUE(em.opt$do.move)) {
        
        basic_info$NAA_where <- om$input$data$NAA_where
        
        em_input <- prepare_wham_input(
          basic_info   = basic_info,
          selectivity  = sel_em,
          M            = M_em,
          NAA_re       = NAA_re_em,
          move         = move_em,
          catchability = catchability_em,
          ecov         = ecov_em_new,
          age_comp     = age_comp_em,
          catch_info   = info$catch_info,
          index_info   = info$index_info,
          F            = info$F
        )
        
        waa_info <- info$par_inputs$user_waa
        em_input <- update_waa(em_input, waa_info = waa_info)
        
        em_input <- apply_update_catch_info(em_input, update_catch_info, ind_em)
        em_input <- apply_update_index_info(em_input, update_index_info, ind_em)
        
      } else {
        
        basic_info$move_dyn <- 0
        basic_info$onto_move <- matrix(0)
        basic_info$apply_re_trend <- 0
        basic_info$apply_mu_trend <- 0
        basic_info$NAA_where <- NULL
        
        em_input <- prepare_wham_input(
          basic_info   = basic_info,
          selectivity  = sel_em,
          M            = M_em,
          NAA_re       = NAA_re_em,
          move         = NULL,
          catchability = catchability_em,
          ecov         = ecov_em_new,
          age_comp     = age_comp_em,
          catch_info   = info$catch_info,
          index_info   = info$index_info,
          F            = info$F
        )
        
        waa_info <- info$par_inputs$user_waa
        em_input <- update_waa(em_input, waa_info = waa_info)
        
        em_input <- apply_update_catch_info(em_input, update_catch_info, ind_em)
        em_input <- apply_update_index_info(em_input, update_index_info, ind_em)
      }
    }
    
    ## ----------------------------------------------------------
    ## Regions removed
    ## ----------------------------------------------------------
    if (!is.null(remove_regions)) {
      
      n_stocks  <- em_info$par_inputs$n_stocks
      n_regions <- em_info$par_inputs$n_regions
      n_fleets  <- em_info$par_inputs$n_fleets
      n_indices <- em_info$par_inputs$n_indices
      
      info <- generate_basic_info_em(
        em_info = em_info,
        em_years = em_years,
        n_stocks = n_stocks,
        n_regions = n_regions,
        n_fleets = n_fleets,
        n_indices = n_indices,
        filter_indices = filter_indices
      )
      
      basic_info <- info$basic_info
      id_fleets  <- info$fleets_to_remove
      id_indices <- info$indices_to_remove
      
      if (is.null(id_fleets)) id_fleets <- numeric(0)
      if (is.null(id_indices) || identical(id_indices, 0)) id_indices <- numeric(0)
      
      if (length(id_fleets) > 0) {
        info$catch_info$agg_catch     <- data$agg_catch[ind_em, -id_fleets, drop = FALSE]
        info$catch_info$catch_paa     <- data$catch_paa[-id_fleets, ind_em, , drop = FALSE]
        info$catch_info$use_agg_catch <- info$catch_info$use_agg_catch[ind_em, -id_fleets, drop = FALSE]
        info$catch_info$use_catch_paa <- info$catch_info$use_catch_paa[ind_em, -id_fleets, drop = FALSE]
      } else {
        info$catch_info$agg_catch     <- data$agg_catch[ind_em, , drop = FALSE]
        info$catch_info$catch_paa     <- data$catch_paa[, ind_em, , drop = FALSE]
        info$catch_info$use_agg_catch <- info$catch_info$use_agg_catch[ind_em, , drop = FALSE]
        info$catch_info$use_catch_paa <- info$catch_info$use_catch_paa[ind_em, , drop = FALSE]
      }
      
      if (!is.null(filter_indices) && any(filter_indices == 0)) {
        info$index_info$agg_indices   <- data$agg_indices[ind_em, idx, drop = FALSE]
        info$index_info$index_paa     <- data$index_paa[idx, ind_em, , drop = FALSE]
        info$index_info$use_indices   <- data$use_indices[ind_em, idx, drop = FALSE]
        info$index_info$use_index_paa <- data$use_index_paa[ind_em, idx, drop = FALSE]
      } else {
        info$index_info$agg_indices   <- data$agg_indices[ind_em, , drop = FALSE]
        info$index_info$index_paa     <- data$index_paa[, ind_em, , drop = FALSE]
        info$index_info$use_indices   <- data$use_indices[ind_em, , drop = FALSE]
        info$index_info$use_index_paa <- data$use_index_paa[ind_em, , drop = FALSE]
      }
      
      if (length(id_indices) > 0) {
        info$index_info$agg_indices   <- info$index_info$agg_indices[, -id_indices, drop = FALSE]
        info$index_info$index_paa     <- info$index_info$index_paa[-id_indices, , , drop = FALSE]
        info$index_info$use_indices   <- info$index_info$use_indices[, -id_indices, drop = FALSE]
        info$index_info$use_index_paa <- info$index_info$use_index_paa[, -id_indices, drop = FALSE]
      }
      
      if (is.null(reduce_region_info)) {
        stop("Users must prepare a list of new model configuration (NAA_where, sel_em, M_em, NAA_re_em, move_em, onto_move_list) if some areas are dropped from the model!")
      }
      
      basic_info$NAA_where <- reduce_region_info$NAA_where_em
      sel_em               <- reduce_region_info$sel_em
      M_em                 <- reduce_region_info$M_em
      NAA_re_em            <- reduce_region_info$NAA_re_em
      move_em              <- reduce_region_info$move_em
      catchability_em      <- reduce_region_info$catchability_em
      ecov_em              <- reduce_region_info$ecov_em
      onto_move_list       <- reduce_region_info$onto_move_list
      
      if (n_regions == 1) move_em <- NULL
      if (n_regions == 1) basic_info$NAA_where <- NULL
      if (n_regions == 1) {
        basic_info$move_dyn <- 0
        basic_info$onto_move <- matrix(0)
        basic_info$apply_re_trend <- 0
        basic_info$apply_mu_trend <- 0
      }
      
      ecov_em_new <- build_ecov_em_input(ecov_em, om, ecov_em_opts)
      
      if (isTRUE(em.opt$do.move) && n_regions > 1) {
        
        em_input <- prepare_wham_input(
          basic_info   = basic_info,
          selectivity  = sel_em,
          M            = M_em,
          NAA_re       = NAA_re_em,
          move         = move_em,
          catchability = catchability_em,
          ecov         = ecov_em_new,
          age_comp     = age_comp_em,
          catch_info   = info$catch_info,
          index_info   = info$index_info,
          F            = info$F
        )
        
        em_input <- update_waa(em_input, waa_info = em_info$par_inputs$user_waa)
        em_input <- apply_update_catch_info(em_input, update_catch_info, ind_em)
        em_input <- apply_update_index_info(em_input, update_index_info, ind_em)
        
      } else {
        
        basic_info$move_dyn <- 0
        basic_info$onto_move <- matrix(0)
        basic_info$apply_re_trend <- 0
        basic_info$apply_mu_trend <- 0
        basic_info$NAA_where <- NULL
        
        em_input <- prepare_wham_input(
          basic_info   = basic_info,
          selectivity  = sel_em,
          M            = M_em,
          NAA_re       = NAA_re_em,
          move         = NULL,
          catchability = catchability_em,
          ecov         = ecov_em_new,
          age_comp     = age_comp_em,
          catch_info   = info$catch_info,
          index_info   = info$index_info,
          F            = info$F
        )
        
        em_input <- update_waa(em_input, waa_info = em_info$par_inputs$user_waa)
        em_input <- apply_update_catch_info(em_input, update_catch_info, ind_em)
        em_input <- apply_update_index_info(em_input, update_index_info, ind_em)
      }
    }
  }
  
  return(em_input)
}