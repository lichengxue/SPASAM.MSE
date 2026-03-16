#' Update the Operating Model and Generate Simulated Data
#'
#' This function updates fishing mortality (F) in the operating model, projects the
#' population forward, and generates new simulated data based on the updated F time
#' series.
#'
#' By default, the function follows the original behavior and overwrites all
#' simulated random effects listed in `random`. When `process_fix = TRUE`, the
#' historical process is preserved and only the future portion of `log_NAA` is
#' replaced, beginning at `first_free_year`.
#'
#' @param om A fitted operating model that includes both burn-in and feedback years.
#' @param interval.info A list containing projected catch advice for future years
#'   from the estimation model. It includes:
#'   \describe{
#'     \item{\code{years}}{Vector of projection years.}
#'     \item{\code{catch}}{Matrix (n_years x n_fleets, or compatible) of projected catch.}
#'   }
#' @param seed Integer. Seed used for generating simulated data to ensure
#'   reproducibility (default = 123).
#' @param random Character vector of process names that are treated as random
#'   effects in the operating model (default = `"log_NAA"`).
#' @param method Character. Optimization method used for solving F. Options include:
#'   \itemize{
#'     \item \code{"nlminb"} (default): Uses `nlminb` optimization.
#'     \item \code{"BFGS"}: Uses `optim` with the BFGS method.
#'   }
#' @param by_fleet Logical. If TRUE, estimates F separately for each fleet.
#'   If FALSE, estimates a single global F (default = TRUE).
#' @param do.brps Logical. If TRUE, calculates reference points in the operating
#'   model (default = FALSE).
#' @param process_fix Logical. If FALSE (default), overwrite all simulated random
#'   effects listed in `random`, matching the original behavior. If TRUE, preserve
#'   the historical process and overwrite only the future portion of `log_NAA`.
#' @param first_free_year Integer. First year index in `log_NAA` that is allowed to
#'   change when `process_fix = TRUE`. Years before this are preserved. Default = 1L.
#'
#' @return An updated operating model (`om`) with:
#'   \itemize{
#'     \item Updated F time series.
#'     \item Simulated observation data.
#'     \item Updated random effect parameters.
#'   }
#'
#' @export
#'
#' @seealso \code{\link{get_F_from_Catch}}, \code{\link{update_om_F}}
#'
update_om_fn <- function(om,
                         interval.info = NULL,
                         seed = 123,
                         random = "log_NAA",
                         method = "nlminb",
                         by_fleet = TRUE,
                         do.brps = FALSE,
                         process_fix = FALSE,
                         first_free_year = 1L) {
  
  # Names of observation data to update
  obs_names <- c("agg_indices", "agg_catch", "catch_paa", "index_paa", "Ecov_obs", "obsvec")
  
  # Helper function: update only the future portion of a 4D array
  update_future_only_4d <- function(target, sim, first_free_year) {
    
    d_target <- dim(target)
    d_sim <- dim(sim)
    
    if (is.null(d_target) || length(d_target) != 4L) {
      stop("`log_NAA` must be a 4D array when `process_fix = TRUE`.")
    }
    
    if (is.null(d_sim) || !identical(d_target, d_sim)) {
      stop(
        "Dimension mismatch between existing and simulated `log_NAA`.\n",
        "Existing dimensions: ", paste(d_target, collapse = " x "), "\n",
        "Simulated dimensions: ", paste(d_sim, collapse = " x ")
      )
    }
    
    if (!is.numeric(first_free_year) || length(first_free_year) != 1L || is.na(first_free_year)) {
      stop("`first_free_year` must be a single non-missing integer.")
    }
    
    first_free_year <- as.integer(first_free_year)
    
    if (first_free_year < 1L) {
      stop("`first_free_year` must be >= 1.")
    }
    
    if (first_free_year > d_target[3]) {
      stop("`first_free_year` exceeds the number of years in `log_NAA`.")
    }
    
    target[, , first_free_year:d_target[3], ] <- sim[, , first_free_year:d_target[3], ]
    target
  }
  
  if (!is.null(interval.info)) {
    
    # Iterative update F in the OM using get_F_from_Catch
    t <- 0
    
    for (y in interval.info$years) {
      
      year <- which(om$years == y)
      t <- t + 1
      Catch <- interval.info$catch[t, ]
      
      rep <- om$rep # generate the reported values given the parameters
      
      cat("\nNow calculating fleet-specific F in year ", y, "\n", sep = "")
      
      Fsolve <- get_F_from_Catch(om, Catch, year, method = method, by_fleet = by_fleet)
      
      cat("\nFishing mortality is ", paste(Fsolve, collapse = ", "), "\n", sep = "")
      cat("\nNow updating F in OM for year ", y, "\n", sep = "")
      
      om <- update_om_F(om, year, Fsolve)
      
      # Set seed for reproducibility
      set.seed(seed)
      
      cat("\nNow simulating data for year ", y, "\n", sep = "")
      
      # Simulate the population and observations
      om_sim <- om$simulate(complete = TRUE)
      
      # Update the simulated data in the operating model
      keep_obs <- intersect(obs_names, names(om_sim))
      om$input$data[keep_obs] <- om_sim[keep_obs]
      
      if (!process_fix) {
        
        # Original behavior: overwrite all simulated random effects
        om$input$par[random] <- om_sim[random]
        
        if (!is.null(om$parList)) {
          keep_par <- intersect(random, names(om$parList))
          om$parList[keep_par] <- om_sim[keep_par]
        }
        
      } else {
        
        # New behavior: preserve historical process and overwrite only future log_NAA
        if (!(length(random) == 1L && random == "log_NAA")) {
          stop("When `process_fix = TRUE`, `random` must be exactly 'log_NAA'.")
        }
        
        if (is.null(om$input$par$log_NAA)) {
          stop("`om$input$par$log_NAA` is NULL.")
        }
        
        if (is.null(om_sim$log_NAA)) {
          stop("Simulated `log_NAA` is NULL.")
        }
        
        om$input$par$log_NAA <- update_future_only_4d(
          target = om$input$par$log_NAA,
          sim = om_sim$log_NAA,
          first_free_year = first_free_year
        )
        
        if (!is.null(om$parList) && !is.null(om$parList$log_NAA)) {
          om$parList$log_NAA <- update_future_only_4d(
            target = om$parList$log_NAA,
            sim = om_sim$log_NAA,
            first_free_year = first_free_year
          )
        }
      }
      
      cat("\nNow projecting data for years after ", y, "\n", sep = "")
      
      # Fit the WHAM model without actually performing the fit
      om <- fit_wham(
        om$input,
        do.fit = FALSE,
        do.brps = do.brps,
        MakeADFun.silent = TRUE
      )
    }
    
  } else {
    
    set.seed(seed)
    
    om_sim <- om$simulate(complete = TRUE) # resimulate the population and observations
    
    om$input$data[intersect(obs_names, names(om_sim))] <- om_sim[intersect(obs_names, names(om_sim))]
    
    if (!process_fix) {
      
      # Original behavior
      om$input$par[random] <- om_sim[random]
      
      if (!is.null(om$parList)) {
        keep_par <- intersect(random, names(om$parList))
        om$parList[keep_par] <- om_sim[keep_par]
      }
      
    } else {
      
      # Preserve historical log_NAA, update future only
      if (!(length(random) == 1L && random == "log_NAA")) {
        stop("When `process_fix = TRUE`, `random` must be exactly 'log_NAA'.")
      }
      
      if (is.null(om$input$par$log_NAA)) {
        stop("`om$input$par$log_NAA` is NULL.")
      }
      
      if (is.null(om_sim$log_NAA)) {
        stop("Simulated `log_NAA` is NULL.")
      }
      
      om$input$par$log_NAA <- update_future_only_4d(
        target = om$input$par$log_NAA,
        sim = om_sim$log_NAA,
        first_free_year = first_free_year
      )
      
      if (!is.null(om$parList) && !is.null(om$parList$log_NAA)) {
        om$parList$log_NAA <- update_future_only_4d(
          target = om$parList$log_NAA,
          sim = om_sim$log_NAA,
          first_free_year = first_free_year
        )
      }
    }
    
    cat("\nNow simulating data\n")
    
    # reset the om
    om <- fit_wham(
      om$input,
      do.fit = FALSE,
      do.brps = do.brps,
      MakeADFun.silent = TRUE
    )
  }
  
  return(om)
}