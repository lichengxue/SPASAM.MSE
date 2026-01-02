#' Extract scalar OM outputs (e.g., SSB, Catch) from MSE simulations
#'
#' Generic helper to extract scalar-valued outputs (e.g., SSB, predicted catch)
#' from a list of WHAM/whamMSE operating model objects. It supports both
#' single-simulation and multi-simulation (`is.nsim = TRUE`) layouts and can
#' optionally restrict to the first or last \code{use.n.years} of the time
#' series and/or compute a global sum across columns.
#'
#' @param mods A list of model outputs. For single-simulation runs this is
#'   typically \code{list(Mod1, Mod2, ...)}. For multi-simulation runs it is
#'   typically \code{list(sim1 = list(Mod1, Mod2, ...), sim2 = list(...), ...)}.
#' @param is.nsim Logical; \code{FALSE} if \code{mods} is a simple list of
#'   models, \code{TRUE} if \code{mods} is a list of simulations each
#'   containing a list of models.
#' @param slot Character; name of the element inside \code{om$rep} to extract
#'   (e.g., \code{"SSB"}, \code{"pred_catch"}).
#' @param var_name Character; base name used for the resulting columns
#'   (e.g., \code{"SSB"}, \code{"Catch"}). Columns will be named
#'   \code{var_name} (single-column case) or \code{var_name.1}, \code{var_name.2}, etc.
#' @param scope Character; which portion of the time series to extract.
#'   One of \code{"all"}, \code{"last"}, or \code{"first"}.
#' @param use.n.years Integer or \code{NULL}; number of years to keep when
#'   \code{scope} is \code{"last"} or \code{"first"}. If \code{NULL} and
#'   \code{scope != "all"}, a default of 5 years is used (with a message).
#' @param start.years Integer; first year index to use when
#'   \code{scope = "first"}. Defaults to 1 (start of the time series).
#' @param add_global Logical; if \code{TRUE}, an additional column named
#'   \code{paste0(var_name, "_Global")} is added, computed as the row-wise sum
#'   of all columns whose names start with \code{var_name}.
#'
#' @return A \code{data.frame} in wide format with:
#' \itemize{
#'   \item One or more columns containing the extracted quantity
#'         (e.g., \code{SSB}, \code{SSB.1}, \code{SSB.2}, ...).
#'   \item \code{Model}: factor or character indicating the model index
#'         (e.g., \code{"Model1"}, \code{"Model2"}, ...).
#'   \item \code{Year}: numeric vector of years.
#'   \item \code{Realization}: integer index of the simulation replicate
#'         (1 for non-simulation runs).
#'   \item Optionally \code{var_name_Global} if \code{add_global = TRUE}.
#' }
#'
#' @details
#' This helper is designed to standardize the extraction of quantities like SSB
#' and Catch from MSE operating models so that multiple plotting functions can
#' reuse the same logic. It handles both single and multiple simulations,
#' and provides flexible control over which portion of the time series is
#' returned.
#'
#' @examples
#' \dontrun{
#' # Extract full SSB time series from a single-simulation list of models:
#' ssb_ts <- extract_mods_scalar(mods, is.nsim = FALSE,
#'                               slot = "SSB", var_name = "SSB",
#'                               scope = "all")
#'
#' # Extract last 10 years of Catch (with global total) from multi-simulation:
#' catch_last <- extract_mods_scalar(mods, is.nsim = TRUE,
#'                                   slot = "pred_catch",
#'                                   var_name = "Catch",
#'                                   scope = "last",
#'                                   use.n.years = 10,
#'                                   add_global = TRUE)
#' }
#'
#' @keywords internal

extract_mods_scalar <- function(mods, is.nsim,
                                slot,
                                var_name,
                                scope        = c("all", "last", "first"),
                                use.n.years  = NULL,
                                start.years  = 1,
                                add_global   = FALSE) {
  scope <- match.arg(scope)
  
  # Default window length if needed
  if (scope != "all" && is.null(use.n.years)) {
    cat("\n[extract_mods_scalar] use.n.years not specified, using default (5 years).\n")
    use.n.years <- 5
  }
  
  # Years
  Years <- if (!is.nsim) mods[[1]]$om$years else mods[[1]][[1]]$om$years
  
  build_one <- function(mat, model_name, realization) {
    df <- as.data.frame(mat)
    
    # Name columns: SSB, SSB.1, SSB.2, ... or Catch, Catch.1, ...
    if (ncol(df) == 1L) {
      names(df) <- var_name
    } else {
      names(df) <- paste0(var_name, ".", seq_len(ncol(df)))
    }
    
    df$Model       <- model_name
    df$Year        <- Years
    df$Realization <- realization
    
    # Apply time window
    if (scope == "last") {
      df <- utils::tail(df, use.n.years)
    } else if (scope == "first") {
      start_idx <- start.years
      end_idx   <- min(start.years + use.n.years - 1L, nrow(df))
      df        <- df[start_idx:end_idx, , drop = FALSE]
    }
    
    # Optional global sum across columns
    if (add_global) {
      df <- dplyr::rowwise(df) |>
        dplyr::mutate(
          !!paste0(var_name, "_Global") :=
            sum(dplyr::c_across(dplyr::starts_with(var_name)), na.rm = TRUE)
        ) |>
        dplyr::ungroup()
    }
    
    df
  }
  
  if (!is.nsim) {
    out <- lapply(seq_along(mods), function(i) {
      mat <- mods[[i]]$om$rep[[slot]]
      build_one(mat, paste0("Model", i), 1L)
    })
  } else {
    out <- lapply(seq_along(mods), function(r) {
      inner <- lapply(seq_along(mods[[r]]), function(m) {
        mat <- mods[[r]][[m]]$om$rep[[slot]]
        build_one(mat, paste0("Model", m), r)
      })
      dplyr::bind_rows(inner)
    })
  }
  
  dplyr::bind_rows(out)
  
}


#' Extract Fbar by fleet, region, or global from MSE simulations
#'
#' Helper to extract Fbar (fishing mortality) summaries from WHAM/whamMSE
#' operating model objects. It supports extracting Fbar by fleet, by region,
#' or for the global aggregate, and can restrict to the first or last
#' \code{use.n.years} of the time series.
#'
#' @param mods A list of model outputs. For single-simulation runs this is
#'   typically \code{list(Mod1, Mod2, ...)}. For multi-simulation runs it is
#'   typically \code{list(sim1 = list(Mod1, Mod2, ...), sim2 = list(...), ...)}.
#' @param is.nsim Logical; \code{FALSE} if \code{mods} is a simple list of
#'   models, \code{TRUE} if \code{mods} is a list of simulations each
#'   containing a list of models.
#' @param level Character; which Fbar level to extract. One of:
#'   \itemize{
#'     \item \code{"fleet"}: per-fleet Fbar columns.
#'     \item \code{"region"}: per-region Fbar columns.
#'     \item \code{"global"}: the global Fbar column.
#'   }
#' @param scope Character; which portion of the time series to extract.
#'   One of \code{"all"}, \code{"last"}, or \code{"first"}.
#' @param use.n.years Integer or \code{NULL}; number of years to keep when
#'   \code{scope} is \code{"last"} or \code{"first"}. If \code{NULL} and
#'   \code{scope != "all"}, a default of 5 years is used (with a message).
#' @param start.years Integer; first year index to use when
#'   \code{scope = "first"}. Defaults to 1 (start of the time series).
#'
#' @return A \code{data.frame} in wide format with:
#' \itemize{
#'   \item One or more Fbar columns named \code{Fleet_1}, \code{Fleet_2}, ...
#'         (for \code{level = "fleet"}), \code{Region_1}, ... (for
#'         \code{level = "region"}), or \code{Global1}, ... (for
#'         \code{level = "global"}).
#'   \item \code{Model}: factor or character indicating the model index
#'         (e.g., \code{"Model1"}, \code{"Model2"}, ...).
#'   \item \code{Year}: numeric vector of years.
#'   \item \code{Realization}: integer index of the simulation replicate
#'         (1 for non-simulation runs).
#' }
#'
#' @details
#' The function assumes that Fbar is stored in \code{om$rep$Fbar} and that
#' the columns correspond to fleets, regions, and a global column in that order.
#' The number of fleets and regions is taken from
#' \code{om$input$data$n_fleets[1]} and \code{om$input$data$n_regions[1]}.
#'
#' @examples
#' \dontrun{
#' # Extract full, per-fleet Fbar time series:
#' fbar_fleet <- extract_mods_Fbar(mods, is.nsim = FALSE,
#'                                 level = "fleet", scope = "all")
#'
#' # Extract last 5 years of global Fbar from multi-simulation:
#' fbar_global_last <- extract_mods_Fbar(mods, is.nsim = TRUE,
#'                                       level = "global",
#'                                       scope = "last",
#'                                       use.n.years = 5)
#' }
#'
#' @keywords internal

extract_mods_Fbar <- function(mods, is.nsim,
                              level       = c("fleet", "region", "global"),
                              scope       = c("all", "last", "first"),
                              use.n.years = NULL,
                              start.years = 1) {
  level <- match.arg(level)
  scope <- match.arg(scope)
  
  # Default window length if needed
  if (scope != "all" && is.null(use.n.years)) {
    cat("\n[extract_mods_Fbar] use.n.years not specified, using default (5 years).\n")
    use.n.years <- 5
  }
  
  # Get Years and data slot
  if (!is.nsim) {
    Years <- mods[[1]]$om$years
    dat   <- mods[[1]]$om$input$data
  } else {
    Years <- mods[[1]][[1]]$om$years
    dat   <- mods[[1]][[1]]$om$input$data
  }
  
  n_fleets  <- dat$n_fleets[1]
  n_regions <- dat$n_regions[1]
  
  # Decide which columns and labels based on level
  if (level == "fleet") {
    index_range  <- 1:n_fleets
    label_prefix <- "Fleet_"
  } else if (level == "region") {
    index_range  <- (n_fleets + 1):(n_fleets + n_regions)
    label_prefix <- "Region_"
  } else { # global
    index_range  <- n_fleets + n_regions + 1
    label_prefix <- "Global"
  }
  
  build_one <- function(mat, model_name, realization) {
    df <- as.data.frame(mat[, index_range, drop = FALSE])
    names(df) <- paste0(label_prefix, seq_along(index_range))
    df$Model       <- model_name
    df$Year        <- Years
    df$Realization <- realization
    
    if (scope == "last") {
      df <- utils::tail(df, use.n.years)
    } else if (scope == "first") {
      start_idx <- start.years
      end_idx   <- min(start.years + use.n.years - 1L, nrow(df))
      df        <- df[start_idx:end_idx, , drop = FALSE]
    }
    
    df
  }
  
  if (!is.nsim) {
    out <- lapply(seq_along(mods), function(i) {
      mat <- mods[[i]]$om$rep$Fbar
      build_one(mat, paste0("Model", i), 1L)
    })
  } else {
    out <- lapply(seq_along(mods), function(r) {
      inner <- lapply(seq_along(mods[[r]]), function(m) {
        mat <- mods[[r]][[m]]$om$rep$Fbar
        build_one(mat, paste0("Model", m), r)
      })
      dplyr::bind_rows(inner)
    })
  }
  
  dplyr::bind_rows(out)
}

#' Extract Catch by fleet, region, or global from MSE simulations
#'
#' Helper to extract predicted catch summaries from WHAM/whamMSE operating
#' model objects. It supports extracting Catch by fleet, by region, or for
#' the global aggregate, and can restrict to the first or last
#' \code{use.n.years} of the time series.
#'
#' @param mods A list of model outputs. For single-simulation runs this is
#'   typically \code{list(Mod1, Mod2, ...)}. For multi-simulation runs it is
#'   typically \code{list(sim1 = list(Mod1, Mod2, ...), sim2 = list(...), ...)}.
#' @param is.nsim Logical; \code{FALSE} if \code{mods} is a simple list of
#'   models, \code{TRUE} if \code{mods} is a list of simulations each
#'   containing a list of models.
#' @param level Character; which Catch level to extract. One of:
#'   \itemize{
#'     \item \code{"fleet"}: per-fleet Catch columns.
#'     \item \code{"region"}: per-region Catch columns (fleets are summed
#'           by \code{fleet_regions}).
#'     \item \code{"global"}: a single global Catch column (row sum over fleets).
#'   }
#' @param scope Character; which portion of the time series to extract.
#'   One of \code{"all"}, \code{"last"}, or \code{"first"}.
#' @param use.n.years Integer or \code{NULL}; number of years to keep when
#'   \code{scope} is \code{"last"} or \code{"first"}. If \code{NULL} and
#'   \code{scope != "all"}, a default of 5 years is used (with a message).
#' @param start.years Integer; first year index to use when
#'   \code{scope = "first"}. Defaults to 1 (start of the time series).
#'
#' @return A \code{data.frame} in wide format with:
#' \itemize{
#'   \item One or more Catch columns named \code{Fleet_1}, \code{Fleet_2}, ...
#'         (for \code{level = "fleet"}), \code{Region_1}, ... (for
#'         \code{level = "region"}), or \code{Global} (for
#'         \code{level = "global"}).
#'   \item \code{Model}: factor or character indicating the model index
#'         (e.g., \code{"Model1"}, \code{"Model2"}, ...).
#'   \item \code{Year}: numeric vector of years.
#'   \item \code{Realization}: integer index of the simulation replicate
#'         (1 for non-simulation runs).
#' }
#'
#' @details
#' The function assumes that predicted catch is stored in
#' \code{om$rep$pred_catch} as a matrix of dimension
#' \code{n_years × n_fleets}. Region-level Catch is obtained by summing
#' across fleets belonging to each region using
#' \code{om$input$data$fleet_regions}. Global Catch is the row sum across
#' all fleets.
#'
#' @keywords internal
extract_mods_catch <- function(mods, is.nsim,
                               level       = c("fleet", "region", "global"),
                               scope       = c("all", "last", "first"),
                               use.n.years = NULL,
                               start.years = 1) {
  level <- match.arg(level)
  scope <- match.arg(scope)
  
  # Default window length if needed
  if (scope != "all" && is.null(use.n.years)) {
    cat("\n[extract_mods_catch] use.n.years not specified, using default (5 years).\n")
    use.n.years <- 5L
  }
  
  # Grab years and data template from the first model
  if (!is.nsim) {
    Years <- mods[[1]]$om$years
    dat   <- mods[[1]]$om$input$data
  } else {
    Years <- mods[[1]][[1]]$om$years
    dat   <- mods[[1]][[1]]$om$input$data
  }
  
  n_fleets      <- dat$n_fleets[1]
  n_regions     <- dat$n_regions[1]
  fleet_regions <- as.integer(dat$fleet_regions)  # length n_fleets
  
  # Build one data.frame for a given pred_catch matrix
  build_one <- function(mat, model_name, realization) {
    mat <- as.matrix(mat)          # make sure it’s a matrix
    ny  <- nrow(mat)
    
    # ---------------------------
    # 1. Build wide Catch matrix for requested level
    # ---------------------------
    if (level == "fleet") {
      
      # Each column is a fleet
      df <- as.data.frame(mat)
      # In case pred_catch has fewer cols than n_fleets, truncate names to ncol(mat)
      ncol_mat <- ncol(mat)
      df_names <- paste0("Fleet_", seq_len(max(n_fleets, ncol_mat)))
      names(df) <- df_names[seq_len(ncol_mat)]
      
    } else if (level == "region") {
      
      # Aggregate fleets -> regions by fleet_regions
      region_mat <- matrix(NA_real_, nrow = ny, ncol = n_regions)
      
      for (r in seq_len(n_regions)) {
        idx <- which(fleet_regions == r)
        if (length(idx) > 0) {
          region_mat[, r] <- rowSums(mat[, idx, drop = FALSE], na.rm = TRUE)
        } else {
          region_mat[, r] <- NA_real_
        }
      }
      
      df <- as.data.frame(region_mat)
      names(df) <- paste0("Region_", seq_len(n_regions))
      
    } else { # level == "global"
      
      # Single global column: row sum over fleets
      df <- data.frame(Global = rowSums(mat, na.rm = TRUE))
    }
    
    # ---------------------------
    # 2. Attach Model, Year, Realization
    # ---------------------------
    df$Model       <- model_name
    df$Year        <- Years
    df$Realization <- realization
    
    # ---------------------------
    # 3. Apply 'scope'
    # ---------------------------
    if (scope == "last") {
      # tail() is safe even if use.n.years > nrow(df)
      df <- utils::tail(df, use.n.years)
    } else if (scope == "first") {
      start_idx <- start.years
      end_idx   <- min(start.years + use.n.years - 1L, nrow(df))
      df        <- df[start_idx:end_idx, , drop = FALSE]
    }
    
    df
  }
  
  # ---------------------------
  # 4. Loop over models / sims
  # ---------------------------
  if (!is.nsim) {
    out <- lapply(seq_along(mods), function(i) {
      mat <- mods[[i]]$om$rep$pred_catch
      build_one(mat, paste0("Model", i), 1L)
    })
  } else {
    out <- lapply(seq_along(mods), function(r) {
      inner <- lapply(seq_along(mods[[r]]), function(m) {
        mat <- mods[[r]][[m]]$om$rep$pred_catch
        build_one(mat, paste0("Model", m), r)
      })
      dplyr::bind_rows(inner)
    })
  }
  
  dplyr::bind_rows(out)
}

#' Extract SSB by region or global from MSE simulations
#'
#' Helper to extract spawning stock biomass (SSB) summaries from WHAM/whamMSE
#' operating model objects. It supports extracting SSB by region, or for the
#' global aggregate (row-sum over regions), and can restrict to the first or
#' last \code{use.n.years} of the time series.
#'
#' @param mods A list of model outputs. For single-simulation runs this is
#'   typically \code{list(Mod1, Mod2, ...)}. For multi-simulation runs it is
#'   typically \code{list(sim1 = list(Mod1, Mod2, ...), sim2 = list(...), ...)}.
#' @param is.nsim Logical; \code{FALSE} if \code{mods} is a simple list of
#'   models, \code{TRUE} if \code{mods} is a list of simulations each
#'   containing a list of models.
#' @param level Character; which SSB level to extract. One of:
#'   \itemize{
#'     \item \code{"region"}: per-region SSB columns.
#'     \item \code{"global"}: a single global SSB column (row sum over regions).
#'   }
#' @param scope Character; which portion of the time series to extract.
#'   One of \code{"all"}, \code{"last"}, or \code{"first"}.
#' @param use.n.years Integer or \code{NULL}; number of years to keep when
#'   \code{scope} is \code{"last"} or \code{"first"}. If \code{NULL} and
#'   \code{scope != "all"}, a default of 5 years is used (with a message).
#' @param start.years Integer; first year index to use when
#'   \code{scope = "first"}. Defaults to 1 (start of the time series).
#'
#' @return A \code{data.frame} in wide format with:
#' \itemize{
#'   \item One or more SSB columns named \code{Region_1}, \code{Region_2}, ...
#'         (for \code{level = "region"}), or \code{Global} (for
#'         \code{level = "global"}).
#'   \item \code{Model}: factor or character indicating the model index
#'         (e.g., \code{"Model1"}, \code{"Model2"}, ...).
#'   \item \code{Year}: numeric vector of years.
#'   \item \code{Realization}: integer index of the simulation replicate
#'         (1 for non-simulation runs).
#' }
#'
#' @keywords internal
extract_mods_SSB <- function(mods, is.nsim,
                             level       = c("global", "region"),
                             scope       = c("all", "last", "first"),
                             use.n.years = NULL,
                             start.years = 1) {
  level <- match.arg(level)
  scope <- match.arg(scope)
  
  # Default window length if needed
  if (scope != "all" && is.null(use.n.years)) {
    cat("\n[extract_mods_SSB] use.n.years not specified, using default (5 years).\n")
    use.n.years <- 5L
  }
  
  # Grab years and data template from the first model
  if (!is.nsim) {
    Years <- mods[[1]]$om$years
    dat   <- mods[[1]]$om$input$data
  } else {
    Years <- mods[[1]][[1]]$om$years
    dat   <- mods[[1]][[1]]$om$input$data
  }
  
  n_regions <- dat$n_regions[1]
  
  # Build one data.frame for a given SSB matrix
  build_one <- function(mat, model_name, realization) {
    mat <- as.matrix(mat)  # n_years x n_regions (typically)
    ny  <- nrow(mat)
    
    # 1. Build wide SSB matrix for requested level
    if (level == "region") {
      # columns = regions
      df <- as.data.frame(mat[, seq_len(n_regions), drop = FALSE])
      names(df) <- paste0("Region_", seq_len(n_regions))
    } else { # level == "global"
      # single global column: row sum over regions
      df <- data.frame(Global = rowSums(mat[, seq_len(n_regions), drop = FALSE],
                                        na.rm = TRUE))
    }
    
    # 2. Attach Model, Year, Realization
    df$Model       <- model_name
    df$Year        <- Years
    df$Realization <- realization
    
    # 3. Apply 'scope'
    if (scope == "last") {
      df <- utils::tail(df, use.n.years)
    } else if (scope == "first") {
      start_idx <- start.years
      end_idx   <- min(start.years + use.n.years - 1L, nrow(df))
      df        <- df[start_idx:end_idx, , drop = FALSE]
    }
    
    df
  }
  
  # 4. Loop over models / sims
  if (!is.nsim) {
    out <- lapply(seq_along(mods), function(i) {
      mat <- mods[[i]]$om$rep$SSB
      build_one(mat, paste0("Model", i), 1L)
    })
  } else {
    out <- lapply(seq_along(mods), function(r) {
      inner <- lapply(seq_along(mods[[r]]), function(m) {
        mat <- mods[[r]][[m]]$om$rep$SSB
        build_one(mat, paste0("Model", m), r)
      })
      dplyr::bind_rows(inner)
    })
  }
  
  dplyr::bind_rows(out)
}

#' Build a reusable summary of MSE outputs
#'
#' Construct a list of commonly used summaries (full time series, first-window,
#' and last-window extracts) for SSB, Catch, and Fbar (by fleet, region, and
#' global) from a set of MSE operating model outputs. This is intended to be
#' computed once (e.g., in \code{plot_mse_output()}) and then reused by
#' downstream plotting functions via \code{getOption("mse_summary")}.
#'
#' This updated version also stores **SSB by region and global** using
#' \code{extract_mods_SSB()}, so plotting functions can directly use:
#' \code{ssb_region_ts/first/last} and \code{ssb_global_ts/first/last}.
#'
#' @param mods A list of model outputs, either a simple list
#'   (\code{is.nsim = FALSE}) or a list of simulation lists
#'   (\code{is.nsim = TRUE}).
#' @param is.nsim Logical; \code{FALSE} if \code{mods} is a simple list of
#'   models, \code{TRUE} if \code{mods} is a list of simulations each
#'   containing a list of models.
#' @param start.years Integer; first row index used for the short-term
#'   summaries (i.e., the starting row for \code{scope="first"}).
#' @param use.n.years.first Integer; number of years used for the
#'   short-term (first-window) summaries.
#' @param use.n.years.last Integer; number of years used for the
#'   long-term (last-window) summaries.
#'
#' @return A named \code{list} containing:
#' \itemize{
#'   \item Metadata: \code{is.nsim}, \code{start.years},
#'         \code{use.n.years.first}, \code{use.n.years.last}
#'
#'   \item Full time series (all years):
#'     \itemize{
#'       \item \code{ssb_ts}: SSB time series via \code{extract_mods_scalar()}.
#'       \item \code{ssb_region_ts}, \code{ssb_global_ts}: SSB by region/global
#'             via \code{extract_mods_SSB()}.
#'       \item \code{catch_ts}: Catch time series via \code{extract_mods_scalar()}.
#'       \item \code{fbar_fleet_ts}, \code{fbar_region_ts}, \code{fbar_global_ts}:
#'             Fbar time series via \code{extract_mods_Fbar()}.
#'       \item \code{catch_fleet_ts}, \code{catch_region_ts}, \code{catch_global_ts}:
#'             Catch time series via \code{extract_mods_catch()}.
#'     }
#'
#'   \item First-window (short term):
#'     \itemize{
#'       \item \code{ssb_first}, \code{catch_first}: via \code{extract_mods_scalar()}
#'             with \code{add_global = TRUE}.
#'       \item \code{ssb_region_first}, \code{ssb_global_first}: via \code{extract_mods_SSB()}.
#'       \item \code{fbar_fleet_first}, \code{fbar_region_first}, \code{fbar_global_first}
#'       \item \code{catch_fleet_first}, \code{catch_region_first}, \code{catch_global_first}
#'     }
#'
#'   \item Last-window (long term):
#'     \itemize{
#'       \item \code{ssb_last}, \code{catch_last}: via \code{extract_mods_scalar()}
#'             with \code{add_global = TRUE}.
#'       \item \code{ssb_region_last}, \code{ssb_global_last}: via \code{extract_mods_SSB()}.
#'       \item \code{fbar_fleet_last}, \code{fbar_region_last}, \code{fbar_global_last}
#'       \item \code{catch_fleet_last}, \code{catch_region_last}, \code{catch_global_last}
#'     }
#' }
#'
#' @details
#' All components are generated via the helper extractors:
#' \code{extract_mods_scalar()}, \code{extract_mods_SSB()},
#' \code{extract_mods_Fbar()}, and \code{extract_mods_catch()}.
#'
#' A common workflow is:
#' \preformatted{
#' summ <- build_mse_summary(mods, is.nsim,
#'                           start.years = 1,
#'                           use.n.years.first = 5,
#'                           use.n.years.last  = 10)
#' options(mse_summary = summ)
#' }
#'
#' @keywords internal
build_mse_summary <- function(mods,
                              is.nsim,
                              start.years       = 1,
                              use.n.years.first = 5,
                              use.n.years.last  = 5) {
  
  ## -----------------------------
  ## Time series (all years)
  ## -----------------------------
  ssb_ts <- extract_mods_scalar(
    mods, is.nsim,
    slot     = "SSB",
    var_name = "SSB",
    scope    = "all"
  )
  
  # NEW: SSB region/global time series (for plot_ssb_performance)
  ssb_region_ts <- extract_mods_SSB(
    mods, is.nsim,
    level = "region",
    scope = "all"
  )
  ssb_global_ts <- extract_mods_SSB(
    mods, is.nsim,
    level = "global",
    scope = "all"
  )
  
  catch_ts <- extract_mods_scalar(
    mods, is.nsim,
    slot     = "pred_catch",
    var_name = "Catch",
    scope    = "all"
  )
  
  fbar_fleet_ts  <- extract_mods_Fbar(mods, is.nsim, level = "fleet",  scope = "all")
  fbar_region_ts <- extract_mods_Fbar(mods, is.nsim, level = "region", scope = "all")
  fbar_global_ts <- extract_mods_Fbar(mods, is.nsim, level = "global", scope = "all")
  
  catch_fleet_ts  <- extract_mods_catch(mods, is.nsim, level = "fleet",  scope = "all")
  catch_region_ts <- extract_mods_catch(mods, is.nsim, level = "region", scope = "all")
  catch_global_ts <- extract_mods_catch(mods, is.nsim, level = "global", scope = "all")
  
  ## -----------------------------
  ## First part of series (short term)
  ## -----------------------------
  ssb_first <- extract_mods_scalar(
    mods, is.nsim,
    slot        = "SSB",
    var_name    = "SSB",
    scope       = "first",
    use.n.years = use.n.years.first,
    start.years = start.years,
    add_global  = TRUE
  )
  
  # NEW: SSB region/global first-window
  ssb_region_first <- extract_mods_SSB(
    mods, is.nsim,
    level       = "region",
    scope       = "first",
    use.n.years = use.n.years.first,
    start.years = start.years
  )
  ssb_global_first <- extract_mods_SSB(
    mods, is.nsim,
    level       = "global",
    scope       = "first",
    use.n.years = use.n.years.first,
    start.years = start.years
  )
  
  catch_first <- extract_mods_scalar(
    mods, is.nsim,
    slot        = "pred_catch",
    var_name    = "Catch",
    scope       = "first",
    use.n.years = use.n.years.first,
    start.years = start.years,
    add_global  = TRUE
  )
  
  fbar_fleet_first  <- extract_mods_Fbar(mods, is.nsim, level = "fleet",  scope = "first",
                                         use.n.years = use.n.years.first, start.years = start.years)
  fbar_region_first <- extract_mods_Fbar(mods, is.nsim, level = "region", scope = "first",
                                         use.n.years = use.n.years.first, start.years = start.years)
  fbar_global_first <- extract_mods_Fbar(mods, is.nsim, level = "global", scope = "first",
                                         use.n.years = use.n.years.first, start.years = start.years)
  
  catch_fleet_first  <- extract_mods_catch(mods, is.nsim, level = "fleet",  scope = "first",
                                           use.n.years = use.n.years.first, start.years = start.years)
  catch_region_first <- extract_mods_catch(mods, is.nsim, level = "region", scope = "first",
                                           use.n.years = use.n.years.first, start.years = start.years)
  catch_global_first <- extract_mods_catch(mods, is.nsim, level = "global", scope = "first",
                                           use.n.years = use.n.years.first, start.years = start.years)
  
  ## -----------------------------
  ## Last part of series (long term)
  ## -----------------------------
  ssb_last <- extract_mods_scalar(
    mods, is.nsim,
    slot        = "SSB",
    var_name    = "SSB",
    scope       = "last",
    use.n.years = use.n.years.last,
    add_global  = TRUE
  )
  
  # NEW: SSB region/global last-window
  ssb_region_last <- extract_mods_SSB(
    mods, is.nsim,
    level       = "region",
    scope       = "last",
    use.n.years = use.n.years.last
  )
  ssb_global_last <- extract_mods_SSB(
    mods, is.nsim,
    level       = "global",
    scope       = "last",
    use.n.years = use.n.years.last
  )
  
  catch_last <- extract_mods_scalar(
    mods, is.nsim,
    slot        = "pred_catch",
    var_name    = "Catch",
    scope       = "last",
    use.n.years = use.n.years.last,
    add_global  = TRUE
  )
  
  fbar_fleet_last  <- extract_mods_Fbar(mods, is.nsim, level = "fleet",  scope = "last",
                                        use.n.years = use.n.years.last)
  fbar_region_last <- extract_mods_Fbar(mods, is.nsim, level = "region", scope = "last",
                                        use.n.years = use.n.years.last)
  fbar_global_last <- extract_mods_Fbar(mods, is.nsim, level = "global", scope = "last",
                                        use.n.years = use.n.years.last)
  
  catch_fleet_last  <- extract_mods_catch(mods, is.nsim, level = "fleet",  scope = "last",
                                          use.n.years = use.n.years.last)
  catch_region_last <- extract_mods_catch(mods, is.nsim, level = "region", scope = "last",
                                          use.n.years = use.n.years.last)
  catch_global_last <- extract_mods_catch(mods, is.nsim, level = "global", scope = "last",
                                          use.n.years = use.n.years.last)
  
  ## -----------------------------
  ## Return list
  ## -----------------------------
  list(
    is.nsim           = is.nsim,
    start.years       = start.years,
    use.n.years.first = use.n.years.first,
    use.n.years.last  = use.n.years.last,
    
    ssb_ts         = ssb_ts,
    ssb_region_ts  = ssb_region_ts,
    ssb_global_ts  = ssb_global_ts,
    catch_ts       = catch_ts,
    
    fbar_fleet_ts   = fbar_fleet_ts,
    fbar_region_ts  = fbar_region_ts,
    fbar_global_ts  = fbar_global_ts,
    catch_fleet_ts  = catch_fleet_ts,
    catch_region_ts = catch_region_ts,
    catch_global_ts = catch_global_ts,
    
    ssb_first        = ssb_first,
    ssb_region_first = ssb_region_first,
    ssb_global_first = ssb_global_first,
    catch_first      = catch_first,
    
    fbar_fleet_first   = fbar_fleet_first,
    fbar_region_first  = fbar_region_first,
    fbar_global_first  = fbar_global_first,
    catch_fleet_first  = catch_fleet_first,
    catch_region_first = catch_region_first,
    catch_global_first = catch_global_first,
    
    ssb_last        = ssb_last,
    ssb_region_last = ssb_region_last,
    ssb_global_last = ssb_global_last,
    catch_last      = catch_last,
    
    fbar_fleet_last   = fbar_fleet_last,
    fbar_region_last  = fbar_region_last,
    fbar_global_last  = fbar_global_last,
    catch_fleet_last  = catch_fleet_last,
    catch_region_last = catch_region_last,
    catch_global_last = catch_global_last
  )
}

#' Plot realized SSB time series across models and simulations
#'
#' Generate time-series plots of spawning stock biomass (SSB) for each model
#' and simulation realization. If a precomputed MSE summary is available via
#' \code{options(mse_summary = build_mse_summary(...))}, the function will use
#' that; otherwise it falls back to extracting SSB directly from \code{mods}.
#'
#' @param mods A list of model outputs, either a simple list
#'   (\code{is.nsim = FALSE}) or a list of simulation lists
#'   (\code{is.nsim = TRUE}).
#' @param is.nsim Logical; \code{FALSE} if \code{mods} is a simple list of
#'   models, \code{TRUE} if \code{mods} is a list of simulations each
#'   containing a list of models.
#' @param main.dir Character; main directory where plots should be saved.
#' @param sub.dir Character; subdirectory inside \code{main.dir} where plots
#'   will be stored. A \code{"Time_Series"} folder is created within this
#'   subdirectory.
#' @param var Character; plotting label for the variable (default \code{"SSB"}).
#'   Used in plot titles and file names.
#' @param width Numeric; width of the saved plot in inches (default 10).
#' @param height Numeric; height of the saved plot in inches (default 7).
#' @param dpi Numeric; resolution of the saved plot in dots per inch (default 300).
#' @param col.opt Character; viridis color palette option (passed to
#'   \code{scale_color_viridis_d(option = col.opt)}).
#' @param new_model_names Optional character vector of custom model names.
#'   If supplied, its length must match the number of models; these names
#'   are then used in the \code{Model} factor.
#'
#' @return A \code{ggplot} object (invisibly saved to disk as a PNG). The file
#'   is written to:
#'   \preformatted{
#'   file.path(main.dir, sub.dir, "Time_Series", paste0(var, ".PNG"))
#'   }
#'
#' @details
#' The function first attempts to retrieve a precomputed summary via
#' \code{getOption("mse_summary")}. If this list exists and contains
#' \code{ssb_ts}, that data frame is used directly. Otherwise, SSB is
#' extracted from \code{mods} using \code{\link{extract_mods_scalar}} with
#' \code{scope = "all"}.
#'
#' The resulting data are converted to long format and plotted with one facet
#' per SSB column (e.g., SSB by region), colored by model and grouped by
#' \code{Model} and \code{Realization}.
#'
#' @examples
#' \dontrun{
#' # Basic SSB time series plot:
#' plot_ssb_time_series(mods, is.nsim = FALSE,
#'                      main.dir = "Results",
#'                      sub.dir  = "OM1",
#'                      var      = "SSB")
#' }
#'
#' @export

plot_ssb_time_series <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB",
                                 width = 10, height = 7, dpi = 300, col.opt = "D",
                                 new_model_names = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Try to use precomputed summary if available
  summ <- getOption("mse_summary", default = NULL)
  
  if (!is.null(summ) && !is.null(summ$ssb_ts)) {
    res <- summ$ssb_ts
  } else {
    # Fallback: compute the usual way (keeps backwards compatibility)
    res <- extract_mods_scalar(
      mods       = mods,
      is.nsim    = is.nsim,
      slot       = "SSB",
      var_name   = "SSB",
      scope      = "all",
      add_global = FALSE
    )
  }
  
  # Rename models if needed
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(
      res$Model,
      levels = paste0("Model", seq_along(new_model_names)),
      labels = new_model_names
    )
  }
  
  # Long format
  res_long <- tidyr::pivot_longer(
    res,
    cols      = dplyr::starts_with("SSB"),
    names_to  = "Label",
    values_to = "SSB"
  )
  
  p <- ggplot(res_long,
              aes(x = Year, y = SSB,
                  color = Model,
                  group = interaction(Model, Realization))) +
    geom_line(size = 0.3, alpha = 0.5) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(var) +
    ylab("SSB") +
    theme_bw()
  
  new_sub_dir <- file.path(main.dir, sub.dir, "Time_Series")
  if (!file.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
  
  ggsave(file.path(new_sub_dir, paste0(var, ".PNG")),
         plot = p, width = width, height = height, dpi = dpi)
  
  return(p)
}

#' Plot realized Fbar time series across models and simulations
#'
#' Generate time-series plots of fishing mortality (Fbar) for each model and
#' simulation realization at three aggregation levels: fleet, region, and global.
#' If a precomputed MSE summary is available via
#' \code{options(mse_summary = build_mse_summary(...))}, the function will use
#' that (if it contains \code{fbar_ts}); otherwise it falls back to extracting
#' Fbar directly from \code{mods}.
#'
#' @param mods A list of model outputs, either a simple list
#'   (\code{is.nsim = FALSE}) or a list of simulation lists
#'   (\code{is.nsim = TRUE}).
#' @param is.nsim Logical; \code{FALSE} if \code{mods} is a simple list of
#'   models, \code{TRUE} if \code{mods} is a list of simulations each
#'   containing a list of models.
#' @param main.dir Character; main directory where plots should be saved.
#' @param sub.dir Character; subdirectory inside \code{main.dir} where plots
#'   will be stored. A \code{"Time_Series"} folder is created within this
#'   subdirectory.
#' @param var Character; plotting label for the variable (default \code{"Fbar"}).
#'   Used in plot titles and file names.
#' @param width Numeric; width of the saved plot in inches (default 10).
#' @param height Numeric; height of the saved plot in inches (default 7).
#' @param dpi Numeric; resolution of the saved plot in dots per inch (default 300).
#' @param col.opt Character; viridis color palette option (passed to
#'   \code{scale_color_viridis_d(option = col.opt)}).
#' @param new_model_names Optional character vector of custom model names.
#'   If supplied, its length must match the number of models; these names
#'   are then used in the \code{Model} factor.
#'
#' @return A named list with three \code{ggplot} objects:
#' \code{list(fleet = <ggplot>, region = <ggplot>, global = <ggplot>)}.
#' Each plot is also saved to:
#' \preformatted{
#' file.path(main.dir, sub.dir, "Time_Series", "Fbar_fleet.PNG")
#' file.path(main.dir, sub.dir, "Time_Series", "Fbar_region.PNG")
#' file.path(main.dir, sub.dir, "Time_Series", "Fbar_global.PNG")
#' }
#'
#' @details
#' The function first attempts to retrieve a precomputed summary via
#' \code{getOption("mse_summary")}. If this list exists and contains
#' \code{fbar_ts}, that data frame is used directly (it must include
#' \code{Year}, \code{Model}, \code{Realization}, and columns beginning with
#' \code{"Fleet_"}, \code{"Region_"}, and/or \code{"Global"}).
#'
#' Otherwise, the function constructs fleet-, region-, and global-level Fbar
#' time series from \code{mods[[...]]$om$rep$Fbar}. It assumes the columns of
#' \code{Fbar} are ordered as fleet (1:\code{n_fleets}), region
#' (\code{n_fleets+1}:\code{n_fleets+n_regions}), then global
#' (\code{n_fleets+n_regions+1}).
#'
#' @examples
#' \dontrun{
#' out <- plot_fbar_time_series(mods, is.nsim = TRUE,
#'                              main.dir = "Results",
#'                              sub.dir  = "OM1")
#' print(out$global)
#' }
#'
#' @export
plot_fbar_time_series <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar",
                                  width = 10, height = 7, dpi = 300, col.opt = "D",
                                  new_model_names = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # --------------------------------------------------
  # 0) Try to use precomputed summary if available
  # --------------------------------------------------
  summ <- getOption("mse_summary", default = NULL)
  
  # Helper: rename Model if requested
  rename_models <- function(df) {
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(df$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      df$Model <- factor(
        df$Model,
        levels = paste0("Model", seq_along(new_model_names)),
        labels = new_model_names
      )
    }
    df
  }
  
  # Helper: plot + save (SSB-style)
  plot_and_save <- function(df, title, ylab_text, filename_stem) {
    df <- rename_models(df)
    
    # pivot only the numeric series columns, keep Model/Year/Realization
    df_long <- tidyr::pivot_longer(
      df,
      cols      = setdiff(names(df), c("Model", "Year", "Realization")),
      names_to  = "Label",
      values_to = "Fbar"
    )
    
    p <- ggplot(df_long,
                aes(x = Year, y = Fbar,
                    color = Model,
                    group = interaction(Model, Realization))) +
      geom_line(size = 0.3, alpha = 0.5) +
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(title) +
      ylab(ylab_text) +
      theme_bw()
    
    new_sub_dir <- file.path(main.dir, sub.dir, "Time_Series")
    if (!file.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
    
    ggsave(file.path(new_sub_dir, paste0(filename_stem, ".PNG")),
           plot = p, width = width, height = height, dpi = dpi)
    
    return(p)
  }
  
  # --------------------------------------------------
  # 1) If summary exists, use it
  # --------------------------------------------------
  if (!is.null(summ) && !is.null(summ$fbar_ts)) {
    
    res_all <- summ$fbar_ts
    # Expect columns like Fleet_1..., Region_1..., Global (or Global1 etc.)
    fleet_cols  <- grep("^Fleet_",  names(res_all), value = TRUE)
    region_cols <- grep("^Region_", names(res_all), value = TRUE)
    global_cols <- grep("^Global",  names(res_all), value = TRUE)
    
    if (length(fleet_cols) == 0 && length(region_cols) == 0 && length(global_cols) == 0) {
      stop("mse_summary$fbar_ts found, but no columns starting with 'Fleet_', 'Region_', or 'Global'.")
    }
    
    res_fleet  <- if (length(fleet_cols)  > 0) res_all[, c("Model","Year","Realization", fleet_cols),  drop = FALSE] else NULL
    res_region <- if (length(region_cols) > 0) res_all[, c("Model","Year","Realization", region_cols), drop = FALSE] else NULL
    res_global <- if (length(global_cols) > 0) res_all[, c("Model","Year","Realization", global_cols), drop = FALSE] else NULL
    
  } else {
    
    # --------------------------------------------------
    # 2) Fallback: build from mods$om$rep$Fbar
    # --------------------------------------------------
    if (!is.nsim) {
      Years     <- mods[[1]]$om$years
      n_fleets  <- mods[[1]]$om$input$data$n_fleets[1]
      n_regions <- mods[[1]]$om$input$data$n_regions[1]
    } else {
      Years     <- mods[[1]][[1]]$om$years
      n_fleets  <- mods[[1]][[1]]$om$input$data$n_fleets[1]
      n_regions <- mods[[1]][[1]]$om$input$data$n_regions[1]
    }
    
    make_plot_data <- function(index_range, label_prefix) {
      if (!is.nsim) {
        lapply(seq_along(mods), function(i) {
          tmp <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
          tmp <- as.data.frame(tmp)
          names(tmp) <- paste0(label_prefix, seq_along(index_range))
          tmp$Model <- paste0("Model", i)
          tmp$Year <- Years
          tmp$Realization <- 1
          tmp
        }) %>% bind_rows()
      } else {
        lapply(seq_along(mods), function(r) {
          lapply(seq_along(mods[[r]]), function(m) {
            tmp <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
            tmp <- as.data.frame(tmp)
            names(tmp) <- paste0(label_prefix, seq_along(index_range))
            tmp$Model <- paste0("Model", m)
            tmp$Year <- Years
            tmp$Realization <- r
            tmp
          }) %>% bind_rows()
        }) %>% bind_rows()
      }
    }
    
    # Fleet-level Fbar
    res_fleet  <- make_plot_data(1:n_fleets, "Fleet_")
    # Region-level Fbar
    res_region <- make_plot_data((n_fleets + 1):(n_fleets + n_regions), "Region_")
    # Global Fbar (single column)
    res_global <- make_plot_data(n_fleets + n_regions + 1, "Global")
  }
  
  # --------------------------------------------------
  # 3) Plot + save (return ggplots)
  # --------------------------------------------------
  out <- list()
  
  if (!is.null(res_fleet)) {
    out$fleet <- plot_and_save(res_fleet, "Fbar by Fleet", "Fbar", paste0(var, "_fleet"))
  }
  if (!is.null(res_region)) {
    out$region <- plot_and_save(res_region, "Fbar by Region", "Fbar", paste0(var, "_region"))
  }
  if (!is.null(res_global)) {
    out$global <- plot_and_save(res_global, "Global Fbar", "Fbar", paste0(var, "_global"))
  }
  
  return(out)
}



#' Plot realized Catch time series across models and simulations
#'
#' Generate time-series plots of predicted catch for each model and simulation
#' realization. If a precomputed MSE summary is available via
#' \code{options(mse_summary = build_mse_summary(...))}, the function will use
#' that (if it contains \code{catch_ts}); otherwise it falls back to extracting
#' catch directly from \code{mods}.
#'
#' @param mods A list of model outputs, either a simple list
#'   (\code{is.nsim = FALSE}) or a list of simulation lists
#'   (\code{is.nsim = TRUE}).
#' @param is.nsim Logical; \code{FALSE} if \code{mods} is a simple list of
#'   models, \code{TRUE} if \code{mods} is a list of simulations each
#'   containing a list of models.
#' @param main.dir Character; main directory where plots should be saved.
#' @param sub.dir Character; subdirectory inside \code{main.dir} where plots
#'   will be stored. A \code{"Time_Series"} folder is created within this
#'   subdirectory.
#' @param var Character; plotting label for the variable (default \code{"Catch"}).
#'   Used in plot titles and file names.
#' @param width Numeric; width of the saved plot in inches (default 10).
#' @param height Numeric; height of the saved plot in inches (default 7).
#' @param dpi Numeric; resolution of the saved plot in dots per inch (default 300).
#' @param col.opt Character; viridis color palette option (passed to
#'   \code{scale_color_viridis_d(option = col.opt)}).
#' @param new_model_names Optional character vector of custom model names.
#'   If supplied, its length must match the number of models; these names
#'   are then used in the \code{Model} factor.
#'
#' @return A \code{ggplot} object (invisibly saved to disk as a PNG). The file
#' is written to:
#' \preformatted{
#' file.path(main.dir, sub.dir, "Time_Series", paste0(var, ".PNG"))
#' }
#'
#' @details
#' The function first attempts to retrieve a precomputed summary via
#' \code{getOption("mse_summary")}. If this list exists and contains
#' \code{catch_ts}, that data frame is used directly. Otherwise, catch is
#' extracted from \code{mods[[...]]$om$rep$pred_catch}.
#'
#' @examples
#' \dontrun{
#' p <- plot_catch_time_series(mods, is.nsim = FALSE,
#'                             main.dir = "Results",
#'                             sub.dir  = "OM1")
#' print(p)
#' }
#'
#' @export
plot_catch_time_series <- function(mods, is.nsim, main.dir, sub.dir, var = "Catch",
                                   width = 10, height = 7, dpi = 300, col.opt = "D",
                                   new_model_names = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Try to use precomputed summary if available
  summ <- getOption("mse_summary", default = NULL)
  
  if (!is.null(summ) && !is.null(summ$catch_ts)) {
    res <- summ$catch_ts
  } else {
    
    # Fallback: compute from mods (keeps backwards compatibility)
    if (!is.nsim) {
      Years <- mods[[1]]$om$years
      
      res <- lapply(seq_along(mods), function(i) {
        data.frame(
          Catch       = mods[[i]]$om$rep$pred_catch,
          Model       = paste0("Model", i),
          Year        = Years,
          Realization = 1
        )
      }) %>% bind_rows()
      
    } else {
      Years <- mods[[1]][[1]]$om$years
      
      res <- lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          data.frame(
            Catch       = mods[[r]][[m]]$om$rep$pred_catch,
            Model       = paste0("Model", m),
            Year        = Years,
            Realization = r
          )
        }) %>% bind_rows()
      }) %>% bind_rows()
    }
  }
  
  # Rename models if needed
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(
      res$Model,
      levels = paste0("Model", seq_along(new_model_names)),
      labels = new_model_names
    )
  }
  
  # Long format (keeps future extensibility if you add more Catch_* columns)
  res_long <- tidyr::pivot_longer(
    res,
    cols      = dplyr::starts_with("Catch"),
    names_to  = "Label",
    values_to = "Catch"
  )
  
  p <- ggplot(res_long,
              aes(x = Year, y = Catch,
                  color = Model,
                  group = interaction(Model, Realization))) +
    geom_line(size = 0.3, alpha = 0.5) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(var) +
    ylab("Catch") +
    theme_bw()
  
  new_sub_dir <- file.path(main.dir, sub.dir, "Time_Series")
  if (!file.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
  
  ggsave(file.path(new_sub_dir, paste0(var, ".PNG")),
         plot = p, width = width, height = height, dpi = dpi)
  
  return(p)
}

#' Plot performance of SSB across models and levels
#'
#' @description
#' Create boxplot or median–IQR summaries of spawning stock biomass (SSB)
#' performance across estimation models, optionally comparing to a baseline
#' model and summarizing either the **last** or the **first** N years.
#' Supports plotting at the global or region level and returns a list of
#' ggplot objects (one per requested level).
#'
#' If a global option `mse_summary` is available (as created elsewhere in
#' your workflow), the function will use pre-computed SSB summaries from that
#' object. Otherwise, it will call `extract_mods_SSB()` on `mods` to derive
#' the SSB data.
#'
#' @param mods A list (or list of lists) of MSE model results.
#' @param is.nsim Logical; if `TRUE`, `mods[[realization]][[model]]` structure.
#' @param main.dir Character. Main directory for saving plots.
#' @param sub.dir Character. Subdirectory inside `main.dir`.
#' @param var Character. Name of SSB value column, default `"SSB"`.
#' @param level Character vector; one or both of `"global"`, `"region"`.
#' @param width,height Numeric. Plot size in inches.
#' @param dpi Numeric. Resolution for PNG.
#' @param col.opt Viridis option, e.g. `"D"`.
#' @param method `NULL`, `"mean"`, or `"median"`. If non-`NULL`, aggregate the
#'   selected years for each (Model, Realization, Label) by mean/median before plotting.
#' @param outlier.opt Outlier shape for `geom_boxplot()` (use `NA` to hide).
#' @param plot.style `"boxplot"` or `"median_iqr"`.
#' @param show.whisker Logical; if `TRUE`, draw whiskers in `"median_iqr"` style.
#' @param ssb.ymin,ssb.ymax Optional y-axis limits.
#' @param use.n.years Integer; number of years in the window (first or last).
#' @param new_model_names Optional character vector of replacement model names.
#' @param base.model Optional baseline model name for relative differences.
#' @param total Ignored for SSB (kept only so your plot_mse calls don't error).
#' @param period `"last"` or `"first"`; determines which part of the time series
#'   is used.
#' @param start.years Integer or `NULL`. Starting index for `"first"` period.
#'   If `NULL` and `period = "first"`, defaults to 1.
#'
#' @return A named list of ggplot objects, one per requested level.
plot_ssb_performance <- function(mods, is.nsim, main.dir, sub.dir,
                                 var = "SSB",
                                 level = c("global", "region"),
                                 width = 10, height = 7, dpi = 300, col.opt = "D",
                                 method = NULL,
                                 outlier.opt = NA,
                                 plot.style = "median_iqr",
                                 show.whisker = TRUE,
                                 ssb.ymin = NULL,
                                 ssb.ymax = NULL,
                                 use.n.years = NULL,
                                 new_model_names = NULL,
                                 base.model = NULL,
                                 total = NULL,  # accepted but ignored
                                 period = c("last", "first"),
                                 start.years = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  lvl_vec <- match.arg(level, c("global", "region"), several.ok = TRUE)
  period  <- match.arg(period)
  
  if (is.null(use.n.years)) {
    cat("\n[plot_ssb_performance] use.n.years is not specified, defaulting to 5 years.\n")
    use.n.years <- 5L
  }
  if (is.null(start.years)) start.years <- 1L
  
  plot_one_level <- function(lv) {
    
    use_n   <- use.n.years
    start_y <- start.years
    summ    <- getOption("mse_summary", default = NULL)
    res     <- NULL
    
    ## =========================================================
    ## 1) Try mse_summary (LAST always ok; FIRST only if start matches)
    ## =========================================================
    if (!is.null(summ) && period %in% c("last", "first")) {
      
      if (period == "first" &&
          !is.null(start_y) &&
          !is.null(summ$start.years) &&
          start_y != summ$start.years) {
        
        res_full <- NULL  # force fallback to mods
        
      } else {
        
        slot_name <- switch(
          lv,
          "global" = if (period == "last") "ssb_global_last" else "ssb_global_first",
          "region" = if (period == "last") "ssb_region_last" else "ssb_region_first"
        )
        
        res_full <- summ[[slot_name]]
      }
      
      if (!is.null(res_full)) {
        
        # If use_n not set, prefer summary defaults
        if (is.null(use_n)) {
          use_n <- if (period == "last") summ$use.n.years.last else summ$use.n.years.first
        }
        if (is.null(use_n)) use_n <- 5L
        
        # Trim if needed (safe even if already exact)
        res_full <- res_full %>%
          group_by(Model, Realization) %>%
          arrange(Year, .by_group = TRUE) %>%
          { if (period == "last") slice_tail(., n = use_n) else slice_head(., n = use_n) } %>%
          ungroup()
        
        res <- res_full
      }
    }
    
    ## =========================================================
    ## 2) Fallback: compute directly from mods
    ## =========================================================
    if (is.null(res)) {
      
      if (period == "last") {
        
        res <- extract_mods_SSB(
          mods        = mods,
          is.nsim     = is.nsim,
          level       = lv,
          scope       = "last",
          use.n.years = use_n,
          start.years = 1L
        )
        
      } else { # period == "first"
        
        if (start_y <= 1L) {
          
          res <- extract_mods_SSB(
            mods        = mods,
            is.nsim     = is.nsim,
            level       = lv,
            scope       = "first",
            use.n.years = use_n,
            start.years = 1L
          )
          
        } else {
          
          use_n_total <- (start_y - 1L) + use_n
          
          res_full <- extract_mods_SSB(
            mods        = mods,
            is.nsim     = is.nsim,
            level       = lv,
            scope       = "first",
            use.n.years = use_n_total,
            start.years = 1L
          )
          
          res <- res_full %>%
            group_by(Model, Realization) %>%
            arrange(Year, .by_group = TRUE) %>%
            mutate(row_id = dplyr::row_number()) %>%
            filter(row_id >= start_y,
                   row_id <= (start_y + use_n - 1L)) %>%
            ungroup() %>%
            select(-row_id)
        }
      }
    }
    
    ## =========================================================
    ## 3) Rename models if requested
    ## =========================================================
    if (!is.null(new_model_names)) {
      old_levels <- unique(as.character(res$Model))
      if (length(new_model_names) != length(old_levels)) {
        stop("Length of new_model_names must match the number of models.")
      }
      
      if (!is.null(base.model) && base.model %in% old_levels &&
          !(base.model %in% new_model_names)) {
        idx <- match(base.model, old_levels)
        base.model_local <- new_model_names[idx]
      } else {
        base.model_local <- base.model
      }
      
      res$Model <- factor(res$Model, levels = old_levels, labels = new_model_names)
      
      if (!is.null(base.model_local) && !(base.model_local %in% levels(res$Model))) {
        warning("base.model does not match any of the new_model_names.")
      }
    } else {
      base.model_local <- base.model
      res$Model <- factor(res$Model)
    }
    
    ## =========================================================
    ## 4) Long format: Label + var
    ## =========================================================
    res <- tidyr::pivot_longer(
      res,
      cols      = -c(Model, Year, Realization),
      names_to  = "Label",
      values_to = var
    )
    
    var_sym <- rlang::sym(var)
    
    ## =========================================================
    ## 5) Relative vs base.model (optional)
    ## =========================================================
    if (!is.null(base.model_local)) {
      base_df <- res %>%
        filter(Model == base.model_local) %>%
        rename(base_val = !!var_sym) %>%
        select(Realization, Year, Label, base_val)
      
      res <- res %>%
        left_join(base_df, by = c("Realization", "Year", "Label")) %>%
        mutate(!!var_sym := .data[[var]] / base_val - 1)
    }
    
    ## =========================================================
    ## 6) Aggregate across years if method != NULL
    ## =========================================================
    if (!is.null(method)) {
      res <- res %>%
        group_by(Model, Realization, Label) %>%
        summarise(
          !!var_sym := if (method == "mean") {
            mean(.data[[var]], na.rm = TRUE)
          } else {
            median(.data[[var]], na.rm = TRUE)
          },
          .groups = "drop"
        )
    }
    
    ## =========================================================
    ## 7) Build plot
    ## =========================================================
    title_base <- switch(
      lv,
      "global" = "Global SSB",
      "region" = "SSB by Region"
    )
    
    if (period == "last") {
      period_label <- paste0("Last ", use_n, " Years")
      file_suffix  <- paste0("_last_", use_n, "_years")
    } else {
      period_label <- paste0("Years ", start_y, "–", start_y + use_n - 1L)
      file_suffix  <- paste0("_first_", use_n, "_years_from_", start_y)
    }
    
    title_txt <- paste0(
      ifelse(is.null(base.model_local),
             title_base,
             paste0("Relative ", title_base, " vs ", base.model_local)),
      ": ", period_label
    )
    
    ylab_txt <- if (is.null(base.model_local)) var else "Relative Difference in SSB"
    
    # y-limits
    if (!is.null(base.model_local)) {
      y1 <- if (!is.null(ssb.ymin)) ssb.ymin else -1
      y2 <- if (!is.null(ssb.ymax)) ssb.ymax else  2
    } else {
      y1 <- if (!is.null(ssb.ymin)) ssb.ymin else NA_real_
      y2 <- if (!is.null(ssb.ymax)) ssb.ymax else NA_real_
    }
    
    if (plot.style == "boxplot") {
      
      p <- ggplot(res, aes(x = Model, y = !!var_sym, color = Model)) +
        geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(title_txt) +
        ylab(ylab_txt) +
        xlab("Model") +
        theme_bw()
      
      if (!is.na(y1) && !is.na(y2)) p <- p + coord_cartesian(ylim = c(y1, y2))
      
    } else if (plot.style == "median_iqr") {
      
      res_summary <- res %>%
        group_by(Model, Label) %>%
        summarise(
          q1  = quantile(.data[[var]], 0.25, na.rm = TRUE),
          med = median(.data[[var]], na.rm = TRUE),
          q3  = quantile(.data[[var]], 0.75, na.rm = TRUE),
          iqr = q3 - q1,
          .groups = "drop"
        ) %>%
        mutate(
          x    = as.numeric(factor(Model)),
          ymin = if (show.whisker) q1 - 1.5 * iqr else NA_real_,
          ymax = if (show.whisker) q3 + 1.5 * iqr else NA_real_
        )
      
      res_limits <- res %>%
        group_by(Model, Label) %>%
        summarise(
          min_val = min(.data[[var]], na.rm = TRUE),
          max_val = max(.data[[var]], na.rm = TRUE),
          .groups = "drop"
        )
      
      res_summary <- left_join(res_summary, res_limits, by = c("Model", "Label")) %>%
        mutate(
          ymin = pmax(ymin, min_val),
          ymax = pmin(ymax, max_val)
        )
      
      mod_levels <- levels(factor(res$Model))
      
      p <- ggplot(res_summary, aes(x = x, color = Model)) +
        { if (show.whisker) geom_segment(aes(x = x, xend = x, y = ymin, yend = q1)) } +
        { if (show.whisker) geom_segment(aes(x = x, xend = x, y = q3, yend = ymax)) } +
        geom_rect(aes(xmin = x - 0.3, xmax = x + 0.3, ymin = q1, ymax = q3, col = Model),
                  fill = NA, linewidth = 0.8) +
        geom_segment(aes(x = x - 0.3, xend = x + 0.3, y = med, yend = med, color = Model),
                     linewidth = 0.8) +
        scale_x_continuous(breaks = seq_along(mod_levels), labels = mod_levels) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(title_txt) +
        ylab(ylab_txt) +
        xlab("Model") +
        theme_bw()
      
      if (!is.na(y1) && !is.na(y2)) p <- p + coord_cartesian(ylim = c(y1, y2))
      
    } else {
      stop("Unknown plot.style. Choose 'boxplot' or 'median_iqr'.")
    }
    
    ## =========================================================
    ## 8) Save figure (add _perf; include start if FIRST)
    ## =========================================================
    plot_name <- paste0(
      var, "_", lv, "_perf",
      ifelse(is.null(base.model_local), "", "_Relative"),
      file_suffix, ".PNG"
    )
    
    new_sub_dir <- file.path(main.dir, sub.dir, "Performance_Boxplot")
    if (!file.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
    
    ggsave(
      filename = file.path(new_sub_dir, plot_name),
      plot     = p,
      width    = width,
      height   = height,
      dpi      = dpi
    )
    
    return(p)
  }
  
  plot_list <- lapply(lvl_vec, plot_one_level)
  names(plot_list) <- lvl_vec
  return(plot_list)
}


#' Plot performance of Fbar across models, periods, and aggregation levels
#'
#' @description
#' Create publication-ready boxplot or median–IQR summaries of fishing mortality
#' (Fbar) performance across estimation models, optionally comparing to a
#' baseline model and/or summarizing over a specified time window.
#'
#' The function supports plotting at the global, region, or fleet level and
#' returns a list of ggplot objects (one per requested level). You can choose
#' whether to use the **last** \code{use.n.years} of the series or a window
#' of \code{use.n.years} starting at \code{start.years} (the "first" period
#' option).
#'
#' If a global option \code{mse_summary} is available (as created elsewhere in
#' your workflow), the function will use pre-computed Fbar summaries from that
#' object for \code{period = "last"}. Otherwise, it will call
#' \code{extract_mods_Fbar()} on \code{mods} to derive the necessary Fbar data.
#'
#' IMPORTANT FIX (minimal):
#' \itemize{
#'   \item Saved filenames now include \code{"_perf"} so your report filters
#'         (grepl("perf", ...)) can detect these plots.
#' }
#'
#' @param mods A list (or list of lists) of MSE model results. This is the same
#'   \code{mods} object used throughout your MSE workflow. For
#'   \code{is.nsim = TRUE}, it is assumed that \code{mods[[realization]][[model]]}
#'   contains an \code{om} list with Fbar outputs.
#' @param is.nsim Logical. If \code{FALSE}, \code{mods} is a simple list of
#'   models (single simulation). If \code{TRUE}, \code{mods} is a list over
#'   realizations, each containing a list of models.
#' @param main.dir Character. Main directory for saving plots.
#' @param sub.dir Character. Subdirectory (inside \code{main.dir}) where plots
#'   will be saved. Plots are saved under
#'   \code{file.path(main.dir, sub.dir, "Performance_Boxplot")}.
#' @param var Character. Name of the response variable column to use from the
#'   Fbar extraction step. Defaults to \code{"Fbar"}.
#' @param level Character vector. One or more of \code{"global"},
#'   \code{"region"}, \code{"fleet"}.
#' @param width,height Numeric. Width and height (in inches) for the saved
#'   figure(s).
#' @param dpi Numeric. Resolution in dots per inch for saved figures.
#' @param col.opt Character. Viridis color option passed to
#'   \code{scale_color_viridis_d(option = col.opt)}.
#' @param method Character or \code{NULL}. If \code{NULL}, all years in the
#'   selected window are retained in the distribution. If \code{"mean"} or
#'   \code{"median"}, values are aggregated within each group before plotting.
#' @param outlier.opt Outlier plotting option passed to \code{geom_boxplot()}
#'   as \code{outlier.shape}. Use \code{NA} to hide outliers.
#' @param plot.style Character. Either \code{"boxplot"} or \code{"median_iqr"}.
#' @param show.whisker Logical. If \code{TRUE}, show whiskers for median–IQR style.
#' @param f.ymin,f.ymax Numeric or \code{NULL}. Optional y-axis limits.
#' @param use.n.years Integer or \code{NULL}. Number of years for the selected window.
#' @param new_model_names Optional character vector of new model names.
#' @param base.model Optional character. Baseline model for relative comparison.
#' @param period Character. Either \code{"last"} or \code{"first"}.
#' @param start.years Integer. Starting year index (1-based) for \code{period="first"}.
#'
#' @return A named list of ggplot objects (one per requested \code{level}).
#'
#' @export
plot_fbar_performance <- function(mods, is.nsim, main.dir, sub.dir,
                                  var = "Fbar",
                                  level = c("global", "region", "fleet"),
                                  width = 10, height = 7, dpi = 300, col.opt = "D",
                                  method = NULL,
                                  outlier.opt = NULL,
                                  plot.style = "median_iqr",
                                  show.whisker = TRUE,
                                  f.ymin = NULL,
                                  f.ymax = NULL,
                                  use.n.years = NULL,
                                  new_model_names = NULL,
                                  base.model = NULL,
                                  period = c("last", "first"),
                                  start.years = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  ## --------------------------------------------------
  ## 0) Handle vector level + period + defaults
  ## --------------------------------------------------
  lvl_vec <- match.arg(level, c("global", "region", "fleet"), several.ok = TRUE)
  period  <- match.arg(period)
  
  if (is.null(use.n.years)) {
    cat("\n[plot_fbar_performance] use.n.years is not specified, defaulting to 5 years.\n")
    use.n.years <- 5L
  }
  if (is.null(start.years)) start.years <- 1L
  
  plot_one_level <- function(lv) {
    
    use_n   <- use.n.years
    start_y <- start.years
    summ    <- getOption("mse_summary", default = NULL)
    res     <- NULL
    
    ## =========================================================
    ## 1) Try mse_summary (LAST always ok; FIRST only if start matches)
    ## =========================================================
    if (!is.null(summ) && period %in% c("last", "first")) {
      
      if (period == "first" &&
          !is.null(start_y) &&
          !is.null(summ$start.years) &&
          start_y != summ$start.years) {
        
        res_full <- NULL
        
      } else {
        
        slot_name <- switch(
          lv,
          "global" = if (period == "last") "fbar_global_last" else "fbar_global_first",
          "region" = if (period == "last") "fbar_region_last" else "fbar_region_first",
          "fleet"  = if (period == "last") "fbar_fleet_last"  else "fbar_fleet_first"
        )
        
        res_full <- summ[[slot_name]]
      }
      
      if (!is.null(res_full)) {
        
        if (is.null(use_n)) {
          use_n <- if (period == "last") summ$use.n.years.last else summ$use.n.years.first
        }
        if (is.null(use_n)) use_n <- 5L
        
        # trim to requested n (safe even if already correct)
        res_full <- res_full %>%
          group_by(Model, Realization) %>%
          arrange(Year, .by_group = TRUE) %>%
          { if (period == "last") slice_tail(., n = use_n) else slice_head(., n = use_n) } %>%
          ungroup()
        
        res <- res_full
      }
    }
    
    ## =========================================================
    ## 2) Fallback: compute directly from mods
    ## =========================================================
    if (is.null(res)) {
      
      if (period == "last") {
        
        res <- extract_mods_Fbar(
          mods        = mods,
          is.nsim     = is.nsim,
          level       = lv,
          scope       = "last",
          use.n.years = use_n
        )
        
      } else { # period == "first"
        
        if (start_y <= 1L) {
          
          res <- extract_mods_Fbar(
            mods        = mods,
            is.nsim     = is.nsim,
            level       = lv,
            scope       = "first",
            use.n.years = use_n,
            start.years = 1L
          )
          
        } else {
          
          use_n_total <- (start_y - 1L) + use_n
          
          res_full <- extract_mods_Fbar(
            mods        = mods,
            is.nsim     = is.nsim,
            level       = lv,
            scope       = "first",
            use.n.years = use_n_total,
            start.years = 1L
          )
          
          res <- res_full %>%
            group_by(Model, Realization) %>%
            arrange(Year, .by_group = TRUE) %>%
            mutate(row_id = dplyr::row_number()) %>%
            filter(row_id >= start_y,
                   row_id <= (start_y + use_n - 1L)) %>%
            ungroup() %>%
            select(-row_id)
        }
      }
    }
    
    ## --------------------------------------------------
    ## 3) Rename models if requested
    ## --------------------------------------------------
    if (!is.null(new_model_names)) {
      old_levels <- unique(as.character(res$Model))
      if (length(new_model_names) != length(old_levels)) {
        stop("Length of new_model_names must match the number of models.")
      }
      
      if (!is.null(base.model) && base.model %in% old_levels &&
          !(base.model %in% new_model_names)) {
        idx <- match(base.model, old_levels)
        base.model_local <- new_model_names[idx]
      } else {
        base.model_local <- base.model
      }
      
      res$Model <- factor(res$Model, levels = old_levels, labels = new_model_names)
      
      if (!is.null(base.model_local) && !(base.model_local %in% levels(res$Model))) {
        warning("base.model does not match any of the new_model_names.")
      }
    } else {
      base.model_local <- base.model
      res$Model <- factor(res$Model)
    }
    
    ## --------------------------------------------------
    ## 4) Pivot longer
    ## --------------------------------------------------
    res <- tidyr::pivot_longer(
      res,
      cols      = -c(Model, Year, Realization),
      names_to  = "Label",
      values_to = var
    )
    
    var_sym <- rlang::sym(var)
    
    ## --------------------------------------------------
    ## 5) Relative vs base.model
    ## --------------------------------------------------
    if (!is.null(base.model_local)) {
      base_df <- res %>%
        filter(Model == base.model_local) %>%
        rename(base_val = !!var_sym) %>%
        select(Realization, Year, Label, base_val)
      
      res <- res %>%
        left_join(base_df, by = c("Realization", "Year", "Label")) %>%
        mutate(!!var_sym := .data[[var]] / base_val - 1)
    }
    
    ## --------------------------------------------------
    ## 6) Aggregate over years if requested
    ## --------------------------------------------------
    if (!is.null(method)) {
      res <- res %>%
        group_by(Model, Realization, Label) %>%
        summarise(
          !!var_sym := if (method == "mean") {
            mean(.data[[var]], na.rm = TRUE)
          } else {
            median(.data[[var]], na.rm = TRUE)
          },
          .groups = "drop"
        )
    }
    
    ## --------------------------------------------------
    ## 7) Build plot
    ## --------------------------------------------------
    title_base <- switch(
      lv,
      "global" = "Global Fbar",
      "region" = "Fbar by Region",
      "fleet"  = "Fbar by Fleet"
    )
    
    period_label <- if (period == "last") {
      paste0("Last ", use_n, " Years")
    } else {
      paste0("Years ", start_y, "–", start_y + use_n - 1L)
    }
    
    title_txt <- paste0(
      ifelse(is.null(base.model_local),
             title_base,
             paste0("Relative ", title_base, " vs ", base.model_local)),
      ": ", period_label
    )
    
    ylab_txt <- if (is.null(base.model_local)) var else "Relative Difference in Fbar"
    
    if (!is.null(base.model_local)) {
      y1 <- if (!is.null(f.ymin)) f.ymin else -1
      y2 <- if (!is.null(f.ymax)) f.ymax else  2
    } else {
      y1 <- if (!is.null(f.ymin)) f.ymin else NA_real_
      y2 <- if (!is.null(f.ymax)) f.ymax else NA_real_
    }
    
    if (plot.style == "boxplot") {
      
      p <- ggplot(res, aes(x = Model, y = !!var_sym, color = Model)) +
        geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(title_txt) +
        ylab(ylab_txt) +
        xlab("Model") +
        theme_bw()
      
      if (!is.na(y1) && !is.na(y2)) p <- p + coord_cartesian(ylim = c(y1, y2))
      
    } else if (plot.style == "median_iqr") {
      
      res_summary <- res %>%
        group_by(Model, Label) %>%
        summarise(
          q1  = quantile(.data[[var]], 0.25, na.rm = TRUE),
          med = median(.data[[var]], na.rm = TRUE),
          q3  = quantile(.data[[var]], 0.75, na.rm = TRUE),
          iqr = q3 - q1,
          .groups = "drop"
        ) %>%
        mutate(
          x    = as.numeric(factor(Model)),
          ymin = if (show.whisker) q1 - 1.5 * iqr else NA_real_,
          ymax = if (show.whisker) q3 + 1.5 * iqr else NA_real_
        )
      
      res_limits <- res %>%
        group_by(Model, Label) %>%
        summarise(
          min_val = min(.data[[var]], na.rm = TRUE),
          max_val = max(.data[[var]], na.rm = TRUE),
          .groups = "drop"
        )
      
      res_summary <- left_join(res_summary, res_limits, by = c("Model", "Label")) %>%
        mutate(
          ymin = pmax(ymin, min_val),
          ymax = pmin(ymax, max_val)
        )
      
      mod_levels <- levels(factor(res$Model))
      
      p <- ggplot(res_summary, aes(x = x, color = Model)) +
        { if (show.whisker) geom_segment(aes(x = x, xend = x, y = ymin, yend = q1)) } +
        { if (show.whisker) geom_segment(aes(x = x, xend = x, y = q3, yend = ymax)) } +
        geom_rect(aes(xmin = x - 0.3, xmax = x + 0.3, ymin = q1, ymax = q3, col = Model),
                  fill = NA, linewidth = 0.8) +
        geom_segment(aes(x = x - 0.3, xend = x + 0.3, y = med, yend = med, color = Model),
                     linewidth = 0.8) +
        scale_x_continuous(breaks = seq_along(mod_levels), labels = mod_levels) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(title_txt) +
        ylab(ylab_txt) +
        xlab("Model") +
        theme_bw()
      
      if (!is.na(y1) && !is.na(y2)) p <- p + coord_cartesian(ylim = c(y1, y2))
      
    } else {
      stop("Unknown plot.style. Choose 'boxplot' or 'median_iqr'.")
    }
    
    ## --------------------------------------------------
    ## 8) Save figure
    ## --------------------------------------------------
    suffix <- if (period == "last") {
      paste0("last_", use_n, "_years")
    } else {
      paste0("first_", use_n, "_years_from_", start_y)
    }
    
    plot_name <- paste0(
      var, "_", lv, "_perf",
      ifelse(is.null(base.model_local), "", "_Relative"),
      "_", suffix, ".PNG"
    )
    
    new_sub_dir <- file.path(main.dir, sub.dir, "Performance_Boxplot")
    if (!file.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
    
    ggsave(
      filename = file.path(new_sub_dir, plot_name),
      plot     = p,
      width    = width,
      height   = height,
      dpi      = dpi
    )
    
    return(p)
  }
  
  plot_list <- lapply(lvl_vec, plot_one_level)
  names(plot_list) <- lvl_vec
  return(plot_list)
}


#' Plot performance of Catch across models and levels
#'
#' @description
#' Create publication-ready boxplot or median–IQR summaries of realized Catch
#' across estimation models, optionally comparing to a baseline model and
#' summarizing either the **first**, **last**, or **all** years in the time
#' series. Catch can be plotted at the global, region, or fleet level, and
#' either on a per-year basis or as a **total sum** over the chosen period.
#'
#' If a global option \code{mse_summary} is available (as created by
#' \code{build_mse_summary}), the function may use pre-computed Catch summaries
#' from that object for \code{period = "first"} or \code{period = "last"}.
#' For \code{period = "all"}, this function **always extracts from mods**
#' to guarantee all years are included (avoids the case where cached summaries
#' are already truncated).
#'
#' IMPORTANT FIX (minimal):
#' \itemize{
#'   \item Saved filenames include \code{"_perf"} so your report filters
#'         (grepl("perf", ...)) can detect these plots.
#'   \item \code{period="all"} now truly uses all years (bypasses \code{mse_summary}).
#' }
#'
#' @param mods A list (or list of lists) of MSE model results.
#' @param is.nsim Logical; whether \code{mods} is nested over realizations.
#' @param main.dir Character. Main directory for saving plots.
#' @param sub.dir Character. Subdirectory (inside \code{main.dir}) where plots
#'   will be saved under \code{Performance_Boxplot}.
#' @param var Character. Value column name (default \code{"Catch"}).
#' @param level Character vector: \code{"global"}, \code{"region"}, \code{"fleet"}.
#' @param width,height Numeric plot size (inches).
#' @param dpi Numeric resolution.
#' @param col.opt Viridis option.
#' @param method \code{NULL}, \code{"mean"}, or \code{"median"}.
#' @param outlier.opt Outlier shape for boxplot (use \code{NA} to hide).
#' @param plot.style \code{"boxplot"} or \code{"median_iqr"}.
#' @param show.whisker Logical; show whiskers for median–IQR style.
#' @param f.ymin,f.ymax Numeric or \code{NULL}; included for interface consistency.
#' @param use.n.years Integer or \code{NULL}; used for \code{first} / \code{last}.
#' @param new_model_names Optional model relabeling vector.
#' @param base.model Optional baseline model for relative plots.
#' @param total Logical; if \code{TRUE} sum over selected period.
#' @param period One of \code{"last"}, \code{"first"}, or \code{"all"}.
#' @param start.years Integer start year for \code{period="first"}.
#'
#' @return Named list of ggplot objects (one per requested level).
#'
#' @export
plot_catch_performance <- function(mods, is.nsim, main.dir, sub.dir,
                                   var = "Catch",
                                   level = c("global", "region", "fleet"),
                                   width = 10, height = 7, dpi = 300, col.opt = "D",
                                   method = NULL,
                                   outlier.opt = NA,
                                   plot.style = "median_iqr",
                                   show.whisker = TRUE,
                                   f.ymin = NULL,
                                   f.ymax = NULL,
                                   use.n.years = NULL,
                                   new_model_names = NULL,
                                   base.model = NULL,
                                   total = FALSE,
                                   period = c("last", "first", "all"),
                                   start.years = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  lvl_vec <- match.arg(level, c("global", "region", "fleet"), several.ok = TRUE)
  period  <- match.arg(period, c("last", "first", "all"))
  if (is.null(start.years)) start.years <- 1L
  
  plot_one_level <- function(lv) {
    
    use_n   <- use.n.years
    start_y <- start.years
    summ    <- getOption("mse_summary", default = NULL)
    res     <- NULL
    
    ## =========================================================
    ## 1) Use mse_summary only for FIRST/LAST (NOT for ALL)
    ##    and only if start.years matches the summary start
    ## =========================================================
    if (!is.null(summ) && period %in% c("last", "first")) {
      
      # If caller wants a different start than summary was built with,
      # skip mse_summary and fall back to extraction from mods.
      if (period == "first" &&
          !is.null(start_y) &&
          !is.null(summ$start.years) &&
          start_y != summ$start.years) {
        
        res_full <- NULL
        
      } else {
        
        slot_name <- switch(
          lv,
          "global" = if (period == "last") "catch_global_last" else "catch_global_first",
          "region" = if (period == "last") "catch_region_last" else "catch_region_first",
          "fleet"  = if (period == "last") "catch_fleet_last"  else "catch_fleet_first"
        )
        
        res_full <- summ[[slot_name]]
      }
      
      if (!is.null(res_full)) {
        
        if (is.null(use_n)) {
          use_n <- if (period == "last") summ$use.n.years.last else summ$use.n.years.first
        }
        if (is.null(use_n)) use_n <- 5L
        
        # Summary already represents the target window in most cases,
        # but trimming is harmless and ensures consistency.
        res_full <- res_full %>%
          group_by(Model, Realization) %>%
          arrange(Year, .by_group = TRUE) %>%
          { if (period == "last") slice_tail(., n = use_n) else slice_head(., n = use_n) } %>%
          ungroup()
        
        res <- res_full
      }
    }
    
    ## =========================================================
    ## 2) Fallback / ALWAYS for period == "all"
    ##    Also used when start.years doesn't match summary
    ## =========================================================
    if (is.null(res)) {
      
      if (period %in% c("last", "first") && is.null(use_n)) {
        cat("\n[plot_catch_performance] use.n.years is not specified, defaulting to 5 years.\n")
        use_n <- 5L
      }
      if (period == "first" && is.null(start_y)) start_y <- 1L
      
      if (period == "all") {
        
        res <- extract_mods_catch(
          mods    = mods,
          is.nsim = is.nsim,
          level   = lv,
          scope   = "all"
        )
        
      } else if (period == "last") {
        
        res <- extract_mods_catch(
          mods        = mods,
          is.nsim     = is.nsim,
          level       = lv,
          scope       = "last",
          use.n.years = use_n
        )
        
      } else { # period == "first"
        
        # We want rows start_y:(start_y+use_n-1) within each Model/Realization.
        # Easiest: extract up to that point, then slice the desired window.
        use_n_total <- (start_y - 1L) + use_n
        
        res_full <- extract_mods_catch(
          mods        = mods,
          is.nsim     = is.nsim,
          level       = lv,
          scope       = "first",
          use.n.years = use_n_total,
          start.years = 1L
        )
        
        res <- res_full %>%
          group_by(Model, Realization) %>%
          arrange(Year, .by_group = TRUE) %>%
          mutate(row_id = dplyr::row_number()) %>%
          filter(row_id >= start_y,
                 row_id <= (start_y + use_n - 1L)) %>%
          ungroup() %>%
          select(-row_id)
      }
    }
    
    ## -------------------------------------------
    ## 3) Rename models if requested
    ## -------------------------------------------
    if (!is.null(new_model_names)) {
      old_levels <- unique(as.character(res$Model))
      if (length(new_model_names) != length(old_levels)) {
        stop("Length of new_model_names must match the number of models.")
      }
      
      if (!is.null(base.model) && base.model %in% old_levels &&
          !(base.model %in% new_model_names)) {
        idx <- match(base.model, old_levels)
        base.model_local <- new_model_names[idx]
      } else {
        base.model_local <- base.model
      }
      
      res$Model <- factor(res$Model, levels = old_levels, labels = new_model_names)
      
      if (!is.null(base.model_local) && !(base.model_local %in% levels(res$Model))) {
        warning("base.model does not match any of the new_model_names.")
      }
    } else {
      base.model_local <- base.model
      res$Model <- factor(res$Model)
    }
    
    ## -------------------------------------------
    ## 4) Pivot longer -> Label + var
    ## -------------------------------------------
    res <- tidyr::pivot_longer(
      res,
      cols      = -c(Model, Year, Realization),
      names_to  = "Label",
      values_to = var
    )
    
    var_sym <- rlang::sym(var)
    
    ## -------------------------------------------
    ## 5) Total over years (optional)
    ## -------------------------------------------
    if (isTRUE(total)) {
      res <- res %>%
        group_by(Model, Realization, Label) %>%
        summarise(!!var_sym := sum(.data[[var]], na.rm = TRUE), .groups = "drop")
    }
    
    ## -------------------------------------------
    ## 6) Relative vs base.model (optional)
    ## -------------------------------------------
    if (!is.null(base.model_local)) {
      
      base_df <- res %>%
        filter(Model == base.model_local) %>%
        rename(base_val = !!var_sym)
      
      if (!isTRUE(total)) {
        base_df <- base_df %>% select(Realization, Year, Label, base_val)
        res <- res %>%
          left_join(base_df, by = c("Realization", "Year", "Label")) %>%
          mutate(!!var_sym := .data[[var]] / base_val - 1)
      } else {
        base_df <- base_df %>% select(Realization, Label, base_val)
        res <- res %>%
          left_join(base_df, by = c("Realization", "Label")) %>%
          mutate(!!var_sym := .data[[var]] / base_val - 1)
      }
    }
    
    ## -------------------------------------------
    ## 7) Aggregate if requested
    ## -------------------------------------------
    if (!is.null(method)) {
      res <- res %>%
        group_by(Model, Realization, Label) %>%
        summarise(
          !!var_sym := if (method == "mean") {
            mean(.data[[var]], na.rm = TRUE)
          } else {
            median(.data[[var]], na.rm = TRUE)
          },
          .groups = "drop"
        )
    }
    
    ## -------------------------------------------
    ## 8) Titles + suffix
    ## -------------------------------------------
    title_base <- switch(
      lv,
      "global" = "Global Catch",
      "region" = "Catch by Region",
      "fleet"  = "Catch by Fleet"
    )
    if (isTRUE(total)) title_base <- paste("Total", title_base)
    
    if (period == "last") {
      if (is.null(use_n)) use_n <- 5L
      period_label <- paste0("Last ", use_n, " Years")
      file_suffix  <- paste0("_last_", use_n, "_years")
    } else if (period == "first") {
      if (is.null(use_n)) use_n <- 5L
      if (is.null(start_y)) start_y <- 1L
      period_label <- paste0("Years ", start_y, "–", start_y + use_n - 1L)
      file_suffix  <- paste0("_first_", use_n, "_years")
    } else {
      period_label <- "All Years"
      file_suffix  <- "_all_years"
    }
    
    title_txt <- paste0(
      ifelse(is.null(base.model_local),
             title_base,
             paste0("Relative ", title_base, " vs ", base.model_local)),
      ": ", period_label
    )
    
    ylab_txt <- if (is.null(base.model_local)) {
      if (isTRUE(total)) paste0("Total ", var) else var
    } else {
      if (isTRUE(total)) paste0("Relative Difference in Total ", var)
      else paste0("Relative Difference in ", var)
    }
    
    if (!is.null(base.model_local)) {
      y1 <- if (!is.null(f.ymin)) f.ymin else -1
      y2 <- if (!is.null(f.ymax)) f.ymax else  2
    } else {
      y1 <- if (!is.null(f.ymin)) f.ymin else NA_real_
      y2 <- if (!is.null(f.ymax)) f.ymax else NA_real_
    }
    
    ## -------------------------------------------
    ## 9) Plot
    ## -------------------------------------------
    if (plot.style == "boxplot") {
      
      p <- ggplot(res, aes(x = Model, y = !!var_sym, color = Model)) +
        geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(title_txt) +
        ylab(ylab_txt) +
        xlab("Model") +
        theme_bw()
      
      if (!is.na(y1) && !is.na(y2)) p <- p + coord_cartesian(ylim = c(y1, y2))
      
    } else if (plot.style == "median_iqr") {
      
      res_summary <- res %>%
        group_by(Model, Label) %>%
        summarise(
          q1  = quantile(.data[[var]], 0.25, na.rm = TRUE),
          med = median(.data[[var]], na.rm = TRUE),
          q3  = quantile(.data[[var]], 0.75, na.rm = TRUE),
          iqr = q3 - q1,
          .groups = "drop"
        ) %>%
        mutate(
          x    = as.numeric(factor(Model)),
          ymin = if (show.whisker) q1 - 1.5 * iqr else NA_real_,
          ymax = if (show.whisker) q3 + 1.5 * iqr else NA_real_
        )
      
      res_limits <- res %>%
        group_by(Model, Label) %>%
        summarise(
          min_val = min(.data[[var]], na.rm = TRUE),
          max_val = max(.data[[var]], na.rm = TRUE),
          .groups = "drop"
        )
      
      res_summary <- left_join(res_summary, res_limits, by = c("Model", "Label")) %>%
        mutate(
          ymin = pmax(ymin, min_val),
          ymax = pmin(ymax, max_val)
        )
      
      mod_levels <- levels(factor(res$Model))
      
      p <- ggplot(res_summary, aes(x = x, color = Model)) +
        { if (show.whisker) geom_segment(aes(x = x, xend = x, y = ymin, yend = q1)) } +
        { if (show.whisker) geom_segment(aes(x = x, xend = x, y = q3, yend = ymax)) } +
        geom_rect(aes(xmin = x - 0.3, xmax = x + 0.3, ymin = q1, ymax = q3, col = Model),
                  fill = NA, linewidth = 0.8) +
        geom_segment(aes(x = x - 0.3, xend = x + 0.3, y = med, yend = med, color = Model),
                     linewidth = 0.8) +
        scale_x_continuous(breaks = seq_along(mod_levels), labels = mod_levels) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(title_txt) +
        ylab(ylab_txt) +
        xlab("Model") +
        theme_bw()
      
      if (!is.na(y1) && !is.na(y2)) p <- p + coord_cartesian(ylim = c(y1, y2))
      
    } else {
      stop("Unknown plot.style. Choose 'boxplot' or 'median_iqr'.")
    }
    
    ## -------------------------------------------
    ## 10) Save
    ## -------------------------------------------
    prefix <- if (isTRUE(total)) paste0("Total_", var) else var
    
    plot_name <- paste0(
      prefix, "_", lv, "_perf",
      ifelse(is.null(base.model_local), "", "_Relative"),
      file_suffix, ".PNG"
    )
    
    new_sub_dir <- file.path(main.dir, sub.dir, "Performance_Boxplot")
    if (!file.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
    
    ggsave(
      filename = file.path(new_sub_dir, plot_name),
      plot     = p,
      width    = width,
      height   = height,
      dpi      = dpi
    )
    
    return(p)
  }
  
  plot_list <- lapply(lvl_vec, plot_one_level)
  names(plot_list) <- lvl_vec
  return(plot_list)
}



#' Plot variation of Catch, SSB, or Fbar across models
#'
#' @description
#' Unified plotting function for variation (across years and/or realizations)
#' of Catch, SSB, or Fbar. It can:
#' \itemize{
#'   \item use the \strong{first} N years of the feedback period,
#'   \item use the \strong{last} N years, or
#'   \item use \strong{all years from \code{start.years} to the end}.
#' }
#'
#' It supports:
#' \itemize{
#'   \item \code{quantity = "Catch"} with \code{extract_mods_catch()},
#'   \item \code{quantity = "SSB"}   with \code{extract_mods_scalar()},
#'   \item \code{quantity = "Fbar"}  with \code{extract_mods_Fbar()}.
#' }
#'
#' @param mods List (or list of lists) of MSE model results.
#' @param is.nsim Logical; TRUE if \code{mods[[r]][[m]]} structure.
#' @param main.dir,sub.dir Directories for saving plots.
#' @param quantity Character: one of \code{"Catch"}, \code{"SSB"}, \code{"Fbar"}.
#' @param level Character vector of levels:
#'   \itemize{
#'     \item For \code{"Catch"}: \code{"global"}, \code{"region"}, \code{"fleet"}.
#'     \item For \code{"SSB"}:   \code{"global"}, \code{"region"} (ignored if scalar).
#'     \item For \code{"Fbar"}:  \code{"global"}, \code{"region"}, \code{"fleet"}.
#'   }
#' @param width,height,dpi Plot size and resolution.
#' @param col.opt Viridis color option.
#' @param method NULL, "mean", or "median" (aggregates within each
#'   Model–Realization–Label group).
#' @param outlier.opt Outlier shape for boxplots (NA to hide).
#' @param plot.style "boxplot" or "median_iqr".
#' @param show.whisker Logical; whiskers for median_iqr.
#' @param use.n.years Integer or NULL. For period = "first"/"last": number
#'   of years in the window. For period = "all" this is ignored.
#' @param new_model_names Optional vector to relabel models.
#' @param base.model Optional baseline model name for relative plots.
#' @param period One of "all", "first", "last". For:
#'   \itemize{
#'     \item \code{"first"}: use \code{start.years} .. \code{start.years+use.n.years-1}.
#'     \item \code{"last"}: last \code{use.n.years} years.
#'     \item \code{"all"}: \strong{from \code{start.years} to the end}.
#'   }
#' @param start.years Integer; starting index (in the feedback period) for
#'   "first" and "all". Defaults to 1.
#'
#' @return Named list of ggplot objects, one per requested level.
plot_variation_performance <- function(mods, is.nsim, main.dir, sub.dir,
                                       quantity = c("Catch", "SSB", "Fbar"),
                                       level    = c("global", "region", "fleet"),
                                       width = 10, height = 7, dpi = 300,
                                       col.opt = "D",
                                       method = NULL,
                                       outlier.opt = NA,
                                       plot.style = "median_iqr",
                                       show.whisker = TRUE,
                                       use.n.years = NULL,
                                       new_model_names = NULL,
                                       base.model = NULL,
                                       period = c("all", "first", "last"),
                                       start.years = 1) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  quantity <- match.arg(quantity, c("Catch", "SSB", "Fbar"))
  period   <- match.arg(period, c("all", "first", "last"))
  
  # allowed levels depend on quantity
  if (quantity == "SSB") {
    lvl_vec <- match.arg(level, c("global", "region"), several.ok = TRUE)
  } else {
    lvl_vec <- match.arg(level, c("global", "region", "fleet"), several.ok = TRUE)
  }
  
  # choose extractor + var name based on quantity
  get_data_for_level <- function(lv) {
    if (quantity == "Catch") {
      scope <- if (period == "all") "all" else period
      
      df <- extract_mods_catch(
        mods        = mods,
        is.nsim     = is.nsim,
        level       = lv,
        scope       = scope,
        use.n.years = if (period == "all") NULL else use.n.years,
        start.years = start.years
      )
      var_name <- "Catch"
      
    } else if (quantity == "SSB") {
      scope <- if (period == "all") "all" else period
      
      df <- extract_mods_scalar(
        mods        = mods,
        is.nsim     = is.nsim,
        slot        = "SSB",
        var_name    = "SSB",
        scope       = scope,
        use.n.years = if (period == "all") NULL else use.n.years,
        start.years = start.years,
        add_global  = (lv == "global")
      )
      
      var_name <- "SSB"
      
      if (lv == "global") {
        global_cols <- grep("^SSB_Global$", names(df), value = TRUE)
        df <- df[, c(global_cols, "Model", "Year", "Realization")]
        names(df)[1] <- "SSB_Global"
      }
      
    } else { # quantity == "Fbar"
      scope <- if (period == "all") "all" else period
      
      df <- extract_mods_Fbar(
        mods        = mods,
        is.nsim     = is.nsim,
        level       = lv,
        scope       = scope,
        use.n.years = if (period == "all") NULL else use.n.years,
        start.years = start.years
      )
      var_name <- "Fbar"
    }
    
    # If period == "all", slice from start.years to end explicitly
    if (period == "all" && start.years > 1L) {
      df <- df %>%
        group_by(Model, Realization) %>%
        arrange(Year, .by_group = TRUE) %>%
        mutate(row_id = dplyr::row_number()) %>%
        filter(row_id >= start.years) %>%
        select(-row_id) %>%
        ungroup()
    }
    
    list(df = df, var_name = var_name)
  }
  
  # internal plotting for one level
  plot_one_level <- function(lv) {
    dat_info <- get_data_for_level(lv)
    res      <- dat_info$df
    var      <- dat_info$var_name
    var_sym  <- sym(var)
    
    ## 1) Optional model renaming
    if (!is.null(new_model_names)) {
      old_levels <- unique(as.character(res$Model))
      if (length(new_model_names) != length(old_levels)) {
        stop("Length of new_model_names must match the number of models.")
      }
      
      if (!is.null(base.model) && base.model %in% old_levels &&
          !(base.model %in% new_model_names)) {
        idx <- match(base.model, old_levels)
        base.model_local <- new_model_names[idx]
      } else {
        base.model_local <- base.model
      }
      
      res$Model <- factor(res$Model,
                          levels = old_levels,
                          labels = new_model_names)
      
      if (!is.null(base.model_local) && !(base.model_local %in% levels(res$Model))) {
        warning("base.model does not match any of the new_model_names.")
      }
    } else {
      base.model_local <- base.model
      res$Model <- factor(res$Model)
    }
    
    ## 2) Pivot longer → Label + value
    res <- tidyr::pivot_longer(
      res,
      cols      = -c(Model, Year, Realization),
      names_to  = "Label",
      values_to = var
    )
    
    ## 3) Relative vs base.model (per-year values)
    if (!is.null(base.model_local)) {
      base_df <- res %>%
        filter(Model == base.model_local) %>%
        rename(base_val = !!var_sym) %>%
        select(Realization, Year, Label, base_val)
      
      res <- res %>%
        left_join(base_df, by = c("Realization", "Year", "Label")) %>%
        mutate(!!var_sym := .data[[var]] / base_val - 1)
    }
    
    ## 4) Optional aggregation across years/realizations
    if (!is.null(method)) {
      res <- res %>%
        group_by(Model, Realization, Label) %>%
        summarise(
          !!var_sym := if (method == "mean") {
            mean(.data[[var]], na.rm = TRUE)
          } else {
            median(.data[[var]], na.rm = TRUE)
          },
          .groups = "drop"
        )
    }
    
    ## 5) Titles: just "Average annual xx variation"
    title_base <- switch(
      quantity,
      "Catch" = switch(lv,
                       "global" = "Global catch",
                       "region" = "catch by region",
                       "fleet"  = "catch by fleet"),
      "SSB"   = switch(lv,
                       "global" = "Global SSB",
                       "region" = "SSB by region"),
      "Fbar"  = switch(lv,
                       "global" = "Global Fbar",
                       "region" = "Fbar by region",
                       "fleet"  = "Fbar by fleet")
    )
    
    if (is.null(base.model_local)) {
      title_txt <- paste0("Average annual ", title_base, " variation")
      ylab_txt  <- quantity
    } else {
      title_txt <- paste0(
        "Average annual relative ", title_base,
        " variation (vs ", base.model_local, ")"
      )
      ylab_txt  <- paste0("Relative ", quantity, " difference")
    }
    
    ## 6) Build plot
    if (plot.style == "boxplot") {
      
      p <- ggplot(res, aes(x = Model, y = !!var_sym, color = Model)) +
        geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(title_txt) +
        ylab(ylab_txt) +
        xlab("Model") +
        theme_bw()
      
    } else if (plot.style == "median_iqr") {
      
      res_summary <- res %>%
        group_by(Model, Label) %>%
        summarise(
          q1  = quantile(.data[[var]], 0.25, na.rm = TRUE),
          med = median(.data[[var]], na.rm = TRUE),
          q3  = quantile(.data[[var]], 0.75, na.rm = TRUE),
          iqr = q3 - q1,
          .groups = "drop"
        ) %>%
        mutate(
          x    = as.numeric(factor(Model)),
          ymin = if (show.whisker) q1 - 1.5 * iqr else NA_real_,
          ymax = if (show.whisker) q3 + 1.5 * iqr else NA_real_
        )
      
      res_limits <- res %>%
        group_by(Model, Label) %>%
        summarise(
          min_val = min(.data[[var]], na.rm = TRUE),
          max_val = max(.data[[var]], na.rm = TRUE),
          .groups = "drop"
        )
      
      res_summary <- left_join(res_summary, res_limits,
                               by = c("Model", "Label")) %>%
        mutate(
          ymin = pmax(ymin, min_val),
          ymax = pmin(ymax, max_val)
        )
      
      p <- ggplot(res_summary, aes(x = x, color = Model)) +
        { if (show.whisker)
          geom_segment(aes(x = x, xend = x, y = ymin, yend = q1))
        } +
        { if (show.whisker)
          geom_segment(aes(x = x, xend = x, y = q3, yend = ymax))
        } +
        geom_rect(aes(xmin = x - 0.3, xmax = x + 0.3,
                      ymin = q1, ymax = q3, col = Model),
                  fill = NA, linewidth = 0.8) +
        geom_segment(aes(x = x - 0.3, xend = x + 0.3,
                         y = med, yend = med, color = Model),
                     linewidth = 0.8) +
        scale_x_continuous(breaks = res_summary$x,
                           labels = res_summary$Model) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(title_txt) +
        ylab(ylab_txt) +
        xlab("Model") +
        theme_bw()
      
    } else {
      stop("Unknown plot.style. Choose 'boxplot' or 'median_iqr'.")
    }
    
    ## 7) Save plot
    file_mid <- paste0("_", quantity, "_", lv,
                       ifelse(is.null(base.model_local), "", "_Relative"))
    
    file_tail <- switch(
      period,
      "all"   = paste0("_all_from_", start.years, ".PNG"),
      "first" = paste0("_first_", use.n.years, "_from_", start.years, ".PNG"),
      "last"  = paste0("_last_", use.n.years, "_years.PNG")
    )
    
    plot_name  <- paste0("Var", file_mid, file_tail)
    new_subdir <- file.path(main.dir, sub.dir, "Variation_Boxplot")
    if (!file.exists(new_subdir)) dir.create(new_subdir, recursive = TRUE)
    
    ggsave(
      filename = file.path(new_subdir, plot_name),
      plot     = p,
      width    = width,
      height   = height,
      dpi      = dpi
    )
    
    p
  }
  
  plot_list <- lapply(lvl_vec, plot_one_level)
  names(plot_list) <- lvl_vec
  plot_list
}


#' Plot SSB or Fbar status relative to reference points
#'
#' This function produces status plots for either spawning stock biomass
#' (SSB/SSB\eqn{_FXSPR}) or fishing mortality (F/F\eqn{_FXSPR}), over either
#' the last \code{use.n.years} of the time series or the first
#' \code{use.n.years} starting at \code{start.years}. It replaces the
#' previous \code{plot_ssb_status}, \code{plot_ssb_status2},
#' \code{plot_fbar_status}, and \code{plot_fbar_status2} functions by
#' controlling behavior through \code{metric} and \code{period}.
#'
#' For \code{metric = "SSB"}, the function:
#' \itemize{
#'   \item Computes SSB by region and global (row-summed across regions).
#'   \item Normalizes by \code{SSB_FXSPR} to obtain SSB/SSB\eqn{_FXSPR}.
#'   \item Extracts either the last \code{use.n.years} or the first
#'         \code{use.n.years} years depending on \code{period}.
#'   \item Optionally computes relative differences vs. \code{base.model}.
#'   \item Produces a boxplot or median–IQR plot across models, with optional
#'         summarization across years by mean/median.
#'   \item Optionally produces a probability plot of
#'         \code{Pr(SSB/SSB_FXSPR < 0.5)} by model.
#' }
#'
#' For \code{metric = "Fbar"}, the function:
#' \itemize{
#'   \item Computes F/F\eqn{_FXSPR} by fleet, region, and global.
#'   \item Extracts either the last \code{use.n.years} or the first
#'         \code{use.n.years} years depending on \code{period}.
#'   \item Optionally computes relative differences vs. \code{base.model}.
#'   \item Produces boxplots or median–IQR plots across models for
#'         fleets, regions, and global.
#'   \item Optionally produces probability plots of \code{Pr(F/F_FXSPR > 1)}
#'         by fleet, region, and global.
#' }
#'
#' All plots are saved into \code{file.path(main.dir, sub.dir, "Status_Boxplot")}
#' with file names indicating the metric (SSB or Fbar), scale (fleet/region/global),
#' whether the plot is relative, and whether it covers the first or last years.
#'
#' @param mods List of model objects. If \code{is.nsim = FALSE}, this is a list
#'   of EM/OM fits for a single simulation. If \code{is.nsim = TRUE}, this is
#'   a list over realizations, where each element is itself a list of models.
#' @param is.nsim Logical; \code{TRUE} if \code{mods} is a list over simulations
#'   (i.e., \code{mods[[sim]][[model]]}), \code{FALSE} if \code{mods[[model]]}.
#' @param main.dir Character; main output directory.
#' @param sub.dir Character; subdirectory within \code{main.dir} where the
#'   \code{"Status_Boxplot"} folder will be created.
#' @param metric Character; which status to plot:
#'   \code{"SSB"} for SSB/SSB\eqn{_FXSPR} or
#'   \code{"Fbar"} for F/F\eqn{_FXSPR}.
#' @param period Character; time window to summarize:
#'   \code{"last"} for the last \code{use.n.years} of the time series or
#'   \code{"first"} for \code{use.n.years} starting at \code{start.years}.
#' @param width,height Numeric; width and height of the saved plots in inches.
#' @param dpi Numeric; resolution of the saved plots in dots per inch.
#' @param col.opt Character; color palette option passed to
#'   \code{ggplot2::scale_color_viridis_d()} (e.g., \code{"D"}).
#' @param method Optional character; if not \code{NULL}, summarises the
#'   time dimension within each realization using either \code{"mean"} or
#'   \code{"median"} before plotting.
#' @param outlier.opt Passed to \code{ggplot2::geom_boxplot(outlier.shape = ...)}.
#'   Use \code{NA} to suppress plotting of outliers.
#' @param plot.style Character; either \code{"boxplot"} for standard boxplots
#'   or \code{"median_iqr"} for custom median–IQR plots with optional whiskers.
#' @param show.whisker Logical; if \code{TRUE} and \code{plot.style = "median_iqr"},
#'   whiskers at 1.5 × IQR are shown (clipped to observed min/max).
#' @param use.n.years Integer; number of years to use in the time window
#'   (either last or first). If \code{NULL}, defaults to 5.
#' @param start.years Integer; starting year index (row index in the time
#'   dimension) when \code{period = "first"}. If \code{NULL}, defaults to 1.
#'   Ignored when \code{period = "last"}.
#' @param f.ymin,f.ymax Optional numeric; lower and upper y-axis limits for
#'   F/F\eqn{_FXSPR} plots. If \code{NULL}, defaults depend on whether
#'   \code{base.model} is \code{NULL} (absolute scale) or not (relative scale).
#' @param new_model_names Optional character vector of model names to replace
#'   the default \code{"Model1"}, \code{"Model2"}, ... levels. Length must
#'   match the number of models in \code{mods}.
#' @param base.model Optional; if \code{NULL}, plots absolute status
#'   (SSB/SSB\eqn{_FXSPR} or F/F\eqn{_FXSPR}). If non-\code{NULL}, it should
#'   match one of the \code{new_model_names} (if provided), and the function
#'   plots relative differences \code{(value / base - 1)}.
#' @param plot_prob Logical; if \code{TRUE}, probability plots
#'   (\code{Pr(SSB/SSB_FXSPR < 0.5)} or \code{Pr(F/F_FXSPR > 1)}) are also
#'   generated and saved. If \code{FALSE}, only the status boxplots are produced.
#'
#' @details
#' The function expects the WHAM/whamMSE objects in \code{mods} to contain:
#' \itemize{
#'   \item \code{om$rep$SSB}, \code{om$rep$log_SSB_FXSPR} and
#'         \code{om$input$data$percentSPR} for \code{metric = "SSB"}.
#'   \item \code{om$rep$Fbar}, \code{om$rep$log_Fbar_XSPR},
#'         \code{om$input$data$n_fleets}, and
#'         \code{om$input$data$n_regions} for \code{metric = "Fbar"}.
#' }
#' It also assumes \code{om$years} gives the vector of years.
#'
#' When \code{is.nsim = TRUE}, the function treats \code{mods} as a list over
#' realizations, aggregating across both year and realization dimensions as
#' specified by \code{method} and \code{plot.style}.
#'
#' @return
#' For \code{metric = "SSB"}, a list with:
#' \item{boxplot}{A \code{ggplot} object showing SSB/SSB\eqn{_FXSPR} (or relative
#'                differences) by model.}
#' \item{probplot}{A \code{ggplot} object showing probabilities of
#'                 \code{SSB/SSB_FXSPR < 0.5} by model, or \code{NULL} if
#'                 \code{plot_prob = FALSE}.}
#'
#' For \code{metric = "Fbar"}, a list with:
#' \item{fleet_box}{\code{ggplot} of F/F\eqn{_FXSPR} (or relative differences)
#'                  by fleet and model.}
#' \item{region_box}{\code{ggplot} of F/F\eqn{_FXSPR} by region and model.}
#' \item{global_box}{\code{ggplot} of global F/F\eqn{_FXSPR} by model.}
#' \item{fleet_prob}{\code{ggplot} of \code{Pr(F/F_FXSPR > 1)} by fleet and model,
#'                   or \code{NULL} if \code{plot_prob = FALSE}.}
#' \item{region_prob}{As above, for regions.}
#' \item{global_prob}{As above, for the global F/F\eqn{_FXSPR}.}
#'
#' Invisibly, the function saves all plots as PNG files in the
#' \code{"Status_Boxplot"} subdirectory.
#'
#' @seealso
#' Functions that call \code{plot_status_all} from within your MSE plotting
#' workflow (e.g., \code{plot_mse}, \code{plot_mse_output}).
#'
#' @export
plot_status_all <- function(mods, is.nsim,
                            main.dir, sub.dir,
                            metric = c("SSB", "Fbar"),
                            period = c("last", "first"),
                            width = 10, height = 7, dpi = 300, col.opt = "D",
                            method = NULL,
                            outlier.opt = NA,
                            plot.style = "median_iqr",
                            show.whisker = TRUE,
                            use.n.years = NULL,
                            start.years = NULL,
                            f.ymin = NULL,
                            f.ymax = NULL,
                            new_model_names = NULL,
                            base.model = NULL,
                            plot_prob = TRUE) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  metric <- match.arg(metric, c("SSB", "Fbar"))
  period <- match.arg(period, c("last", "first"))
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  if (period == "first" && is.null(start.years)) {
    cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
    start.years <- 1
  }
  
  ## ------------------------------------------------
  ## BRP existence / sanity check (metric-specific)
  ## ------------------------------------------------
  get_first_obj <- if (!is.nsim) mods[[1]] else mods[[1]][[1]]
  
  if (metric == "SSB") {
    brp_ssb <- get_first_obj$om$rep$log_SSB_FXSPR
    
    if (is.null(brp_ssb) || all(is.na(brp_ssb)) || all(!is.finite(brp_ssb))) {
      message("log_SSB_FXSPR is NULL or contains only NA/NaN/Inf. Skipping SSB status plots.")
      return(invisible(NULL))
    }
    
  } else if (metric == "Fbar") {
    brp_fbar <- get_first_obj$om$rep$log_Fbar_XSPR
    
    if (is.null(brp_fbar) || all(is.na(brp_fbar)) || all(!is.finite(brp_fbar))) {
      message("log_Fbar_XSPR is NULL or contains only NA/NaN/Inf. Skipping Fbar status plots.")
      return(invisible(NULL))
    }
  }
  
  ## -----------------------------
  ## Helper for model renaming
  ## -----------------------------
  rename_models <- function(df) {
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(df$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      df$Model <- factor(df$Model,
                         levels = paste0("Model", seq_along(new_model_names)),
                         labels = new_model_names)
      if (!is.null(base.model)) {
        if (!(base.model %in% new_model_names)) {
          warning("base.model does not match any of the new_model_names.")
        }
      }
    }
    df
  }
  
  ## =======================================================================
  ## 1. SSB PATH  (equivalent to plot_ssb_status + plot_ssb_status2)
  ## =======================================================================
  if (metric == "SSB") {
    
    ## -----------------------------
    ## Build res and prob
    ## -----------------------------
    if (!is.nsim) {
      Years      <- mods[[1]]$om$years
      percentSPR <- mods[[1]]$om$input$data$percentSPR
      
      res_list <- lapply(seq_along(mods), function(i) {
        tmp <- mods[[i]]$om$rep$SSB
        tmp <- cbind(tmp, rowSums(tmp))
        tmp <- tmp / exp(mods[[i]]$om$rep$log_SSB_FXSPR)
        tmp <- as.data.frame(tmp)
        name_tmp <- paste0("SSB/SSB", percentSPR, "%")
        names(tmp) <- paste0(name_tmp, ".s", 1:ncol(tmp))
        names(tmp)[ncol(tmp)] <- name_tmp
        tmp$Model <- paste0("Model", i)
        tmp$Year  <- Years
        tmp$Realization <- 1
        
        if (period == "last") {
          tmp <- tail(tmp, use.n.years)
        } else {
          start_idx <- start.years
          end_idx   <- min(start.years + use.n.years - 1, nrow(tmp))
          tmp <- tmp[start_idx:end_idx, ]
        }
        tmp
      })
      
      res <- bind_rows(res_list)
      
      prob <- lapply(seq_along(mods), function(i) {
        x <- res_list[[i]]
        y <- x[, grepl("^SSB/SSB", names(x)), drop = FALSE]
        out <- as.data.frame(t(colMeans(y < 0.5, na.rm = TRUE)))
        out$Model <- paste0("Model", i)
        out
      }) %>% bind_rows()
      
      title_main <- if (period == "last") {
        paste0("Probability SSB/SSB", percentSPR, "% < 0.5: Last ", use.n.years, " Years")
      } else {
        paste0("Probability SSB/SSB", percentSPR, "% < 0.5: Years ",
               start.years, " to ", start.years + use.n.years - 1)
      }
      
    } else {
      Years      <- mods[[1]][[1]]$om$years
      percentSPR <- mods[[1]][[1]]$om$input$data$percentSPR
      
      res_list <- lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          tmp <- mods[[r]][[m]]$om$rep$SSB
          tmp <- cbind(tmp, rowSums(tmp))
          tmp <- tmp / exp(mods[[r]][[m]]$om$rep$log_SSB_FXSPR)
          tmp <- as.data.frame(tmp)
          name_tmp <- paste0("SSB/SSB", percentSPR, "%")
          names(tmp) <- paste0(name_tmp, ".s", 1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- name_tmp
          tmp$Model <- paste0("Model", m)
          tmp$Year  <- Years
          tmp$Realization <- r
          
          if (period == "last") {
            tmp <- tail(tmp, use.n.years)
          } else {
            start_idx <- start.years
            end_idx   <- min(start.years + use.n.years - 1, nrow(tmp))
            tmp <- tmp[start_idx:end_idx, ]
          }
          tmp
        })
      })
      
      res <- bind_rows(res_list)
      
      prob <- lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          x <- res_list[[r]][[m]]
          y <- x[, grepl("^SSB/SSB", names(x)), drop = FALSE]
          out <- as.data.frame(t(colMeans(y < 0.5, na.rm = TRUE)))
          out$Model <- paste0("Model", m)
          out
        }) %>% bind_rows()
      }) %>% bind_rows()
      
      title_main <- if (period == "last") {
        paste0("Probability SSB/SSB", percentSPR, "% < 0.5: Last ", use.n.years, " Years")
      } else {
        paste0("Probability SSB/SSB", percentSPR, "% < 0.5: Years ",
               start.years, " to ", start.years + use.n.years - 1)
      }
    }
    
    ## Names & reshape
    res  <- rename_models(res)
    prob <- rename_models(prob)
    
    var_name <- unique(gsub("\\.s\\d+", "", grep("^SSB[./]SSB", names(res), value = TRUE)))
    
    res_long <- pivot_longer(
      res,
      cols = matches("^SSB[./]SSB"),
      names_to = "Label",
      values_to = "value"
    )
    
    ## Relative vs base model
    if (!is.null(base.model)) {
      base_df <- res_long %>%
        filter(Model == base.model) %>%
        rename(base_val = value) %>%
        select(Realization, Year, Label, base_val)
      
      res_long <- left_join(res_long, base_df, by = c("Realization", "Year", "Label")) %>%
        mutate(value = value / base_val - 1)
    }
    
    ## Mean/median summarization over years if method is set
    if (!is.null(method)) {
      var <- "value"
      res_long <- res_long %>%
        group_by(Model, Realization, Label) %>%
        summarise(!!var := if (method == "mean") mean(!!sym(var)) else median(!!sym(var)),
                  .groups = "drop")
    }
    
    ## Plot
    if (plot.style == "boxplot") {
      p1 <- ggplot(res_long, aes(x = Model, y = value, color = Model)) +
        geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(
          if (period == "last") {
            ifelse(is.null(base.model),
                   paste0(var_name, ": Last ", use.n.years, " Years"),
                   paste0("Relative ", var_name, " vs ", base.model, ": Last ", use.n.years, " Years"))
          } else {
            paste0(
              ifelse(is.null(base.model),
                     var_name,
                     paste0("Relative ", var_name, " vs ", base.model)),
              ": Years ", start.years, " to ", start.years + use.n.years - 1
            )
          }
        ) +
        ylab(ifelse(is.null(base.model),
                    var_name,
                    paste0("Relative ", var_name, " Difference"))) +
        xlab("Model") +
        theme_bw()
    } else if (plot.style == "median_iqr") {
      res_summary <- res_long %>%
        group_by(Model, Label) %>%
        summarise(
          q1  = quantile(value, 0.25, na.rm = TRUE),
          med = median(value, na.rm = TRUE),
          q3  = quantile(value, 0.75, na.rm = TRUE),
          iqr = q3 - q1,
          .groups = "drop"
        ) %>%
        mutate(
          x    = as.numeric(factor(Model)),
          ymin = if (show.whisker) q1 - 1.5 * iqr else NA_real_,
          ymax = if (show.whisker) q3 + 1.5 * iqr else NA_real_
        )
      
      res_limits <- res_long %>%
        group_by(Model, Label) %>%
        summarise(
          min_val = min(value, na.rm = TRUE),
          max_val = max(value, na.rm = TRUE),
          .groups = "drop"
        )
      
      res_summary <- left_join(res_summary, res_limits, by = c("Model", "Label")) %>%
        mutate(
          ymin = pmax(ymin, min_val),
          ymax = pmin(ymax, max_val)
        )
      
      p1 <- ggplot(res_summary, aes(x = x, color = Model)) +
        {if (show.whisker) geom_segment(aes(x = x, xend = x, y = ymin, yend = q1))} +
        {if (show.whisker) geom_segment(aes(x = x, xend = x, y = q3, yend = ymax))} +
        geom_rect(aes(xmin = x - 0.3, xmax = x + 0.3, ymin = q1, ymax = q3, col = Model),
                  fill = NA, linewidth = 0.8) +
        geom_segment(aes(x = x - 0.3, xend = x + 0.3, y = med, yend = med, color = Model),
                     linewidth = 0.8) +
        scale_x_continuous(breaks = res_summary$x, labels = res_summary$Model) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(
          if (period == "last") {
            ifelse(is.null(base.model),
                   paste0(var_name, ": Last ", use.n.years, " Years"),
                   paste0("Relative ", var_name, " vs ", base.model, ": Last ", use.n.years, " Years"))
          } else {
            paste0(
              ifelse(is.null(base.model),
                     var_name,
                     paste0("Relative ", var_name, " vs ", base.model)),
              ": Years ", start.years, " to ", start.years + use.n.years - 1
            )
          }
        ) +
        ylab(ifelse(is.null(base.model),
                    var_name,
                    paste0("Relative ", var_name, " Difference"))) +
        xlab("Model") +
        theme_bw()
    } else {
      stop("Unknown plot.style. Choose 'boxplot' or 'median_iqr'.")
    }
    
    ## Save SSB main boxplot
    suffix_period <- if (period == "last") {
      paste0("last_", use.n.years, "_years")
    } else {
      paste0("first_", use.n.years, "_years")
    }
    
    plot_name <- paste0("SSB_status",
                        ifelse(is.null(base.model), "", "_Relative"),
                        "_", suffix_period, ".PNG")
    
    new_sub_dir <- file.path(main.dir, sub.dir, "Status_Boxplot")
    if (!file.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
    
    ggsave(file.path(new_sub_dir, plot_name), p1,
           width = width, height = height, dpi = dpi)
    
    ## Probability plot (SSB/SSBSPR < 0.5)
    prob_long <- pivot_longer(prob, cols = matches("^SSB[./]SSB"),
                              names_to = "Label", values_to = "Prob")
    
    if (plot_prob) {
      p2 <- ggplot(prob_long, aes(x = Model, y = Prob, color = Model)) +
        geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(title_main) +
        ylab("Probability") +
        theme_bw()
      
      prob_name <- paste0("SSB_status_overfished_prob_", suffix_period, ".PNG")
      
      ggsave(file.path(new_sub_dir, prob_name), p2,
             width = width, height = height, dpi = dpi)
    } else {
      p2 <- NULL
    }
    
    return(list(
      boxplot  = p1,
      probplot = p2
    ))
  }
  
  ## =======================================================================
  ## 2. Fbar PATH (equivalent to plot_fbar_status + plot_fbar_status2)
  ## =======================================================================
  ## Helper to construct F/FSPR data & probs by index range
  make_fbar_data <- function(index_range, label_prefix) {
    if (!is.nsim) {
      Years <- mods[[1]]$om$years
      res_list <- lapply(seq_along(mods), function(i) {
        tmp1 <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
        tmp2 <- exp(mods[[i]]$om$rep$log_Fbar_XSPR[, index_range, drop = FALSE])
        tmp <- as.data.frame(tmp1 / tmp2)
        names(tmp) <- paste0(label_prefix, seq_along(index_range))
        tmp$Model <- paste0("Model", i)
        tmp$Year  <- Years
        tmp$Realization <- 1
        
        if (period == "last") {
          tmp <- tail(tmp, use.n.years)
        } else {
          start_idx <- start.years
          end_idx   <- min(start.years + use.n.years - 1, nrow(tmp))
          tmp <- tmp[start_idx:end_idx, ]
        }
        tmp
      })
      res <- bind_rows(res_list)
      prob <- lapply(res_list, function(x) {
        y <- x[, 1:length(index_range), drop = FALSE]
        data.frame(t(colMeans(y > 1, na.rm = TRUE)))
      }) %>% bind_rows() %>% mutate(Model = paste0("Model", seq_along(mods)))
      list(data = res, prob = prob)
    } else {
      Years <- mods[[1]][[1]]$om$years
      res_list <- lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          tmp1 <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
          tmp2 <- exp(mods[[r]][[m]]$om$rep$log_Fbar_XSPR[, index_range, drop = FALSE])
          tmp <- as.data.frame(tmp1 / tmp2)
          names(tmp) <- paste0(label_prefix, seq_along(index_range))
          tmp$Model <- paste0("Model", m)
          tmp$Year  <- Years
          tmp$Realization <- r
          
          if (period == "last") {
            tmp <- tail(tmp, use.n.years)
          } else {
            start_idx <- start.years
            end_idx   <- min(start.years + use.n.years - 1, nrow(tmp))
            tmp <- tmp[start_idx:end_idx, ]
          }
          tmp
        })
      })
      res <- bind_rows(res_list)
      prob <- lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          x <- res_list[[r]][[m]]
          y <- x[, 1:length(index_range), drop = FALSE]
          df <- data.frame(t(colMeans(y > 1, na.rm = TRUE)))
          df$Model <- paste0("Model", m)
          df
        }) %>% bind_rows()
      }) %>% bind_rows()
      list(data = res, prob = prob)
    }
  }
  
  ## Setup Fbar titles and dimensions
  if (!is.nsim) {
    percentSPR <- mods[[1]]$om$input$data$percentSPR
    n_fleets   <- mods[[1]]$om$input$data$n_fleets[1]
    n_regions  <- mods[[1]]$om$input$data$n_regions[1]
  } else {
    percentSPR <- mods[[1]][[1]]$om$input$data$percentSPR
    n_fleets   <- mods[[1]][[1]]$om$input$data$n_fleets[1]
    n_regions  <- mods[[1]][[1]]$om$input$data$n_regions[1]
  }
  title_main <- paste0("F/F", percentSPR, "%")
  
  res_fleet_all  <- make_fbar_data(1:n_fleets, "Fleet_")
  res_region_all <- make_fbar_data((n_fleets + 1):(n_fleets + n_regions), "Region_")
  res_global_all <- make_fbar_data(n_fleets + n_regions + 1, "Global")
  
  res_fleet   <- rename_models(res_fleet_all$data)
  res_region  <- rename_models(res_region_all$data)
  res_global  <- rename_models(res_global_all$data)
  prob_fleet  <- rename_models(res_fleet_all$prob)
  prob_region <- rename_models(res_region_all$prob)
  prob_global <- rename_models(res_global_all$prob)
  
  ## Plot Fbar boxplot helper
  plot_fbar_box <- function(res, title, ylab_text, filename_prefix) {
    res_long <- pivot_longer(res,
                             cols = starts_with(c("Fleet_", "Region_", "Global")),
                             names_to = "Label", values_to = "Fbar")
    
    if (!is.null(base.model)) {
      base_df <- res_long %>%
        filter(Model == base.model) %>%
        rename(base_val = Fbar) %>%
        select(Realization, Year, Label, base_val)
      
      res_long <- left_join(res_long, base_df, by = c("Realization", "Year", "Label")) %>%
        mutate(Fbar = Fbar / base_val - 1)
    }
    
    if (!is.null(base.model)) {
      y1 <- if (!is.null(f.ymin)) f.ymin else -1
      y2 <- if (!is.null(f.ymax)) f.ymax else 2
    } else {
      y1 <- if (!is.null(f.ymin)) f.ymin else 0
      y2 <- if (!is.null(f.ymax)) f.ymax else 2
    }
    
    if (!is.null(method)) {
      var <- "Fbar"
      res_long <- res_long %>%
        group_by(Model, Realization, Label) %>%
        summarise(!!var := if (method == "mean") mean(!!sym(var)) else median(!!sym(var)),
                  .groups = "drop")
    }
    
    if (plot.style == "boxplot") {
      p1 <- ggplot(res_long, aes(x = Model, y = Fbar, color = Model)) +
        geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
        coord_cartesian(ylim = c(y1, y2)) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(
          if (period == "last") {
            ifelse(is.null(base.model),
                   paste0(title, ": Last ", use.n.years, " Years"),
                   paste0("Relative ", title, " vs ", base.model,
                          ": Last ", use.n.years, " Years"))
          } else {
            paste0(
              ifelse(is.null(base.model),
                     title,
                     paste0("Relative ", title, " vs ", base.model)),
              ": Years ", start.years, " to ", start.years + use.n.years - 1
            )
          }
        ) +
        ylab(ifelse(is.null(base.model), ylab_text, "Relative Difference")) +
        xlab("Model") +
        theme_bw()
    } else if (plot.style == "median_iqr") {
      res_long$value <- res_long$Fbar
      
      res_summary <- res_long %>%
        group_by(Model, Label) %>%
        summarise(
          q1  = quantile(value, 0.25, na.rm = TRUE),
          med = median(value, na.rm = TRUE),
          q3  = quantile(value, 0.75, na.rm = TRUE),
          iqr = q3 - q1,
          .groups = "drop"
        ) %>%
        mutate(
          x    = as.numeric(factor(Model)),
          ymin = if (show.whisker) q1 - 1.5 * iqr else NA_real_,
          ymax = if (show.whisker) q3 + 1.5 * iqr else NA_real_
        )
      
      res_limits <- res_long %>%
        group_by(Model, Label) %>%
        summarise(
          min_val = min(value, na.rm = TRUE),
          max_val = max(value, na.rm = TRUE),
          .groups = "drop"
        )
      
      res_summary <- left_join(res_summary, res_limits, by = c("Model", "Label")) %>%
        mutate(
          ymin = pmax(ymin, min_val),
          ymax = pmin(ymax, max_val)
        )
      
      p1 <- ggplot(res_summary, aes(x = x, color = Model)) +
        {if (show.whisker) geom_segment(aes(x = x, xend = x, y = ymin, yend = q1))} +
        {if (show.whisker) geom_segment(aes(x = x, xend = x, y = q3, yend = ymax))} +
        geom_rect(aes(xmin = x - 0.3, xmax = x + 0.3, ymin = q1, ymax = q3, col = Model),
                  fill = NA, linewidth = 0.8) +
        geom_segment(aes(x = x - 0.3, xend = x + 0.3, y = med, yend = med, color = Model),
                     linewidth = 0.8) +
        scale_x_continuous(breaks = res_summary$x, labels = res_summary$Model) +
        facet_grid(Label ~ ., scales = "free") +
        scale_color_viridis_d(option = col.opt) +
        ggtitle(
          if (period == "last") {
            ifelse(is.null(base.model),
                   paste0(title, ": Last ", use.n.years, " Years"),
                   paste0("Relative ", title, " vs ", base.model,
                          ": Last ", use.n.years, " Years"))
          } else {
            paste0(
              ifelse(is.null(base.model),
                     title,
                     paste0("Relative ", title, " vs ", base.model)),
              ": Years ", start.years, " to ", start.years + use.n.years - 1
            )
          }
        ) +
        ylab(ifelse(is.null(base.model), ylab_text, "Relative Difference")) +
        xlab("Model") +
        theme_bw()
    } else {
      stop("Unknown plot.style. Choose 'boxplot' or 'median_iqr'.")
    }
    
    new_sub_dir <- file.path(main.dir, sub.dir, "Status_Boxplot")
    if (!file.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
    
    suffix_period <- if (period == "last") {
      paste0("last_", use.n.years, "_years")
    } else {
      paste0("first_", use.n.years, "_years")
    }
    
    file_name <- paste0(filename_prefix,
                        ifelse(is.null(base.model), "", "_Relative"),
                        "_", suffix_period, ".PNG")
    
    ggsave(file.path(new_sub_dir, file_name),
           p1, width = width, height = height, dpi = dpi)
    
    p1
  }
  
  ## Probability plot helper for Fbar
  plot_fbar_prob <- function(prob, title, filename_prefix) {
    prob_long <- pivot_longer(prob,
                              cols = starts_with(c("Fleet_", "Region_", "Global")),
                              names_to = "Label", values_to = "Prob")
    
    p2 <- ggplot(prob_long, aes(x = Model, y = Prob, color = Model)) +
      geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(title) +
      ylab("Probability") +
      xlab("Model") +
      theme_bw()
    
    new_sub_dir <- file.path(main.dir, sub.dir, "Status_Boxplot")
    if (!file.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
    
    suffix_period <- if (period == "last") {
      paste0("last_", use.n.years, "_years")
    } else {
      paste0("first_", use.n.years, "_years")
    }
    
    file_name <- paste0(filename_prefix, "_", suffix_period, ".PNG")
    ggsave(file.path(new_sub_dir, file_name),
           p2, width = width, height = height, dpi = dpi)
    
    p2
  }
  
  ## Make Fbar plots (fleet, region, global)
  p_fleet_box <- plot_fbar_box(
    res_fleet,
    paste(title_main, "by Fleet"),
    title_main,
    paste0("Fbar_status_fleet")
  )
  p_region_box <- plot_fbar_box(
    res_region,
    paste(title_main, "by Region"),
    title_main,
    paste0("Fbar_status_region")
  )
  p_global_box <- plot_fbar_box(
    res_global,
    paste(title_main, "Global"),
    title_main,
    paste0("Fbar_status_global")
  )
  
  if (plot_prob) {
    title_fleet <- if (period == "last") {
      paste0("Probability (", title_main, " > 1) - Fleet: Last ", use.n.years, " Years")
    } else {
      paste0("Probability (", title_main, " > 1) - Fleet: Years ",
             start.years, " to ", start.years + use.n.years - 1)
    }
    title_region <- sub("Fleet", "Region", title_fleet)
    title_global <- sub("Fleet", "Global", title_fleet)
    
    p_fleet_point <- plot_fbar_prob(
      prob_fleet,
      title_fleet,
      "Fbar_status_fleet_overfishing_prob_fleet"
    )
    p_region_point <- plot_fbar_prob(
      prob_region,
      title_region,
      "Fbar_status_region_overfishing_prob_fleet"
    )
    p_global_point <- plot_fbar_prob(
      prob_global,
      title_global,
      "Fbar_status_global_overfishing_prob_fleet"
    )
  } else {
    p_fleet_point <- p_region_point <- p_global_point <- NULL
  }
  
  return(list(
    fleet_box   = p_fleet_box,
    region_box  = p_region_box,
    global_box  = p_global_box,
    fleet_prob  = p_fleet_point,
    region_prob = p_region_point,
    global_prob = p_global_point
  ))
}


#' Plot Kobe-style stock status (SSB/SSB_X% vs F/F_X%).
#'
#' Computes stock status ratios using OM reference points and makes Kobe-style
#' phase plots by region and globally. Supports plotting the last N years or a
#' first-window of N years starting at start.years. Can save points, density,
#' or both styles to a single output folder.
#'
#' @param mods List of models (or list over realizations if is.nsim=TRUE).
#' @param is.nsim Logical. TRUE if mods is realizations x models.
#' @param main.dir Character. Main output directory where plots will be saved.
#' @param sub.dir Character. Subdirectory under main.dir (KOBE_Plot created inside).
#' @param width,height Numeric. Plot size in inches for ggsave().
#' @param dpi Numeric. DPI for ggsave().
#' @param col.opt Character. Viridis option.
#' @param new_model_names Optional character vector of display names for models.
#' @param use.n.years Integer. Window length. Default 1.
#' @param period Character. "last" or "first".
#' @param start.years Integer. 1-based start index for period="first".
#' @param plot_style Character. "both", "points", or "density".
#' @param nbins Integer. Density bin levels when plot_style includes "density".
#'
#' @return Invisibly returns a list of ggplot objects (base panel per stratum).
#'         Files are saved to file.path(main.dir, sub.dir, "KOBE_Plot").
#' @export
plot_kobe_status <- function(mods, is.nsim, main.dir, sub.dir,
                             width = 10, height = 7, dpi = 300, col.opt = "D",
                             new_model_names = NULL,
                             use.n.years = NULL,
                             period = c("last", "first"),
                             start.years = 1,
                             plot_style = c("both", "points", "density"),
                             nbins = 20) {
  
  pick_idx <- function(nT, period, use.n.years, start.years) {
    if (nT <= 0) return(integer(0))
    if (period == "last") idx <- (nT - use.n.years + 1L):nT
    else idx <- start.years:(start.years + use.n.years - 1L)
    idx[idx >= 1L & idx <= nT]
  }
  
  # deps
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Need 'dplyr'")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Need 'ggplot2'")

  period     <- match.arg(period, c("last", "first"))
  plot_style <- match.arg(plot_style, c("both", "points", "density"))
  
  if (is.null(use.n.years)) {
    message("use.n.years is not specified; defaulting to 1 (terminal year).")
    use.n.years <- 1L
  } else {
    use.n.years <- as.integer(use.n.years)
  }
  start.years <- as.integer(start.years)
  
  # helper: select row indices
  pick_idx <- function(nT) {
    if (nT <= 0) return(integer(0))
    if (period == "last") {
      idx <- (nT - use.n.years + 1L):nT
    } else {
      idx <- start.years:(start.years + use.n.years - 1L)
    }
    idx[idx >= 1L & idx <= nT]
  }
  
  # ---- BRP existence checks ----
  if (!is.nsim) {
    if (is.null(mods[[1]]$om$rep$log_SSB_FXSPR) || is.null(mods[[1]]$om$rep$log_Fbar_XSPR)) {
      message("Biological Reference Points not found (log_SSB_FXSPR / log_Fbar_XSPR).")
      return(invisible(NULL))
    }
  } else {
    if (is.null(mods[[1]][[1]]$om$rep$log_SSB_FXSPR) || is.null(mods[[1]][[1]]$om$rep$log_Fbar_XSPR)) {
      message("Biological Reference Points not found (log_SSB_FXSPR / log_Fbar_XSPR).")
      return(invisible(NULL))
    }
  }
  
  # ---- Extract SSB ratios ----
  if (!is.nsim) {
    Years <- mods[[1]]$om$years
    ssb_list <- lapply(seq_along(mods), function(m) {
      log_ref <- mods[[m]]$om$rep$log_SSB_FXSPR
      if (any(!is.finite(log_ref))) return(NULL)
      
      ssb_mat <- mods[[m]]$om$rep$SSB
      ssb_mat <- cbind(ssb_mat, rowSums(ssb_mat))  # add global
      ssb_mat <- ssb_mat / exp(log_ref)
      
      idx <- pick_idx(nrow(ssb_mat))
      
      if (length(idx) == 0) return(NULL)
      
      out <- data.frame(Model = paste0("Model", m),
                        Realization = 1,
                        Year = Years[idx],
                        ssb_mat[idx, , drop = FALSE],
                        check.names = FALSE)
      out
    })
  } else {
    Years <- mods[[1]][[1]]$om$years
    ssb_list <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        log_ref <- mods[[r]][[m]]$om$rep$log_SSB_FXSPR
        if (any(!is.finite(log_ref))) return(NULL)
        
        ssb_mat <- mods[[r]][[m]]$om$rep$SSB
        ssb_mat <- cbind(ssb_mat, rowSums(ssb_mat))
        ssb_mat <- ssb_mat / exp(log_ref)
        
        idx <- pick_idx(nrow(ssb_mat))
        if (length(idx) == 0) return(NULL)
        
        out <- data.frame(Model = paste0("Model", m),
                          Realization = r,
                          Year = Years[idx],
                          ssb_mat[idx, , drop = FALSE],
                          check.names = FALSE)
        out
      })
    })
    ssb_list <- unlist(ssb_list, recursive = FALSE)
  }
  
  ssb_list <- ssb_list[!vapply(ssb_list, is.null, logical(1))]
  if (length(ssb_list) == 0) {
    message("No finite SSB ratio data available for Kobe plots.")
    return(invisible(NULL))
  }
  ssb_df <- dplyr::bind_rows(ssb_list)
  
  # strata columns = everything after Model/Realization/Year
  n_strata <- ncol(ssb_df) - 3L
  if (n_strata < 1) stop("SSB ratio extraction failed: no strata columns found.")
  strata_names <- names(ssb_df)[4:ncol(ssb_df)]
  
  # ---- Extract F ratios ----
  if (!is.nsim) {
    f_list <- lapply(seq_along(mods), function(m) {
      log_ref <- mods[[m]]$om$rep$log_Fbar_XSPR
      if (any(!is.finite(log_ref))) return(NULL)
      
      n_fleets <- mods[[m]]$om$input$data$n_fleets
      Fbar     <- mods[[m]]$om$rep$Fbar[, -c(1:n_fleets), drop = FALSE]
      Fref     <- exp(log_ref[, -c(1:n_fleets), drop = FALSE])
      
      ratio <- Fbar / Fref
      idx <- pick_idx(nrow(ratio))
      if (length(idx) == 0) return(NULL)
      
      out <- data.frame(Model = paste0("Model", m),
                        Realization = 1,
                        Year = Years[idx],
                        ratio[idx, , drop = FALSE],
                        check.names = FALSE)
      out
    })
  } else {
    f_list <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        log_ref <- mods[[r]][[m]]$om$rep$log_Fbar_XSPR
        if (any(!is.finite(log_ref))) return(NULL)
        
        n_fleets <- mods[[r]][[m]]$om$input$data$n_fleets
        Fbar     <- mods[[r]][[m]]$om$rep$Fbar[, -c(1:n_fleets), drop = FALSE]
        Fref     <- exp(log_ref[, -c(1:n_fleets), drop = FALSE])
        
        ratio <- Fbar / Fref
        idx <- pick_idx(nrow(ratio))
        if (length(idx) == 0) return(NULL)
        
        out <- data.frame(Model = paste0("Model", m),
                          Realization = r,
                          Year = Years[idx],
                          ratio[idx, , drop = FALSE],
                          check.names = FALSE)
        out
      })
    })
    f_list <- unlist(f_list, recursive = FALSE)
  }
  
  f_list <- f_list[!vapply(f_list, is.null, logical(1))]
  if (length(f_list) == 0) {
    message("No finite F ratio data available for Kobe plots.")
    return(invisible(NULL))
  }
  f_df <- dplyr::bind_rows(f_list)
  
  # Must match strata count (regions+global)
  if ((ncol(f_df) - 3L) != n_strata) {
    stop("Number of SSB strata (regions+global) does not match F strata.")
  }
  
  # rename ratio columns to match SSB strata colnames (so join is simple)
  names(f_df)[4:ncol(f_df)] <- strata_names
  
  # percentSPR label
  percentSPR <- if (!is.nsim) mods[[1]]$om$input$data$percentSPR else mods[[1]][[1]]$om$input$data$percentSPR
  
  kobe_dir <- file.path(main.dir, sub.dir)
  # if sub.dir is already .../KOBE_Plot, don't append again
  if (basename(kobe_dir) != "KOBE_Plot") {
    kobe_dir <- file.path(kobe_dir, "KOBE_Plot")
  }
  if (!dir.exists(kobe_dir)) dir.create(kobe_dir, recursive = TRUE, showWarnings = FALSE)
  
  
  # which styles to do
  styles_to_do <- switch(plot_style,
                         both    = c("points", "density"),
                         points  = "points",
                         density = "density")
  
  # plotting loop
  p_list <- vector("list", n_strata)
  
  for (i in seq_len(n_strata)) {
    
    ssb_i <- ssb_df[, c("Model", "Realization", "Year", strata_names[i]), drop = FALSE]
    f_i   <- f_df[,   c("Model", "Realization", "Year", strata_names[i]), drop = FALSE]
    names(ssb_i)[4] <- "Overfished"
    names(f_i)[4]   <- "Overfishing"
    
    temp <- dplyr::left_join(ssb_i, f_i, by = c("Model", "Realization", "Year"))
    
    # apply display names if provided
    if (!is.null(new_model_names)) {
      nm_now <- sort(unique(as.character(temp$Model)))
      # expect Model1..ModelK
      k <- length(nm_now)
      if (length(new_model_names) != k) {
        stop("Length of new_model_names must match number of models (", k, ").")
      }
      temp$Model <- factor(temp$Model,
                           levels = paste0("Model", seq_len(k)),
                           labels = new_model_names)
    }
    
    # drop non-finite
    temp <- dplyr::filter(temp, is.finite(.data$Overfished), is.finite(.data$Overfishing))
    
    if (nrow(temp) == 0) next
    
    who <- if (i < n_strata) paste0("Region ", i) else "Global"
    who_tag <- if (i < n_strata) paste0("R", i) else "G"
    when <- if (period == "last") {
      paste0("Last ", use.n.years, " Year(s)")
    } else {
      paste0("First ", use.n.years, " Year(s) (start.years=", start.years, ")")
    }
    
    # axis limits: avoid 0-width
    x95 <- as.numeric(stats::quantile(temp$Overfished,  0.95, na.rm = TRUE))
    y95 <- as.numeric(stats::quantile(temp$Overfishing, 0.95, na.rm = TRUE))
    if (!is.finite(x95) || x95 <= 0) x95 <- max(temp$Overfished,  na.rm = TRUE)
    if (!is.finite(y95) || y95 <= 0) y95 <- max(temp$Overfishing, na.rm = TRUE)
    x95 <- max(x95, 1e-6)
    y95 <- max(y95, 1e-6)
    
    # base plot (no geom_point here; we add per style)
    g <- ggplot2::ggplot(temp, ggplot2::aes(x = .data$Overfished, y = .data$Overfishing)) +
      ggplot2::facet_wrap(~ Model) +
      ggplot2::xlab(bquote(SSB / SSB[.(percentSPR)*"%"])) +
      ggplot2::ylab(bquote(F / F[.(percentSPR)*"%"])) +
      ggplot2::ggtitle(paste0("Stock Status (", when, ")\n", who)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text   = ggplot2::element_text(size = 12),
        axis.title  = ggplot2::element_text(size = 20),
        plot.title  = ggplot2::element_text(size = 12),
        strip.text  = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12),
        legend.title= ggplot2::element_text(size = 12),
        aspect.ratio = 1,
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::coord_cartesian(xlim = c(0, x95), ylim = c(0, y95)) +
      ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = 1) +
      ggplot2::geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", linewidth = 1)
    
    # save requested styles (to ONE folder)
    for (sty in styles_to_do) {
      gg <- g
      
      if (sty == "density") {
        gg <- gg +
          ggplot2::geom_point(size = 1, alpha = 0.1) +
          ggplot2::geom_density_2d_filled(
            ggplot2::aes(fill = ggplot2::after_stat(level)),
            alpha = 0.9, contour_var = "density", bins = nbins
          ) +
          ggplot2::scale_fill_viridis_d(option = col.opt, guide = "none")
         
      } else {
        gg <- gg +
          ggplot2::geom_point(ggplot2::aes(color = Model), size = 1.5, alpha = 0.9) +
          ggplot2::scale_color_viridis_d(option = col.opt) + 
          ggplot2::annotate(
            "rect",
            xmin = 0.5, xmax = Inf, ymin = -Inf, ymax = 1,
            alpha = 0.2, fill = "yellow"
          )
      }
      
      plot_name <- sprintf("Kobe_%s_s%d_%dyr_%s_%s.png",
                           period, start.years, use.n.years, who_tag, sty)
      
      ggplot2::ggsave(filename = file.path(kobe_dir, plot_name),
                      plot = gg, width = width, height = height, dpi = dpi)
    }
    
    p_list[[i]] <- g
  }
  
  invisible(p_list)
}


#' Plot model performance as radar charts (three types)
#'
#' Compute multi-metric performance scores for each estimation model and
#' visualize them as radar charts. The function supports three radar “types”
#' that correspond to three layouts:
#'
#' \itemize{
#'   \item \strong{Type 1} (\code{radar_type = 1}):
#'     \itemize{
#'       \item Catch\eqn{_{ST}}: short-term average catch
#'       \item SSB\eqn{_{ST}}: short-term average spawning stock biomass
#'       \item F\eqn{_{ST}}: short-term average global fishing mortality
#'       \item Catch\eqn{_{LT}}: long-term average catch
#'       \item SSB\eqn{_{LT}}: long-term average spawning stock biomass
#'       \item F\eqn{_{LT}}: long-term average global fishing mortality
#'     }
#'
#'   \item \strong{Type 2} (\code{radar_type = 2}):
#'     \itemize{
#'       \item Catch\eqn{_{ST}}, SSB\eqn{_{ST}}, Catch\eqn{_{LT}}, SSB\eqn{_{LT}}
#'       \item Catch\eqn{_{AAV}}: average annual variation (AAV) in catch
#'       \item SSB\eqn{_{AAV}}: AAV in SSB
#'       \item F\eqn{_{AAV}}: AAV in global fishing mortality
#'     }
#'
#'   \item \strong{Type 3} (\code{radar_type = 3}):
#'     \itemize{
#'       \item All metrics in Type 2
#'       \item \eqn{P(F_{ST} > F_{XSPR})}: probability that short-term F exceeds
#'             the reference F\eqn{_{XSPR}} (e.g., F\eqn{_{MSY}}-proxy)
#'       \item \eqn{P(F_{LT} > F_{XSPR})}: same probability in the long term
#'     }
#' }
#'
#' For simulation runs (\code{is.nsim = TRUE}), metrics are computed for each
#' realization and then summarized by the median across realizations. For
#' single-run objects (\code{is.nsim = FALSE}), metrics are computed directly
#' from that run.
#'
#' All metrics are normalized to a 0–100 scale so that higher values always
#' indicate better performance:
#' \itemize{
#'   \item For “good” metrics (e.g., Catch, SSB), larger is better:
#'         \deqn{Score = 100 \times (x - \min(x)) / (\max(x) - \min(x))}
#'   \item For “cost” metrics (e.g., F, AAV, probabilities of overfishing),
#'         smaller is better and the scale is inverted:
#'         \deqn{Score = 100 \times \bigl(1 - (x - \min(x))/(\max(x)-\min(x))\bigr)}
#' }
#'
#' The function writes a PNG file to \code{file.path(main.dir, sub.dir,
#' "Radar_Holistic_Plot", "model_performance_radar_typeX.png")} and also plots
#' the radar chart to the active graphics device.
#'
#' @param mods Either:
#'   \itemize{
#'     \item A list of lists of model objects (when \code{is.nsim = TRUE}),
#'           i.e. \code{mods[[r]][[m]]}, where \code{r} indexes realizations
#'           and \code{m} indexes models; or
#'     \item A simple list of model objects (when \code{is.nsim = FALSE}),
#'           i.e. \code{mods[[m]]}.
#'   }
#'   Each model object is expected to contain an OM report list
#'   \code{$om$rep} with at least:
#'   \itemize{
#'     \item \code{pred_catch}: matrix (years × regions) of predicted catch
#'     \item \code{SSB}: matrix (years × regions) of spawning stock biomass
#'     \item \code{Fbar}: matrix (years × components), whose last column (or
#'           \code{n_fleets + n_regions + 1}) is the global F.
#'     \item \code{log_SSB_FXSPR}, \code{log_Fbar_XSPR} (only needed for
#'           \code{radar_type = 3}; may be \code{NULL}, in which case the
#'           probability metrics are left as \code{NA} but plotting continues
#'           or is skipped, see Details).
#'   }
#' @param is.nsim Logical. \code{TRUE} if \code{mods} is a list of realizations,
#'   each containing a list of models; \code{FALSE} if \code{mods} is a single
#'   list of models (no simulation dimension).
#' @param main.dir Character. Top-level directory for results.
#' @param sub.dir Character. Subdirectory within \code{main.dir}.
#' @param width,height Numeric. Width and height of the output PNG (in inches).
#' @param dpi Numeric. PNG resolution (dots per inch).
#' @param col.opt Character. Option passed to \code{viridisLite::viridis()} to
#'   choose the color map (e.g., \code{"D"}).
#' @param method Character. Summary statistic for short/long-term metrics:
#'   either \code{"median"} or \code{"mean"}. Defaults to \code{"median"}.
#' @param use.n.years.first Integer. Number of years at the beginning of the
#'   time series used to define the short-term metrics.
#' @param use.n.years.last Integer. Number of terminal years used to define
#'   the long-term metrics.
#' @param start.years Integer. First year index to use when calculating
#'   short-term metrics (e.g., \code{start.years = 1} uses years
#'   1:\code{use.n.years.first}).
#' @param new_model_names Optional character vector of new model names. If
#'   provided, must have length equal to the number of models. Used to relabel
#'   the radar-chart spokes (the curves), not the metrics.
#' @param radar_type Integer or character indicating the radar layout:
#'   \itemize{
#'     \item \code{1}: Type 1 (Catch/SSB/F short- and long-term)
#'     \item \code{2}: Type 2 (Catch/SSB short- and long-term + AAVs)
#'     \item \code{3}: Type 3 (Type 2 + probabilities of F exceeding F\eqn{_{XSPR}})
#'   }
#'
#' @return Invisibly returns a data frame of median (or mean) performance
#'   scores for each model (one row per model, one column per metric, plus
#'   \code{Model}). The function is called mainly for its side effect of
#'   generating and saving radar plots.
#'
#' @details
#' Average Annual Variation (AAV) is computed as:
#' \deqn{
#'   \text{AAV}(x) = \frac{\sum_{t} |x_{t+1} - x_t|}{\sum_t x_t}
#' }
#' where \eqn{x_t} is a non-negative time series (e.g., Catch, SSB, or Fbar).
#' If the denominator is zero or the time series length is less than 2,
#' AAV is returned as \code{NA}.
#'
#' When \code{radar_type = 3}, the probability metrics \code{prob_first} and
#' \code{prob_last} are computed based on:
#' \deqn{
#'   F_{ratio}(t) = F_{global}(t) / F_{XSPR}(t)
#' }
#' Only finite values (non-\code{NA}, non-\code{NaN}, non-\code{Inf}) of
#' \eqn{F_{global}} and \eqn{F_{XSPR}} are used. If, for a given model, the
#' reference series contains non-finite values, the corresponding probabilities
#' are set to \code{NA} for that model.
#'
#' If, after summarizing across realizations, \emph{all} models have
#' \code{prob_first} and \code{prob_last} equal to \code{NA}, the function
#' assumes that the reference-point quantities are unusable (all NA/NaN/Inf)
#' and \strong{skips the Type 3 radar plot entirely}. In that case, a message
#' is printed, and only the performance table is returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Type 1 radar (short/long-term Catch, SSB, F) for simulation results:
#' plot_model_performance_radar(
#'   mods           = mods_sim,
#'   is.nsim        = TRUE,
#'   main.dir       = "Results",
#'   sub.dir        = "Scenario1",
#'   radar_type     = 1,
#'   use.n.years.first = 5,
#'   use.n.years.last  = 5
#' )
#'
#' # Type 3 radar (includes overfishing probabilities) for a single run:
#' plot_model_performance_radar(
#'   mods           = mods_single,
#'   is.nsim        = FALSE,
#'   main.dir       = "Results",
#'   sub.dir        = "Scenario2",
#'   radar_type     = 3,
#'   new_model_names = c("EM1", "EM2", "EM3")
#' )
#' }
#'
#' @export
plot_model_performance_radar <- function(mods, is.nsim, main.dir, sub.dir, 
                                         width = 10, height = 10, dpi = 300, col.opt = "D",
                                         method = NULL,
                                         use.n.years.first = 5,
                                         use.n.years.last = 5,
                                         start.years = 1, 
                                         new_model_names = NULL,
                                         radar_type = 1) {
  ## -----------------------------
  ## Libraries
  ## -----------------------------
  library(dplyr)
  library(tidyr)
  library(fmsb)
  library(viridisLite)
  
  ## -----------------------------
  ## Args & helpers
  ## -----------------------------
  if (is.null(method)) method <- "median"
  
  # Normalize radar_type to character "1"/"2"/"3"
  if (is.numeric(radar_type)) radar_type <- as.character(radar_type)
  radar_type <- match.arg(radar_type, c("1", "2", "3"))
  
  # Helper: Average Annual Variation
  calculate_aacv <- function(values) {
    if (!is.numeric(values)) stop("Input must be a numeric vector.")
    if (length(values) < 2) return(NA_real_)
    diffs <- abs(diff(values))
    denom <- sum(values[-length(values)])
    if (denom <= 0) return(NA_real_)
    sum(diffs) / denom
  }
  
  # Helper: compute metrics for ONE replicate (a list of model objects)
  compute_metrics_for_rep <- function(mod_list) {
    n_models <- length(mod_list)
    tmp <- data.frame(Model = paste0("Model", seq_len(n_models)))
    
    for (m in seq_len(n_models)) {
      rep_obj <- mod_list[[m]]$om$rep
      
      # Time series
      catch_ts <- rowSums(rep_obj$pred_catch)
      ssb_ts   <- rowSums(rep_obj$SSB)
      
      n_fleets  <- mod_list[[m]]$om$input$data$n_fleets[1]
      n_regions <- mod_list[[m]]$om$input$data$n_regions[1]
      # Global Fbar column
      fbar_ts   <- rep_obj$Fbar[, n_fleets + n_regions + 1]
      
      # Indices for ST/LT
      idx_first <- start.years:(start.years + use.n.years.first - 1)
      idx_last  <- (length(catch_ts) - use.n.years.last + 1):length(catch_ts)
      
      if (method == "median") {
        tmp$Catch_first[m] <- median(catch_ts[idx_first])
        tmp$SSB_first[m]   <- median(ssb_ts[idx_first])
        tmp$Catch_last[m]  <- median(catch_ts[idx_last])
        tmp$SSB_last[m]    <- median(ssb_ts[idx_last])
        
        if (radar_type == "1") {
          tmp$Fbar_first[m] <- median(fbar_ts[idx_first])
          tmp$Fbar_last[m]  <- median(fbar_ts[idx_last])
        }
        
      } else if (method == "mean") {
        tmp$Catch_first[m] <- mean(catch_ts[idx_first])
        tmp$SSB_first[m]   <- mean(ssb_ts[idx_first])
        tmp$Catch_last[m]  <- mean(catch_ts[idx_last])
        tmp$SSB_last[m]    <- mean(ssb_ts[idx_last])
        
        if (radar_type == "1") {
          tmp$Fbar_first[m] <- mean(fbar_ts[idx_first])
          tmp$Fbar_last[m]  <- mean(fbar_ts[idx_last])
        }
      }
      
      # AAV metrics (types 2 & 3)
      if (radar_type %in% c("2", "3")) {
        tmp$catch_aacv[m] <- calculate_aacv(rowSums(rep_obj$pred_catch))
        tmp$ssb_aacv[m]   <- calculate_aacv(rowSums(rep_obj$SSB))
        # Fbar AAV: global column
        tmp$fbar_aacv[m]  <- calculate_aacv(rep_obj$Fbar[, ncol(rep_obj$Fbar)])
      }
      
      # Prob(F > F_XSPR) (type 3)
      if (radar_type == "3") {
        if (!is.null(rep_obj$log_SSB_FXSPR) && !is.null(rep_obj$log_Fbar_XSPR)) {
          F_global <- rep_obj$Fbar[, ncol(rep_obj$Fbar)]
          F_XSPR   <- exp(rep_obj$log_Fbar_XSPR[, ncol(rep_obj$Fbar)])
          
          # Guard against NA/NaN/Inf in reference or F
          if (all(is.finite(F_global)) && all(is.finite(F_XSPR))) {
            ratio_F <- F_global / F_XSPR
            
            ratio_first <- ratio_F[idx_first]
            ratio_last  <- ratio_F[idx_last]
            
            tmp$prob_first[m] <- mean(ratio_first > 1, na.rm = TRUE)
            tmp$prob_last[m]  <- mean(ratio_last > 1, na.rm = TRUE)
          } else {
            # Any non-finite reference or F: mark probabilities as NA
            tmp$prob_first[m] <- NA_real_
            tmp$prob_last[m]  <- NA_real_
          }
        } else {
          tmp$prob_first[m] <- NA_real_
          tmp$prob_last[m]  <- NA_real_
        }
      }
    }
    
    # Normalize
    metric_cols <- setdiff(names(tmp), "Model")
    
    good_vars <- intersect(metric_cols,
                           c("Catch_first", "SSB_first",
                             "Catch_last",  "SSB_last"))
    
    cost_vars <- intersect(metric_cols,
                           c("Fbar_first", "Fbar_last",
                             "catch_aacv",  "ssb_aacv", "fbar_aacv",
                             "prob_first",  "prob_last"))
    
    # Good variables
    for (v in good_vars) {
      if (all(is.na(tmp[[v]]))) {
        tmp[[v]] <- NA_real_
      } else {
        range_val <- max(tmp[[v]], na.rm = TRUE) - min(tmp[[v]], na.rm = TRUE)
        if (range_val == 0) {
          tmp[[v]] <- 100
        } else {
          tmp[[v]] <- 100 * (tmp[[v]] - min(tmp[[v]], na.rm = TRUE)) / range_val
        }
      }
    }
    
    # Cost variables
    for (v in cost_vars) {
      if (all(is.na(tmp[[v]]))) {
        tmp[[v]] <- NA_real_
      } else {
        range_val <- max(tmp[[v]], na.rm = TRUE) - min(tmp[[v]], na.rm = TRUE)
        if (range_val == 0) {
          tmp[[v]] <- 100
        } else {
          norm_f   <- (tmp[[v]] - min(tmp[[v]], na.rm = TRUE)) / range_val
          tmp[[v]] <- 100 * (1 - norm_f)
        }
      }
    }
    
    tmp
  }
  
  ## -----------------------------
  ## Build results for all reps
  ## -----------------------------
  if (is.nsim) {
    n_reps  <- length(mods)
    results <- vector("list", n_reps)
    for (r in seq_len(n_reps)) {
      results[[r]] <- compute_metrics_for_rep(mods[[r]])
    }
  } else {
    results <- list(compute_metrics_for_rep(mods))
  }
  
  combined <- bind_rows(results, .id = "Realization")
  
  scores_median <- combined %>%
    group_by(Model) %>%
    summarise(across(-Realization, median, na.rm = TRUE), .groups = "drop")
  
  ## -----------------------------
  ## Optional renaming
  ## -----------------------------
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(scores_median$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    scores_median$Model <- factor(scores_median$Model,
                                  levels = paste0("Model", seq_along(new_model_names)),
                                  labels = new_model_names)
  }
  
  ## -----------------------------
  ## If radar_type 3 & all probs NA -> skip plotting
  ## -----------------------------
  if (radar_type == "3" &&
      ("prob_first" %in% names(scores_median)) &&
      ("prob_last"  %in% names(scores_median))) {
    
    all_prob_na <- all(is.na(scores_median$prob_first)) &&
      all(is.na(scores_median$prob_last))
    
    if (all_prob_na) {
      message("Reference-point F_XSPR is NA/NaN/Inf for all models; skipping Type 3 radar plot.")
      return(invisible(scores_median))
    }
  }
  
  ## -----------------------------
  ## Radar chart prep
  ## -----------------------------
  plot_df <- as.data.frame(scores_median)
  rownames(plot_df) <- plot_df$Model
  plot_df$Model <- NULL
  plot_df <- as.data.frame(t(plot_df))   # rows = metrics, cols = models
  
  # Desired metric order for each type
  if (radar_type == "1") {
    desired_order <- c("Catch_first", "SSB_first", "Fbar_first",
                       "Catch_last",  "SSB_last",  "Fbar_last")
  } else if (radar_type == "2") {
    desired_order <- c("Catch_first", "SSB_first",
                       "Catch_last",  "SSB_last",
                       "catch_aacv",  "ssb_aacv", "fbar_aacv")
  } else { # "3"
    desired_order <- c("Catch_first", "SSB_first",
                       "Catch_last",  "SSB_last",
                       "catch_aacv",  "ssb_aacv", "fbar_aacv",
                       "prob_first",  "prob_last")
  }
  existing_order <- rownames(plot_df)
  use_order      <- intersect(desired_order, existing_order)
  plot_df        <- plot_df[use_order, , drop = FALSE]
  
  # Add max/min rows
  plot_df <- rbind(
    max = rep(100, ncol(plot_df)),
    min = rep(0,   ncol(plot_df)),
    plot_df
  )
  
  if (ncol(plot_df) < 3) {
    message("Radar chart needs at least 3 models (columns). Nothing plotted.")
    return(invisible(scores_median))
  }
  
  ## -----------------------------
  ## Plotting
  ## -----------------------------
  n_axes <- nrow(plot_df) - 2
  colors <- viridisLite::viridis(n = ncol(plot_df), option = col.opt)
  
  new_sub_dir <- file.path(main.dir, sub.dir, "Radar_Holistic_Plot")
  if (!dir.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
  
  if (radar_type == "1") {
    my_legend_labels <- c(
      expression(Catch[ST]),
      expression(SSB[ST]),
      expression(F[ST]),
      expression(Catch[LT]),
      expression(SSB[LT]),
      expression(F[LT])
    )
    out_file <- file.path(new_sub_dir, "model_performance_radar_type1.png")
  } else if (radar_type == "2") {
    my_legend_labels <- c(
      expression(Catch[ST]),
      expression(SSB[ST]),
      expression(Catch[LT]),
      expression(SSB[LT]),
      expression(Catch[AAV]),
      expression(SSB[AAV]),
      expression(F[AAV])
    )
    out_file <- file.path(new_sub_dir, "model_performance_radar_type2.png")
  } else {
    my_legend_labels <- c(
      expression(Catch[ST]),
      expression(SSB[ST]),
      expression(Catch[LT]),
      expression(SSB[LT]),
      expression(Catch[AAV]),
      expression(SSB[AAV]),
      expression(F[AAV]),
      expression(P(F[ST] > F[MSY])),
      expression(P(F[LT] > F[MSY]))
    )
    out_file <- file.path(new_sub_dir, "model_performance_radar_type3.png")
  }
  
  if (length(my_legend_labels) != n_axes) {
    warning("Number of legend labels does not match number of axes; check metric order.")
  }
  
  ## ---- PNG file ----
  png(filename = out_file, width = width, height = height, units = "in", res = dpi)
  
  layout(matrix(c(1, 2), nrow = 1), widths = c(3, 1))
  
  par(mar = c(1, 1, 1, 1))
  radarchart(
    plot_df,
    axistype   = 4,
    pcol       = colors,
    plwd       = 3,
    plty       = rep(1, ncol(plot_df)),   # <-- force solid lines
    cglcol     = "grey80",
    cglty      = 1,
    axislabcol = "grey30",
    vlcex      = 1.2
  )
  
  par(mar = c(1, 1, 1, 1))
  plot.new()
  legend(
    "center",
    legend    = my_legend_labels,
    col       = colors,
    lty       = rep(1, ncol(plot_df)),    # <-- force solid lines
    lwd       = 3,
    cex       = 0.9,
    y.intersp = 1.5,
    bty       = "n"
  )
  
  dev.off()
  
  ## ---- Screen plot ----
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  layout(matrix(c(1, 2), nrow = 1), widths = c(3, 1))
  
  par(mar = c(1, 1, 2, 1))
  radarchart(
    plot_df,
    axistype   = 4,
    pcol       = colors,
    plwd       = 3,
    plty       = rep(1, ncol(plot_df)),   # <-- force solid lines
    cglcol     = "grey80",
    cglty      = 1,
    axislabcol = "grey30",
    vlcex      = 1.2
  )
  
  par(mar = c(1, 1, 2, 1))
  plot.new()
  legend(
    "center",
    legend    = my_legend_labels,
    col       = colors,
    lty       = rep(1, ncol(plot_df)),    # <-- force solid lines
    lwd       = 3,
    cex       = 0.9,
    y.intersp = 1.5,
    bty       = "n"
  )
  
  invisible(scores_median)
}

#' Plot holistic model performance as bar charts (types 1–4)
#'
#' This function summarizes MSE outputs into normalized performance scores
#' (0–100) for each estimation model and displays them as bar charts.
#'
#' It supports four bar types:
#' \itemize{
#'   \item \code{bar_type = 1}: Short- and long-term Catch, SSB, and F
#'         (\code{Catch_first}, \code{SSB_first}, \code{Fbar_first},
#'          \code{Catch_last}, \code{SSB_last}, \code{Fbar_last});
#'         y-axis is Estimation Model.
#'   \item \code{bar_type = 2}: Same metrics as type 1,
#'         but y-axis is Metric (bars grouped by Model).
#'   \item \code{bar_type = 3}: Type 1 + Average Annual Variation metrics
#'         (\code{catch_aacv}, \code{ssb_aacv}, \code{fbar_aacv}) and
#'         probabilities that F exceeds F\eqn{_{MSY}} in short and long term
#'         (\code{prob_first}, \code{prob_last}); y-axis is Estimation Model.
#'   \item \code{bar_type = 4}: Same metrics as type 3, but y-axis is Metric
#'         (bars grouped by Model).
#' }
#'
#' When \code{is.nsim = TRUE}, the function first computes performance scores
#' within each realization (sim), then summarizes across realizations using
#' the median and interquartile range (IQR) for error bars.
#'
#' Scores are normalized so that higher values are always better:
#' \itemize{
#'   \item For Catch and SSB means: larger is better.
#'   \item For F, AAV, and probabilities of overfishing: smaller is better,
#'         so scores are inverted.
#' }
#'
#' For \code{bar_type = 3} and \code{4}, if biological reference points
#' (\code{log_SSB_FXSPR} and \code{log_Fbar_XSPR}) are missing or all
#' non-finite (NA/NaN/Inf) across models, the function prints a message and
#' skips plotting (returns silently) instead of failing.
#'
#' @param mods List of model objects (nested list if \code{is.nsim = TRUE}).
#' @param is.nsim Logical; \code{TRUE} if \code{mods} is a list over
#'   realizations (sims) and each element is a list of EMs.
#' @param main.dir Main directory for output.
#' @param sub.dir Subdirectory within \code{main.dir} for output.
#' @param new_model_names Optional character vector of new labels for models.
#' @param width Plot width in inches (for PNG).
#' @param height Plot height in inches (for PNG).
#' @param dpi Resolution (dots per inch) for PNG.
#' @param col.opt Viridis palette option passed to
#'   \code{viridisLite::viridis()}.
#' @param method Character, either \code{"median"} or \code{"mean"} for the
#'   short- and long-term period summaries.
#' @param use.n.years.first Number of years in the first period (short term).
#' @param use.n.years.last Number of years in the last period (long term).
#' @param start.years Starting index (1-based) for the first period.
#' @param bar_type Integer or character: \code{1}, \code{2}, \code{3},
#'   or \code{4}, indicating which bar type to plot.
#'
#' @return Invisibly returns the summarized scores data frame used for plotting.
#'   Also writes a PNG file to disk.
#' @export
plot_model_performance_bar <- function(mods, is.nsim,
                                       main.dir = ".",
                                       sub.dir = ".",
                                       new_model_names = NULL,
                                       width = 12, height = 8, dpi = 300,
                                       col.opt = "D",
                                       method = NULL,
                                       use.n.years.first = 5,
                                       use.n.years.last  = 5,
                                       start.years = 1,
                                       bar_type = 1,
                                       base.model = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridisLite)
  
  if (is.null(method)) method <- "median"
  if (is.numeric(bar_type)) bar_type <- as.character(bar_type)
  bar_type <- match.arg(bar_type, c("1","2","3","4"))
  
  ## ------------------------------------------------------------------
  ## Helper: Average Annual Variation (AAV)
  ## ------------------------------------------------------------------
  calculate_aacv <- function(values) {
    if (!is.numeric(values)) return(NA_real_)
    values <- values[is.finite(values)]
    if (length(values) < 2) return(NA_real_)
    diffs <- abs(diff(values))
    denom <- sum(values[-length(values)])
    if (!is.finite(denom) || denom <= 0) return(NA_real_)
    sum(diffs) / denom
  }
  
  ## ------------------------------------------------------------------
  ## Helper: compute normalized metrics for one realization (list of EMs)
  ## ------------------------------------------------------------------
  compute_metrics_for_rep <- function(mod_list) {
    n_models <- length(mod_list)
    tmp <- data.frame(Model = paste0("Model", seq_len(n_models)))
    
    for (m in seq_len(n_models)) {
      om_rep <- mod_list[[m]]$om$rep
      dat    <- mod_list[[m]]$om$input$data
      
      n_fleets  <- dat$n_fleets[1]
      n_regions <- dat$n_regions[1]
      
      catch_ts <- rowSums(om_rep$pred_catch)
      ssb_ts   <- rowSums(om_rep$SSB)
      fbar_ts  <- om_rep$Fbar[, n_fleets + n_regions + 1]
      
      idx_first <- start.years:(start.years + use.n.years.first - 1)
      idx_last  <- (length(catch_ts) - use.n.years.last + 1):length(catch_ts)
      
      if (method == "median") {
        tmp$Catch_first[m] <- median(catch_ts[idx_first])
        tmp$SSB_first[m]   <- median(ssb_ts[idx_first])
        tmp$Catch_last[m]  <- median(catch_ts[idx_last])
        tmp$SSB_last[m]    <- median(ssb_ts[idx_last])
        tmp$Fbar_first[m]  <- median(fbar_ts[idx_first])
        tmp$Fbar_last[m]   <- median(fbar_ts[idx_last])
      } else {
        tmp$Catch_first[m] <- mean(catch_ts[idx_first])
        tmp$SSB_first[m]   <- mean(ssb_ts[idx_first])
        tmp$Catch_last[m]  <- mean(catch_ts[idx_last])
        tmp$SSB_last[m]    <- mean(ssb_ts[idx_last])
        tmp$Fbar_first[m]  <- mean(fbar_ts[idx_first])
        tmp$Fbar_last[m]   <- mean(fbar_ts[idx_last])
      }
      
      # AAV metrics
      tmp$catch_aacv[m] <- calculate_aacv(rowSums(om_rep$pred_catch))
      tmp$ssb_aacv[m]   <- calculate_aacv(rowSums(om_rep$SSB))
      tmp$fbar_aacv[m]  <- calculate_aacv(om_rep$Fbar[, ncol(om_rep$Fbar)])
      
      # Prob(F > F_XSPR) – may be NA if BRPs missing or non-finite
      if (!is.null(om_rep$log_SSB_FXSPR) && !is.null(om_rep$log_Fbar_XSPR)) {
        F_global <- om_rep$Fbar[, ncol(om_rep$Fbar)]
        F_XSPR   <- exp(om_rep$log_Fbar_XSPR[, ncol(om_rep$Fbar)])
        
        if (all(is.finite(F_global)) && all(is.finite(F_XSPR))) {
          ratio_F <- F_global / F_XSPR
          ratio_first <- ratio_F[idx_first]
          ratio_last  <- ratio_F[idx_last]
          tmp$prob_first[m] <- mean(ratio_first > 1, na.rm = TRUE)
          tmp$prob_last[m]  <- mean(ratio_last  > 1, na.rm = TRUE)
        } else {
          tmp$prob_first[m] <- NA_real_
          tmp$prob_last[m]  <- NA_real_
        }
      } else {
        tmp$prob_first[m] <- NA_real_
        tmp$prob_last[m]  <- NA_real_
      }
    }
    
    # Define “good” vs “cost” metrics
    metric_cols <- setdiff(names(tmp), "Model")
    
    good_vars <- intersect(metric_cols,
                           c("Catch_first", "SSB_first",
                             "Catch_last",  "SSB_last"))
    cost_vars <- intersect(metric_cols,
                           c("Fbar_first", "Fbar_last",
                             "catch_aacv",  "ssb_aacv", "fbar_aacv",
                             "prob_first",  "prob_last"))
    
    # Larger is better
    for (v in good_vars) {
      x <- tmp[[v]]
      if (all(is.na(x))) {
        tmp[[v]] <- NA_real_
      } else {
        range_val <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
        if (range_val == 0) {
          tmp[[v]] <- 100
        } else {
          tmp[[v]] <- 100 * (x - min(x, na.rm = TRUE)) / range_val
        }
      }
    }
    
    # Smaller is better
    for (v in cost_vars) {
      x <- tmp[[v]]
      if (all(is.na(x))) {
        tmp[[v]] <- NA_real_
      } else {
        range_val <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
        if (range_val == 0) {
          tmp[[v]] <- 100
        } else {
          norm_f   <- (x - min(x, na.rm = TRUE)) / range_val
          tmp[[v]] <- 100 * (1 - norm_f)
        }
      }
    }
    
    tmp
  }
  
  ## ------------------------------------------------------------------
  ## Build across realizations
  ## ------------------------------------------------------------------
  if (is.nsim) {
    n_reps  <- length(mods)
    results <- vector("list", n_reps)
    
    for (r in seq_len(n_reps)) {
      tmp <- compute_metrics_for_rep(mods[[r]])
      tmp$Realization <- r
      results[[r]] <- tmp
    }
    
    combined <- bind_rows(results)
    
    summary_df <- combined |>
      tidyr::pivot_longer(-c(Model, Realization),
                          names_to = "Metric",
                          values_to = "Score") |>
      dplyr::group_by(Model, Metric) |>
      dplyr::summarise(
        Median = median(Score, na.rm = TRUE),
        Q1     = quantile(Score, 0.25, na.rm = TRUE),
        Q3     = quantile(Score, 0.75, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    tmp <- compute_metrics_for_rep(mods)
    tmp$Realization <- 1
    
    summary_df <- tmp |>
      tidyr::pivot_longer(-c(Model, Realization),
                          names_to = "Metric",
                          values_to = "Median") |>
      dplyr::mutate(Q1 = Median, Q3 = Median)
  }
  
  ## ------------------------------------------------------------------
  ## Optional rename of models
  ## ------------------------------------------------------------------
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(summary_df$Model))) {
      stop("Length of new_model_names must match number of models.")
    }
    summary_df$Model <- factor(
      summary_df$Model,
      levels = paste0("Model", seq_along(new_model_names)),
      labels = new_model_names
    )
  }
  
  ## ------------------------------------------------------------------
  ## Metric sets and labels by bar_type
  ## ------------------------------------------------------------------
  base_vars <- c(
    "Catch_first","SSB_first","Fbar_first",
    "Catch_last", "SSB_last", "Fbar_last"
  )
  aav_vars  <- c("catch_aacv","ssb_aacv","fbar_aacv")
  prob_vars <- c("prob_first","prob_last")
  
  if (bar_type %in% c("1","2")) {
    metric_levels <- base_vars
  } else {
    metric_levels <- c(base_vars, aav_vars, prob_vars)
  }
  
  # Expression labels (KEEP ORDER aligned with metric_levels above)
  metric_labels_full <- c(
    expression(bold(Catch[ST])),
    expression(bold(SSB[ST])),
    expression(bold(F[ST])),
    expression(bold(Catch[LT])),
    expression(bold(SSB[LT])),
    expression(bold(F[LT])),
    expression(bold(Catch[AAV])),
    expression(bold(SSB[AAV])),
    expression(bold(F[AAV])),
    expression(bold(P(F[ST] > F[MSY]))),
    expression(bold(P(F[LT] > F[MSY])))
  )
  names(metric_labels_full) <- c(base_vars, aav_vars, prob_vars)
  
  metric_labels <- metric_labels_full[metric_levels]
  
  ## ------------------------------------------------------------------
  ## If bar_type 3/4 and BRP-based probs are all NA/NaN/Inf -> skip
  ## ------------------------------------------------------------------
  if (bar_type %in% c("3","4")) {
    prob_df <- summary_df[summary_df$Metric %in% prob_vars, , drop = FALSE]
    prob_ok <- (nrow(prob_df) > 0) && any(is.finite(prob_df$Median))
    if (!prob_ok) {
      message("Skipping bar_type = ", bar_type,
              ": reference-point-based probabilities are all NA/NaN/Inf.")
      return(invisible(summary_df))
    }
  }
  
  ## ------------------------------------------------------------------
  ## Filter to selected metrics and set Metric factor (NO expression labels here)
  ## ------------------------------------------------------------------
  summary_df <- summary_df[summary_df$Metric %in% metric_levels, , drop = FALSE]
  summary_df$Metric <- factor(summary_df$Metric, levels = metric_levels)
  
  ## ------------------------------------------------------------------
  ## Colors
  ## ------------------------------------------------------------------
  n_models  <- length(unique(summary_df$Model))
  n_metrics <- length(metric_levels)
  
  if (bar_type %in% c("1","3")) {
    # y = Model, fill = Metric
    colors <- viridisLite::viridis(n_metrics, option = col.opt)
    names(colors) <- metric_levels
    
    plot_obj <- ggplot(summary_df,
                       aes(x = Median, y = Model,
                           fill = Metric, color = Metric)) +
      geom_bar(stat = "identity",
               position = position_dodge(width = 0.9),
               width = 0.7) +
      geom_errorbar(aes(xmin = Q1, xmax = Q3),
                    col = "black",
                    position = position_dodge(width = 0.9),
                    width = 0.3, alpha = 0.3) +
      scale_fill_manual(values = colors,
                        breaks = metric_levels,
                        labels = metric_labels) +
      scale_color_manual(values = colors,
                         breaks = metric_levels,
                         labels = metric_labels) +
      theme_bw() +
      labs(title = "Holistic Model Performance",
           x = "Score (0–100)",
           y = "Estimation Model",
           fill = "Metric",
           color = "Metric") +
      theme(
        axis.text  = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        legend.text  = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.title   = element_text(size = 18, face = "bold")
      )
    
  } else {
    # bar_type 2 or 4: y = Metric, fill = Model
    colors <- viridisLite::viridis(n_models, option = col.opt)
    
    plot_obj <- ggplot(summary_df,
                       aes(x = Median, y = Metric,
                           fill = Model, color = Model)) +
      geom_bar(stat = "identity",
               position = position_dodge(width = 0.9),
               width = 0.7) +
      geom_errorbar(aes(xmin = Q1, xmax = Q3),
                    col = "black",
                    position = position_dodge(width = 0.9),
                    width = 0.3, alpha = 0.3) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      scale_y_discrete(
        breaks = metric_levels,
        labels = metric_labels,
        limits = rev(metric_levels)
      ) +
      theme_bw() +
      labs(title = "Holistic Model Performance",
           x = "Score (0–100)",
           y = "Metric",
           fill = "Model",
           color = "Model") +
      theme(
        axis.text  = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        legend.text  = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.title   = element_text(size = 18, face = "bold")
      )
  }
  
  print(plot_obj)
  
  ## ------------------------------------------------------------------
  ## Save
  ## ------------------------------------------------------------------
  out_dir <- file.path(main.dir, sub.dir, "Holistic_Bar_Plot")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  out_file <- file.path(out_dir,
                        paste0("Overall_Performance_type", bar_type, ".png"))
  
  ggsave(filename = out_file,
         plot = plot_obj, width = width, height = height, dpi = dpi)
  
  invisible(summary_df)
}

plot_rec_and_NAA_par <- function(mods, is.nsim, main.dir, sub.dir,
                                 width = 10, height = 10, dpi = 300,
                                 col.opt = "D",
                                 outlier.opt = NA,
                                 new_model_names = NULL) {
  
  library(dplyr)
  library(ggplot2)
  library(viridisLite)
  library(stringr)
  
  ## --------------------------------------------------
  ## 0. True values from OM
  ## --------------------------------------------------
  om_tmp <- if (is.nsim) mods[[1]][[1]] else mods[[1]]
  mean_rec_true <- exp(om_tmp$om$parList$mean_rec_pars[, 1])
  rec_sig_true  <- exp(om_tmp$om$parList$log_NAA_sigma[, 1, 1])
  naa_sig_true  <- exp(om_tmp$om$parList$log_NAA_sigma[, 1, 2])
  is.n.regions  <- om_tmp$om$input$data$n_regions > 1
  
  ## --------------------------------------------------
  ## 1. Collect results for mean_rec_par and log_NAA_sigma
  ## --------------------------------------------------
  res_rec <- NULL
  res_sig <- NULL
  
  if (is.nsim) {
    # multiple realizations
    for (i in seq_along(mods)) {
      for (j in seq_along(mods[[1]])) {
        tmp <- mods[[i]][[j]]
        k   <- length(tmp$par.est)
        
        ## --- mean_rec_pars ---
        if (any(names(tmp$par.est[[k]]) == "mean_rec_pars")) {
          temp_rec <- exp(tmp$par.est[[k]]$mean_rec_pars[, 1])
        } else {
          m <- length(tmp$par.est[[k]])
          temp_rec <- NULL
          for (n in seq_len(m)) {
            temp1 <- exp(tmp$par.est[[k]][[n]]$mean_rec_pars[, 1])
            temp_rec <- c(temp_rec, temp1)
          }
        }
        
        res1_rec <- data.frame(
          Model = j,
          nsim  = i,
          Value = temp_rec,
          Var   = if (length(temp_rec) == 1) {
            "Mean_Rec"
          } else {
            paste0("Mean_Rec_", seq_along(temp_rec))
          }
        )
        
        res_rec <- rbind(res_rec, res1_rec)
        
        ## --- log_NAA_sigma (rec_sigma & naa_sigma) ---
        if (any(names(tmp$par.est[[k]]) == "log_NAA_sigma")) {
          rec_sig <- exp(tmp$par.est[[k]]$log_NAA_sigma[, 1, 1])
          naa_sig <- exp(tmp$par.est[[k]]$log_NAA_sigma[, 1, 2])
          temp_sig <- c(rec_sig, naa_sig)
        } else {
          m <- length(tmp$par.est[[k]])
          temp_sig <- NULL
          for (n in seq_len(m)) {
            rec_sig <- exp(tmp$par.est[[k]][[n]]$log_NAA_sigma[, 1, 1])
            naa_sig <- exp(tmp$par.est[[k]][[n]]$log_NAA_sigma[, 1, 2])
            temp_sig <- c(temp_sig, rec_sig, naa_sig)
          }
        }
        
        res1_sig <- data.frame(
          Model = j,
          nsim  = i,
          Value = temp_sig,
          Var   = if (length(temp_sig) == 2) {
            c("Rec_sigma", "NAA_sigma")
          } else {
            c(paste0("Rec_sigma", seq_along(rec_sig)),
              paste0("NAA_sigma", seq_along(naa_sig)))
          }
        )
        
        res_sig <- rbind(res_sig, res1_sig)
      }
    }
  } else {
    # single realization
    for (j in seq_along(mods)) {
      tmp <- mods[[j]]
      k   <- length(tmp$par.est)
      
      ## --- mean_rec_pars ---
      if (any(names(tmp$par.est[[k]]) == "mean_rec_pars")) {
        temp_rec <- exp(tmp$par.est[[k]]$mean_rec_pars[, 1])
      } else {
        m <- length(tmp$par.est[[k]])
        temp_rec <- NULL
        for (n in seq_len(m)) {
          temp1 <- exp(tmp$par.est[[k]][[n]]$mean_rec_pars[, 1])
          temp_rec <- c(temp_rec, temp1)
        }
      }
      
      res1_rec <- data.frame(
        Model = j,
        Value = temp_rec,
        Var   = if (length(temp_rec) == 1) {
          "Mean_Rec"
        } else {
          paste0("Mean_Rec_", seq_along(temp_rec))
        }
      )
      res_rec <- rbind(res_rec, res1_rec)
      
      ## --- log_NAA_sigma ---
      if (any(names(tmp$par.est[[k]]) == "log_NAA_sigma")) {
        rec_sig <- exp(tmp$par.est[[k]]$log_NAA_sigma[, 1, 1])
        naa_sig <- exp(tmp$par.est[[k]]$log_NAA_sigma[, 1, 2])
        temp_sig <- c(rec_sig, naa_sig)
      } else {
        m <- length(tmp$par.est[[k]])
        temp_sig <- NULL
        for (n in seq_len(m)) {
          rec_sig <- exp(tmp$par.est[[k]][[n]]$log_NAA_sigma[, 1, 1])
          naa_sig <- exp(tmp$par.est[[k]][[n]]$log_NAA_sigma[, 1, 2])
          temp_sig <- c(temp_sig, rec_sig, naa_sig)
        }
      }
      
      res1_sig <- data.frame(
        Model = j,
        Value = temp_sig,
        Var   = if (length(temp_sig) == 2) {
          c("Rec_sigma", "NAA_sigma")
        } else {
          c(paste0("Rec_sigma", seq_along(rec_sig)),
            paste0("NAA_sigma", seq_along(naa_sig)))
        }
      )
      res_sig <- rbind(res_sig, res1_sig)
    }
  }
  
  ## --------------------------------------------------
  ## 2. Factor Model and optional renaming
  ## --------------------------------------------------
  res_rec$Model <- factor(paste0("Model", res_rec$Model))
  res_sig$Model <- factor(paste0("Model", res_sig$Model))
  
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res_rec$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res_rec$Model <- factor(res_rec$Model,
                            levels = paste0("Model", seq_along(new_model_names)),
                            labels = new_model_names)
    res_sig$Model <- factor(res_sig$Model,
                            levels = paste0("Model", seq_along(new_model_names)),
                            labels = new_model_names)
  }
  
  ## --------------------------------------------------
  ## 3. Attach True Values for mean_rec
  ## --------------------------------------------------
  res_rec$True_Value <- NA_real_
  
  if (length(unique(res_rec$Var)) > 1) {
    # entries like Mean_Rec_1, Mean_Rec_2, etc.
    is_indexed <- grepl("^Mean_Rec_\\d+$", res_rec$Var)
    rec_idx    <- as.numeric(str_extract(res_rec$Var[is_indexed], "\\d+"))
    res_rec$True_Value[is_indexed] <- mean_rec_true[rec_idx]
    
    # base "Mean_Rec"
    res_rec$True_Value[res_rec$Var == "Mean_Rec"] <- mean_rec_true[1]
    
    if (length(res_rec$Var) > 1) {
      # avoid double-using True_Value for aggregated things
      res_rec$True_Value[res_rec$Var == "Mean_Rec"] <- NA_real_
    }
  } else {
    if (is.n.regions) {
      is_indexed <- grepl("Mean_Rec", res_rec$Var)
      res_rec$True_Value[is_indexed] <- sum(mean_rec_true)
    } else {
      is_indexed <- grepl("Mean_Rec", res_rec$Var)
      rec_idx    <- as.numeric(str_extract(res_rec$Var[is_indexed], "\\d+"))
      res_rec$True_Value[is_indexed] <- mean_rec_true[rec_idx]
      res_rec$True_Value[res_rec$Var == "Mean_Rec"] <- mean_rec_true[1]
    }
  }
  
  ## --------------------------------------------------
  ## 4. Attach True Values for NAA sigmas
  ## --------------------------------------------------
  res_sig$True_Value <- NA_real_
  
  if (!is.n.regions) {
    if (length(unique(res_sig$Var)) == 2 &&
        all(sort(unique(res_sig$Var)) == c("NAA_sigma","Rec_sigma"))) {
      # Simple case
      res_sig$True_Value[res_sig$Var == "Rec_sigma"]  <- rec_sig_true
      res_sig$True_Value[res_sig$Var == "NAA_sigma"]  <- naa_sig_true
    } else {
      # Multiple, match by index
      rec_idx <- as.numeric(gsub("Rec_sigma", "", res_sig$Var[grepl("Rec_sigma", res_sig$Var)]))
      naa_idx <- as.numeric(gsub("NAA_sigma", "", res_sig$Var[grepl("NAA_sigma", res_sig$Var)]))
      
      res_sig$True_Value[grepl("Rec_sigma", res_sig$Var)] <-
        rec_sig_true[rec_idx]
      res_sig$True_Value[grepl("NAA_sigma", res_sig$Var)] <-
        naa_sig_true[naa_idx]
    }
  }
  
  ## --------------------------------------------------
  ## 5. Make directory
  ## --------------------------------------------------
  new_sub_dir <- file.path(main.dir, sub.dir, "Diagnostic_Results")
  if (!file.exists(new_sub_dir)) dir.create(new_sub_dir, recursive = TRUE)
  
  ## --------------------------------------------------
  ## 6. Plot 1: Mean Recruitment Pars
  ## --------------------------------------------------
  p_rec <- ggplot(res_rec, aes(x = Model, y = Value, col = Model)) +
    geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
    geom_hline(aes(yintercept = True_Value),
               col = "red", linetype = "dashed") +
    facet_grid(Var ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle("Mean Recruitment from the Last EM") +
    ylab("") +
    theme_bw() +
    theme(
      axis.text   = element_text(size = 10),
      axis.title  = element_text(size = 15),
      plot.title  = element_text(size = 12),
      strip.text  = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 10),
      legend.title= element_text(size = 10)
    )
  
  ggsave(file.path(new_sub_dir, "Mean_rec_par.png"),
         p_rec, width = width, height = height, dpi = dpi)
  
  ## --------------------------------------------------
  ## 7. Plot 2: NAA Sigma Pars
  ## --------------------------------------------------
  p_sig <- ggplot(res_sig, aes(x = Model, y = Value, col = Model)) +
    geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
    { if (!is.n.regions)
      geom_hline(aes(yintercept = True_Value),
                 col = "red", linetype = "dashed") } +
    facet_grid(Var ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle("Standard Deviation of NAA from the Last EM") +
    ylab("") +
    theme_bw() +
    theme(
      axis.text   = element_text(size = 10),
      axis.title  = element_text(size = 15),
      plot.title  = element_text(size = 12),
      strip.text  = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 10),
      legend.title= element_text(size = 10)
    )
  
  ggsave(file.path(new_sub_dir, "Variance_Para_NAA.png"),
         p_sig, width = width, height = height, dpi = dpi)
  
  ## --------------------------------------------------
  ## 8. Return both plots
  ## --------------------------------------------------
  invisible(list(mean_rec_plot = p_rec,
                 naa_sigma_plot = p_sig))
}
#' Plot average annual variation for Catch, SSB, or Fbar
#'
#' Computes **Average Annual Variation (AAV)** over a selected time window
#' (from \code{start.years} through the end of the time series) for each model
#' (and realization, if \code{is.nsim = TRUE}). AAV is computed as:
#' \deqn{AAV = \frac{\sum_t |x_{t+1}-x_t|}{\sum_t x_t}}
#' where the denominator uses all but the last year (consistent with the diff).
#'
#' The function supports:
#' \itemize{
#'   \item \strong{Catch}: from \code{om$rep$pred_catch} (years x fleets);
#'         produces fleet-level and global (sum over fleets) AAV.
#'   \item \strong{SSB}: from \code{om$rep$SSB} (years x regions or years x 1);
#'         produces region-level and global (rowSums) AAV.
#'   \item \strong{Fbar}: from \code{om$rep$Fbar} (years x columns);
#'         produces column-level and global (last column) AAV.
#' }
#'
#' If \code{base.model} is provided, AAV is converted to a relative difference:
#' \deqn{\text{RelAAV} = \frac{AAV}{AAV_{base}} - 1}
#'
#' @param mods List of model outputs. If \code{is.nsim = FALSE}, it should be a list
#'   over models: \code{mods[[m]]}. If \code{is.nsim = TRUE}, it should be nested:
#'   \code{mods[[r]][[m]]} for realizations \code{r} and models \code{m}.
#' @param is.nsim Logical. Whether \code{mods} is nested by realizations.
#' @param main.dir Main directory for saving outputs.
#' @param sub.dir Subdirectory under \code{main.dir} for saving outputs.
#' @param metric Character. One of \code{"Catch"}, \code{"SSB"}, or \code{"Fbar"}.
#' @param start.years Either (i) a 1-based index into the time series (e.g. 1, 6, 10),
#'   or (ii) an actual year value that appears in \code{om$years} (e.g. 1982).
#'   The AAV is computed from that starting position to the end.
#' @param width,height,dpi Output size/resolution for PNG.
#' @param col.opt Viridis discrete option (passed to \code{scale_color_viridis_d}).
#' @param outlier.opt Outlier shape for boxplots; use \code{NA} to hide outliers.
#' @param new_model_names Optional character vector to relabel models.
#'   Length must equal number of models.
#' @param base.model Optional. If not \code{NULL}, compute relative AAV vs this model.
#'   Must match the model labels (after \code{new_model_names} if supplied).
#'
#' @return A \code{ggplot} object (and saves PNG).
#' @export
plot_annual_variation <- function(mods, is.nsim,
                                  main.dir, sub.dir,
                                  metric = c("Catch", "SSB", "Fbar"),
                                  start.years = 1,
                                  width = 10, height = 7, dpi = 300, col.opt = "D",
                                  outlier.opt = NA,
                                  new_model_names = NULL,
                                  base.model = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  metric <- match.arg(metric, c("Catch", "SSB", "Fbar"))
  
  ## -------------------------
  ## Helpers
  ## -------------------------
  calculate_aav <- function(x) {
    x <- as.numeric(x)
    if (length(x) < 2) return(NA_real_)
    
    # keep ordering; drop only leading/trailing non-finite if needed
    ok <- is.finite(x)
    if (sum(ok) < 2) return(NA_real_)
    x <- x[ok]
    
    denom <- sum(x[-length(x)], na.rm = TRUE)
    if (!is.finite(denom) || denom == 0) return(NA_real_)
    num <- sum(abs(diff(x)), na.rm = TRUE)
    num / denom
  }
  
  start_to_row <- function(years_vec, start_y) {
    n <- length(years_vec)
    if (is.null(start_y) || length(start_y) != 1 || is.na(start_y)) return(1L)
    
    # Prefer exact year match only if it truly looks like a year vector
    idx_year <- which(years_vec == start_y)
    if (length(idx_year) > 0) return(as.integer(idx_year[1]))
    
    # Otherwise treat as 1-based index
    idx <- suppressWarnings(as.integer(start_y))
    if (!is.finite(idx)) return(1L)
    idx <- max(1L, min(n, idx))
    as.integer(idx)
  }
  
  extract_metric_mat <- function(one, metric) {
    if (metric == "Catch") {
      mat <- one$om$rep$pred_catch
      prefix <- "Catch_Fleet"
      global_fun <- function(M) rowSums(M, na.rm = TRUE)
      global_name <- "Catch_Global"
      return(list(mat = mat, prefix = prefix, global_fun = global_fun, global_name = global_name))
    }
    if (metric == "SSB") {
      mat <- one$om$rep$SSB
      prefix <- "SSB_Region"
      global_fun <- function(M) {
        if (is.null(dim(M))) return(as.numeric(M))
        if (ncol(M) == 1) return(as.numeric(M[, 1]))
        rowSums(M, na.rm = TRUE)
      }
      global_name <- "SSB_Global"
      return(list(mat = mat, prefix = prefix, global_fun = global_fun, global_name = global_name))
    }
    # Fbar
    mat <- one$om$rep$Fbar
    prefix <- "Fbar_Col"
    global_fun <- function(M) {
      if (is.null(dim(M))) return(as.numeric(M))
      as.numeric(M[, ncol(M)])
    }
    global_name <- "Fbar_Global"
    return(list(mat = mat, prefix = prefix, global_fun = global_fun, global_name = global_name))
  }
  
  ## -------------------------
  ## Build results table
  ## -------------------------
  build_one_row <- function(one, m, r) {
    info <- extract_metric_mat(one, metric)
    mat <- info$mat
    if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1)
    
    years_vec <- tryCatch(one$om$years, error = function(e) NULL)
    if (is.null(years_vec) || length(years_vec) != nrow(mat)) years_vec <- seq_len(nrow(mat))
    
    i0 <- start_to_row(years_vec, start.years)
    mat2 <- mat[i0:nrow(mat), , drop = FALSE]
    
    local_vals <- vapply(seq_len(ncol(mat2)), function(j) calculate_aav(mat2[, j]), numeric(1))
    global_val <- calculate_aav(info$global_fun(mat2))
    
    out <- data.frame(
      Model = paste0("Model", m),
      Realization = r,
      stringsAsFactors = FALSE
    )
    for (j in seq_len(ncol(mat2))) {
      out[[paste0(info$prefix, j)]] <- local_vals[j]
    }
    out[[info$global_name]] <- global_val
    out
  }
  
  if (!is.nsim) {
    res <- bind_rows(lapply(seq_along(mods), function(m) build_one_row(mods[[m]], m, 1L)))
  } else {
    res <- bind_rows(lapply(seq_along(mods), function(r) {
      bind_rows(lapply(seq_along(mods[[r]]), function(m) build_one_row(mods[[r]][[m]], m, r)))
    }))
  }
  
  ## -------------------------
  ## Rename models if requested
  ## -------------------------
  model_levels <- sort(unique(as.character(res$Model)))
  
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(model_levels)) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model, levels = model_levels, labels = new_model_names)
    
    # If base.model was given in old names, map it
    if (!is.null(base.model) && base.model %in% model_levels && !(base.model %in% new_model_names)) {
      base.model <- new_model_names[match(base.model, model_levels)]
    }
    if (!is.null(base.model) && !(base.model %in% levels(res$Model))) {
      warning("base.model does not match any model label after applying new_model_names.")
    }
  } else {
    res$Model <- factor(res$Model, levels = model_levels)
  }
  
  ## -------------------------
  ## Long format + relative option
  ## -------------------------
  metric_cols <- setdiff(names(res), c("Model", "Realization"))
  
  res_long <- res %>%
    pivot_longer(cols = all_of(metric_cols), names_to = "Label", values_to = "AAV")
  
  if (!is.null(base.model)) {
    base_df <- res_long %>%
      filter(Model == base.model) %>%
      rename(base_val = AAV) %>%
      select(Realization, Label, base_val)
    
    res_long <- res_long %>%
      left_join(base_df, by = c("Realization", "Label")) %>%
      mutate(AAV = ifelse(is.finite(base_val) & base_val != 0, AAV / base_val - 1, NA_real_))
  }
  
  ## -------------------------
  ## Plot
  ## -------------------------
  title_suffix <- if (!is.null(base.model)) paste0(" (Relative to ", base.model, ")") else ""
  ylab_txt <- if (!is.null(base.model)) "Relative AAV Difference" else "AAV"
  
  p <- ggplot(res_long, aes(x = Model, y = AAV, color = Model)) +
    geom_boxplot(lwd = 0.8, outlier.shape = outlier.opt) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(paste0("Average Annual Variation: ", metric, title_suffix,
                   "\nFrom start.years = ", start.years, " to end")) +
    ylab(ylab_txt) +
    xlab("Model") +
    theme_bw()
  
  ## -------------------------
  ## Save
  ## -------------------------
  out_dir <- file.path(main.dir, sub.dir, "Annual_Variation_Boxplot")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  start_tag <- paste0("start_", start.years)
  rel_tag <- ifelse(is.null(base.model), "", "_Relative")
  plot_name <- paste0(metric, "_AAV_", start_tag, rel_tag, ".png")
  
  ggsave(filename = file.path(out_dir, plot_name),
         plot = p, width = width, height = height, dpi = dpi)
  
  return(p)
}

plot_model_performance_holistic <- function(mods, is.nsim,
                                            main.dir = ".",
                                            sub.dir  = ".",
                                            new_model_names = NULL,
                                            width = 12, height = 8, dpi = 300,
                                            col.opt = "D",
                                            method = NULL,
                                            use.n.years.first = 5,
                                            use.n.years.last  = 5,
                                            start.years = 1,
                                            plot_type = c("rose","heat"),
                                            facet_ncol = 4,
                                            highlight_best = TRUE) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridisLite)
  
  if (is.null(method)) method <- "median"
  plot_type <- match.arg(plot_type)
  
  ## -----------------------------------------------------------
  ## Helper: Average Annual Catch Variation
  ## -----------------------------------------------------------
  calculate_aacv <- function(values) {
    values <- values[is.finite(values)]
    if (length(values) < 2) return(NA_real_)
    diffs <- abs(diff(values))
    denom <- sum(values[-length(values)])
    if (!is.finite(denom) || denom <= 0) return(NA_real_)
    sum(diffs) / denom
  }
  
  ## -----------------------------------------------------------
  ## Helper: compute normalized metrics for one realization
  ## -----------------------------------------------------------
  compute_metrics_for_rep <- function(mod_list) {
    
    n_models <- length(mod_list)
    tmp <- data.frame(Model = paste0("Model", seq_len(n_models)))
    
    for (m in seq_len(n_models)) {
      
      rep <- mod_list[[m]]$om$rep
      
      catch_ts <- rowSums(rep$pred_catch)
      ssb_ts   <- rowSums(rep$SSB)
      fbar_ts  <- rep$Fbar[, ncol(rep$Fbar)]
      
      idx_first <- start.years:(start.years + use.n.years.first - 1)
      idx_last  <- (length(catch_ts) - use.n.years.last + 1):length(catch_ts)
      
      stat_fun <- if (method == "median") median else mean
      
      tmp$Catch_first[m] <- stat_fun(catch_ts[idx_first])
      tmp$SSB_first[m]   <- stat_fun(ssb_ts[idx_first])
      tmp$Catch_last[m]  <- stat_fun(catch_ts[idx_last])
      tmp$SSB_last[m]    <- stat_fun(ssb_ts[idx_last])
      
      tmp$catch_aacv[m] <- calculate_aacv(catch_ts)
      tmp$ssb_aacv[m]   <- calculate_aacv(ssb_ts)
      tmp$fbar_aacv[m]  <- calculate_aacv(fbar_ts)
      
      ## --- Overfishing / Overfished probabilities ---
      if (!is.null(rep$log_Fbar_XSPR_static) &&
          !is.null(rep$log_SSB_FXSPR_static)) {
        
        Fref   <- exp(rep$log_Fbar_XSPR_static[7])
        SSBref <- exp(rep$log_SSB_FXSPR_static[3])
        
        Frat  <- fbar_ts / Fref
        SSBr  <- ssb_ts  / SSBref
        
        tmp$prob_first[m] <- mean(Frat[idx_first] > 1, na.rm = TRUE)
        tmp$prob_last[m]  <- mean(tail(Frat, use.n.years.last) > 1, na.rm = TRUE)
        tmp$prob_SSB_first[m] <- mean(SSBr[idx_first] < 1, na.rm = TRUE)
        tmp$prob_SSB_last[m]  <- mean(tail(SSBr, use.n.years.last) < 1, na.rm = TRUE)
      } else {
        tmp[, c("prob_first","prob_last",
                "prob_SSB_first","prob_SSB_last")] <- NA_real_
      }
    }
    
    ## ---- normalize (higher is better except costs) ----
    good_vars <- c("Catch_first","SSB_first","Catch_last","SSB_last")
    cost_vars <- setdiff(names(tmp), c("Model", good_vars))
    
    for (v in good_vars) {
      x <- tmp[[v]]
      r <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
      tmp[[v]] <- if (r == 0) 100 else 100 * (x - min(x, na.rm = TRUE)) / r
    }
    
    for (v in cost_vars) {
      x <- tmp[[v]]
      r <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
      tmp[[v]] <- if (r == 0) 100 else 100 * (1 - (x - min(x, na.rm = TRUE)) / r)
    }
    
    tmp
  }
  
  ## -----------------------------------------------------------
  ## Aggregate across realizations
  ## -----------------------------------------------------------
  if (is.nsim) {
    
    res <- lapply(seq_along(mods), function(r) {
      x <- compute_metrics_for_rep(mods[[r]])
      x$Realization <- r
      x
    })
    
    combined <- bind_rows(res)
    
    summary_df <- combined |>
      pivot_longer(-c(Model, Realization),
                   names_to = "Metric",
                   values_to = "Score") |>
      group_by(Model, Metric) |>
      summarise(
        Median = median(Score, na.rm = TRUE),
        Q1     = quantile(Score, 0.25, na.rm = TRUE),
        Q3     = quantile(Score, 0.75, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    
    tmp <- compute_metrics_for_rep(mods)
    summary_df <- tmp |>
      pivot_longer(-Model,
                   names_to = "Metric",
                   values_to = "Median") |>
      mutate(Q1 = Median, Q3 = Median)
  }
  
  ## -----------------------------------------------------------
  ## Optional rename models
  ## -----------------------------------------------------------
  if (!is.null(new_model_names)) {
    summary_df$Model <- factor(
      summary_df$Model,
      levels = paste0("Model", seq_along(new_model_names)),
      labels = new_model_names
    )
  }
  
  ## -----------------------------------------------------------
  ## Metric order + labels
  ## -----------------------------------------------------------
  metric_levels <- c(
    "Catch_first","SSB_first",
    "Catch_last","SSB_last",
    "catch_aacv","ssb_aacv","fbar_aacv",
    "prob_first","prob_last",
    "prob_SSB_first","prob_SSB_last"
  )
  
  metric_labels <- c(
    expression(bold(Catch[ST])),
    expression(bold(SSB[ST])),
    expression(bold(Catch[LT])),
    expression(bold(SSB[LT])),
    expression(bold(Catch[AAV])),
    expression(bold(SSB[AAV])),
    expression(bold(F[AAV])),
    expression(bold(P(F[ST] > F[MSY]))),
    expression(bold(P(F[LT] > F[MSY]))),
    expression(bold(P(SSB[ST] < SSB[MSY]))),
    expression(bold(P(SSB[LT] < SSB[MSY])))
  )
  
  names(metric_labels) <- metric_levels
  
  plot_df <- summary_df |>
    filter(Metric %in% metric_levels) |>
    mutate(
      Metric = factor(Metric, levels = metric_levels),
      Score01 = Median / 100
    )
  
  ## -----------------------------------------------------------
  ## Plot
  ## -----------------------------------------------------------
  if (plot_type == "rose") {
    
    if (highlight_best) {
      best <- plot_df |>
        group_by(Model) |>
        summarise(Total = sum(Score01), .groups = "drop") |>
        filter(Total == max(Total)) |>
        mutate(is_best = TRUE)
      
      plot_df <- plot_df |>
        left_join(best[, c("Model","is_best")], by = "Model") |>
        mutate(is_best = ifelse(is.na(is_best), FALSE, is_best))
    }
    
    p <- ggplot(plot_df,
                aes(x = Model, y = Score01, fill = Metric)) +
      geom_col(color = "white", width = 1) +
      coord_polar(theta = "x") +
      scale_fill_viridis_d(option = col.opt,
                           breaks = metric_levels,
                           labels = metric_labels) +
      labs(title = "Holistic Performance",
           x = NULL, y = "Normalized score", fill = NULL) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 9),
        plot.title  = element_text(face = "bold", hjust = 0.5)
      )
    
    if (highlight_best) {
      p <- p + geom_col(
        data = subset(plot_df, is_best),
        fill = NA, size = 0.3, width = 1
      )
    }
    
  } else {
    
    p <- ggplot(plot_df,
                aes(x = Model, y = Metric, fill = Score01)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = col.opt, name = "Score (0–1)") +
      scale_y_discrete(labels = metric_labels) +
      labs(title = "Holistic Performance",
           x = "Estimation model", y = NULL) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 9),
        plot.title  = element_text(face = "bold", hjust = 0.5)
      )
  }
  
  print(p)
  
  ## -----------------------------------------------------------
  ## Save
  ## -----------------------------------------------------------
  out_dir <- file.path(main.dir, sub.dir, "Holistic_Performance")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  ggsave(
    file.path(out_dir,
              paste0("Holistic_", plot_type, ".png")),
    p, width = width, height = height, dpi = dpi
  )
  
  invisible(summary_df)
}


plot_relative_trajectories <- function(mods, is.nsim,
                                       main.dir, sub.dir,
                                       base.model = NULL,
                                       new_model_names = NULL,
                                       width = 10, height = 7, dpi = 300,
                                       col.opt = "D",
                                       user.Q1 = NULL,
                                       user.Q3 = NULL) {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(rlang)
  
  # ---- Helper: extract relative trajectories ----
  extract_relative_trajectories <- function(mods, base_em_idx) {
    
    data <- NULL
    Years <- if (!is.nsim) mods[[1]]$om$years else mods[[1]][[1]]$om$years
    
    if (!is.nsim) {
      n_stocks <- ncol(mods[[1]]$om$rep$SSB)
      n_fleets <- ncol(mods[[1]]$om$rep$pred_catch)
      
      for (m in seq_along(mods)) {
        ssb_mat <- mods[[m]]$om$rep$SSB
        ssb_base <- mods[[base_em_idx]]$om$rep$SSB
        catch_mat <- mods[[m]]$om$rep$pred_catch
        catch_base <- mods[[base_em_idx]]$om$rep$pred_catch
        
        # SSB per stock
        for (s in seq_len(n_stocks)) {
          tmp <- ssb_mat[, s] / ssb_base[, s] - 1
          res <- data.frame(Realization = 1, EM = m, Years = Years,
                            Index = paste0("SSB_", s), Value = tmp)
          data <- bind_rows(data, res)
        }
        
        # SSB global
        tmp <- rowSums(ssb_mat) / rowSums(ssb_base) - 1
        res <- data.frame(Realization = 1, EM = m, Years = Years,
                          Index = "SSB_Global", Value = tmp)
        data <- bind_rows(data, res)
        
        # Catch per fleet
        for (f in seq_len(n_fleets)) {
          tmp <- catch_mat[, f] / catch_base[, f] - 1
          res <- data.frame(Realization = 1, EM = m, Years = Years,
                            Index = paste0("Catch_", f), Value = tmp)
          data <- bind_rows(data, res)
        }
        
        # Catch global
        tmp <- rowSums(catch_mat) / rowSums(catch_base) - 1
        res <- data.frame(Realization = 1, EM = m, Years = Years,
                          Index = "Catch_Global", Value = tmp)
        data <- bind_rows(data, res)
      }
      
    } else {
      n_stocks <- ncol(mods[[1]][[1]]$om$rep$SSB)
      n_fleets <- ncol(mods[[1]][[1]]$om$rep$pred_catch)
      
      for (r in seq_along(mods)) {
        for (m in seq_along(mods[[r]])) {
          ssb_mat <- mods[[r]][[m]]$om$rep$SSB
          ssb_base <- mods[[r]][[base_em_idx]]$om$rep$SSB
          catch_mat <- mods[[r]][[m]]$om$rep$pred_catch
          catch_base <- mods[[r]][[base_em_idx]]$om$rep$pred_catch
          
          # SSB per stock
          for (s in seq_len(n_stocks)) {
            tmp <- ssb_mat[, s] / ssb_base[, s] - 1
            res <- data.frame(Realization = r, EM = m, Years = Years,
                              Index = paste0("SSB_", s), Value = tmp)
            data <- bind_rows(data, res)
          }
          
          # SSB global
          tmp <- rowSums(ssb_mat) / rowSums(ssb_base) - 1
          res <- data.frame(Realization = r, EM = m, Years = Years,
                            Index = "SSB_Global", Value = tmp)
          data <- bind_rows(data, res)
          
          # Catch per fleet
          for (f in seq_len(n_fleets)) {
            tmp <- catch_mat[, f] / catch_base[, f] - 1
            res <- data.frame(Realization = r, EM = m, Years = Years,
                              Index = paste0("Catch_", f), Value = tmp)
            data <- bind_rows(data, res)
          }
          
          # Catch global
          tmp <- rowSums(catch_mat) / rowSums(catch_base) - 1
          res <- data.frame(Realization = r, EM = m, Years = Years,
                            Index = "Catch_Global", Value = tmp)
          data <- bind_rows(data, res)
        }
      }
    }
    
    return(data)
  }
  
  # --- Determine base_em_idx ---
  n_models <- if (!is.nsim) length(mods) else length(mods[[1]])
  
  if (is.null(new_model_names)) {
    new_model_names <- paste0("Model", seq_len(n_models))
  }
  
  if (is.character(base.model)) {
    base_em_idx <- which(new_model_names == base.model)
    if (length(base_em_idx) == 0) stop("base.model not found in new_model_names.")
  } else {
    stop("base.model must be specified as a model name (e.g., 'Model1').")
  }
  
  # --- Extract data ---
  data <- extract_relative_trajectories(mods, base_em_idx = base_em_idx)
  
  # --- Rename EMs ---
  data$EM <- factor(data$EM,
                    levels = seq_along(new_model_names),
                    labels = new_model_names)
  
  # --- Summarize ---
  if(is.null(user.Q1)) user.Q1 = 0.4
  if(is.null(user.Q3)) user.Q3 = 0.6
  sum_data <- data %>%
    group_by(EM, Index, Years) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, user.Q1, na.rm = TRUE),
      Q3 = quantile(Value, user.Q3, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create the new subfolder if it doesn't exist
  new_sub_dir <- file.path(main.dir, sub.dir, "Relative_Trajectory")
  
  if (!file.exists(new_sub_dir)){
    dir.create(new_sub_dir)
  }
  
  plot = list()
  
  t = 0
  # --- Plot loop ---
  for (name in unique(sum_data$Index)) {
    
    subset_data <- filter(sum_data, Index == name)
    
    t = t + 1
    
    plot[[t]] <- ggplot(subset_data, aes(x = Years, y = Median, color = EM)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = EM), alpha = 0.1, color = NA) +
      scale_color_viridis_d(option = col.opt) +
      scale_fill_viridis_d(option = col.opt) +
      labs(x = "Years", y = "Relative Difference", color = "EM", fill = "EM") +
      ggtitle(name) +
      theme_bw() +
      geom_hline(yintercept = 0, col = "red", linetype = "dashed")
    
    plot_name <- paste0(name,"_trajectories.png")
    
    ggsave(file.path(new_sub_dir, plot_name), plot[[t]], width = width, height = height, dpi = dpi)
    
    # return(list(plot[[t]]))
    # print(plot[[t]])
  }
  print(plot[[t]])
}
