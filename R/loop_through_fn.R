#' Perform Management Strategy Evaluation (MSE)
#'
#' Iteratively fit estimation models, generate catch advice, optionally apply
#' implementation error, and update the operating model over a sequence of
#' assessment years.
#'
#' This function is the main wrapper for linking an operating model (OM) and an
#' estimation model (EM) within a management strategy evaluation (MSE)
#' framework. At each assessment year, it constructs the EM input from the
#' current OM state, fits the EM, evaluates convergence, generates projected
#' catch advice under a specified harvest control rule (HCR), optionally
#' converts advised catch to realized catch through implementation error, and
#' then updates the OM forward over the assessment interval.
#'
#' The function supports both a single combined EM and several separate-EM
#' configurations. Separate EMs may be used for spatially aggregated
#' panmictic-style advice with later reallocation back to fleets, fleets-as-
#' areas formulations, or fully separate stock- or region-specific fits,
#' depending on \code{em.opt$separate.em.type}.
#'
#' If \code{assess_interval = 0}, the function behaves as a simulation-
#' estimation workflow. In that case, the EM is fit at each assessment year,
#' but no projected catch advice is generated and the OM is not advanced
#' forward.
#'
#' @param om List. Operating model object containing simulated population
#'   dynamics, observed data, input structures, and all information required by
#'   \code{\link{make_em_input}} and \code{\link{update_om_fn}}.
#'
#' @param em_info List. Estimation-model information used to construct the EM
#'   input. This object is typically passed directly to
#'   \code{\link{make_em_input}} and may include biological settings, fleet and
#'   survey structures, parameter mappings, priors, initial values, and other
#'   EM-specific configuration settings.
#'
#' @param random Character vector or \code{NULL}. Names of OM processes treated
#'   as random effects when the OM is updated forward. Passed to
#'   \code{\link{update_om_fn}}.
#'
#' @param M_em Optional object specifying natural mortality settings in the EM.
#'
#' @param sel_em Optional object specifying selectivity settings in the EM.
#'
#' @param NAA_re_em Optional object specifying numbers-at-age random-effects
#'   settings in the EM.
#'
#' @param move_em Optional object specifying movement settings in the EM.
#'   Movement must be \code{NULL} when \code{em.opt$separate.em = TRUE}. When a
#'   combined EM is used, \code{move_em} is only used if
#'   \code{em.opt$do.move = TRUE}.
#'
#' @param catchability_em Optional object specifying survey catchability
#'   settings in the EM.
#'
#' @param ecov_em Optional object specifying environmental covariates in the
#'   EM. This may include latent environmental processes, observation error for
#'   environmental covariates, and links between environmental covariates and
#'   biological processes such as recruitment, mortality, catchability, or
#'   movement.
#'
#'   When \code{gauss_rec_em$use = TRUE}, \code{ecov_em} should still provide
#'   the environmental covariate time series when needed by the EM, but users
#'   generally should not also specify an additional standard Ecov effect on
#'   recruitment unless multiple recruitment-environment effects are intended.
#'
#' @param ecov_em_opts List or \code{NULL}. Optional settings controlling how
#'   environmental covariates are handled during projection and catch-advice
#'   generation. These options are passed to \code{\link{advice_fn}} and may be
#'   used to override or project Ecov values in the forecast period.
#'
#'   Typical components may include:
#'   \describe{
#'     \item{\code{use_ecov_em}}{Logical. Whether to use user-specified or
#'       projected Ecov values in the EM projection period.}
#'     \item{\code{lag}}{Integer lag used to align environmental covariates with
#'       a biological process such as recruitment.}
#'     \item{\code{period}}{Optional integer vector specifying projection years
#'       whose Ecov values should be replaced.}
#'   }
#'
#' @param gauss_rec_em List controlling an optional Gaussian environmental
#'   effect on recruitment in the EM. This information is passed to
#'   \code{\link{add_gauss_rec_to_em_input}} to augment the EM input.
#'
#'   Expected components are:
#'   \describe{
#'     \item{\code{use}}{Logical. Whether the Gaussian recruitment effect is
#'       included.}
#'     \item{\code{Ecov_rec_T_col}}{Integer. One-based R index indicating which
#'       environmental covariate column is used as the recruitment covariate.}
#'     \item{\code{Topt_rec}}{Numeric. Environmental optimum at which
#'       recruitment is maximized.}
#'     \item{\code{width_rec}}{Numeric. Width of the Gaussian response on the
#'       original environmental scale. Must be positive.}
#'     \item{\code{beta_T_rec}}{Numeric scalar or vector. Strength of the
#'       Gaussian environmental effect on log recruitment. If a scalar is
#'       supplied, it is recycled across stocks.}
#'     \item{\code{estimate}}{Logical. Whether \code{Topt_rec},
#'       \code{width_rec}, and \code{beta_T_rec} are estimated in the EM. If
#'       \code{FALSE}, they are fixed at the supplied values.}
#'   }
#'
#' @param age_comp_em Character. Likelihood used for age-composition data in
#'   the EM. Default is \code{"multinomial"}.
#'
#'   Supported values may include:
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
#'
#' @param em.opt List of EM options.
#'
#'   Typical components are:
#'   \describe{
#'     \item{\code{separate.em}}{Logical. Whether to use separate EM
#'       formulations instead of a single combined EM.}
#'     \item{\code{separate.em.type}}{Integer describing the separate-EM mode:
#'       \itemize{
#'         \item \code{1}: fit a single EM that produces aggregate or panmictic
#'           catch advice, then allocate that advice back to OM fleets with
#'           \code{\link{calculate_catch_advice}}.
#'         \item \code{2}: use a fleets-as-areas formulation in which advice is
#'           already on the fleet scale.
#'         \item \code{3}: fit multiple separate EMs, usually one per stock or
#'           one per region, and combine their advice before updating the OM.
#'       }}
#'     \item{\code{do.move}}{Logical. Whether movement is included in the EM.}
#'     \item{\code{est.move}}{Logical. Whether movement parameters are
#'       estimated. If \code{FALSE} and \code{do.move = TRUE}, movement is fixed
#'       before fitting.}
#'   }
#'
#' @param aggregate_catch_info List or \code{NULL}. Optional settings used to
#'   aggregate OM catch data when constructing the EM input.
#'
#'   Typical components include:
#'   \describe{
#'     \item{\code{n_fleets}}{Integer. Number of aggregated fleets in the EM.}
#'     \item{\code{catch_cv}}{Numeric vector of observation CVs for aggregated
#'       catch.}
#'     \item{\code{catch_Neff}}{Numeric vector of effective sample sizes for
#'       catch age composition.}
#'     \item{\code{use_agg_catch}}{Integer vector indicating which aggregated
#'       catch series are included.}
#'     \item{\code{use_catch_paa}}{Integer vector indicating which catch age
#'       compositions are included.}
#'     \item{\code{fleet_pointer}}{Integer vector mapping original OM fleets to
#'       aggregated EM fleets; \code{0} excludes a fleet.}
#'     \item{\code{use_catch_weighted_waa}}{Logical. Whether aggregate fleet
#'       weight-at-age is computed using catch-weighted averaging. If omitted,
#'       the function treats this as \code{FALSE}.}
#'   }
#'
#' @param aggregate_index_info List or \code{NULL}. Optional settings used to
#'   aggregate OM survey index data when constructing the EM input.
#'
#'   Typical components include:
#'   \describe{
#'     \item{\code{n_indices}}{Integer. Number of aggregated indices in the EM.}
#'     \item{\code{index_cv}}{Numeric vector of survey observation CVs.}
#'     \item{\code{index_Neff}}{Numeric vector of effective sample sizes for
#'       survey age composition.}
#'     \item{\code{fracyr_indices}}{Numeric vector of within-year timing for
#'       each aggregated survey.}
#'     \item{\code{q}}{Numeric vector of initial survey catchability values.}
#'     \item{\code{use_indices}}{Integer vector indicating which aggregated
#'       indices are included.}
#'     \item{\code{use_index_paa}}{Integer vector indicating which survey age
#'       compositions are included.}
#'     \item{\code{units_indices}}{Integer vector defining survey units, such
#'       as biomass or numbers.}
#'     \item{\code{units_index_paa}}{Integer vector defining age-composition
#'       units.}
#'     \item{\code{index_pointer}}{Integer vector mapping original OM indices to
#'       aggregated EM indices; \code{0} excludes an index.}
#'     \item{\code{use_catch_weighted_waa}}{Logical. Whether aggregate
#'       index-specific weight-at-age is computed using index-magnitude-weighted
#'       averaging. If omitted, the function treats this as \code{FALSE}.}
#'   }
#'
#'   For compatibility with aggregation helpers, this function standardizes
#'   \code{aggregate_index_info$use_index_weighted_waa} to
#'   \code{aggregate_index_info$use_catch_weighted_waa} if the former is
#'   supplied instead of the latter.
#'
#' @param aggregate_weights_info List or \code{NULL}. Optional settings for how
#'   weight-at-age and maturity-at-age are combined when catch or index data are
#'   aggregated.
#'
#'   Typical components include:
#'   \describe{
#'     \item{\code{ssb_waa_weights}}{List controlling how spawning-biomass
#'       weight-at-age is averaged.}
#'     \item{\code{maturity_weights}}{List controlling how maturity-at-age is
#'       averaged.}
#'   }
#'
#' @param reduce_region_info List or \code{NULL}. Optional information used when
#'   the EM contains fewer modeled regions than the OM.
#'
#'   Common components may include:
#'   \describe{
#'     \item{\code{remove_regions}}{Integer vector indicating retained and
#'       removed regions.}
#'     \item{\code{reassign}}{Code describing how retained regions are
#'       reassigned or merged.}
#'     \item{\code{NAA_where}}{Array indicating stock presence by stock, region,
#'       and age.}
#'     \item{\code{sel_em}, \code{M_em}, \code{NAA_re_em},
#'       \code{move_em}}{EM configuration objects used for the reduced-region
#'       EM.}
#'     \item{\code{onto_move_list}}{List of ontogenetic movement inputs such as
#'       age-specific movement definitions or movement parameters.}
#'     \item{\code{fleet_catch}}{Optional catch values inserted back into OM
#'       fleets corresponding to removed regions when advice is generated on the
#'       reduced fleet set only.}
#'   }
#'
#' @param filter_indices Optional integer vector used to exclude or retain
#'   survey indices when building the EM input.
#'
#' @param update_catch_info List or \code{NULL}. Optional information used to
#'   update catch CVs or effective sample sizes in the EM input after
#'   construction.
#'
#'   Typical components include:
#'   \describe{
#'     \item{\code{agg_catch_sigma}}{Matrix of catch observation standard
#'       deviations on the log scale.}
#'     \item{\code{catch_Neff}}{Matrix of effective sample sizes for catch age
#'       composition.}
#'     \item{\code{remove_agg}}{Logical. Whether selected aggregate catch
#'       observations should be removed.}
#'     \item{\code{remove_agg_pointer}}{Pointers identifying which aggregate
#'       catch series should be removed.}
#'     \item{\code{remove_agg_years}}{Years in which aggregate catch
#'       observations should be removed.}
#'     \item{\code{remove_paa}}{Logical. Whether selected catch age-composition
#'       observations should be removed.}
#'     \item{\code{remove_paa_pointer}}{Pointers identifying which catch
#'       age-composition series should be removed.}
#'     \item{\code{remove_paa_years}}{Years in which catch age-composition
#'       observations should be removed.}
#'   }
#'
#' @param update_index_info List or \code{NULL}. Optional information used to
#'   update survey index CVs, effective sample sizes, or remove selected survey
#'   observations and age composition in the EM input after construction.
#'
#'   Typical components include:
#'   \describe{
#'     \item{\code{agg_index_sigma}}{Matrix of survey observation standard
#'       deviations.}
#'     \item{\code{index_Neff}}{Matrix of effective sample sizes for survey age
#'       composition.}
#'     \item{\code{remove_agg}}{Logical. Whether selected aggregate survey
#'       observations should be removed.}
#'     \item{\code{remove_agg_pointer}}{Pointers identifying which aggregate
#'       survey series should be removed.}
#'     \item{\code{remove_agg_years}}{Years in which aggregate survey
#'       observations should be removed.}
#'     \item{\code{remove_paa}}{Logical. Whether selected survey age-
#'       composition observations should be removed.}
#'     \item{\code{remove_paa_pointer}}{Pointers identifying which survey age-
#'       composition series should be removed.}
#'     \item{\code{remove_paa_years}}{Years in which survey age-composition
#'       observations should be removed.}
#'   }
#'
#' @param user_SPR_weights_info List or \code{NULL}. Optional settings passed to
#'   \code{update_SPR_weights()} to modify how biological reference-point
#'   weights are assigned.
#'
#'   Typical components include:
#'   \describe{
#'     \item{\code{method}}{Character string specifying the weighting method.}
#'     \item{\code{weight_years}}{Integer. Number of historical years used to
#'       compute average weights.}
#'     \item{\code{index_pointer}}{Optional survey pointer used when weighting
#'       is based on survey information.}
#'   }
#'
#' @param assess_years Integer vector giving the years in which stock
#'   assessments are performed and advice is generated.
#'
#' @param assess_interval Integer. Number of years projected forward after each
#'   assessment. This also determines the number of years over which the OM is
#'   updated after each EM fit. If \code{0}, no projected catch is generated and
#'   the OM is not updated.
#'
#' @param base_years Integer vector giving the burn-in or historical period used
#'   to initialize the EM fitting window.
#'
#' @param year.use Integer. Number of years of data included in each EM fit.
#'   Default is \code{20}. Depending on \code{\link{make_em_input}}, this may
#'   define a moving window or a truncated historical fitting period.
#'
#' @param add.years Logical. Whether the fitting window should grow through time
#'   by adding new years after each assessment rather than keeping a fixed
#'   window length.
#'
#' @param by_fleet Logical. Whether the OM update should calculate fishing
#'   mortality separately by fleet when applying realized catch.
#'
#' @param process_fix Logical or numeric 0/1. Passed to \code{\link{update_om_fn}}
#'   when the operating model is updated. Values \code{TRUE} and \code{1} are
#'   treated as enabling process fixing; values \code{FALSE} and \code{0} are
#'   treated as disabling it. If \code{FALSE} (default), all simulated random
#'   effects listed in \code{random} are overwritten. If \code{TRUE}, the
#'   historical process is preserved and only the future portion of
#'   \code{log_NAA} is overwritten.
#'
#' @param first_free_year Integer. Passed to \code{\link{update_om_fn}} when
#'   \code{process_fix = TRUE}. This gives the first year index in
#'   \code{log_NAA} that is allowed to change; earlier years are kept fixed.
#'   
#' @param FXSPR_init Numeric or \code{NULL}. Optional initial value used in
#'   biological reference-point calculations involving \eqn{F_{XSPR}}. If
#'   supplied, this value is copied into the EM input before fitting.
#'
#' @param hcr List describing the harvest control rule used in catch-advice
#'   generation.
#'
#'   Expected components include:
#'   \describe{
#'     \item{\code{hcr.type}}{Integer specifying the HCR form. Common options
#'       include:
#'       \itemize{
#'         \item \code{1}: \eqn{F_{XSPR}}
#'         \item \code{2}: constant catch
#'         \item \code{3}: hockey-stick rule
#'       }}
#'     \item{\code{hcr.opts}}{List of options passed to the advice routine, such
#'       as target SPR, target F fractions, averaging windows, bounds on
#'       interannual change, biomass thresholds, or settings controlling whether
#'       random effects are continued in projection.}
#'   }
#'
#' @param proj.opts List of additional projection settings passed to
#'   \code{\link{advice_fn}}. These may control the forecast model,
#'   continuation of random effects, projection constraints, or other advanced
#'   settings.
#'
#' @param catch_alloc List controlling how aggregate or EM-level advice is
#'   allocated back to OM fleets, especially for
#'   \code{em.opt$separate.em.type = 1}.
#'
#'   Typical components include:
#'   \describe{
#'     \item{\code{weight_type}}{Integer specifying the general weighting class.}
#'     \item{\code{method}}{Character string specifying the allocation method,
#'       for example equal allocation, fleet-based allocation, region-based
#'       allocation, catch-based allocation, survey-based allocation, or
#'       user-defined allocation.}
#'     \item{\code{user_weights}}{Optional numeric vector of user-supplied
#'       weights when the chosen method requires them.}
#'     \item{\code{weight_years}}{Integer number of historical years used to
#'       average weighting quantities.}
#'     \item{\code{survey_pointer}}{Optional survey pointer or pointers used when
#'       survey-based weighting is selected.}
#'   }
#'
#' @param implementation_error List or \code{NULL}. Optional specification for
#'   converting advised catch into realized catch before updating the OM.
#'
#'   Expected components include:
#'   \describe{
#'     \item{\code{method}}{Character string naming the error distribution, such
#'       as \code{"lognormal"}, \code{"normal"}, \code{"uniform"}, or
#'       \code{"constant"}.}
#'     \item{\code{mean}}{Numeric mean parameter. For lognormal error this is
#'       generally on the log scale.}
#'     \item{\code{cv}}{Numeric coefficient of variation for lognormal error.}
#'     \item{\code{sd}}{Numeric standard deviation for normal error.}
#'     \item{\code{min}}{Numeric lower bound for uniform error.}
#'     \item{\code{max}}{Numeric upper bound for uniform error.}
#'     \item{\code{constant_value}}{Numeric multiplier used when the error
#'       method is constant.}
#'   }
#'
#' @param do.retro Logical. Whether retrospective analysis is requested during
#'   EM fitting.
#'
#' @param do.osa Logical. Whether one-step-ahead residuals are requested during
#'   EM fitting.
#'
#' @param do.brps Logical. Whether biological reference points are calculated
#'   when the OM is updated.
#'
#' @param seed Integer random seed used for OM updating and optional
#'   implementation error generation.
#'
#' @param save.sdrep Logical. Whether to save the full fitted EM object,
#'   including \code{sdrep}, at every assessment year.
#'
#' @param save.last.em Logical. Whether to save only the final EM fit when
#'   \code{save.sdrep = FALSE}. If \code{TRUE}, the final EM object is returned
#'   in \code{em_full[[1]]}.
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{\code{om}}{The updated operating model after all assessments and OM
#'       updates are completed.}
#'     \item{\code{em_list}}{List of EM report objects. For
#'       \code{separate.em.type = 3}, each entry is itself a list of report
#'       objects, one per fitted EM.}
#'     \item{\code{par.est}}{List of parameter estimates extracted from
#'       \code{sdrep}.}
#'     \item{\code{par.se}}{List of parameter standard errors extracted from
#'       \code{sdrep}.}
#'     \item{\code{adrep.est}}{List of AD-report estimates extracted from
#'       \code{sdrep} with \code{report = TRUE}.}
#'     \item{\code{adrep.se}}{List of AD-report standard errors extracted from
#'       \code{sdrep} with \code{report = TRUE}.}
#'     \item{\code{opt_list}}{List of optimizer output objects from each EM
#'       fit.}
#'     \item{\code{converge_list}}{List of convergence summaries, including the
#'       optimizer convergence flag, Hessian status, and a simple combined
#'       convergence code. For \code{separate.em.type = 3}, each entry contains
#'       vectors over the component EMs.}
#'     \item{\code{catch_advice}}{List of advised catch matrices. If
#'       \code{assess_interval = 0}, entries are \code{NULL}.}
#'     \item{\code{catch_realized}}{List of realized catch matrices after
#'       implementation error. If no implementation error is used, this is equal
#'       to advised catch. If \code{assess_interval = 0}, entries are
#'       \code{NULL}.}
#'     \item{\code{em_full}}{List containing full fitted EM objects when
#'       requested through \code{save.sdrep} or \code{save.last.em}.}
#'     \item{\code{em_input}}{List of EM input objects used at each assessment
#'       year.}
#'     \item{\code{runtime}}{Elapsed runtime of the full MSE loop.}
#'     \item{\code{seed.save}}{The seed value used in the function call.}
#'   }
#'
#' @details
#' The behavior of the function depends first on whether separate EMs are used,
#' and second on the value of \code{em.opt$separate.em.type}.
#'
#' When \code{em.opt$separate.em = TRUE}:
#' \itemize{
#'   \item For \strong{type 1}, a single EM is fit and the resulting projection
#'   is interpreted as aggregate or panmictic advice. This projected advice is
#'   then converted back to OM fleet-level catch using
#'   \code{\link{calculate_catch_advice}} and the user-specified
#'   \code{catch_alloc} settings.
#'
#'   \item For \strong{type 2}, the EM is assumed to produce advice directly on
#'   the fleet scale, such as in a fleets-as-areas configuration, so no extra
#'   allocation step is needed.
#'
#'   \item For \strong{type 3}, multiple EMs are fit separately, usually one per
#'   stock or one per region, and their projected advice is combined across
#'   fleets before updating the OM.
#' }
#'
#' When \code{em.opt$separate.em = FALSE}, a single combined EM is fit at each
#' assessment year. If \code{reduce_region_info} is supplied, advice produced on
#' the reduced region set may be expanded back to the full OM fleet set before
#' the OM is updated.
#'
#' In all cases where \code{assess_interval > 0}, catch advice is generated
#' using \code{\link{advice_fn}}. In type-1 separate EM mode, the output from
#' \code{advice_fn()} is first treated as EM-level advice and then passed to
#' \code{\link{calculate_catch_advice}} for fleet allocation. This distinction
#' is important when the EM advice is not already on the original OM fleet
#' scale.
#'
#' Before constructing EM input objects, the function standardizes
#' \code{aggregate_catch_info} and \code{aggregate_index_info} so that missing
#' \code{use_catch_weighted_waa} flags default to \code{FALSE}. For index
#' aggregation, \code{use_index_weighted_waa} is accepted as an alias and
#' internally standardized to \code{use_catch_weighted_waa} for compatibility
#' with aggregation helpers.
#'
#' Convergence is assessed using both the optimizer convergence code and the
#' positive-definiteness of the Hessian from \code{sdrep}. A simple convergence
#' code is returned for convenience, but users should inspect full EM objects
#' when diagnosing fitting issues.
#'
#' @seealso
#' \code{\link{make_em_input}},
#' \code{\link{fit_wham}},
#' \code{\link{advice_fn}},
#' \code{\link{calculate_catch_advice}},
#' \code{\link{update_om_fn}},
#' \code{\link{add_implementation_error}},
#' \code{\link{add_gauss_rec_to_em_input}}
#'
#' @export
loop_through_fn <- function(om,
                            em_info = NULL,
                            random = NULL,
                            M_em = NULL,
                            sel_em = NULL,
                            NAA_re_em = NULL,
                            move_em = NULL,
                            catchability_em = NULL,
                            ecov_em = NULL,
                            ecov_em_opts = NULL,
                            gauss_rec_em = list(
                              use = FALSE,
                              Ecov_rec_T_col = NULL,
                              Topt_rec = NULL,
                              width_rec = NULL,
                              beta_T_rec = NULL,
                              estimate = TRUE
                            ),
                            age_comp_em = "multinomial",
                            em.opt = list(separate.em = TRUE,
                                          separate.em.type = 1,
                                          do.move = FALSE,
                                          est.move = FALSE),
                            aggregate_catch_info = NULL,
                            aggregate_index_info = NULL,
                            aggregate_weights_info = NULL,
                            reduce_region_info = NULL,
                            filter_indices = NULL,
                            update_catch_info = NULL,
                            update_index_info = NULL,
                            user_SPR_weights_info = NULL,
                            assess_years = NULL,
                            assess_interval = NULL,
                            base_years = NULL,
                            year.use = 20,
                            add.years = FALSE,
                            by_fleet = TRUE,
                            process_fix = FALSE,
                            first_free_year = 1L,
                            FXSPR_init = NULL,
                            hcr = list(hcr.type = 1, hcr.opts = NULL),
                            proj.opts = list(),
                            catch_alloc = list(weight_type = 1,
                                               method = "equal",
                                               user_weights = NULL,
                                               weight_years = 1,
                                               survey_pointer = 1),
                            implementation_error = NULL,
                            do.retro = FALSE,
                            do.osa = FALSE,
                            do.brps = FALSE,
                            seed = 123,
                            save.sdrep = FALSE,
                            save.last.em = FALSE) {
  
  start.time <- Sys.time()
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # -----------------------------------------------------------
  # Helper functions
  # -----------------------------------------------------------
  
  check_conv <- function(em) {
    conv <- FALSE
    if (!is.null(em$opt) && !is.null(em$opt$convergence)) {
      conv <- isTRUE(em$opt$convergence == 0)
    }
    
    pdHess <- FALSE
    if (!is.null(em$sdrep) && !is.null(em$sdrep$pdHess)) {
      pdHess <- isTRUE(em$sdrep$pdHess)
    } else if (!is.null(em$na_sdrep) && !is.na(em$na_sdrep)) {
      pdHess <- !isTRUE(em$na_sdrep)
    }
    
    list(
      conv = conv,
      pdHess = pdHess,
      converge_code = as.integer(conv) + as.integer(pdHess)
    )
  }
  
  apply_implementation_error <- function(advice, implementation_error, seed) {
    if (is.null(implementation_error)) {
      return(advice)
    }
    
    cat("\nNow generating implementation error on catch advice...\n")
    
    real_catch <- add_implementation_error(
      catch_advice = advice,
      method = implementation_error$method,
      mean = implementation_error$mean,
      cv = implementation_error$cv,
      sd = implementation_error$sd,
      min = implementation_error$min,
      max = implementation_error$max,
      constant_value = implementation_error$constant_value,
      seed = seed
    )
    
    cat("\nRealized catch is...\n")
    print(real_catch)
    
    real_catch
  }
  
  coerce_advice_matrix <- function(x, nrow_target = NULL, ncol_target = NULL) {
    if (is.null(x)) return(NULL)
    if (is.matrix(x)) return(x)
    
    if (is.vector(x)) {
      if (!is.null(nrow_target) && !is.null(ncol_target)) {
        return(matrix(x, nrow = nrow_target, ncol = ncol_target, byrow = TRUE))
      } else if (!is.null(nrow_target)) {
        return(matrix(x, nrow = nrow_target, byrow = TRUE))
      } else if (!is.null(ncol_target)) {
        return(matrix(x, ncol = ncol_target, byrow = TRUE))
      } else {
        return(as.matrix(t(x)))
      }
    }
    
    x
  }
  
  store_em_outputs <- function(em_obj, i) {
    em_list[[i]] <<- em_obj$rep
    opt_list[[i]] <<- em_obj$opt
    
    if (!is.null(em_obj$sdrep)) {
      par.est[[i]] <<- as.list(em_obj$sdrep, "Estimate")
      par.se[[i]] <<- as.list(em_obj$sdrep, "Std. Error")
      adrep.est[[i]] <<- as.list(em_obj$sdrep, "Estimate", report = TRUE)
      adrep.se[[i]] <<- as.list(em_obj$sdrep, "Std. Error", report = TRUE)
    } else {
      par.est[[i]] <<- NULL
      par.se[[i]] <<- NULL
      adrep.est[[i]] <<- NULL
      adrep.se[[i]] <<- NULL
    }
  }
  
  save_em_object <- function(em_obj, i, y) {
    if (isTRUE(save.sdrep)) {
      em_full[[i]] <<- em_obj
    } else if (isTRUE(save.last.em) &&
               identical(y, assess_years[length(assess_years)])) {
      em_full[[1]] <<- em_obj
    }
  }
  
  apply_gauss_rec_to_input <- function(em_input, gauss_rec_em, separate_type = NULL) {
    if (is.null(gauss_rec_em)) return(em_input)
    
    if (isTRUE(em.opt$separate.em) && identical(separate_type, 3)) {
      if (is.list(em_input) && length(em_input) > 0) {
        em_input <- lapply(
          em_input,
          add_gauss_rec_to_em_input,
          gauss_rec_em = gauss_rec_em
        )
      }
    } else {
      em_input <- add_gauss_rec_to_em_input(
        em_input = em_input,
        gauss_rec_em = gauss_rec_em
      )
    }
    
    em_input
  }
  
  apply_user_spr_weights <- function(em_input, user_SPR_weights_info) {
    if (is.null(user_SPR_weights_info)) return(em_input)
    
    user_SPR_weights_info$method <- user_SPR_weights_info$method %||% "equal"
    user_SPR_weights_info$weight_years <- user_SPR_weights_info$weight_years %||% 1
    user_SPR_weights_info$index_pointer <- user_SPR_weights_info$index_pointer %||% NULL
    
    update_SPR_weights(
      em_input,
      method = user_SPR_weights_info$method,
      weight_years = user_SPR_weights_info$weight_years,
      index_pointer = user_SPR_weights_info$index_pointer
    )
  }
  
  apply_user_spr_weights_to_input <- function(em_input, user_SPR_weights_info, separate_type = NULL) {
    if (is.null(user_SPR_weights_info)) return(em_input)
    
    if (isTRUE(em.opt$separate.em) && identical(separate_type, 3)) {
      em_input <- lapply(
        em_input,
        apply_user_spr_weights,
        user_SPR_weights_info = user_SPR_weights_info
      )
    } else {
      em_input <- apply_user_spr_weights(em_input, user_SPR_weights_info)
    }
    
    em_input
  }
  
  get_advice <- function(em, assess_interval, hcr, proj.opts, ecov_em_opts) {
    out <- tryCatch(
      {
        advice_fn(
          em = em,
          pro.yr = assess_interval,
          hcr = hcr,
          proj.opts = proj.opts,
          ecov_em_opts = ecov_em_opts
        )
      },
      error = function(e) {
        stop("Error in `advice_fn()`: ", e$message, call. = FALSE)
      }
    )
    
    if (is.null(out)) {
      stop("`advice_fn()` returned NULL.", call. = FALSE)
    }
    
    out
  }
  
  finalize_no_projection <- function(em_obj, em_input, cc, i, y) {
    store_em_outputs(em_obj, i)
    converge_list[[i]] <<- cc
    catch_advice[[i]] <<- NULL
    catch_realized[[i]] <<- NULL
    save_em_object(em_obj, i, y)
    em_input_list[[i]] <<- em_input
  }
  
  prepare_common_inputs <- function(aggregate_catch_info, aggregate_index_info) {
    if (is.null(aggregate_catch_info)) {
      aggregate_catch_info <- list()
    }
    aggregate_catch_info$use_catch_weighted_waa <-
      aggregate_catch_info$use_catch_weighted_waa %||% FALSE
    
    if (is.null(aggregate_index_info)) {
      aggregate_index_info <- list()
    }
    
    if (is.null(aggregate_index_info$use_catch_weighted_waa) &&
        !is.null(aggregate_index_info$use_index_weighted_waa)) {
      aggregate_index_info$use_catch_weighted_waa <-
        aggregate_index_info$use_index_weighted_waa
    }
    
    aggregate_index_info$use_catch_weighted_waa <-
      aggregate_index_info$use_catch_weighted_waa %||% FALSE
    
    list(
      aggregate_catch_info = aggregate_catch_info,
      aggregate_index_info = aggregate_index_info
    )
  }
  
  add_fleet_names <- function(advice, y, n_fleets, assess_interval) {
    if (is.null(advice)) return(NULL)
    colnames(advice) <- paste0("Fleet_", seq_len(n_fleets))
    rownames(advice) <- paste0("Year_", y + seq_len(assess_interval))
    advice
  }
  
  # -----------------------------------------------------------
  # Input checks
  # -----------------------------------------------------------
  
  if (is.null(em.opt)) stop("`em.opt` must be specified.", call. = FALSE)
  if (is.null(em_info)) stop("`em_info` must be specified.", call. = FALSE)
  if (is.null(assess_years)) stop("`assess_years` must be specified.", call. = FALSE)
  if (is.null(base_years)) stop("`base_years` must be specified.", call. = FALSE)
  if (is.null(assess_interval)) stop("`assess_interval` must be specified.", call. = FALSE)
  
  if (!is.null(move_em) && isTRUE(em.opt$separate.em)) {
    stop("`move_em` must be NULL if `em.opt$separate.em = TRUE`.", call. = FALSE)
  }
  if (isTRUE(em.opt$separate.em)) move_em <- NULL
  
  if (length(process_fix) != 1L || is.na(process_fix)) {
    stop("`process_fix` must be a single non-missing TRUE/FALSE or 0/1 value.", call. = FALSE)
  }
  
  if (is.numeric(process_fix)) {
    if (!process_fix %in% c(0, 1)) {
      stop("Numeric `process_fix` must be 0 or 1.", call. = FALSE)
    }
    process_fix <- as.logical(process_fix)
  } else if (!is.logical(process_fix)) {
    stop("`process_fix` must be a single TRUE/FALSE or 0/1 value.", call. = FALSE)
  }
  
  if (!is.numeric(first_free_year) || length(first_free_year) != 1L || is.na(first_free_year)) {
    stop("`first_free_year` must be a single non-missing integer.", call. = FALSE)
  }
  first_free_year <- as.integer(first_free_year)
  
  if (first_free_year < 1L) {
    stop("`first_free_year` must be >= 1.", call. = FALSE)
  }
  
  tmp_inputs <- prepare_common_inputs(
    aggregate_catch_info = aggregate_catch_info,
    aggregate_index_info = aggregate_index_info
  )
  aggregate_catch_info <- tmp_inputs$aggregate_catch_info
  aggregate_index_info <- tmp_inputs$aggregate_index_info
  
  # -----------------------------------------------------------
  # Output containers
  # -----------------------------------------------------------
  
  n_assess <- length(assess_years)
  em_list <- vector("list", n_assess)
  par.est <- vector("list", n_assess)
  par.se <- vector("list", n_assess)
  adrep.est <- vector("list", n_assess)
  adrep.se <- vector("list", n_assess)
  opt_list <- vector("list", n_assess)
  converge_list <- vector("list", n_assess)
  catch_advice <- vector("list", n_assess)
  catch_realized <- vector("list", n_assess)
  em_full <- list()
  em_input_list <- vector("list", n_assess)
  
  # -----------------------------------------------------------
  # Separate EM branch
  # -----------------------------------------------------------
  
  if (isTRUE(em.opt$separate.em)) {
    
    for (y in assess_years) {
      
      cat(paste0("\nNow conducting stock assessment for year ", y, "\n"))
      
      i <- which(assess_years == y)
      em.years <- base_years[1]:y
      
      year.use_i <- year.use
      if (isTRUE(add.years) && i != 1) {
        year.use_i <- year.use + assess_interval * (i - 1)
      }
      
      em_input <- make_em_input(
        om = om,
        em_info = em_info,
        M_em = M_em,
        sel_em = sel_em,
        NAA_re_em = NAA_re_em,
        move_em = move_em,
        catchability_em = catchability_em,
        ecov_em = ecov_em,
        ecov_em_opts = ecov_em_opts,
        em.opt = em.opt,
        em_years = em.years,
        year.use = year.use_i,
        age_comp_em = age_comp_em,
        aggregate_catch_info = aggregate_catch_info,
        aggregate_index_info = aggregate_index_info,
        aggregate_weights_info = aggregate_weights_info,
        reduce_region_info = reduce_region_info,
        filter_indices = filter_indices,
        update_catch_info = update_catch_info,
        update_index_info = update_index_info
      )
      
      em_input <- apply_gauss_rec_to_input(
        em_input = em_input,
        gauss_rec_em = gauss_rec_em,
        separate_type = em.opt$separate.em.type
      )
      
      em_input <- apply_user_spr_weights_to_input(
        em_input = em_input,
        user_SPR_weights_info = user_SPR_weights_info,
        separate_type = em.opt$separate.em.type
      )
      
      if (!is.null(FXSPR_init)) {
        if (isTRUE(em.opt$separate.em.type == 3) && is.list(em_input)) {
          em_input <- lapply(em_input, function(x) {
            x$data$FXSPR_init[] <- FXSPR_init
            x
          })
        } else {
          em_input$data$FXSPR_init[] <- FXSPR_init
        }
      }
      
      if (em.opt$separate.em.type == 1) {
        
        cat("\nNow fitting assessment model...\n")
        em <- fit_wham(
          em_input,
          do.retro = do.retro,
          do.osa = do.osa,
          do.brps = TRUE,
          MakeADFun.silent = TRUE
        )
        
        cat("\nNow checking convergence of assessment model...\n")
        cc <- check_conv(em)
        if (cc$conv && cc$pdHess) {
          cat("\nAssessment model is converged.\n")
        } else {
          warning("\nAssessment model is not converged!\n")
        }
        
        if (assess_interval == 0) {
          cat("\nNow performing simulation-estimation experiments...\n")
          finalize_no_projection(em, em_input, cc, i, y)
          next
        }
        
        cat("\nNow using the EM to project catch...\n")
        em.advice <- get_advice(
          em = em,
          assess_interval = assess_interval,
          hcr = hcr,
          proj.opts = proj.opts,
          ecov_em_opts = ecov_em_opts
        )
        
        em.advice <- coerce_advice_matrix(
          em.advice,
          nrow_target = assess_interval
        )
        
        cat("\nProjected catch from assessment model is\n")
        print(em.advice)
        
        cat("\nNow allocating catch...\n")
        advice <- tryCatch(
          {
            calculate_catch_advice(
              om = om,
              advice = em.advice,
              aggregate_catch_info = aggregate_catch_info,
              aggregate_index_info = aggregate_index_info,
              final_year = y,
              catch_alloc = catch_alloc
            )
          },
          error = function(e) {
            stop(
              "Error in `calculate_catch_advice()` inside `loop_through_fn()`: ",
              e$message,
              call. = FALSE
            )
          }
        )
        
        if (is.null(advice)) {
          stop("`calculate_catch_advice()` returned NULL in `loop_through_fn()`.", call. = FALSE)
        }
        
        advice <- coerce_advice_matrix(
          advice,
          nrow_target = assess_interval,
          ncol_target = om$input$data$n_fleets
        )
        advice <- add_fleet_names(advice, y, om$input$data$n_fleets, assess_interval)
        
        cat("\nNow generating catch advice...\n")
        print(advice)
        
        real_catch <- apply_implementation_error(advice, implementation_error, seed)
        
        interval.info <- list(
          catch = real_catch,
          years = y + seq_len(assess_interval)
        )
        
        cat("\nNow calculating F at age in the OM given the catch advice...\n")
        om <- update_om_fn(
          om,
          interval.info,
          seed = seed,
          random = random,
          method = "nlminb",
          by_fleet = by_fleet,
          do.brps = do.brps,
          process_fix = process_fix,
          first_free_year = first_free_year
        )
        
        store_em_outputs(em, i)
        converge_list[[i]] <- cc
        catch_advice[[i]] <- advice
        catch_realized[[i]] <- real_catch
        save_em_object(em, i, y)
        em_input_list[[i]] <- em_input
        
      } else if (em.opt$separate.em.type == 2) {
        
        cat("\nNow fitting assessment model...\n")
        em <- fit_wham(
          em_input,
          do.retro = do.retro,
          do.osa = do.osa,
          do.brps = TRUE,
          MakeADFun.silent = TRUE
        )
        
        cat("\nNow checking convergence of assessment model...\n")
        cc <- check_conv(em)
        if (cc$conv && cc$pdHess) {
          cat("\nAssessment model is converged.\n")
        } else {
          warning("\nAssessment model is not converged!\n")
        }
        
        if (assess_interval == 0) {
          cat("\nNow performing simulation-estimation experiments...\n")
          finalize_no_projection(em, em_input, cc, i, y)
          next
        }
        
        cat("\nNow using the EM to project catch...\n")
        advice <- get_advice(
          em = em,
          assess_interval = assess_interval,
          hcr = hcr,
          proj.opts = proj.opts,
          ecov_em_opts = ecov_em_opts
        )
        
        advice <- coerce_advice_matrix(
          advice,
          nrow_target = assess_interval,
          ncol_target = om$input$data$n_fleets
        )
        advice <- add_fleet_names(advice, y, om$input$data$n_fleets, assess_interval)
        
        cat("\nNow generating catch advice...\n")
        print(advice)
        
        real_catch <- apply_implementation_error(advice, implementation_error, seed)
        
        interval.info <- list(
          catch = real_catch,
          years = y + seq_len(assess_interval)
        )
        
        cat("\nNow calculating F at age in the OM given the catch advice...\n")
        om <- update_om_fn(
          om,
          interval.info,
          seed = seed,
          random = random,
          method = "nlminb",
          by_fleet = by_fleet,
          do.brps = do.brps,
          process_fix = process_fix,
          first_free_year = first_free_year
        )
        
        store_em_outputs(em, i)
        converge_list[[i]] <- cc
        catch_advice[[i]] <- advice
        catch_realized[[i]] <- real_catch
        save_em_object(em, i, y)
        em_input_list[[i]] <- em_input
        
      } else if (em.opt$separate.em.type == 3) {
        
        em_list[[i]] <- list()
        par.est[[i]] <- list()
        par.se[[i]] <- list()
        adrep.est[[i]] <- list()
        adrep.se[[i]] <- list()
        opt_list[[i]] <- list()
        converge_list[[i]] <- list()
        em_input_list[[i]] <- list()
        
        if (isTRUE(save.sdrep)) {
          em_full[[i]] <- list()
        } else if (isTRUE(save.last.em) &&
                   identical(y, assess_years[length(assess_years)])) {
          em_full[[1]] <- list()
        }
        
        n_models <- length(em_input)
        em <- vector("list", n_models)
        conv_vec <- rep(FALSE, n_models)
        pdHess_vec <- rep(FALSE, n_models)
        advice <- NULL
        real_catch <- NULL
        
        if (assess_interval != 0) {
          cat("\nNow generating catch advice...\n")
        }
        
        for (s in seq_len(n_models)) {
          
          cat("\nNow fitting assessment model...\n")
          em[[s]] <- fit_wham(
            em_input[[s]],
            do.retro = do.retro,
            do.osa = do.osa,
            do.brps = TRUE,
            MakeADFun.silent = TRUE
          )
          
          cat("\nNow checking convergence of assessment model...\n")
          cc_s <- check_conv(em[[s]])
          conv_vec[s] <- cc_s$conv
          pdHess_vec[s] <- cc_s$pdHess
          
          if (cc_s$conv && cc_s$pdHess) {
            cat("\nAssessment model is converged.\n")
          } else {
            warning("\nAssessment model is not converged!\n")
          }
          
          if (assess_interval != 0) {
            tmp <- get_advice(
              em = em[[s]],
              assess_interval = assess_interval,
              hcr = hcr,
              proj.opts = proj.opts,
              ecov_em_opts = ecov_em_opts
            )
            tmp <- coerce_advice_matrix(tmp, nrow_target = assess_interval)
            advice <- cbind(advice, tmp)
          }
        }
        
        if (assess_interval == 0) {
          cat("\nNow performing simulation-estimation experiments...\n")
        } else {
          advice <- coerce_advice_matrix(
            advice,
            nrow_target = assess_interval,
            ncol_target = om$input$data$n_fleets
          )
          advice <- add_fleet_names(advice, y, om$input$data$n_fleets, assess_interval)
          
          print(advice)
          
          real_catch <- apply_implementation_error(advice, implementation_error, seed)
          
          interval.info <- list(
            catch = real_catch,
            years = y + seq_len(assess_interval)
          )
          
          cat("\nNow calculating F at age in the OM given the catch advice...\n")
          om <- update_om_fn(
            om,
            interval.info,
            seed = seed,
            random = random,
            method = "nlminb",
            by_fleet = by_fleet,
            do.brps = do.brps,
            process_fix = process_fix,
            first_free_year = first_free_year
          )
        }
        
        for (s in seq_len(n_models)) {
          em_list[[i]][[s]] <- em[[s]]$rep
          opt_list[[i]][[s]] <- em[[s]]$opt
          
          if (!is.null(em[[s]]$sdrep)) {
            par.est[[i]][[s]] <- as.list(em[[s]]$sdrep, "Estimate")
            par.se[[i]][[s]] <- as.list(em[[s]]$sdrep, "Std. Error")
            adrep.est[[i]][[s]] <- as.list(em[[s]]$sdrep, "Estimate", report = TRUE)
            adrep.se[[i]][[s]] <- as.list(em[[s]]$sdrep, "Std. Error", report = TRUE)
          } else {
            par.est[[i]][[s]] <- NULL
            par.se[[i]][[s]] <- NULL
            adrep.est[[i]][[s]] <- NULL
            adrep.se[[i]][[s]] <- NULL
          }
          
          em_input_list[[i]][[s]] <- em_input[[s]]
          
          if (isTRUE(save.sdrep)) {
            em_full[[i]][[s]] <- em[[s]]
          } else if (isTRUE(save.last.em) &&
                     identical(y, assess_years[length(assess_years)])) {
            em_full[[1]][[s]] <- em[[s]]
          }
        }
        
        converge_list[[i]] <- list(
          conv = conv_vec,
          pdHess = pdHess_vec,
          converge_code = as.integer(conv_vec) + as.integer(pdHess_vec)
        )
        catch_advice[[i]] <- if (assess_interval == 0) NULL else advice
        catch_realized[[i]] <- if (assess_interval == 0) NULL else real_catch
        
      } else {
        stop("Unsupported `em.opt$separate.em.type`. Must be 1, 2, or 3.", call. = FALSE)
      }
    }
    
  } else {
    
    # ---------------------------------------------------------
    # Combined spatial EM branch
    # ---------------------------------------------------------
    
    for (y in assess_years) {
      
      cat(paste0("\nNow conducting stock assessment for year ", y, "\n"))
      
      i <- which(assess_years == y)
      em.years <- base_years[1]:y
      
      year.use_i <- year.use
      if (isTRUE(add.years) && i != 1) {
        year.use_i <- year.use + assess_interval * (i - 1)
      }
      
      em_input <- make_em_input(
        om = om,
        em_info = em_info,
        M_em = M_em,
        sel_em = sel_em,
        NAA_re_em = NAA_re_em,
        move_em = move_em,
        catchability_em = catchability_em,
        ecov_em = ecov_em,
        ecov_em_opts = ecov_em_opts,
        em.opt = em.opt,
        em_years = em.years,
        year.use = year.use_i,
        age_comp_em = age_comp_em,
        aggregate_catch_info = aggregate_catch_info,
        aggregate_index_info = aggregate_index_info,
        aggregate_weights_info = aggregate_weights_info,
        reduce_region_info = reduce_region_info,
        filter_indices = filter_indices,
        update_catch_info = update_catch_info,
        update_index_info = update_index_info
      )
      
      em_input <- apply_gauss_rec_to_input(
        em_input = em_input,
        gauss_rec_em = gauss_rec_em,
        separate_type = NULL
      )
      em_input <- apply_user_spr_weights(em_input, user_SPR_weights_info)
      
      if (!is.null(FXSPR_init)) {
        em_input$data$FXSPR_init[] <- FXSPR_init
      }
      
      cat("\nNow fitting assessment model...\n")
      
      if (isTRUE(em.opt$do.move)) {
        if (isTRUE(em.opt$est.move)) {
          em <- fit_wham(
            em_input,
            do.retro = do.retro,
            do.osa = do.osa,
            do.brps = TRUE,
            MakeADFun.silent = TRUE
          )
        } else {
          em_input <- fix_move(em_input)
          em <- fit_wham(
            em_input,
            do.retro = do.retro,
            do.osa = do.osa,
            do.brps = TRUE,
            MakeADFun.silent = TRUE
          )
        }
      } else {
        em <- fit_wham(
          em_input,
          do.retro = do.retro,
          do.osa = do.osa,
          do.brps = TRUE,
          MakeADFun.silent = TRUE
        )
      }
      
      advice <- NULL
      real_catch <- NULL
      
      cat("\nNow checking convergence of assessment model...\n")
      cc <- check_conv(em)
      
      if (cc$conv && cc$pdHess) {
        cat("\nAssessment model is converged.\n")
      } else {
        warning("\nAssessment model is not converged!\n")
      }
      
      if (assess_interval != 0) {
        
        cat("\nNow generating catch advice...\n")
        advice <- get_advice(
          em = em,
          assess_interval = assess_interval,
          hcr = hcr,
          proj.opts = proj.opts,
          ecov_em_opts = ecov_em_opts
        )
        
        if (!is.null(reduce_region_info) && !is.null(reduce_region_info$remove_regions)) {
          remove_regions <- reduce_region_info$remove_regions
          fleets_to_remove <- which(
            om$input$data$fleet_regions %in% which(remove_regions == 0)
          )
          fleets_to_keep <- which(
            !om$input$data$fleet_regions %in% which(remove_regions == 0)
          )
          
          advice <- coerce_advice_matrix(
            advice,
            nrow_target = assess_interval,
            ncol_target = length(fleets_to_keep)
          )
          
          advice.tmp <- matrix(
            0,
            nrow = assess_interval,
            ncol = length(om$input$data$fleet_regions)
          )
          
          advice.tmp[, fleets_to_keep] <- advice
          
          if (!is.null(reduce_region_info$fleet_catch)) {
            fleet_catch_fill <- reduce_region_info$fleet_catch
            
            if (is.vector(fleet_catch_fill)) {
              fleet_catch_fill <- matrix(
                fleet_catch_fill,
                nrow = assess_interval,
                ncol = length(fleets_to_remove),
                byrow = TRUE
              )
            }
            
            advice.tmp[, fleets_to_remove] <- fleet_catch_fill
          }
          
          advice <- advice.tmp
        }
        
        advice <- coerce_advice_matrix(
          advice,
          nrow_target = assess_interval,
          ncol_target = om$input$data$n_fleets
        )
        advice <- add_fleet_names(advice, y, om$input$data$n_fleets, assess_interval)
        
        cat("Catch Advice\n")
        print(advice)
        
        real_catch <- apply_implementation_error(advice, implementation_error, seed)
        
        interval.info <- list(
          catch = real_catch,
          years = y + seq_len(assess_interval)
        )
        
        cat("\nNow calculating F at age in the OM given the catch advice...\n")
        om <- update_om_fn(
          om,
          interval.info,
          seed = seed,
          random = random,
          method = "nlminb",
          by_fleet = by_fleet,
          do.brps = do.brps,
          process_fix = process_fix,
          first_free_year = first_free_year
        )
        
      } else {
        cat("\nNow performing simulation-estimation experiments...\n")
      }
      
      store_em_outputs(em, i)
      converge_list[[i]] <- cc
      catch_advice[[i]] <- advice
      catch_realized[[i]] <- real_catch
      save_em_object(em, i, y)
      em_input_list[[i]] <- em_input
    }
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  cat("Please ignore Warning in check_projF(proj_mod).")
  cat("\nTotal Runtime = ", time.taken, "\n")
  
  return(list(
    om = om,
    em_list = em_list,
    par.est = par.est,
    par.se = par.se,
    adrep.est = adrep.est,
    adrep.se = adrep.se,
    opt_list = opt_list,
    converge_list = converge_list,
    catch_advice = catch_advice,
    catch_realized = catch_realized,
    em_full = em_full,
    em_input = em_input_list,
    runtime = time.taken,
    seed.save = seed
  ))
}