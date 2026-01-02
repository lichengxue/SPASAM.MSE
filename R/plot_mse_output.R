#' Run full MSE plotting pipeline and render HTML/PDF report
#'
#' This wrapper (i) regenerates all PNG figures by calling the available
#' plotting functions in the package/workspace, and (ii) optionally renders
#' an HTML and/or PDF report by embedding the generated PNGs into an Rmd.
#'
#' The function also builds (optional) cached summaries via `build_mse_summary()`
#' and stores them in `options(mse_summary=...)` so downstream plotting
#' functions can reuse them.
#'
#' @param mods Model outputs list. For non-simulation runs:
#'   `list(Mod1, Mod2, ...)`. For simulation runs:
#'   `list(sim1=list(Mod1,Mod2,...), sim2=list(...), ...)`.
#' @param main_dir Base directory for outputs. Default `getwd()`.
#' @param output_dir Output folder under `main_dir`. Default `"Report"`.
#' @param output_format Character vector of outputs to produce. Any of
#'   `"png"`, `"html"`, `"pdf"`. Default `c("png","html","pdf")`.
#' @param width,height,dpi Figure saving size for PNGs.
#' @param col.opt Passed to viridis discrete option where used.
#' @param method Passed to performance-summary plots where used.
#' @param outlier.opt Outlier shape for boxplot style figures. `NA` is treated as NULL.
#' @param plot.style Plot style for performance plots (e.g. `"median_iqr"` or `"boxplot"`).
#' @param show.whisker Logical. Used by `"median_iqr"` style plots.
#' @param f.ymin,f.ymax Optional y-limits for relative performance plots.
#' @param start.years Starting row index for "first" windows (1-based).
#' @param use.n.years.first Window length for short-term/first window.
#' @param use.n.years.last Window length for long-term/last window.
#' @param new_model_names Optional vector of display names for models.
#' @param base.model Baseline model name (e.g. `"Model1"`) for relative plots.
#' @param report_from_pngs Logical. If TRUE, build reports using saved PNGs.
#' @param progress Logical. If TRUE and package `progress` is installed, show progress.
#' @param toc_line_spacing Line spacing factor used for the PDF ToC page.
#' @param toc_entry_vspace_pt Vertical space (pt) between ToC entries in PDF.
#'
#' @return Invisibly returns a list with:
#'   `full_output_dir`, `pngs`, and `reports` (paths for html/pdf if generated).
#' @export

plot_mse_output <- function(mods,
                            main_dir            = getwd(),
                            output_dir          = "Report",
                            output_format       = c("png", "html", "pdf"),
                            width               = 10,
                            height              = 7,
                            dpi                 = 300,
                            col.opt             = "D",
                            method              = "mean",
                            outlier.opt         = NA,
                            plot.style          = "median_iqr",
                            show.whisker        = TRUE,
                            f.ymin              = NULL,
                            f.ymax              = NULL,
                            start.years         = 1,
                            use.n.years.first   = 5,
                            use.n.years.last    = 5,
                            new_model_names     = NULL,
                            base.model          = "Model1",
                            report_from_pngs    = TRUE,
                            progress            = TRUE,
                            toc_line_spacing    = 1.35,
                            toc_entry_vspace_pt = 12) {
  
  ## ---- deps (soft) ----
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for report rendering.")
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for report rendering.")
  }
  
  output_format <- match.arg(output_format, c("png", "html", "pdf"), several.ok = TRUE)
  
  if (length(outlier.opt) == 1L && is.na(outlier.opt)) outlier.opt <- NULL
  if (is.null(f.ymin)) f.ymin <- NA_real_
  if (is.null(f.ymax)) f.ymax <- NA_real_
  
  full_output_dir <- file.path(main_dir, output_dir)
  if (!dir.exists(full_output_dir)) dir.create(full_output_dir, recursive = TRUE, showWarnings = FALSE)
  
  ## ------------------------------
  ## safer is.nsim detection
  ## ------------------------------
  is.nsim <- FALSE
  try({
    is.nsim <- is.list(mods[[1]]) && length(mods[[1]]) > 0 && is.list(mods[[1]][[1]])
  }, silent = TRUE)
  
  ## --------------------------------------------------
  ## helpers
  ## --------------------------------------------------
  safe_exists <- function(fn) exists(fn, mode = "function")
  
  # call function but ONLY with args it supports
  call_if_has_args <- function(fn, args) {
    fml <- tryCatch(formals(fn), error = function(e) NULL)
    if (is.null(fml)) return(invisible(NULL))
    keep <- intersect(names(args), names(fml))
    do.call(fn, args[keep])
  }
  
  # one unified “safe runner”
  run_plot <- function(name, fn_name, args, pb = NULL) {
    if (!is.null(pb)) pb$tick(tokens = list(name = name))
    if (!safe_exists(fn_name)) return(invisible(NULL))
    fn <- get(fn_name, mode = "function")
    tryCatch(
      call_if_has_args(fn, args),
      error = function(e) message(name, " failed: ", e$message)
    )
    invisible(NULL)
  }
  
  # detect allowed integer types from match.arg error message (“1”, “2”, “3” ...)
  detect_types <- function(fn, type_arg = c("bar_type", "radar_type"), base_args = list(), fallback = 1:1) {
    type_arg <- match.arg(type_arg)
    fml <- tryCatch(formals(fn), error = function(e) NULL)
    if (is.null(fml) || !(type_arg %in% names(fml))) return(fallback)
    
    msg <- tryCatch(
      call_if_has_args(fn, c(base_args, setNames(list("___bad___"), type_arg))),
      error = function(e) conditionMessage(e)
    )
    
    if (is.null(msg)) return(fallback)
    
    vals <- unlist(regmatches(msg, gregexpr("“[0-9]+”", msg)))
    vals <- gsub("“|”", "", vals)
    vals <- suppressWarnings(as.integer(vals))
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) return(fallback)
    sort(unique(vals))
  }
  
  ## ------------------------------
  ## Build summary once (optional)
  ## ------------------------------
  mse_summary <- NULL
  if (safe_exists("build_mse_summary")) {
    mse_summary <- tryCatch(
      build_mse_summary(
        mods              = mods,
        is.nsim           = is.nsim,
        start.years       = start.years,
        use.n.years.first = use.n.years.first,
        use.n.years.last  = use.n.years.last
      ),
      error = function(e) {
        message("build_mse_summary() failed: ", e$message)
        NULL
      }
    )
  } else {
    message("build_mse_summary() not found. Continuing without mse_summary.")
  }
  
  old_opt <- getOption("mse_summary", NULL)
  options(mse_summary = mse_summary)
  on.exit(options(mse_summary = old_opt), add = TRUE)
  
  have_progress <- isTRUE(progress) && requireNamespace("progress", quietly = TRUE)
  make_pb <- function(total, label = "Progress") {
    if (!have_progress || is.null(total) || total <= 0) return(NULL)
    progress::progress_bar$new(
      format = paste0(label, " [:bar] :percent | :current/:total | :name"),
      total  = total, clear = FALSE, width = 60
    )
  }
  
  ## --------------------------------------------------
  ## Holistic wind-rose + heatmap
  ## --------------------------------------------------
  call_holistic_windrose_heat <- function() {
    if (!safe_exists("plot_model_performance_holistic")) {
      message("plot_model_performance_holistic() not found. Skipping wind-rose/heatmap holistic plots.")
      return(invisible(NULL))
    }
    
    fn <- get("plot_model_performance_holistic", mode = "function")
    base_args <- list(
      mods = mods, is.nsim = is.nsim,
      main.dir = main_dir, sub.dir = output_dir,
      width = width, height = height, dpi = dpi,
      col.opt = col.opt,
      method = method,
      use.n.years.first = use.n.years.first,
      use.n.years.last  = use.n.years.last,
      start.years       = start.years,
      new_model_names   = new_model_names,
      base.model        = base.model
    )
    
    tryCatch(call_if_has_args(fn, c(base_args, list(plot_type = "rose"))),
             error = function(e) message("Holistic wind-rose failed: ", e$message))
    
    tryCatch(call_if_has_args(fn, c(base_args, list(plot_type = "heat"))),
             error = function(e) message("Holistic heatmap failed: ", e$message))
    
    invisible(NULL)
  }
  
  ## --------------------------------------------------
  ## Radar plots (ALL supported types)
  ## --------------------------------------------------
  call_radar_all <- function() {
    if (!safe_exists("plot_model_performance_radar")) {
      message("plot_model_performance_radar() not found. Skipping radar plots.")
      return(invisible(NULL))
    }
    
    fn <- get("plot_model_performance_radar", mode = "function")
    base_args <- list(
      mods = mods, is.nsim = is.nsim,
      main.dir = main_dir, sub.dir = output_dir,
      width = width, height = height, dpi = dpi, col.opt = col.opt,
      method = method,
      use.n.years.first = use.n.years.first,
      use.n.years.last  = use.n.years.last,
      start.years       = start.years,
      new_model_names   = new_model_names,
      base.model        = base.model
    )
    
    rts <- tryCatch(
      detect_types(fn, "radar_type", base_args = base_args, fallback = 1:3),
      error = function(e) 1:3
    )
    
    for (rt in rts) {
      tryCatch(call_if_has_args(fn, c(base_args, list(radar_type = rt))),
               error = function(e) message("Radar type ", rt, " failed: ", e$message))
    }
    invisible(NULL)
  }
  
  ## --------------------------------------------------
  ## Holistic performance bar plot (ALL supported types)
  ## --------------------------------------------------
  call_holistic_bar_all <- function() {
    if (!safe_exists("plot_model_performance_bar")) {
      message("plot_model_performance_bar() not found. Skipping holistic performance bar plots.")
      return(invisible(NULL))
    }
    
    fn <- get("plot_model_performance_bar", mode = "function")
    base_args <- list(
      mods = mods, is.nsim = is.nsim,
      main.dir = main_dir, sub.dir = output_dir,
      width = width, height = height, dpi = dpi, col.opt = col.opt,
      method = method,
      use.n.years.first = use.n.years.first,
      use.n.years.last  = use.n.years.last,
      start.years       = start.years,
      new_model_names   = new_model_names,
      base.model        = base.model
    )
    
    bts <- tryCatch(
      detect_types(fn, "bar_type", base_args = base_args, fallback = 1:6),
      error = function(e) 1:6
    )
    
    for (bt in bts) {
      tryCatch(call_if_has_args(fn, c(base_args, list(bar_type = bt))),
               error = function(e) message("Holistic bar type ", bt, " failed: ", e$message))
    }
    invisible(NULL)
  }
  
  ## --------------------------------------------------
  ## 1) ALWAYS regenerate PNGs (unless user didn't ask for png at all)
  ## --------------------------------------------------
  if ("png" %in% output_format || isTRUE(report_from_pngs)) {
    cat("\nRegenerating PNG plots...\n")
  }
  
  ## --------------------------------------------------
  ## Plot calls (order matters)
  ## --------------------------------------------------
  plot_calls <- list(
    
    ## ---- Time series ----
    list(name = "SSB time series", fn = "plot_ssb_time_series", args = list(
      mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
      var = "SSB", width = width, height = height, dpi = dpi, col.opt = col.opt,
      new_model_names = new_model_names
    )),
    
    list(name = "Fbar time series", fn = "plot_fbar_time_series", args = list(
      mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
      var = "Fbar", width = width, height = height, dpi = dpi, col.opt = col.opt,
      new_model_names = new_model_names
    )),
    
    list(name = "Catch time series", fn = "plot_catch_time_series", args = list(
      mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
      var = "Catch", width = width, height = height, dpi = dpi, col.opt = col.opt,
      new_model_names = new_model_names
    )),
    
    ## ---- Relative trajectories ----
    list(name = "Relative trajectories", fn = "plot_relative_trajectories", args = list(
      mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
      base.model = base.model, new_model_names = new_model_names,
      width = width, height = height, dpi = dpi, col.opt = col.opt
    )),
    
    ## ---- Catch performance (absolute; per-year window) ----
    list(name = sprintf("Catch performance (first %d years)", use.n.years.first),
         fn = "plot_catch_performance", args = list(
           mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
           level = c("global","region","fleet"),
           period = "first", start.years = start.years, use.n.years = use.n.years.first,
           width = width, height = height, dpi = dpi, col.opt = col.opt,
           method = method, outlier.opt = outlier.opt, plot.style = plot.style,
           show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
           new_model_names = new_model_names,
           base.model = NULL
         )),
    
    list(name = sprintf("Catch performance (last %d years)", use.n.years.last),
         fn = "plot_catch_performance", args = list(
           mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
           level = c("global","region","fleet"),
           period = "last", use.n.years = use.n.years.last,
           width = width, height = height, dpi = dpi, col.opt = col.opt,
           method = method, outlier.opt = outlier.opt, plot.style = plot.style,
           show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
           new_model_names = new_model_names,
           base.model = NULL
         )),
    
    list(name = "Catch performance (all years)",
         fn = "plot_catch_performance", args = list(
           mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
           level = c("global","region","fleet"),
           period = "all",
           width = width, height = height, dpi = dpi, col.opt = col.opt,
           method = method, outlier.opt = outlier.opt, plot.style = plot.style,
           show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
           new_model_names = new_model_names,
           base.model = NULL
         )),
    
    ## ---- TOTAL Catch performance (absolute) ----
    list(name = sprintf("TOTAL Catch performance (first %d years)", use.n.years.first),
         fn = "plot_catch_performance", args = list(
           mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
           level = c("global","region","fleet"),
           period = "first", start.years = start.years, use.n.years = use.n.years.first,
           width = width, height = height, dpi = dpi, col.opt = col.opt,
           method = method, outlier.opt = outlier.opt, plot.style = plot.style,
           show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
           new_model_names = new_model_names,
           base.model = NULL,
           catch_total = c("global","region","fleet")
         )),
    
    list(name = sprintf("TOTAL Catch performance (last %d years)", use.n.years.last),
         fn = "plot_catch_performance", args = list(
           mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
           level = c("global","region","fleet"),
           period = "last", use.n.years = use.n.years.last,
           width = width, height = height, dpi = dpi, col.opt = col.opt,
           method = method, outlier.opt = outlier.opt, plot.style = plot.style,
           show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
           new_model_names = new_model_names,
           base.model = NULL,
           catch_total = c("global","region","fleet")
         )),
    
    list(name = "TOTAL Catch performance (all years)",
         fn = "plot_catch_performance", args = list(
           mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
           level = c("global","region","fleet"),
           period = "all",
           width = width, height = height, dpi = dpi, col.opt = col.opt,
           method = method, outlier.opt = outlier.opt, plot.style = plot.style,
           show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
           new_model_names = new_model_names,
           base.model = NULL,
           catch_total = c("global","region","fleet")
         )),
    
    ## ---- SSB performance ----
    list(name = sprintf("SSB performance (first %d years)", use.n.years.first),
         fn = "plot_ssb_performance", args = list(
           mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
           level = c("global","region"),
           period = "first", start.years = start.years, use.n.years = use.n.years.first,
           width = width, height = height, dpi = dpi, col.opt = col.opt,
           method = method, outlier.opt = outlier.opt, plot.style = plot.style,
           show.whisker = show.whisker,
           new_model_names = new_model_names, base.model = NULL
         )),
    
    list(name = sprintf("SSB performance (last %d years)", use.n.years.last),
         fn = "plot_ssb_performance", args = list(
           mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
           level = c("global","region"),
           period = "last", use.n.years = use.n.years.last,
           width = width, height = height, dpi = dpi, col.opt = col.opt,
           method = method, outlier.opt = outlier.opt, plot.style = plot.style,
           show.whisker = show.whisker,
           new_model_names = new_model_names, base.model = NULL
         )),
    
    ## ---- Fbar performance ----
    list(name = sprintf("Fbar performance (first %d years)", use.n.years.first),
         fn = "plot_fbar_performance", args = list(
           mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
           level = c("global","region","fleet"),
           period = "first", start.years = start.years, use.n.years = use.n.years.first,
           width = width, height = height, dpi = dpi, col.opt = col.opt,
           method = method, outlier.opt = outlier.opt, plot.style = plot.style,
           show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
           new_model_names = new_model_names, base.model = NULL
         )),
    
    list(name = sprintf("Fbar performance (last %d years)", use.n.years.last),
         fn = "plot_fbar_performance", args = list(
           mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
           level = c("global","region","fleet"),
           period = "last", use.n.years = use.n.years.last,
           width = width, height = height, dpi = dpi, col.opt = col.opt,
           method = method, outlier.opt = outlier.opt, plot.style = plot.style,
           show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
           new_model_names = new_model_names, base.model = NULL
         )),
    
    ## ---- Relative performance (vs base.model) ----
    list(name = "Relative performance (Catch/SSB/Fbar)", fn = ".__internal__", args = list()),
    
    ## ---- Annual variation ----
    list(name = sprintf("Average Annual Variation (start.years=%d)", start.years), fn = ".__internal__", args = list()),
    
    ## ---- Status ----
    list(name = "Stock status (first/last)", fn = ".__internal__", args = list()),
    
    ## ---- Kobe ----
    list(name = "Kobe plots (ST/LT)", fn = ".__internal__", args = list()),
    
    ## ---- Holistic performance ----
    list(name = "Holistic performance (bar plots) - all supported types", fn = ".__internal__", args = list()),
    list(name = "Model performance radar (all types)", fn = ".__internal__", args = list()),
    list(name = "Holistic performance (wind-rose + heatmap)", fn = ".__internal__", args = list()),
    
    ## ---- Diagnostics ----
    list(name = "Diagnostics (Recruitment/NAA/etc.)", fn = "plot_rec_and_NAA_par", args = list(
      mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
      width = width, height = height, dpi = dpi, col.opt = col.opt,
      outlier.opt = outlier.opt, new_model_names = new_model_names
    ))
  )
  
  ## ---- run plot calls ----
  pb_png <- make_pb(length(plot_calls), label = "PNG")
  
  for (i in seq_along(plot_calls)) {
    nm <- plot_calls[[i]]$name
    fn <- plot_calls[[i]]$fn
    args <- plot_calls[[i]]$args
    
    ## internal composite steps (still safe: every actual call uses call_if_has_args)
    if (identical(fn, ".__internal__")) {
      
      if (nm == "Relative performance (Catch/SSB/Fbar)") {
        if (!is.null(base.model)) {
          
          # Catch (per-year)
          if (safe_exists("plot_catch_performance")) {
            run_plot("Relative Catch (first)", "plot_catch_performance", c(args, list(
              mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
              level = c("global","region","fleet"),
              period = "first", start.years = start.years, use.n.years = use.n.years.first,
              width = width, height = height, dpi = dpi, col.opt = col.opt,
              method = method, outlier.opt = outlier.opt, plot.style = plot.style,
              show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
              new_model_names = new_model_names, base.model = base.model
            )))
            run_plot("Relative Catch (last)", "plot_catch_performance", list(
              mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
              level = c("global","region","fleet"),
              period = "last", use.n.years = use.n.years.last,
              width = width, height = height, dpi = dpi, col.opt = col.opt,
              method = method, outlier.opt = outlier.opt, plot.style = plot.style,
              show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
              new_model_names = new_model_names, base.model = base.model
            ))
            run_plot("Relative Catch (all)", "plot_catch_performance", list(
              mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
              level = c("global","region","fleet"),
              period = "all",
              width = width, height = height, dpi = dpi, col.opt = col.opt,
              method = method, outlier.opt = outlier.opt, plot.style = plot.style,
              show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
              new_model_names = new_model_names, base.model = base.model
            ))
            
            # TOTAL catch (only if supported)
            run_plot("Relative TOTAL Catch (first)", "plot_catch_performance", list(
              mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
              level = c("global","region","fleet"),
              period = "first", start.years = start.years, use.n.years = use.n.years.first,
              width = width, height = height, dpi = dpi, col.opt = col.opt,
              method = method, outlier.opt = outlier.opt, plot.style = plot.style,
              show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
              new_model_names = new_model_names, base.model = base.model,
              catch_total = c("global","region","fleet")
            ))
            run_plot("Relative TOTAL Catch (last)", "plot_catch_performance", list(
              mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
              level = c("global","region","fleet"),
              period = "last", use.n.years = use.n.years.last,
              width = width, height = height, dpi = dpi, col.opt = col.opt,
              method = method, outlier.opt = outlier.opt, plot.style = plot.style,
              show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
              new_model_names = new_model_names, base.model = base.model,
              catch_total = c("global","region","fleet")
            ))
            run_plot("Relative TOTAL Catch (all)", "plot_catch_performance", list(
              mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
              level = c("global","region","fleet"),
              period = "all",
              width = width, height = height, dpi = dpi, col.opt = col.opt,
              method = method, outlier.opt = outlier.opt, plot.style = plot.style,
              show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
              new_model_names = new_model_names, base.model = base.model,
              catch_total = c("global","region","fleet")
            ))
          }
          
          # SSB
          if (safe_exists("plot_ssb_performance")) {
            run_plot("Relative SSB (first)", "plot_ssb_performance", list(
              mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
              level = c("global","region"),
              period = "first", start.years = start.years, use.n.years = use.n.years.first,
              width = width, height = height, dpi = dpi, col.opt = col.opt,
              method = method, outlier.opt = outlier.opt, plot.style = plot.style,
              show.whisker = show.whisker,
              new_model_names = new_model_names, base.model = base.model
            ))
            run_plot("Relative SSB (last)", "plot_ssb_performance", list(
              mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
              level = c("global","region"),
              period = "last", use.n.years = use.n.years.last,
              width = width, height = height, dpi = dpi, col.opt = col.opt,
              method = method, outlier.opt = outlier.opt, plot.style = plot.style,
              show.whisker = show.whisker,
              new_model_names = new_model_names, base.model = base.model
            ))
          }
          
          # Fbar
          if (safe_exists("plot_fbar_performance")) {
            run_plot("Relative Fbar (first)", "plot_fbar_performance", list(
              mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
              level = c("global","region","fleet"),
              period = "first", start.years = start.years, use.n.years = use.n.years.first,
              width = width, height = height, dpi = dpi, col.opt = col.opt,
              method = method, outlier.opt = outlier.opt, plot.style = plot.style,
              show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
              new_model_names = new_model_names, base.model = base.model
            ))
            run_plot("Relative Fbar (last)", "plot_fbar_performance", list(
              mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
              level = c("global","region","fleet"),
              period = "last", use.n.years = use.n.years.last,
              width = width, height = height, dpi = dpi, col.opt = col.opt,
              method = method, outlier.opt = outlier.opt, plot.style = plot.style,
              show.whisker = show.whisker, f.ymin = f.ymin, f.ymax = f.ymax,
              new_model_names = new_model_names, base.model = base.model
            ))
          }
        }
        
      } else if (grepl("^Average Annual Variation", nm)) {
        
        if (safe_exists("plot_annual_variation")) {
          run_plot("Annual variation (Catch)", "plot_annual_variation", list(
            mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
            metric = "Catch", start.years = start.years,
            width = width, height = height, dpi = dpi, col.opt = col.opt,
            outlier.opt = outlier.opt, new_model_names = new_model_names, base.model = NULL
          ))
          run_plot("Annual variation (SSB)", "plot_annual_variation", list(
            mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
            metric = "SSB", start.years = start.years,
            width = width, height = height, dpi = dpi, col.opt = col.opt,
            outlier.opt = outlier.opt, new_model_names = new_model_names, base.model = NULL
          ))
          run_plot("Annual variation (Fbar)", "plot_annual_variation", list(
            mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
            metric = "Fbar", start.years = start.years,
            width = width, height = height, dpi = dpi, col.opt = col.opt,
            outlier.opt = outlier.opt, new_model_names = new_model_names, base.model = NULL
          ))
        }
        
      } else if (nm == "Stock status (first/last)") {
        
        if (safe_exists("plot_status_all")) {
          run_plot("Status SSB (first)", "plot_status_all", list(
            mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
            metric = "SSB", period = "first",
            width = width, height = height, dpi = dpi, col.opt = col.opt,
            method = method, outlier.opt = outlier.opt, plot.style = plot.style,
            show.whisker = show.whisker, use.n.years = use.n.years.first,
            start.years = start.years, f.ymin = f.ymin, f.ymax = f.ymax,
            new_model_names = new_model_names, base.model = base.model,
            plot_prob = TRUE
          ))
          run_plot("Status SSB (last)", "plot_status_all", list(
            mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
            metric = "SSB", period = "last",
            width = width, height = height, dpi = dpi, col.opt = col.opt,
            method = method, outlier.opt = outlier.opt, plot.style = plot.style,
            show.whisker = show.whisker, use.n.years = use.n.years.last,
            start.years = start.years, f.ymin = f.ymin, f.ymax = f.ymax,
            new_model_names = new_model_names, base.model = base.model,
            plot_prob = TRUE
          ))
          run_plot("Status Fbar (first)", "plot_status_all", list(
            mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
            metric = "Fbar", period = "first",
            width = width, height = height, dpi = dpi, col.opt = col.opt,
            method = method, outlier.opt = outlier.opt, plot.style = plot.style,
            show.whisker = show.whisker, use.n.years = use.n.years.first,
            start.years = start.years, f.ymin = f.ymin, f.ymax = f.ymax,
            new_model_names = new_model_names, base.model = base.model,
            plot_prob = TRUE
          ))
          run_plot("Status Fbar (last)", "plot_status_all", list(
            mods = mods, is.nsim = is.nsim, main.dir = main_dir, sub.dir = output_dir,
            metric = "Fbar", period = "last",
            width = width, height = height, dpi = dpi, col.opt = col.opt,
            method = method, outlier.opt = outlier.opt, plot.style = plot.style,
            show.whisker = show.whisker, use.n.years = use.n.years.last,
            start.years = start.years, f.ymin = f.ymin, f.ymax = f.ymax,
            new_model_names = new_model_names, base.model = base.model,
            plot_prob = TRUE
          ))
        }
        
      } else if (nm == "Kobe plots (ST/LT)") {
        
        if (safe_exists("plot_kobe_status")) {
          base_args <- list(
            mods = mods, is.nsim = is.nsim,
            main.dir = main_dir, sub.dir = output_dir,
            width = width, height = height, dpi = dpi, col.opt = col.opt,
            new_model_names = new_model_names, nbins = 20
          )
          run_plot("Kobe (first)", "plot_kobe_status", c(base_args, list(
            period = "first",
            start.years = start.years,
            use.n.years = use.n.years.first,
            plot_style = "both"
          )))
          run_plot("Kobe (last)", "plot_kobe_status", c(base_args, list(
            period = "last",
            use.n.years = use.n.years.last,
            plot_style = "both"
          )))
        }
        
      } else if (nm == "Holistic performance (bar plots) - all supported types") {
        if (!is.null(pb_png)) pb_png$tick(tokens = list(name = nm))
        call_holistic_bar_all()
        
      } else if (nm == "Model performance radar (all types)") {
        if (!is.null(pb_png)) pb_png$tick(tokens = list(name = nm))
        call_radar_all()
        
      } else if (nm == "Holistic performance (wind-rose + heatmap)") {
        if (!is.null(pb_png)) pb_png$tick(tokens = list(name = nm))
        call_holistic_windrose_heat()
      }
      
      next
    }
    
    ## normal single-function calls
    run_plot(nm, fn, args, pb = pb_png)
  }
  
  ## ---- collect all PNGs ----
  all_pngs <- list.files(full_output_dir, pattern = "\\.png$", recursive = TRUE,
                         full.names = TRUE, ignore.case = TRUE)
  all_pngs <- unique(normalizePath(all_pngs, winslash = "/", mustWork = FALSE))
  all_pngs <- all_pngs[file.exists(all_pngs)]
  
  ## --------------------------------------------------
  ## 2) Render HTML/PDF report from PNGs
  ## --------------------------------------------------
  render_from_pngs <- function(fmt) {
    stopifnot(fmt %in% c("pdf", "html"))
    report_dir <- full_output_dir
    tl <- tolower(all_pngs)
    
    ## ---- classifiers ----
    is_reltraj <- function(p) grepl("relative_traject|relative-?traject|rel_traject", tolower(p))
    
    is_rel_perf  <- function(p) grepl("relative", tolower(p)) && !is_reltraj(p)
    is_rel_catch <- function(p) is_rel_perf(p) && grepl("catch", tolower(p))
    is_rel_fbar  <- function(p) is_rel_perf(p) && grepl("fbar",  tolower(p))
    is_rel_ssb   <- function(p) is_rel_perf(p) && grepl("ssb",   tolower(p))
    
    is_status <- function(p) grepl("status", tolower(p))
    is_first  <- function(p) grepl("first_\\d+_years", tolower(p))
    is_last   <- function(p) grepl("last_\\d+_years",  tolower(p))
    
    is_perf_window_first <- function(p) grepl("_first_\\d+_years", tolower(p))
    is_perf_window_last  <- function(p) grepl("_last_\\d+_years",  tolower(p))
    is_perf_window_all   <- function(p) grepl("_all_?years|_all_?year", tolower(p))
    
    is_not_other_family <- function(p) {
      x <- tolower(p)
      !grepl("time_series|timeseries|relative|status|kobe|radar|annual_variation", x)
    }
    
    is_ssb_perf   <- function(p) is_not_other_family(p) && grepl("ssb",  tolower(p))
    is_fbar_perf  <- function(p) is_not_other_family(p) && grepl("fbar", tolower(p))
    is_catch_perf <- function(p) is_not_other_family(p) && grepl("catch", tolower(p))
    
    is_annual_var <- function(p) grepl("annual_variation_boxplot|annual_variation", tolower(p))
    
    is_holistic_bar <- function(p) {
      x <- tolower(p); b <- tolower(basename(p))
      grepl("holistic_bar_plot", x) || grepl("^overall_performance_type[0-9]+\\.png$", b)
    }
    
    is_radar <- function(p) grepl("radar", tolower(p))
    
    is_holistic_rose <- function(p) {
      x <- tolower(p); b <- tolower(basename(p))
      grepl("holistic_performance", x) && grepl("holistic_rose\\.png$", b)
    }
    
    is_holistic_heat <- function(p) {
      x <- tolower(p); b <- tolower(basename(p))
      grepl("holistic_performance", x) && grepl("holistic_heat\\.png$", b)
    }
    
    is_diag_recnaa <- function(p) grepl("mean_rec|\\bnaa\\b", tolower(p))
    is_diag_other  <- function(p) grepl("variance_para|diagnostic", tolower(p))
    
    make_subsection <- function(title, files) list(level = 2, title = title, files = files)
    make_section    <- function(title, subsections) list(level = 1, title = title, subsections = subsections)
    
    plan <- list(
      
      make_section("Realized Time Series", list(
        make_subsection("Time series plots",
                        all_pngs[grepl("time_series|timeseries|ssb.*time|fbar.*time|catch.*time", tl)])
      )),
      
      make_section("Relative Trajectories", list(
        make_subsection("Relative trajectories",
                        all_pngs[vapply(all_pngs, is_reltraj, logical(1))])
      )),
      
      make_section("Relative Performance", list(
        make_subsection(sprintf("Catch (short-term, first %d years)", use.n.years.first),
                        all_pngs[vapply(all_pngs, is_rel_catch, logical(1)) &
                                   vapply(all_pngs, is_perf_window_first, logical(1))]),
        make_subsection(sprintf("Catch (long-term, last %d years)", use.n.years.last),
                        all_pngs[vapply(all_pngs, is_rel_catch, logical(1)) &
                                   vapply(all_pngs, is_perf_window_last, logical(1))]),
        make_subsection("Catch (all years)",
                        all_pngs[vapply(all_pngs, is_rel_catch, logical(1)) &
                                   vapply(all_pngs, is_perf_window_all, logical(1))]),
        make_subsection(sprintf("SSB (short-term, first %d years)", use.n.years.first),
                        all_pngs[vapply(all_pngs, is_rel_ssb, logical(1)) &
                                   vapply(all_pngs, is_perf_window_first, logical(1))]),
        make_subsection(sprintf("SSB (long-term, last %d years)", use.n.years.last),
                        all_pngs[vapply(all_pngs, is_rel_ssb, logical(1)) &
                                   vapply(all_pngs, is_perf_window_last, logical(1))]),
        make_subsection(sprintf("Fbar (short-term, first %d years)", use.n.years.first),
                        all_pngs[vapply(all_pngs, is_rel_fbar, logical(1)) &
                                   vapply(all_pngs, is_perf_window_first, logical(1))]),
        make_subsection(sprintf("Fbar (long-term, last %d years)", use.n.years.last),
                        all_pngs[vapply(all_pngs, is_rel_fbar, logical(1)) &
                                   vapply(all_pngs, is_perf_window_last, logical(1))])
      )),
      
      make_section("Performance (absolute scale)", list(
        make_subsection(sprintf("Catch (short-term, first %d years)", use.n.years.first),
                        all_pngs[vapply(all_pngs, is_catch_perf, logical(1)) &
                                   vapply(all_pngs, is_perf_window_first, logical(1))]),
        make_subsection(sprintf("Catch (long-term, last %d years)", use.n.years.last),
                        all_pngs[vapply(all_pngs, is_catch_perf, logical(1)) &
                                   vapply(all_pngs, is_perf_window_last, logical(1))]),
        make_subsection("Catch (all years)",
                        all_pngs[vapply(all_pngs, is_catch_perf, logical(1)) &
                                   vapply(all_pngs, is_perf_window_all, logical(1))]),
        make_subsection(sprintf("SSB (short-term, first %d years)", use.n.years.first),
                        all_pngs[vapply(all_pngs, is_ssb_perf, logical(1)) &
                                   vapply(all_pngs, is_perf_window_first, logical(1))]),
        make_subsection(sprintf("SSB (long-term, last %d years)", use.n.years.last),
                        all_pngs[vapply(all_pngs, is_ssb_perf, logical(1)) &
                                   vapply(all_pngs, is_perf_window_last, logical(1))]),
        make_subsection(sprintf("Fbar (short-term, first %d years)", use.n.years.first),
                        all_pngs[vapply(all_pngs, is_fbar_perf, logical(1)) &
                                   vapply(all_pngs, is_perf_window_first, logical(1))]),
        make_subsection(sprintf("Fbar (long-term, last %d years)", use.n.years.last),
                        all_pngs[vapply(all_pngs, is_fbar_perf, logical(1)) &
                                   vapply(all_pngs, is_perf_window_last, logical(1))])
      )),
      
      make_section(sprintf("Average Annual Variation (start.years=%d)", start.years), list(
        make_subsection("Annual variation plots",
                        all_pngs[vapply(all_pngs, is_annual_var, logical(1))])
      )),
      
      make_section("Stock Status", list(
        make_subsection(sprintf("Short-term (first %d years)", use.n.years.first),
                        all_pngs[vapply(all_pngs, is_status, logical(1)) &
                                   vapply(all_pngs, is_first, logical(1))]),
        make_subsection(sprintf("Long-term (last %d years)", use.n.years.last),
                        all_pngs[vapply(all_pngs, is_status, logical(1)) &
                                   vapply(all_pngs, is_last, logical(1))])
      )),
      
      make_section("Kobe Plots", list(
        make_subsection(
          sprintf("Short-term (first %d years, start.years=%d)", use.n.years.first, start.years),
          all_pngs[grepl("kobe_plot", tl) & grepl("kobe_first", tl)]
        ),
        make_subsection(
          sprintf("Long-term (last %d years)", use.n.years.last),
          all_pngs[grepl("kobe_last",  tl)]
        )
      )),
      
      make_section("Holistic Performance", list(
        make_subsection("Performance bar plots",
                        all_pngs[vapply(all_pngs, is_holistic_bar, logical(1))]),
        make_subsection("Radar plots",
                        all_pngs[vapply(all_pngs, is_radar, logical(1))]),
        make_subsection("Wind-rose (stacked polar)",
                        all_pngs[vapply(all_pngs, is_holistic_rose, logical(1))]),
        make_subsection("Heatmap (Model × Metric)",
                        all_pngs[vapply(all_pngs, is_holistic_heat, logical(1))])
      )),
      
      make_section("Diagnostics", list(
        make_subsection("Recruitment params",
                        all_pngs[vapply(all_pngs, is_diag_recnaa, logical(1))]),
        make_subsection("NAA params",
                        all_pngs[vapply(all_pngs, is_diag_other, logical(1))])
      ))
    )
    
    ## ---- de-duplicate across subsections (first match wins) ----
    used <- character(0)
    for (si in seq_along(plan)) {
      subs <- plan[[si]]$subsections
      for (sj in seq_along(subs)) {
        f <- subs[[sj]]$files
        f <- f[file.exists(f)]
        f <- f[!f %in% used]
        subs[[sj]]$files <- sort(unique(f))
        used <- c(used, subs[[sj]]$files)
      }
      plan[[si]]$subsections <- subs
    }
    
    total_imgs <- sum(vapply(plan, function(s) {
      sum(vapply(s$subsections, function(ss) length(ss$files), integer(1)))
    }, integer(1)))
    
    rmd_path <- file.path(report_dir, paste0("mse_report_", fmt, ".Rmd"))
    out_file <- paste0("mse_report.", fmt)
    
    to_rel <- function(f) {
      rel <- substring(f, nchar(report_dir) + 2)
      rel <- file.path(".", rel)
      gsub("\\\\", "/", rel)
    }
    
    ## ---- YAML/header ----
    if (fmt == "html") {
      header <- c(
        "---",
        "title: \"MSE Output Report\"",
        "output:",
        "  html_document:",
        "    toc: true",
        "    toc_depth: 2",
        "    toc_float: true",
        "    number_sections: true",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
        "have_progress <- requireNamespace('progress', quietly = TRUE)",
        paste0("total_imgs <- ", total_imgs),
        "pb_env <- new.env(parent = emptyenv())",
        "pb_env$pb <- NULL",
        "if (have_progress && total_imgs > 0) {",
        "  pb_env$pb <- progress::progress_bar$new(",
        "    format = 'Rendering [:bar] :percent | :current/:total | :section',",
        "    total  = total_imgs, clear = FALSE, width = 60)",
        "}",
        "tick_pb <- function(section_label = '') {",
        "  if (!is.null(pb_env$pb)) pb_env$pb$tick(tokens = list(section = section_label))",
        "  invisible(NULL)",
        "}",
        "```",
        ""
      )
    } else {
      header <- c(
        "---",
        "title: \"MSE Output Report\"",
        "output:",
        "  pdf_document:",
        "    toc: false",
        "    number_sections: true",
        "header-includes:",
        "  - \\usepackage{hyperref}",
        "  - \\hypersetup{colorlinks=true,linkcolor=blue,urlcolor=blue}",
        "  - \\usepackage{tocloft}",
        paste0("  - \\setlength{\\cftbeforesecskip}{", toc_entry_vspace_pt, "pt}"),
        paste0("  - \\setlength{\\cftbeforesubsecskip}{", max(0, round(toc_entry_vspace_pt * 0.6)), "pt}"),
        paste0("  - \\renewcommand{\\baselinestretch}{", toc_line_spacing, "}"),
        "  - \\setlength{\\cftsecnumwidth}{2.8em}",
        "  - \\setlength{\\cftsubsecnumwidth}{3.6em}",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
        "have_progress <- requireNamespace('progress', quietly = TRUE)",
        paste0("total_imgs <- ", total_imgs),
        "pb_env <- new.env(parent = emptyenv())",
        "pb_env$pb <- NULL",
        "if (have_progress && total_imgs > 0) {",
        "  pb_env$pb <- progress::progress_bar$new(",
        "    format = 'Rendering [:bar] :percent | :current/:total | :section',",
        "    total  = total_imgs, clear = FALSE, width = 60)",
        "}",
        "tick_pb <- function(section_label = '') {",
        "  if (!is.null(pb_env$pb)) pb_env$pb$tick(tokens = list(section = section_label))",
        "  invisible(NULL)",
        "}",
        "```",
        "",
        "```{=latex}",
        "{\\Large\\bfseries Table of Contents}\\par\\vspace{1.2em}",
        "\\tableofcontents",
        "\\clearpage",
        "```",
        ""
      )
    }
    
    body <- character(0)
    for (sec in plan) {
      body <- c(body, paste0("# ", sec$title), "")
      for (sub in sec$subsections) {
        body <- c(body, paste0("## ", sub$title), "")
        files <- sub$files
        if (length(files) == 0) {
          body <- c(body, "*No plots generated for this subsection.*", "")
          next
        }
        for (k in seq_along(files)) {
          rel <- to_rel(files[[k]])
          chunk_lab <- paste0(
            "fig_",
            gsub("[^A-Za-z0-9]+", "_", sec$title), "_",
            gsub("[^A-Za-z0-9]+", "_", sub$title), "_",
            k
          )
          body <- c(
            body,
            paste0("```{r ", chunk_lab, ", out.width='100%'}"),
            paste0("tick_pb(", shQuote(sec$title), ")"),
            paste0("knitr::include_graphics(", shQuote(rel), ")"),
            "```",
            ""
          )
        }
      }
      if (fmt == "pdf") body <- c(body, "\\newpage", "")
    }
    
    writeLines(c(header, body), con = rmd_path)
    
    rmarkdown::render(
      input       = rmd_path,
      output_file = out_file,
      output_dir  = report_dir,
      envir       = new.env(parent = globalenv()),
      quiet       = FALSE
    )
    
    invisible(file.path(report_dir, out_file))
  }
  
  report_paths <- list(html = NULL, pdf = NULL)
  if (isTRUE(report_from_pngs) && any(c("html", "pdf") %in% output_format)) {
    if ("html" %in% output_format) {
      cat("\nRendering HTML report from PNGs...\n")
      report_paths$html <- tryCatch(render_from_pngs("html"),
                                    error = function(e) { message("HTML render failed: ", e$message); NULL })
    }
    if ("pdf" %in% output_format) {
      cat("\nRendering PDF report from PNGs...\n")
      report_paths$pdf <- tryCatch(render_from_pngs("pdf"),
                                   error = function(e) { message("PDF render failed: ", e$message); NULL })
    }
  }
  
  invisible(list(
    full_output_dir = full_output_dir,
    pngs            = all_pngs,
    reports         = report_paths
  ))
}
