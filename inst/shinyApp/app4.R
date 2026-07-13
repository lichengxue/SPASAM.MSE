# server.R
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)

spasam_mse_version <- tryCatch(as.character(utils::packageVersion("SPASAM.MSE")),
                               error = function(e) "1.1.5")

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ----- Small helper to safely coerce comma-separated numeric text -----
parse_num_csv <- function(txt, fallback) {
  if (is.null(txt) || !nzchar(txt)) return(fallback)
  out <- suppressWarnings(as.numeric(strsplit(txt, ",")[[1]]))
  if (anyNA(out)) fallback else out
}

server <- function(input, output, session) {
  # ---------------- NAVIGATION ----------------
  go <- function(tab) updateTabsetPanel(session, "step_tabs", selected = tab)
  observeEvent(input$next_to_seasons,    { go("2) Seasons") })
  observeEvent(input$back_to_setup,      { go("1) Setup") })
  observeEvent(input$next_to_lifehistory,{ go("3) Life History Trait") })
  observeEvent(input$back_to_seasons,    { go("2) Seasons") })
  observeEvent(input$next_to_lhpars,     { go("4) Life History Parameters") })
  observeEvent(input$back_to_lifehistory,{ go("3) Life History Trait") })
  observeEvent(input$next_to_fishery,    { go("5) Fishery Pattern") })
  observeEvent(input$back_to_lhpars,     { go("4) Life History Parameters") })
  observeEvent(input$next_to_catch,      { go("6) Catch Info") })
  observeEvent(input$back_to_fishery,    { go("5) Fishery Pattern") })
  observeEvent(input$next_to_index,      { go("7) Survey Info") })
  observeEvent(input$back_to_catch,      { go("6) Catch Info") })
  observeEvent(input$next_to_bias,       { go("9) Bias Correction") })
  observeEvent(input$back_to_index,      { go("7) Survey Info") })
  observeEvent(input$next_to_movement,   { go("10) Movement") })
  observeEvent(input$back_to_bias,       { go("9) Bias Correction") })
  observeEvent(input$next_to_review,     { go("11) Review & Export") })
  observeEvent(input$back_to_movement,   { go("10) Movement") })
  
  # ---------------- HELPERS (core generators) ----------------
  # maturity: logistic with a50 & slope
  Generate_Maturity <- function(a50 = 3.5, slope = 1, na = 10) {
    A <- seq_len(na)
    1 / (1 + exp(-((A - a50) / slope)))
  }
  # WAA from von Bertalanffy + length-weight
  Generate_WAA <- function(Linf = 90, k = 0.13, t0 = 0, aLW = 3e-6, bLW = 3, na = 10) {
    Len <- Linf * (1 - exp(-k * ((1:na) - t0)))
    as.numeric(aLW * Len^bLW)
  }
  
  ages_vec <- reactive({ seq_len(as.integer(input$n_ages %||% 10)) })
  years_vec <- reactive({
    start <- input$base_year_start %||% 2000
    end   <- input$base_year_end   %||% 2020
    seq(start, end)
  })
  
  # ============== STEP 1: TIMELINE ==============
  output$timelinePlot <- renderPlot({
    start <- input$base_year_start %||% 2000
    end   <- input$base_year_end   %||% 2020
    n_fb  <- input$n_feedback_years %||% 0
    years  <- start:(end + n_fb)
    period <- c(rep("Base", length(start:end)), rep("Feedback", n_fb))
    df <- data.frame(year = years, period = period)
    ggplot(df, aes(x = year, y = 1, fill = period)) +
      geom_col(width = 1) +
      scale_fill_manual(values = c("Base" = "steelblue", "Feedback" = "orange")) +
      scale_x_continuous(breaks = seq(start, end + n_fb, by = 5)) +
      labs(x = "Year", y = "", fill = "Period") +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "top")
  })
  
  # ============== STEP 2: SEASONS ==============
  output$seasonUI <- renderUI({
    ns <- input$n_seasons %||% 4
    if (input$season_mode == "Sliders") {
      lapply(1:ns, function(i) {
        sliderInput(paste0("season_", i), paste("Season", i, "fraction"),
                    min = 0, max = 1, value = round(1/ns, 3), step = 0.001)
      })
    } else {
      lapply(1:ns, function(i) {
        numericInput(paste0("season_", i), paste("Season", i, "fraction"),
                     value = round(1/ns, 3), min = 0, max = 1, step = 0.001)
      })
    }
  })
  season_vec <- reactive({
    ns <- input$n_seasons %||% 4
    sapply(1:ns, function(i) input[[paste0("season_", i)]] %||% (1/ns))
  })
  output$seasonSum <- renderUI({
    sm <- sum(season_vec())
    cls <- if (abs(sm - 1) < 1e-6) "muted" else "error-text"
    div(class = cls, sprintf("Sum of season fractions: %.3f (should be 1.000)", sm))
  })
  output$seasonPlot <- renderPlot({
    ns <- input$n_seasons %||% 4
    fracs <- season_vec()
    fracs <- if (sum(fracs) > 0) fracs / sum(fracs) else fracs
    barplot(fracs, names.arg = paste("Season", 1:ns),
            col = "skyblue", ylim = c(0, 1), main = "Season Fractions (normalized)")
    abline(h = 1, col = "red", lty = 2)
  })
  
  # ============== STEP 3: LIFE HISTORY TRAIT ==============
  observe({
    if (isTRUE(input$use_user_lh)) shinyjs::disable("life_history_choice") else shinyjs::enable("life_history_choice")
  })
  output$waaPlot_trait <- renderPlot({
    na <- input$n_ages %||% 10; ages <- 1:na
    if (!isTRUE(input$use_user_lh)) {
      # Use whatever mapping you like from life_history_choice to parameters.
      # For now, keep simple presets.
      pars <- switch(tolower(input$life_history_choice %||% "medium"),
                     "short"  = list(Linf = 60,  k = 0.25, t0 = 0, aLW = 3e-6, bLW = 3),
                     "long"   = list(Linf = 120, k = 0.08, t0 = 0, aLW = 3e-6, bLW = 3),
                     list(Linf = 90,  k = 0.13, t0 = 0, aLW = 3e-6, bLW = 3))
      waa <- Generate_WAA(pars$Linf, pars$k, pars$t0, pars$aLW, pars$bLW, na)
      plot(ages, waa, type = "b", pch = 16, col = "darkblue",
           xlab = "Age", ylab = "Weight-at-age", main = paste("WAA -", input$life_history_choice))
    } else { plot.new(); title("WAA locked — define in Step 4") }
  })
  output$matPlot_trait <- renderPlot({
    na <- input$n_ages %||% 10; ages <- 1:na
    if (!isTRUE(input$use_user_lh)) {
      pars <- switch(tolower(input$life_history_choice %||% "medium"),
                     "short"  = list(a50 = 2.5, slope = 0.9),
                     "long"   = list(a50 = 4.5, slope = 1.1),
                     list(a50 = 3.5, slope = 1.0))
      mat <- Generate_Maturity(pars$a50, pars$slope, na)
      plot(ages, mat, type = "b", pch = 16, col = "firebrick",
           xlab = "Age", ylab = "Maturity", main = paste("Maturity -", input$life_history_choice))
    } else { plot.new(); title("Maturity locked — define in Step 4") }
  })
  
  # ============== STEP 4: LIFE HISTORY PARAMETERS ==============
  
  # --- ALK default string synced with n_ages ---
  default_alk_string <- reactive({
    na <- as.integer(input$n_ages %||% 10)
    paste(seq(20, by = 10, length.out = na), collapse = ",")
  })
  
  observeEvent(list(input$waa_option, input$n_ages), {
    if (identical(input$waa_option, "ALK-driven") && isTRUE(input$use_global_WAA)) {
      cur <- input$alk_L_at_age %||% ""
      cur_nums <- suppressWarnings(as.numeric(strsplit(cur, ",")[[1]]))
      if (!isTRUE(length(cur_nums) == (input$n_ages %||% 10))) {
        updateTextInput(session, "alk_L_at_age", value = default_alk_string())
      }
    }
  }, ignoreInit = FALSE)
  
  # --- GLOBAL WAA parameter box (when use_global_WAA = TRUE) ---
  output$lhParams <- renderUI({
    req(input$use_user_lh)
    if (isTRUE(input$use_global_WAA)) {
      if (input$waa_option == "vonB+LW") {
        tagList(
          numericInput("vh_Linf", "L∞ (asymptotic length):", 90, step = 1),
          numericInput("vh_k",    "k (growth coefficient):", 0.13, step = 0.01),
          numericInput("vh_t0",   "t0:", 0, step = 0.1),
          numericInput("vh_aLW",  "aLW (length–weight a):", 3e-6, step = 1e-6),
          numericInput("vh_bLW",  "bLW (length–weight b):", 3, step = 0.1)
        )
      } else if (input$waa_option == "ALK-driven") {
        tagList(
          textInput("alk_L_at_age", "L-at-age vector (comma-separated):",
                    value = input$alk_L_at_age %||% default_alk_string(),
                    placeholder = "e.g. 20,30,40,..."),
          numericInput("alk_aLW", "aLW:", 0.01, step = 0.001),
          numericInput("alk_bLW", "bLW:", 3.0, step = 0.1)
        )
      } else if (input$waa_option == "Direct user array") {
        fileInput("waa_file", "Upload WAA array (.csv or .rds)", accept = c(".csv", ".rds"))
      }
    } else {
      NULL # per-source handled separately
    }
  })
  
  # --- GLOBAL WAA plots (only when use_global_WAA = TRUE) ---
  output$vonBPlot <- renderPlot({
    req(input$use_user_lh, isTRUE(input$use_global_WAA), input$waa_option == "vonB+LW")
    A <- ages_vec()
    Linf <- input$vh_Linf %||% 90
    k    <- input$vh_k    %||% 0.13
    t0   <- input$vh_t0   %||% 0
    Len  <- Linf * (1 - exp(-k * (A - t0)))
    plot(A, Len, type = "b", pch = 16, col = "blue",
         xlab = "Age", ylab = "Length", main = "von Bertalanffy Growth Curve")
  })
  output$LWPlot <- renderPlot({
    req(input$use_user_lh, isTRUE(input$use_global_WAA), input$waa_option == "vonB+LW")
    A <- ages_vec()
    Linf <- input$vh_Linf %||% 90
    k    <- input$vh_k    %||% 0.13
    t0   <- input$vh_t0   %||% 0
    Len  <- Linf * (1 - exp(-k * (A - t0)))
    W    <- (input$vh_aLW %||% 3e-6) * Len^(input$vh_bLW %||% 3)
    plot(Len, W, type = "b", pch = 16, col = "darkgreen",
         xlab = "Length", ylab = "Weight", main = "Length–Weight Relationship")
  })
  output$WAAPlot <- renderPlot({
    req(input$use_user_lh, isTRUE(input$use_global_WAA), input$waa_option == "vonB+LW")
    A <- ages_vec()
    Linf <- input$vh_Linf %||% 90
    k    <- input$vh_k    %||% 0.13
    t0   <- input$vh_t0   %||% 0
    Len  <- Linf * (1 - exp(-k * (A - t0)))
    W    <- (input$vh_aLW %||% 3e-6) * Len^(input$vh_bLW %||% 3)
    plot(A, W, type = "b", pch = 16, col = "purple",
         xlab = "Age", ylab = "Weight-at-age", main = "Weight-at-Age (WAA)")
  })
  output$alkLPlot <- renderPlot({
    req(input$use_user_lh, isTRUE(input$use_global_WAA), input$waa_option == "ALK-driven")
    A <- ages_vec()
    Lvec <- suppressWarnings(as.numeric(strsplit(input$alk_L_at_age %||% "", ",")[[1]]))
    validate(need(length(Lvec) == length(A),
                  sprintf("L-at-age must have length n_ages = %d", length(A))))
    plot(A, Lvec, type = "b", pch = 16, col = "steelblue",
         xlab = "Age", ylab = "Length", main = "Length-at-Age (ALK)")
  })
  output$alkWAAPlot <- renderPlot({
    req(input$use_user_lh, isTRUE(input$use_global_WAA), input$waa_option == "ALK-driven")
    A <- ages_vec()
    Lvec <- suppressWarnings(as.numeric(strsplit(input$alk_L_at_age %||% "", ",")[[1]]))
    validate(need(length(Lvec) == length(A),
                  sprintf("L-at-age must have length n_ages = %d", length(A))))
    aLW <- input$alk_aLW %||% 0.01
    bLW <- input$alk_bLW %||% 3
    W   <- aLW * Lvec^bLW
    plot(A, W, type = "b", pch = 16, col = "purple",
         xlab = "Age", ylab = "Weight-at-age", main = "WAA (ALK-driven)")
  })
  output$waaPlot_param <- renderPlot({
    req(input$use_user_lh, isTRUE(input$use_global_WAA), input$waa_option == "Direct user array")
    A <- ages_vec()
    req(input$waa_file)
    WAAin <- tryCatch({
      if (grepl("\\.csv$", input$waa_file$name, TRUE)) {
        as.matrix(read.csv(input$waa_file$datapath, check.names = FALSE))
      } else readRDS(input$waa_file$datapath)
    }, error = function(e) NULL)
    validate(need(!is.null(WAAin), "Invalid WAA file"))
    matplot(A, t(WAAin), type = "l", lty = 1,
            main = "Direct user WAA", xlab = "Age", ylab = "Weight-at-age")
  })
  
  # --- PER-SOURCE WAA inputs (when use_global_WAA = FALSE) ---
  # ---- VonB+LW per-source inputs (Fleet + Index + Population) ----
  output$waa_vonb_by_source_ui <- renderUI({
    req(input$use_user_lh, isFALSE(input$use_global_WAA), input$waa_option == "vonB+LW")
    ns <- input$n_stocks %||% 1
    nf <- input$n_fleets %||% 1
    ni <- input$n_indices %||% 1
    
    tagList(
      h5("Define WAA parameters for each source (Fleet + Index + Population):"),
      # Fleets
      lapply(1:nf, function(f) {
        wellPanel(
          h5(sprintf("WAA parameters: Fleet %d", f)),
          numericInput(sprintf("waa_fleet_Linf_%d", f), "L∞:", 90, step = 1),
          numericInput(sprintf("waa_fleet_k_%d",    f), "k:",   0.13, step = 0.01),
          numericInput(sprintf("waa_fleet_t0_%d",   f), "t0:",  0,    step = 0.1),
          numericInput(sprintf("waa_fleet_aLW_%d",  f), "aLW:", 3e-6, step = 1e-6),
          numericInput(sprintf("waa_fleet_bLW_%d",  f), "bLW:", 3,    step = 0.1)
        )
      }),
      # Indices
      lapply(1:ni, function(i) {
        wellPanel(
          h5(sprintf("WAA parameters: Index %d", i)),
          numericInput(sprintf("waa_index_Linf_%d", i), "L∞:", 90, step = 1),
          numericInput(sprintf("waa_index_k_%d",    i), "k:",   0.13, step = 0.01),
          numericInput(sprintf("waa_index_t0_%d",   i), "t0:",  0,    step = 0.1),
          numericInput(sprintf("waa_index_aLW_%d",  i), "aLW:", 3e-6, step = 1e-6),
          numericInput(sprintf("waa_index_bLW_%d",  i), "bLW:", 3,    step = 0.1)
        )
      }),
      # Populations
      lapply(1:ns, function(s) {
        wellPanel(
          h5(sprintf("WAA parameters: Population %d", s)),
          numericInput(sprintf("waa_pop_Linf_%d", s), "L∞:", 90, step = 1),
          numericInput(sprintf("waa_pop_k_%d",    s), "k:",   0.13, step = 0.01),
          numericInput(sprintf("waa_pop_t0_%d",   s), "t0:",  0,    step = 0.1),
          numericInput(sprintf("waa_pop_aLW_%d",  s), "aLW:", 3e-6, step = 1e-6),
          numericInput(sprintf("waa_pop_bLW_%d",  s), "bLW:", 3,    step = 0.1)
        )
      })
    )
  })
  
  # ---- for vonB+LW: get best available params for a (fleet, index, pop) ----
  get_vonb_params <- function(f, i, s) {
    # priority Fleet > Index > Population > Global inputs
    pick <- function(prefix, id) input[[sprintf("%s_%s", prefix, id)]]
    # Fleet
    FLinf <- pick("waa_fleet_Linf", f); Fk <- pick("waa_fleet_k", f)
    Ft0   <- pick("waa_fleet_t0",   f); Fa <- pick("waa_fleet_aLW", f)
    Fb    <- pick("waa_fleet_bLW",  f)
    if (!is.null(FLinf) && !is.null(Fk) && !is.null(Ft0) && !is.null(Fa) && !is.null(Fb)) {
      return(list(Linf=FLinf, k=Fk, t0=Ft0, aLW=Fa, bLW=Fb))
    }
    # Index
    ILinf <- pick("waa_index_Linf", i); Ik <- pick("waa_index_k", i)
    It0   <- pick("waa_index_t0",   i); Ia <- pick("waa_index_aLW", i)
    Ib    <- pick("waa_index_bLW",  i)
    if (!is.null(ILinf) && !is.null(Ik) && !is.null(It0) && !is.null(Ia) && !is.null(Ib)) {
      return(list(Linf=ILinf, k=Ik, t0=It0, aLW=Ia, bLW=Ib))
    }
    # Population
    SLinf <- pick("waa_pop_Linf", s); Sk <- pick("waa_pop_k", s)
    St0   <- pick("waa_pop_t0",   s); Sa <- pick("waa_pop_aLW", s)
    Sb    <- pick("waa_pop_bLW",  s)
    if (!is.null(SLinf) && !is.null(Sk) && !is.null(St0) && !is.null(Sa) && !is.null(Sb)) {
      return(list(Linf=SLinf, k=Sk, t0=St0, aLW=Sa, bLW=Sb))
    }
    # Fallback to global panel
    list(
      Linf = input$vh_Linf %||% 90,
      k    = input$vh_k    %||% 0.13,
      t0   = input$vh_t0   %||% 0,
      aLW  = input$vh_aLW  %||% 3e-6,
      bLW  = input$vh_bLW  %||% 3
    )
  }
  
  # ---- for ALK-driven: get best available params for a (fleet, index, pop) ----
  get_alk_params <- function(f, i, s, ages) {
    # priority Fleet > Index > Population > Global inputs
    L <- parse_num_csv(input[[sprintf("waa_fleet_alk_L_%d", f)]], NULL)
    a <- input[[sprintf("waa_fleet_alk_a_%d", f)]]
    b <- input[[sprintf("waa_fleet_alk_b_%d", f)]]
    if (!is.null(L) && length(L)==length(ages) && !is.null(a) && !is.null(b)) {
      return(list(L=L, a=a, b=b))
    }
    L <- parse_num_csv(input[[sprintf("waa_index_alk_L_%d", i)]], NULL)
    a <- input[[sprintf("waa_index_alk_a_%d", i)]]
    b <- input[[sprintf("waa_index_alk_b_%d", i)]]
    if (!is.null(L) && length(L)==length(ages) && !is.null(a) && !is.null(b)) {
      return(list(L=L, a=a, b=b))
    }
    L <- parse_num_csv(input[[sprintf("waa_pop_alk_L_%d", s)]], NULL)
    a <- input[[sprintf("waa_pop_alk_a_%d", s)]]
    b <- input[[sprintf("waa_pop_alk_b_%d", s)]]
    if (!is.null(L) && length(L)==length(ages) && !is.null(a) && !is.null(b)) {
      return(list(L=L, a=a, b=b))
    }
    # Fallback to global panel
    list(
      L = parse_num_csv(input$alk_L_at_age %||% default_alk_string(), rep(NA_real_, length(ages))),
      a = input$alk_aLW %||% 0.01,
      b = input$alk_bLW %||% 3
    )
  }
  
  output$waaPlot_sources <- renderPlot({
    req(input$use_user_lh, isFALSE(input$use_global_WAA))
    na <- input$n_ages %||% 10
    ages <- 1:na
    ns <- input$n_stocks %||% 1
    nf <- input$n_fleets %||% 1
    ni <- input$n_indices %||% 1
    
    # build all curves first to compute ymax
    curves <- list()
    for (f in 1:nf) for (i in 1:ni) for (s in 1:ns) {
      if (input$waa_option == "vonB+LW") {
        p <- get_vonb_params(f,i,s)
        Len <- p$Linf * (1 - exp(-p$k * (ages - p$t0)))
        W   <- p$aLW * Len^(p$bLW)
      } else { # ALK-driven
        p <- get_alk_params(f,i,s, ages)
        L <- p$L
        if (length(L) != length(ages) || anyNA(L)) {
          # if bad length, supply a flat line to avoid errors
          L <- rep(0, length(ages))
        }
        W <- p$a * L^(p$b)
      }
      curves[[length(curves)+1]] <- list(f=f,i=i,s=s, W=W)
    }
    
    ymax <- max(vapply(curves, function(x) max(x$W, na.rm = TRUE), 0.0))
    if (!is.finite(ymax) || ymax <= 0) ymax <- 1
    ylim <- c(0, ymax * 1.15)
    
    plot(NA, xlim = c(1, na), ylim = ylim,
         xlab = "Age", ylab = "Weight-at-age",
         main = "WAA by source (all Fleet × Index × Population)")
    cols <- grDevices::rainbow(length(curves))
    
    for (k in seq_along(curves)) {
      lines(ages, curves[[k]]$W, col = cols[k], lwd = 2)
    }
    
    # three textual legends (not mapped to colors; for orientation)
    legend("topleft",       legend = paste("Fleet", 1:nf),      bty = "n", cex = 0.85)
    legend("topright",      legend = paste("Index", 1:ni),      bty = "n", cex = 0.85)
    legend("bottomright",   legend = paste("Population", 1:ns), bty = "n", cex = 0.85)
  })
  
  # ---- ALK-driven per-source inputs (Fleet + Index + Population) ----
  output$waa_alk_by_source_ui <- renderUI({
    req(input$use_user_lh, isFALSE(input$use_global_WAA), input$waa_option == "ALK-driven")
    ns <- input$n_stocks %||% 1
    nf <- input$n_fleets %||% 1
    ni <- input$n_indices %||% 1
    defL <- default_alk_string()
    
    tagList(
      h5("Define WAA parameters for each source (Fleet + Index + Population):"),
      # Fleets
      lapply(1:nf, function(f) {
        wellPanel(
          h5(sprintf("WAA parameters: Fleet %d", f)),
          textInput(sprintf("waa_fleet_alk_L_%d", f), "L-at-age (comma-separated):", value = defL),
          numericInput(sprintf("waa_fleet_alk_a_%d", f), "aLW:", 0.01, step = 0.001),
          numericInput(sprintf("waa_fleet_alk_b_%d", f), "bLW:", 3.0,  step = 0.1)
        )
      }),
      # Indices
      lapply(1:ni, function(i) {
        wellPanel(
          h5(sprintf("WAA parameters: Index %d", i)),
          textInput(sprintf("waa_index_alk_L_%d", i), "L-at-age (comma-separated):", value = defL),
          numericInput(sprintf("waa_index_alk_a_%d", i), "aLW:", 0.01, step = 0.001),
          numericInput(sprintf("waa_index_alk_b_%d", i), "bLW:", 3.0,  step = 0.1)
        )
      }),
      # Populations
      lapply(1:ns, function(s) {
        wellPanel(
          h5(sprintf("WAA parameters: Population %d", s)),
          textInput(sprintf("waa_pop_alk_L_%d", s), "L-at-age (comma-separated):", value = defL),
          numericInput(sprintf("waa_pop_alk_a_%d", s), "aLW:", 0.01, step = 0.001),
          numericInput(sprintf("waa_pop_alk_b_%d", s), "bLW:", 3.0,  step = 0.1)
        )
      })
    )
  })
  
  # --- MAA GLOBAL / PER-POPULATION / UPLOAD ---
  output$maa_by_stock_ui <- renderUI({
    req(input$use_user_lh, isFALSE(input$use_global_MAA))
    np <- input$n_stocks %||% 1
    tagList(lapply(1:np, function(p) {
      wellPanel(
        h5(sprintf("Maturity (logistic) for Population %d", p)),
        numericInput(paste0("maa_a50_pop_", p), "a50:", 3, step = 0.1),
        numericInput(paste0("maa_slope_pop_", p), "slope:", 1.0, step = 0.1)
      )
    }))
  })
  
  output$maaPlot_global <- renderPlot({
    req(input$use_user_lh, isTRUE(input$use_global_MAA))
    na <- input$n_ages %||% 10; ages <- 1:na
    a50   <- input$maa_a50_global %||% 3
    slope <- input$maa_slope_global %||% 1
    mat <- Generate_Maturity(a50, slope, na)
    plot(ages, mat, type = "b", pch = 16, col = "darkred",
         xlab = "Age", ylab = "Maturity", main = "Global logistic MAA")
  })
  
  output$maaPlot_by_stock <- renderPlot({
    req(input$use_user_lh, isFALSE(input$use_global_MAA))
    na <- input$n_ages %||% 10; ages <- 1:na; np <- input$n_stocks %||% 1
    cols <- grDevices::rainbow(np)
    plot(NA, xlim = c(1, na), ylim = c(0, 1),
         xlab = "Age", ylab = "Maturity", main = "MAA by Population")
    for (p in 1:np) {
      a50   <- input[[paste0("maa_a50_pop_", p)]] %||% 3
      slope <- input[[paste0("maa_slope_pop_", p)]] %||% 1
      MAA   <- Generate_Maturity(a50, slope, na)
      lines(ages, MAA, col = cols[p], lwd = 2)
    }
    legend("topleft", legend = paste("Population", 1:np), col = cols, lty = 1, cex = 0.8, bty = "n")
  })
  
  output$maaPlot_uploaded <- renderPlot({
    req(input$use_user_lh, isTRUE(input$use_user_MAA), input$maa_file)
    A <- ages_vec()
    MAAin <- tryCatch({
      if (grepl("\\.csv$", input$maa_file$name, TRUE)) {
        as.matrix(read.csv(input$maa_file$datapath, check.names = FALSE))
      } else readRDS(input$maa_file$datapath)
    }, error = function(e) NULL)
    validate(need(!is.null(MAAin), "Invalid MAA file"))
    if (is.matrix(MAAin)) {
      validate(need(ncol(MAAin) == length(A),
                    sprintf("Matrix must have ncol = n_ages = %d", length(A))))
      matplot(A, t(MAAin), type="l", lty=1, main="Uploaded MAA (matrix)", xlab="Age", ylab="Maturity")
    } else if (is.array(MAAin) && length(dim(MAAin)) == 3) {
      matplot(A, t(MAAin[1, , ]), type="l", lty=1, main="Uploaded MAA (array, first population)", xlab="Age", ylab="Maturity")
    } else {
      plot.new(); title("Unsupported MAA shape.")
    }
  })
  
  
  # ============== STEP 5: FISHERY (placeholder wiring) ==============
  output$FInputs <- renderUI({
    if (input$F_mode == "Global pattern") {
      tagList(selectInput("F_pattern", "Pattern:", c("constant","ramp","trend")),
              numericInput("F0", "F0:", 0.2, step = 0.01))
    } else if (input$F_mode == "Direct user_F") fileInput("F_file", "Upload F (.csv/.rds)")
    else p("Configure per-fleet patterns (future).")
  })
  output$FPlot <- renderPlot({ plot(1:10, rep(0.2, 10), type="l", main="F preview", xlab="Year", ylab="F") })
  
  # ============== STEP 6–7: CATCH & INDEX (placeholders) ==============
  output$catchInputs <- renderUI({
    tagList(
      numericInput("catch_cv", "Catch CV:", 0.1, step = 0.01),
      numericInput("catch_Neff", "Catch Neff:", 100, step = 1)
    )
  })
  output$catchPlot <- renderPlot({ hist(rnorm(100), main = "Catch preview") })
  
  output$indexInputs <- renderUI({
    tagList(
      numericInput("index_cv", "Index CV:", 0.15, step = 0.01),
      numericInput("index_Neff", "Index Neff:", 150, step = 1),
      numericInput("q", "Catchability q:", 0.5, step = 0.01)
    )
  })
  output$indexPlot <- renderPlot({ hist(rnorm(100), main = "Index preview") })
  
  # ============== STEP 9: BIAS CORRECTION (XSPR_R_opt preserved) ==============
  # (UI provides XSPR_R_opt as selectInput 1..5; no additional server wiring required here)
  
  # ============== STEP 11: REVIEW & EXPORT ==============
  output$summary_text <- renderUI({
    HTML("<b>Summary of configuration will go here</b>")
  })
  
  output$outputList <- renderPrint({
    list(
      n_stocks = input$n_stocks,
      n_regions = input$n_regions,
      n_fleets = input$n_fleets,
      n_indices = input$n_indices,
      n_seasons = input$n_seasons,
      n_ages = input$n_ages,
      fracyr_spawn = input$fracyr_spawn,
      base_year_start = input$base_year_start,
      base_year_end = input$base_year_end,
      n_feedback_years = input$n_feedback_years,
      life_history_choice = input$life_history_choice,
      use_user_lh = input$use_user_lh,
      # Step 4 key toggles
      use_global_WAA = input$use_global_WAA,
      use_global_MAA = input$use_global_MAA,
      waa_option = input$waa_option,
      # Bias option
      XSPR_R_opt = as.integer(input$XSPR_R_opt)
    )
  })
  
  output$downloadConfig <- downloadHandler(
    filename = function() "config.rds",
    content = function(file) {
      saveRDS(list(
        n_stocks = input$n_stocks,
        n_regions = input$n_regions,
        n_fleets = input$n_fleets,
        n_indices = input$n_indices,
        n_seasons = input$n_seasons,
        n_ages = input$n_ages,
        fracyr_spawn = input$fracyr_spawn,
        base_year_start = input$base_year_start,
        base_year_end = input$base_year_end,
        n_feedback_years = input$n_feedback_years,
        life_history_choice = input$life_history_choice,
        use_user_lh = input$use_user_lh,
        use_global_WAA = input$use_global_WAA,
        use_global_MAA = input$use_global_MAA,
        waa_option = input$waa_option,
        XSPR_R_opt = as.integer(input$XSPR_R_opt)
        # If you want to persist the per-source/per-stock parameter sets or uploaded arrays,
        # you can read them here and add them into this list similarly.
      ), file)
    }
  )
  
  observeEvent(input$closeApp, { stopApp() })
}


# ui.R
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(DiagrammeR)

# Modern theme
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto Slab")
)

ui <- fluidPage(
  theme = app_theme,
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .nav-tabs .nav-item .nav-link {
        background-color: #2C3E50;
        color: white;
        font-weight: bold;
      }
      .nav-tabs .nav-link.active {
        background-color: #27AE60 !important;
        color: white !important;
      }
      .tab-content {
        border: 1px solid #ddd;
        border-top: none;
        padding: 20px;
        background-color: #f9f9f9;
      }
      .error-text { color: red; font-weight: bold; }
      .section-title { margin-top: 15px; font-weight: 600; }
    "))
  ),
  
  titlePanel(paste0("SPASAM-MSE Model Input Builder (v", spasam_mse_version, ")")),
  
  tabsetPanel(
    id = "step_tabs",
    
    # ---------------- Step 1 ----------------
    tabPanel("1) Setup",
             sidebarLayout(
               sidebarPanel(
                 numericInput("n_stocks", "Number of Stocks:", 2, min=1, max=12),
                 numericInput("n_regions", "Number of Regions:", 2, min=1, max=12),
                 numericInput("n_fleets", "Number of Fleets:", 2, min=1, max=12),
                 numericInput("n_indices", "Number of Indices:", 2, min=1, max=12),
                 numericInput("n_seasons", "Number of Seasons:", 4, min=1, max=12),
                 numericInput("n_ages", "Max Age:", 10, min=1, max=50),
                 numericInput("fracyr_spawn", "Fraction of Year for Spawning:", 0.5, min=0, max=1, step=0.05),
                 numericInput("base_year_start", "Base Year Start:", 2000, min=1900, max=2100),
                 numericInput("base_year_end", "Base Year End:", 2020, min=1900, max=2100),
                 numericInput("n_feedback_years", "Feedback Years:", 0, min=0, max=50),
                 actionBttn("next_to_seasons", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Define model dimensions and timeline."),
                 plotOutput("timelinePlot", height=200)
               )
             )
    ),
    
    # ---------------- Step 2 ----------------
    tabPanel("2) Seasons",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("season_mode", "Season input:", c("Sliders","Numeric"), inline=TRUE),
                 uiOutput("seasonUI"),
                 uiOutput("seasonSum"),
                 actionBttn("back_to_setup", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_lifehistory", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(plotOutput("seasonPlot", height=200))
             )
    ),
    
    # ---------------- Step 3 ----------------
    tabPanel("3) Life History Trait",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("use_user_lh", "Use user-specified life history?", FALSE),
                 radioButtons("life_history_choice", "Life history trait:",
                              choices = c("Short"="short","Medium"="medium","Long"="long"),
                              selected = "medium"),
                 actionBttn("back_to_seasons", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_lhpars", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 fluidRow(
                   column(6, plotOutput("waaPlot_trait", height=250)),
                   column(6, plotOutput("matPlot_trait", height=250))
                 )
               )
             )
    ),
    
    # ---------------- Step 4 ----------------
    # ---------------- Step 4 ----------------
    tabPanel("4) Life History Parameters",
             sidebarLayout(
               sidebarPanel(
                 conditionalPanel(
                   condition = "input.use_user_lh == true",
                   
                   # -------- WAA controls --------
                   h4("Weight-at-age (WAA)"),
                   checkboxInput("use_global_WAA", "Use Global WAA?", value = TRUE),
                   radioButtons("waa_option", "Choose WAA option:",
                                choices = c("vonB+LW", "ALK-driven", "Direct user array"),
                                selected = "vonB+LW"),
                   
                   # Global WAA parameter box (keeps your existing global inputs)
                   uiOutput("lhParams"),
                   
                   # Per-source WAA parameter boxes
                   conditionalPanel(
                     condition = "!input.use_global_WAA && input.waa_option == 'vonB+LW'",
                     h5("Define WAA parameters for each source (Fleet + Index + Population):"),
                     uiOutput("waa_vonb_by_source_ui")
                   ),
                   
                   tags$hr(),
                   
                   # -------- MAA controls --------
                   h4("Maturity-at-age (MAA)"),
                   checkboxInput("use_global_MAA", "Use Global MAA?", value = TRUE),
                   
                   # Global MAA logistic inputs
                   conditionalPanel(
                     condition = "input.use_global_MAA",
                     numericInput("maa_a50_global", "Global a50 (Age at 50% maturity):", 3, step = 0.1),
                     numericInput("maa_slope_global", "Global slope:", 1.0, step = 0.1)
                   ),
                   
                   # Per-population MAA logistic inputs
                   conditionalPanel(
                     condition = "!input.use_global_MAA",
                     h5("Maturity (logistic) for each Population:"),
                     uiOutput("maa_by_stock_ui")
                   ),
                   
                   # Optional upload
                   checkboxInput("use_user_MAA", "Upload custom MAA (matrix/array)?", FALSE),
                   conditionalPanel(
                     condition = "input.use_user_MAA == true",
                     fileInput("maa_file", "Upload MAA (.csv or .rds)", accept = c(".csv", ".rds"))
                   ),
                   
                   tags$hr(),
                   
                   actionBttn("back_to_lifehistory", "◀ Back", style="material-flat", color="warning"),
                   actionBttn("next_to_fishery", "Next ▶", style="material-flat", color="primary")
                 ),
                 conditionalPanel(
                   condition = "input.use_user_lh == false",
                   p("Life history parameters are locked. Enable 'Use user-specified life history' in Step 3 to edit.")
                 )
               ),
               
               mainPanel(
                 # -------- WAA previews --------
                 conditionalPanel(
                   condition = "input.use_user_lh == true && input.use_global_WAA == true && input.waa_option == 'vonB+LW'",
                   fluidRow(
                     column(4, plotOutput("vonBPlot", height = 260)),
                     column(4, plotOutput("LWPlot",   height = 260)),
                     column(4, plotOutput("WAAPlot",  height = 260))
                   )
                 ),
                 conditionalPanel(
                   condition = "input.use_user_lh == true && input.use_global_WAA == true && input.waa_option == 'ALK-driven'",
                   fluidRow(
                     column(6, plotOutput("alkLPlot",   height = 300)),
                     column(6, plotOutput("alkWAAPlot", height = 300))
                   )
                 ),
                 conditionalPanel(
                   condition = "input.use_user_lh == true && input.use_global_WAA == true && input.waa_option == 'Direct user array'",
                   plotOutput("waaPlot_param", height = 350)
                 ),
                 
                 # Per-source combined preview
                 conditionalPanel(
                   condition = "input.use_user_lh == true && input.use_global_WAA == false",
                   plotOutput("waaPlot_sources", height = 360)
                 ),
                 
                 tags$hr(),
                 
                 # -------- MAA previews --------
                 conditionalPanel(
                   condition = "input.use_user_lh == true && input.use_global_MAA == true && input.use_user_MAA == false",
                   plotOutput("maaPlot_global", height = 320)
                 ),
                 conditionalPanel(
                   condition = "input.use_user_lh == true && input.use_global_MAA == false && input.use_user_MAA == false",
                   plotOutput("maaPlot_by_stock", height = 320)
                 ),
                 conditionalPanel(
                   condition = "input.use_user_lh == true && input.use_user_MAA == true",
                   plotOutput("maaPlot_uploaded", height = 320)
                 )
               )
             )
    ),
    
    
    # ---------------- Step 5 ----------------
    tabPanel("5) Fishery Pattern",
             sidebarLayout(
               sidebarPanel(
                 selectInput("F_mode", "Fishing Mortality Input:",
                             c("Direct user_F","Global pattern","Per-fleet pattern")),
                 uiOutput("FInputs"),
                 actionBttn("back_to_lhpars", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_catch", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(plotOutput("FPlot", height=200))
             )
    ),
    
    # ---------------- Step 6 ----------------
    tabPanel("6) Catch Info",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("catchInputs"),
                 uiOutput("fleetRegionsUI"),
                 actionBttn("back_to_fishery", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_index", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(plotOutput("catchPlot", height=200))
             )
    ),
    
    # ---------------- Step 7 ----------------
    tabPanel("7) Survey Info",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("indexInputs"),
                 uiOutput("indexRegionsUI"),
                 actionBttn("back_to_catch", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_bias", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(plotOutput("indexPlot", height=200))
             )
    ),
    
    # ---------------- Step 9 ----------------
    tabPanel("9) Bias Correction",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("bc_proc", "Process bias correction", FALSE),
                 checkboxInput("bc_obs", "Observation bias correction", FALSE),
                 checkboxInput("bc_brp", "BRPs bias correction", FALSE),
                 
                 selectInput("XSPR_R_opt", "Recruitment treatment for BRPs (XSPR_R_opt):",
                             choices = c(
                               "1 = Annual recruitment estimates (SSB_XSPR per year)" = 1,
                               "2 = Average recruitment estimates" = 2,
                               "3 = Annual recruitment predictions (SSB_XSPR per year)" = 3,
                               "4 = Average recruitment predictions" = 4,
                               "5 = Bias-corrected expected recruitment" = 5
                             ), selected = 2
                 ),
                 
                 actionBttn("back_to_index", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_movement", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Configure bias correction and select recruitment treatment for BRPs (XSPR_R_opt).")
               )
             )
    ),
    
    # ---------------- Step 10 ----------------
    tabPanel("10) Movement",
             sidebarLayout(
               sidebarPanel(
                 selectInput("mig_type", "Migration type:", c("0 = after survival","1 = simultaneous")),
                 selectInput("move_dyn", "Movement dynamics:", c("0 = natal homing","1 = metapopulation")),
                 textInput("onto_move", "Ontogenetic move codes:", "0"),
                 textInput("onto_move_pars", "Ontogenetic move parameters:", ""),
                 checkboxInput("apply_re_trend", "Apply RE trend?", FALSE),
                 fileInput("trend_re_rate", "Upload RE trend array (.rds)"),
                 checkboxInput("apply_mu_trend", "Apply mu trend?", FALSE),
                 fileInput("trend_mu_rate", "Upload mu trend array (.rds)"),
                 actionBttn("back_to_bias", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_review", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(p("Movement configuration."))
             )
    ),
    
    # ---------------- Step 11 ----------------
    tabPanel("11) Review & Export",
             sidebarLayout(
               sidebarPanel(
                 downloadButton("downloadConfig", "Download Config (.rds)"),
                 actionBttn("back_to_movement", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("closeApp", "Close", style="material-flat", color="danger")
               ),
               mainPanel(
                 uiOutput("summary_text"),
                 verbatimTextOutput("outputList")
               )
             )
    )
  )
)

# Run the app
shinyApp(ui = ui, server = server)
