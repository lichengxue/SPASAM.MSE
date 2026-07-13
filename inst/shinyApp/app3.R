# ------------------------------------------------------------------------------
# Server.R (explicit, non-refactored version ~650 lines)
# Movement Model Builder
# ------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

spasam_mse_version <- tryCatch(as.character(utils::packageVersion("SPASAM.MSE")),
                               error = function(e) "1.1.5")

# Safe coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b

server <- function(input, output, session) {
  
  # ============================================================================
  # 0) NAVIGATION HELPERS
  # ============================================================================
  go <- function(tab) updateTabsetPanel(session, "step_tabs", selected = tab)
  
  observeEvent(input$next_to_seasons,     { go("2) Seasons") })
  observeEvent(input$back_to_setup,       { go("1) Setup") })
  observeEvent(input$next_to_populations, { go("3) Populations") })
  observeEvent(input$back_to_seasons,     { go("2) Seasons") })
  observeEvent(input$next_to_rates,       { go("4) Movement Rates") })
  observeEvent(input$back_to_populations, { go("3) Populations") })
  observeEvent(input$next_to_re,          { go("5) Random Effects") })
  observeEvent(input$back_to_rates,       { go("4) Movement Rates") })
  observeEvent(input$next_to_priors,      { go("6) Priors") })
  observeEvent(input$back_to_re,          { go("5) Random Effects") })
  observeEvent(input$next_to_review,      { go("7) Review & Export") })
  observeEvent(input$back_to_priors,      { go("6) Priors") })
  
  # ============================================================================
  # 1) BASIC REACTIVES
  # ============================================================================
  n_regions  <- reactive(as.integer(input$n_regions %||% 2L))
  n_stocks   <- reactive(as.integer(input$n_stocks  %||% 2L))
  n_seasons  <- reactive(as.integer(input$n_seasons %||% 1L))
  
  spawn_season <- reactive({
    ns <- n_seasons()
    fs <- as.numeric(input$fracyr_spawn %||% 0.5)
    max(1, min(ns, ceiling(fs * ns)))
  })
  
  home_region <- reactive({
    # default: stock k returns to region k (bounded)
    pmin(seq_len(n_stocks()), n_regions())
  })
  
  # ============================================================================
  # 2) STEP 1 VALIDATION (regions == stocks)
  # ============================================================================
  output$regionStockError <- renderUI({
    if (n_regions() != n_stocks()) {
      div(class="error-text",
          "Error: Number of regions must equal number of populations (stocks) to proceed.")
    }
  })
  observe({
    shinyjs::toggleState("next_to_seasons", condition = (n_regions() == n_stocks()))
  })
  
  # ============================================================================
  # 3) STEP 2: SEASONS UI (Sliders or Numeric)
  # ============================================================================
  output$seasonFractionsUI <- renderUI({
    ns <- n_seasons()
    if (identical(input$season_input_mode, "Sliders")) {
      tagList(lapply(seq_len(ns), function(s) {
        sliderInput(
          paste0("season_frac_", s),
          label = paste("Season", s, "fraction"),
          min = 0, max = 1, value = round(1/ns, 2), step = 0.01, width = "100%"
        )
      }))
    } else {
      tagList(lapply(seq_len(ns), function(s) {
        numericInput(
          paste0("season_frac_", s),
          label = paste("Season", s, "fraction"),
          min = 0, max = 1, value = round(1/ns, 2), step = 0.01, width = "100%"
        )
      }))
    }
  })
  
  season_frac_vec <- reactive({
    ns <- n_seasons()
    vals <- numeric(ns)
    for (s in seq_len(ns)) {
      v <- input[[paste0("season_frac_", s)]]
      if (is.null(v) || is.na(v)) v <- 1/ns
      vals[s] <- as.numeric(v)
    }
    vals
  })
  
  output$seasonFracSumUI <- renderUI({
    sm <- sum(season_frac_vec())
    cls <- if (abs(sm - 1) < 1e-6) "muted" else "error-text"
    div(class = cls, sprintf("Sum of season fractions: %.2f%s",
                             sm, if (abs(sm - 1) > 1e-6) " (will be normalized to 1 on Review)" else ""))
  })
  
  # -- Step 2: movement allowed seasons + natal return (must move) seasons
  output$canMoveSeasonsUI <- renderUI({
    ns <- n_seasons()
    sel <- setdiff(seq_len(ns), spawn_season())
    pickerInput(
      "canMoveSeasons", "Seasons where movement is allowed:",
      choices = as.character(seq_len(ns)), selected = as.character(sel),
      multiple = TRUE,
      options = list(`actions-box` = TRUE, size = 10), width = "100%"
    )
  })
  
  output$mustMoveSeasonsUI <- renderUI({
    ns <- n_seasons()
    pickerInput(
      "mustMoveSeasons", "Seasons with natal homing (must return):",
      choices = as.character(seq_len(ns)), selected = as.character(spawn_season()),
      multiple = TRUE,
      options = list(`actions-box` = TRUE, size = 10), width = "100%"
    )
  })
  
  observeEvent(input$dynamics, {
    if (identical(input$dynamics, "meta")) {
      updatePickerInput(session, "mustMoveSeasons", selected = character(0))
      shinyjs::disable("mustMoveSeasons")
    } else {
      shinyjs::enable("mustMoveSeasons")
      isolate({
        if (!length(input$mustMoveSeasons)) {
          updatePickerInput(session, "mustMoveSeasons", selected = as.character(spawn_season()))
        }
      })
    }
  }, ignoreInit = TRUE)
  
  # -- Step 2: validation and continue state
  output$seasonErrors <- renderUI({
    errs <- c()
    if (!length(input$canMoveSeasons)) errs <- c(errs, "Select at least one season where movement is allowed.")
    if (identical(input$dynamics, "natal") && !length(input$mustMoveSeasons))
      errs <- c(errs, "Natal homing requires at least one 'must return' season.")
    if (length(errs))
      HTML(paste0("<div class='error-text'>", paste(sprintf("• %s", errs), collapse="<br>"), "</div>"))
  })
  observe({
    ok <- length(input$canMoveSeasons) > 0 &&
      (!identical(input$dynamics, "natal") || length(input$mustMoveSeasons) > 0)
    shinyjs::toggleState("next_to_populations", condition = ok)
  })
  
  # ============================================================================
  # 4) SEASON DIAGRAM (used in Step 2 & Step 7)
  # ============================================================================
  renderSeasonBar <- function(ns, sfrac, spawn) {
    df <- data.frame(season=1:ns, frac=sfrac)
    df$xmin <- c(0, cumsum(head(df$frac, -1)))
    df$xmax <- cumsum(df$frac)
    df$is_spawn <- (df$season == spawn)
    
    ggplot(df) +
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=1, fill=is_spawn),
                color="white", linewidth=0.8) +
      scale_fill_manual(values = c(`TRUE` = "#18BC9C", `FALSE` = "#3498DB"),
                        labels = c("Off-spawning", "Spawning")) +
      geom_text(aes(x=(xmin+xmax)/2, y=0.5,
                    label=paste0("S", season, "\n", sprintf("%.0f%%", 100*frac))),
                size=4, color="white", fontface="bold") +
      theme_minimal(base_size=13) +
      theme(legend.position="top",
            axis.title=element_blank(), axis.text=element_blank(), panel.grid=element_blank(),
            plot.margin=margin(0,0,0,0)) +
      guides(fill=guide_legend(title=NULL))
  }
  output$seasonDiagram       <- renderPlot({ renderSeasonBar(n_seasons(), season_frac_vec(), spawn_season()) })
  output$seasonDiagram_final <- renderPlot({ renderSeasonBar(n_seasons(), season_frac_vec(), spawn_season()) })
  
  # ============================================================================
  # 5) STEP 3: POPULATIONS UI
  # ============================================================================
  output$popMoveUI <- renderUI({
    n <- n_stocks()
    pickerInput("pop_move", "Populations allowed to move:",
                choices = setNames(seq_len(n), paste0("Pop ", seq_len(n))),
                selected = seq_len(n), multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10), width = "100%")
  })
  
  output$separableUI <- renderUI({
    # IMPORTANT: semantics clarified in your note:
    #  - "Separable" ON  -> movement is sequential (applied after mortality step)
    #  - "Separable" OFF -> movement is instantaneous rate (co-occurs with mortality)
    n <- n_stocks()
    pickerInput("separable", "Separable movement (sequential vs instantaneous):",
                choices = setNames(seq_len(n), paste0("Pop ", seq_len(n))),
                selected = seq_len(n), multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10), width = "100%")
  })
  
  output$popErrors <- renderUI({
    if (!length(input$pop_move)) {
      div(class="error-text", "Select at least one population that is allowed to move.")
    }
  })
  observe({
    shinyjs::toggleState("next_to_rates", condition = length(input$pop_move) > 0)
  })
  
  # ============================================================================
  # 6) STEP 4: MOVEMENT RATE INPUTS
  # ============================================================================
  output$movementInputs <- renderUI({
    nR <- n_regions(); ns <- n_seasons(); nK <- n_stocks()
    mm <- input$mean_model %||% "constant"
    if (mm == "none") return(tags$em("mean_model = 'none': no movement parameters."))
    
    asSlider <- identical(input$rate_entry_mode, "Sliders")
    makeInput <- function(id, label) {
      if (asSlider) sliderInput(id, label, min = 0, max = 1, value = 0, step = 0.01, width = "100%")
      else numericInput(id, label, value = 0, min = 0, max = 1, step = 0.01, width = "100%")
    }
    
    ui_list <- list()
    for (from in 1:nR) for (to in setdiff(1:nR, from)) {
      base <- paste0("R", from, " \u2192 R", to, ":")
      if (mm == "constant") {
        ui_list <- append(ui_list, list(makeInput(paste0("mv_r",from,"_to",to), base)))
      } else if (mm == "season") {
        for (t in 1:ns)
          ui_list <- append(ui_list, list(makeInput(paste0("mv_r",from,"_to",to,"_seas",t),
                                                    paste0(base," (Season ",t,")"))))
      } else if (mm == "population_constant") {
        for (k in 1:nK)
          ui_list <- append(ui_list, list(makeInput(paste0("mv_r",from,"_to",to,"_pop",k),
                                                    paste0(base," (Pop ",k,")"))))
      } else if (mm == "population_season") {
        for (k in 1:nK) for (t in 1:ns)
          ui_list <- append(ui_list, list(makeInput(paste0("mv_r",from,"_to",to,"_pop",k,"_seas",t),
                                                    paste0(base," (Pop ",k,", Season ",t,")"))))
      }
    }
    do.call(tagList, ui_list)
  })
  
  # ============================================================================
  # 7) STEP 5: RANDOM EFFECTS UI (Sigma / RhoAge / RhoYear)
  #  - No refactor: explicit renderUI for each scope and each parameter
  # ============================================================================
  
  # ---- sigma scope chooser (shows only if any RE is active) ----
  output$sigmaScopeUI <- renderUI({
    if (input$age_re %in% c("iid","ar1") || input$year_re %in% c("iid","ar1")) {
      selectInput("sigma_scope", "Sigma scope:",
                  choices = c("Global" = "single",
                              "By season" = "by_season",
                              "By population" = "by_stock",
                              "By region" = "by_region",
                              "By population × season" = "by_stock_season"),
                  selected = "single")
    }
  })
  
  # ---- sigma UI by scope (explicit) ----
  output$sigmaUI <- renderUI({
    if (!(input$age_re %in% c("iid","ar1") || input$year_re %in% c("iid","ar1"))) return(NULL)
    scope <- input$sigma_scope %||% "single"
    nK <- n_stocks(); ns <- n_seasons(); nR <- n_regions()
    add_num <- function(id, lab) sliderInput(id, lab, min = 0, max = 2, value = 0.5, step = 0.05, width = "100%")
    ui <- list()
    if (scope == "single") {
      ui <- list(add_num("sig_global", "Sigma (global)"))
    } else if (scope == "by_season") {
      for (t in 1:ns) ui <- append(ui, list(add_num(paste0("sig_seas_",t), paste("Sigma (Season ",t,")"))))
    } else if (scope == "by_stock") {
      for (k in 1:nK) ui <- append(ui, list(add_num(paste0("sig_stock_",k), paste("Sigma (Pop ",k,")"))))
    } else if (scope == "by_region") {
      for (from in 1:nR) for (to in setdiff(1:nR,from))
        ui <- append(ui, list(add_num(paste0("sig_r",from,"_to",to), paste0("Sigma (R",from,"→R",to,")"))))
    } else if (scope == "by_stock_season") {
      for (k in 1:nK) for (t in 1:ns)
        ui <- append(ui, list(add_num(paste0("sig_stock_",k,"_seas_",t),
                                      paste0("Sigma (Pop ",k,", Season ",t,")"))))
    }
    do.call(tagList, ui)
  })
  
  # ---- rho (age) scope chooser ----
  output$rhoAgeScopeUI <- renderUI({
    if (input$age_re == "ar1") {
      selectInput("rhoA_scope", "Rho (age) scope:",
                  choices = c("Global" = "single",
                              "By season" = "by_season",
                              "By population" = "by_stock",
                              "By region" = "by_region",
                              "By population × season" = "by_stock_season"),
                  selected = "single")
    }
  })
  
  # ---- rho (age) UI by scope ----
  output$rhoAgeUI <- renderUI({
    if (input$age_re != "ar1") return(NULL)
    scope <- input$rhoA_scope %||% "single"
    nK <- n_stocks(); ns <- n_seasons(); nR <- n_regions()
    add_num <- function(id, lab) sliderInput(id, lab, min = -0.99, max = 0.99, value = 0.5, step = 0.01, width = "100%")
    ui <- list()
    if (scope == "single") {
      ui <- list(add_num("rhoA_global", "rho_age (global)"))
    } else if (scope == "by_season") {
      for (t in 1:ns) ui <- append(ui, list(add_num(paste0("rhoA_seas_",t), paste("rho_age (Season ",t,")"))))
    } else if (scope == "by_stock") {
      for (k in 1:nK) ui <- append(ui, list(add_num(paste0("rhoA_stock_",k), paste("rho_age (Pop ",k,")"))))
    } else if (scope == "by_region") {
      for (from in 1:nR) for (to in setdiff(1:nR,from))
        ui <- append(ui, list(add_num(paste0("rhoA_r",from,"_to",to), paste0("rho_age (R",from,"→R",to,")"))))
    } else if (scope == "by_stock_season") {
      for (k in 1:nK) for (t in 1:ns)
        ui <- append(ui, list(add_num(paste0("rhoA_stock_",k,"_seas_",t),
                                      paste0("rho_age (Pop ",k,", Season ",t,")"))))
    }
    do.call(tagList, ui)
  })
  
  # ---- rho (year) scope chooser ----
  output$rhoYearScopeUI <- renderUI({
    if (input$year_re == "ar1") {
      selectInput("rhoY_scope", "Rho (year) scope:",
                  choices = c("Global" = "single",
                              "By season" = "by_season",
                              "By population" = "by_stock",
                              "By region" = "by_region",
                              "By population × season" = "by_stock_season"),
                  selected = "single")
    }
  })
  
  # ---- rho (year) UI by scope ----
  output$rhoYearUI <- renderUI({
    if (input$year_re != "ar1") return(NULL)
    scope <- input$rhoY_scope %||% "single"
    nK <- n_stocks(); ns <- n_seasons(); nR <- n_regions()
    add_num <- function(id, lab) sliderInput(id, lab, min = -0.99, max = 0.99, value = 0.5, step = 0.01, width = "100%")
    ui <- list()
    if (scope == "single") {
      ui <- list(add_num("rhoY_global", "rho_year (global)"))
    } else if (scope == "by_season") {
      for (t in 1:ns) ui <- append(ui, list(add_num(paste0("rhoY_seas_",t), paste("rho_year (Season ",t,")"))))
    } else if (scope == "by_stock") {
      for (k in 1:nK) ui <- append(ui, list(add_num(paste0("rhoY_stock_",k), paste("rho_year (Pop ",k,")"))))
    } else if (scope == "by_region") {
      for (from in 1:nR) for (to in setdiff(1:nR,from))
        ui <- append(ui, list(add_num(paste0("rhoY_r",from,"_to",to), paste0("rho_year (R",from,"→R",to,")"))))
    } else if (scope == "by_stock_season") {
      for (k in 1:nK) for (t in 1:ns)
        ui <- append(ui, list(add_num(paste0("rhoY_stock_",k,"_seas_",t),
                                      paste0("rho_year (Pop ",k,", Season ",t,")"))))
    }
    do.call(tagList, ui)
  })
  
  # ============================================================================
  # 8) STEP 6: PRIORS (global or arrays by scope)
  # ============================================================================
  output$priorSigmaInput <- renderUI({
    if (!isTRUE(input$use_prior)) return(NULL)
    ns <- n_seasons(); nK <- n_stocks(); nR <- n_regions()
    scope <- input$prior_scope %||% "single"
    add_num <- function(id, lab) numericInput(id, lab, value=0.2, min=0, max=10, step=0.01)
    ui <- list()
    if (scope == "single") {
      ui <- list(add_num("prior_global", "Prior sigma (global)"))
    } else if (scope == "by_season") {
      for (t in 1:ns) ui <- append(ui, list(add_num(paste0("prior_seas_",t), paste("Prior sigma (Season ",t,")"))))
    } else if (scope == "by_stock") {
      for (k in 1:nK) ui <- append(ui, list(add_num(paste0("prior_stock_",k), paste("Prior sigma (Pop ",k,")"))))
    } else if (scope == "by_region") {
      for (from in 1:nR) for (to in setdiff(1:nR,from))
        ui <- append(ui, list(add_num(paste0("prior_r",from,"_to",to), paste0("Prior sigma (R",from,"→R",to,")"))))
    } else if (scope == "by_stock_season") {
      for (k in 1:nK) for (t in 1:ns)
        ui <- append(ui, list(add_num(paste0("prior_stock_",k,"_seas_",t),
                                      paste0("Prior sigma (Pop ",k,", Season ",t,")"))))
    }
    do.call(tagList, ui)
  })
  
  # ============================================================================
  # HELPER: BUILD OUTPUT LIST
  # ============================================================================
  edge_apply <- function(nR, FUN) {
    for (from in 1:nR) {
      k_idx <- 0
      for (to in 1:nR) if (to != from) {
        k_idx <- k_idx + 1
        FUN(from = from, to = to, k_idx = k_idx)
      }
    }
  }
  
  build_output_list <- function(input, n_regions, n_stocks, n_seasons, spawn_season, season_frac_vec) {
    nR <- n_regions; nK <- n_stocks; ns <- n_seasons
    mm <- input$mean_model %||% "constant"
    
    # Normalize season fractions
    sfrac <- season_frac_vec(); ssum <- sum(sfrac)
    if (abs(ssum - 1) > 1e-6) sfrac <- sfrac / ssum
    
    # Containers
    mean_vals <- array(0, dim = c(nK, ns, nR, nR - 1))
    can_move  <- array(1, dim = c(nK, ns, nR, nR))
    must_move <- array(0, dim = c(nK, ns, nR))
    
    # Population flags
    sm_vec <- rep(FALSE, nK)
    if (length(input$pop_move)) sm_vec[as.integer(input$pop_move)] <- TRUE
    sep_vec <- rep(FALSE, nK)
    if (length(input$separable)) sep_vec[as.integer(input$separable)] <- TRUE
    
    # Allowed seasons
    allowed_seasons <- if (length(input$canMoveSeasons)) as.integer(input$canMoveSeasons) else integer(0)
    for (k in 1:nK) for (t in 1:ns) {
      if (!(t %in% allowed_seasons) || !sm_vec[k]) can_move[k, t, , ] <- 0
    }
    
    # Must move for natal homing
    if (identical(input$dynamics, "natal")) {
      if (!length(input$mustMoveSeasons)) {
        must_move[, spawn_season, ] <- 1
      } else {
        must_move[, as.integer(input$mustMoveSeasons), ] <- 1
      }
    }
    
    # --- Fill means (only for populations allowed to move) ---
    k_active <- which(sm_vec)
    get_val <- function(id) {
      v <- input[[id]]
      if (is.null(v) || is.na(v)) 0 else as.numeric(v)
    }
    
    if (mm != "none") {
      for (from in 1:nR) {
        k_idx <- 0
        for (to in 1:nR) if (to != from) {
          k_idx <- k_idx + 1
          if (mm == "constant") {
            val <- get_val(paste0("mv_r", from, "_to", to))
            if (length(k_active)) mean_vals[k_active, , from, k_idx] <- val
          } else if (mm == "season") {
            for (t in 1:ns) {
              val <- get_val(paste0("mv_r", from, "_to", to, "_seas", t))
              if (length(k_active)) mean_vals[k_active, t, from, k_idx] <- val
            }
          } else if (mm == "stock_constant") {   # <-- renamed
            for (k in k_active) {
              mean_vals[k, , from, k_idx] <- get_val(paste0("mv_r", from, "_to", to, "_stock", k))
            }
          } else if (mm == "stock_season") {     # <-- renamed
            for (k in k_active) for (t in 1:ns) {
              mean_vals[k, t, from, k_idx] <- get_val(paste0("mv_r", from, "_to", to, "_stock", k, "_seas", t))
            }
          }
        }
      }
    } else {
      can_move[] <- 0
    }
    
    # --- Mean-model labels (matrix nR x (nR-1)) ---
    mean_model_edge <- matrix("none", nR, nR - 1)
    
    if (length(k_active)) {
      for (from in 1:nR) {
        k_idx <- 0
        for (to in 1:nR) if (to != from) {
          k_idx <- k_idx + 1
          
          # Only check active stocks
          for (k in k_active) {
            if (any(mean_vals[k, , from, k_idx] > 0)) {
              mean_model_edge[from, k_idx] <- mm
              break  # once we find an active mover, set label and stop
            }
          }
        }
      }
    }
    
    # Sigma
    sigma_vals <- NULL
    if (input$age_re %in% c("iid","ar1") || input$year_re %in% c("iid","ar1")) {
      sigma_vals <- array(0, dim = c(nK, ns, nR, nR - 1))
      scope <- input$sigma_scope %||% "single"
      getv <- function(id, default) as.numeric(input[[id]] %||% default)
      
      if (scope == "single") {
        sigma_vals[] <- getv("sig_global", 0.5)
      } else if (scope == "by_season") {
        for (t in 1:ns) sigma_vals[, t, , ] <- getv(paste0("sig_seas_", t), 0.5)
      } else if (scope == "by_stock") {
        for (k in 1:nK) sigma_vals[k, , , ] <- getv(paste0("sig_stock_", k), 0.5)
      } else if (scope == "by_region") {
        edge_apply(nR, function(from, to, k_idx) {
          sigma_vals[, , from, k_idx] <- getv(paste0("sig_r", from, "_to", to), 0.5)
        })
      } else if (scope == "by_stock_season") {
        for (k in 1:nK) for (t in 1:ns) {
          sigma_vals[k, t, , ] <- getv(paste0("sig_stock_", k, "_seas_", t), 0.5)
        }
      }
    }
    
    # Correlations: last dim = 1 (rho_age), 2 (rho_year)
    cor_vals <- array(0, dim = c(nK, ns, nR, nR - 1, 2))
    
    if (input$age_re == "ar1") {
      scopeA <- input$rhoA_scope %||% "single"
      getr <- function(id, default) as.numeric(input[[id]] %||% default)
      
      if (scopeA == "single") {
        cor_vals[,,,,1] <- getr("rhoA_global", 0.5)
      } else if (scopeA == "by_season") {
        for (t in 1:ns) cor_vals[, t, , , 1] <- getr(paste0("rhoA_seas_", t), 0.5)
      } else if (scopeA == "by_stock") {
        for (k in 1:nK) cor_vals[k, , , , 1] <- getr(paste0("rhoA_stock_", k), 0.5)
      } else if (scopeA == "by_region") {
        edge_apply(nR, function(from, to, k_idx) {
          cor_vals[, , from, k_idx, 1] <- getr(paste0("rhoA_r", from, "_to", to), 0.5)
        })
      } else if (scopeA == "by_stock_season") {
        for (k in 1:nK) for (t in 1:ns) {
          cor_vals[k, t, , , 1] <- getr(paste0("rhoA_stock_", k, "_seas_", t), 0.5)
        }
      }
    }
    
    if (input$year_re == "ar1") {
      scopeY <- input$rhoY_scope %||% "single"
      getr <- function(id, default) as.numeric(input[[id]] %||% default)
      
      if (scopeY == "single") {
        cor_vals[,,,,2] <- getr("rhoY_global", 0.5)
      } else if (scopeY == "by_season") {
        for (t in 1:ns) cor_vals[, t, , , 2] <- getr(paste0("rhoY_seas_", t), 0.5)
      } else if (scopeY == "by_stock") {
        for (k in 1:nK) cor_vals[k, , , , 2] <- getr(paste0("rhoY_stock_", k), 0.5)
      } else if (scopeY == "by_region") {
        edge_apply(nR, function(from, to, k_idx) {
          cor_vals[, , from, k_idx, 2] <- getr(paste0("rhoY_r", from, "_to", to), 0.5)
        })
      } else if (scopeY == "by_stock_season") {
        for (k in 1:nK) for (t in 1:ns) {
          cor_vals[k, t, , , 2] <- getr(paste0("rhoY_stock_", k, "_seas_", t), 0.5)
        }
      }
    }
    
    # Priors
    use_prior   <- array(0, dim = c(nK, ns, nR, nR - 1))
    prior_sigma <- array(NA_real_, dim = c(nK, ns, nR, nR - 1))
    
    if (isTRUE(input$use_prior)) {
      scope <- input$prior_scope %||% "single"
      getp <- function(id, default) as.numeric(input[[id]] %||% default)
      
      if (scope == "single") {
        # one prior sigma value
        val <- getp("prior_global", 0.2)
        use_prior[]   <- 1
        prior_sigma[] <- val
        
      } else if (scope == "by_season") {
        for (t in 1:ns) {
          val <- getp(paste0("prior_seas_", t), 0.2)
          use_prior[, t, , ]   <- 1
          prior_sigma[, t, , ] <- val
        }
        
      } else if (scope == "by_stock") {
        for (k in 1:nK) {
          val <- getp(paste0("prior_stock_", k), 0.2)
          use_prior[k, , , ]   <- 1
          prior_sigma[k, , , ] <- val
        }
        
      } else if (scope == "by_region") {
        edge_apply(nR, function(from, to, k_idx) {
          val <- getp(paste0("prior_r", from, "_to", to), 0.2)
          use_prior[, , from, k_idx]   <- 1
          prior_sigma[, , from, k_idx] <- val
        })
        
      } else if (scope == "by_stock_season") {
        for (k in 1:nK) for (t in 1:ns) {
          val <- getp(paste0("prior_stock_", k, "_seas_", t), 0.2)
          use_prior[k, t, , ]   <- 1
          prior_sigma[k, t, , ] <- val
        }
      }
    }
    
    list(
      n_regions   = nR,
      n_stocks    = nK,
      n_seasons   = ns,
      season_frac = sfrac,
      dynamics    = input$dynamics,
      pop_move    = sm_vec,
      separable   = sep_vec,
      must_move   = must_move,
      can_move    = can_move,
      mean_vals   = mean_vals,
      mean_model  = mean_model_edge,   # matrix [nR x (nR-1)], "none" for inactive edges
      sigma_vals  = sigma_vals,
      cor_vals    = cor_vals,
      use_prior   = use_prior,
      prior_sigma = prior_sigma
    )
  }
  
  # ============================================================================
  # 9) STEP 7: BUILD OUTPUTS ON ENTERING REVIEW TAB
  # ============================================================================
  diagram_reactive <- reactiveVal(NULL)
  output_list_reactive <- reactiveVal(NULL)
  
  observeEvent(input$next_to_review, {
    # Build full output list using helper
    out <- build_output_list(
      input,
      n_regions(),
      n_stocks(),
      n_seasons(),
      spawn_season(),
      season_frac_vec
    )
    output_list_reactive(out)
    
    # Build region diagram
    g <- create_graph() %>%
      add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") %>%
      add_global_graph_attrs(attr = "rankdir", value = "LR", attr_type = "graph")
    
    for (r in 1:out$n_regions) {
      g <- g %>% add_node(label = paste0("R", r))
    }
    for (from in 1:out$n_regions) {
      k_idx <- 0
      for (to in 1:out$n_regions) if (to != from) {
        k_idx <- k_idx + 1
        if (any(out$mean_vals[,,from,k_idx] > 0)) {
          g <- g %>% add_edge(from = from, to = to)
        }
      }
    }
    
    output$movementDiagram <- renderGrViz({
      grViz(DiagrammeR::generate_dot(g))
    })
    diagram_reactive(g)
    
    # Print output list (debug/info)
    output$outputList <- renderPrint({ out })
  })
  
  # ============================================================================
  # 10b) FINALIZE CONFIGURATION
  # ============================================================================
  observeEvent(input$validate_finalize, {
    errs <- c()
    out <- output_list_reactive()
    
    # Fallback: build configuration if not already built
    if (is.null(out)) {
      out <- build_output_list(
        input,
        n_regions(),
        n_stocks(),
        n_seasons(),
        spawn_season(),
        season_frac_vec
      )
      output_list_reactive(out)
    }
    
    # --- Validation checks ---
    if (identical(input$dynamics, "natal") && !is.null(out)) {
      must_by_season <- apply(out$must_move, 2, max)
      if (!any(must_by_season == 1)) {
        errs <- c(errs, "Natal homing requires at least one 'must return' season.")
      }
    }
    
    # --- Report results ---
    if (length(errs)) {
      showModal(modalDialog(
        title = "Finalize failed",
        easyClose = TRUE,
        footer = modalButton("Close"),
        HTML(paste("<ul>",
                   paste(sprintf("<li>%s</li>", errs), collapse=""),
                   "</ul>"))
      ))
    } else {
      showModal(modalDialog(
        title = "Configuration finalized",
        easyClose = TRUE,
        footer = NULL,
        div(style="color:#27AE60; font-weight:700;",
            "Your movement model configuration has been validated and finalized.")
      ))
      session$userData$finalized <- TRUE
    }
  })
  
  # ============================================================================
  # 10c) START OVER
  # ============================================================================
  observeEvent(input$start_over, {
    showModal(modalDialog(
      title = "Reset configuration",
      "Are you sure you want to start over? All current selections will be lost.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmReset", "Yes, reset", class = "btn btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirmReset, {
    removeModal()
    # reset key inputs to defaults
    updateRadioGroupButtons(session, "dynamics", selected = "natal")
    updateNumericInput(session, "n_regions", value = 2)
    updateNumericInput(session, "n_stocks", value = 2)
    updateNumericInput(session, "n_seasons", value = 4)
    updateNumericInput(session, "fracyr_spawn", value = 0.5)
    
    for (s in 1:4) {
      try(updateSliderInput(session, paste0("season_frac_", s), value = 0.25), silent=TRUE)
    }
    updatePickerInput(session, "canMoveSeasons", selected = c("1"))
    updatePickerInput(session, "mustMoveSeasons", selected = "1")
    
    updatePickerInput(session, "pop_move", selected = character(0))
    updatePickerInput(session, "separable", selected = character(0))
    updateSelectInput(session, "mean_model", selected = "constant")
    updateRadioButtons(session, "rate_entry_mode", selected = "Sliders")
    updateSelectInput(session, "age_re", selected = "none")
    updateSelectInput(session, "year_re", selected = "none")
    updateCheckboxInput(session, "use_prior", value = FALSE)
    
    # reset backend
    output_list_reactive(NULL)
    diagram_reactive(NULL)
    
    updateTabsetPanel(session, "step_tabs", selected = "1) Setup")
  })
  
  # ============================================================================
  # 11) DOWNLOADS
  # ============================================================================
  output$downloadMovementMatrix <- downloadHandler(
    filename = function() "movement_list.rds",
    content  = function(file) saveRDS(output_list_reactive(), file)
  )
  
  output$downloadMovementDiagram <- downloadHandler(
    filename = function() "movement_diagram.png",
    content  = function(file) {
      g <- diagram_reactive()
      if (is.null(g)) {
        nR <- n_regions()
        g <- create_graph() %>%
          add_global_graph_attrs(attr = "layout", value = "dot",  attr_type = "graph") %>%
          add_global_graph_attrs(attr = "rankdir", value = "LR",   attr_type = "graph")
        for (r in 1:nR) g <- g %>% add_node(label = paste0("R", r))
      }
      export_svg(grViz(DiagrammeR::generate_dot(g))) %>% charToRaw %>% rsvg_png(file)
    }
  )
  
  # ============================================================================
  # 12) DEMO (ONLY ON STEP 7) + CLOSE (ONLY ON STEP 7)
  # ============================================================================
  
  # --- Metapopulation demo (2 regions) ---
  observeEvent(input$demo_meta, {
    updateRadioGroupButtons(session, "dynamics", selected = "meta")
    updateNumericInput(session, "n_regions", value = 2)
    updateNumericInput(session, "n_stocks", value = 2)
    updateNumericInput(session, "n_seasons", value = 2)
    
    updateSliderInput(session, "season_frac_1", value = 0.5)
    updateSliderInput(session, "season_frac_2", value = 0.5)
    updatePickerInput(session, "canMoveSeasons", selected = c("1","2"))
    updatePickerInput(session, "mustMoveSeasons", selected = character(0))
    updatePickerInput(session, "pop_move", selected = c("1","2"))
    updatePickerInput(session, "separable", selected = c("1","2"))
    updateSelectInput(session, "mean_model", selected = "constant")
    updateRadioButtons(session, "rate_entry_mode", selected = "Sliders")
    
    # also set default values in case user never visited tabs
    if (is.null(input$canMoveSeasons)) updatePickerInput(session, "canMoveSeasons", selected = c("1","2"))
    if (is.null(input$pop_move))       updatePickerInput(session, "pop_move", selected = c("1","2"))
    
    delay(150, {
      try(updateSliderInput(session, "mv_r1_to2", value = 0.25), silent = TRUE)
      try(updateSliderInput(session, "mv_r2_to1", value = 0.15), silent = TRUE)
    })
    
    # --- Build backend arrays ---
    nR <- 2; nK <- 2; ns <- 2
    sfrac <- c(0.5, 0.5)
    
    mean_vals <- array(0, dim = c(nK, ns, nR, nR - 1))
    can_move  <- array(1, dim = c(nK, ns, nR, nR))
    must_move <- array(0, dim = c(nK, ns, nR))
    
    mean_vals[,,1,1] <- 0.25  # R1 → R2
    mean_vals[,,2,1] <- 0.15  # R2 → R1
    
    out <- list(
      n_regions   = nR,
      n_stocks    = nK,
      n_seasons   = ns,
      season_frac = sfrac,
      dynamics    = "meta",
      pop_move    = c(TRUE, TRUE),
      separable   = c(TRUE, TRUE),
      can_move    = can_move,
      must_move   = must_move,
      mean_vals   = mean_vals,
      mean_model  = "constant"
    )
    output_list_reactive(out)
    output$outputList <- renderPrint(out)
    
    # --- Figures ---
    output$seasonDiagram       <- renderPlot({ renderSeasonBar(nR, sfrac, 1) })
    output$seasonDiagram_final <- renderPlot({ renderSeasonBar(nR, sfrac, 1) })
    
    g <- create_graph() %>%
      add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") %>%
      add_global_graph_attrs(attr = "rankdir", value = "LR", attr_type = "graph") %>%
      add_node(label = "R1") %>% add_node(label = "R2") %>%
      add_edge(from = 1, to = 2) %>% add_edge(from = 2, to = 1)
    output$movementDiagram <- renderGrViz({ grViz(DiagrammeR::generate_dot(g)) })
    
    output$summary_text <- renderUI({
      HTML("<ul>
         <li><b>Dynamics:</b> Metapopulation</li>
         <li><b>Regions:</b> 2 | <b>Stocks:</b> 2 | <b>Seasons:</b> 2</li>
         </ul>")
    })
    
    showNotification("Loaded Metapopulation demo (2 regions).", type = "message")
  })
  
  
  # --- Natal homing demo (2 regions) ---
  observeEvent(input$demo_natal, {
    updateRadioGroupButtons(session, "dynamics", selected = "natal")
    updateNumericInput(session, "n_regions", value = 2)
    updateNumericInput(session, "n_stocks", value = 2)
    updateNumericInput(session, "n_seasons", value = 2)
    
    updateSliderInput(session, "season_frac_1", value = 0.6)
    updateSliderInput(session, "season_frac_2", value = 0.4)
    updatePickerInput(session, "canMoveSeasons", selected = c("1","2"))
    updatePickerInput(session, "mustMoveSeasons", selected = c("2"))
    updatePickerInput(session, "pop_move", selected = c("1","2"))
    updatePickerInput(session, "separable", selected = c("1")) # only pop 1 sequential
    updateSelectInput(session, "mean_model", selected = "season")
    updateRadioButtons(session, "rate_entry_mode", selected = "Sliders")
    
    # also set default values in case user never visited tabs
    if (is.null(input$canMoveSeasons)) updatePickerInput(session, "canMoveSeasons", selected = c("1","2"))
    if (is.null(input$pop_move))       updatePickerInput(session, "pop_move", selected = c("1","2"))
    
    delay(150, {
      try(updateSliderInput(session, "mv_r1_to2_seas1", value = 0.20), silent = TRUE)
      try(updateSliderInput(session, "mv_r1_to2_seas2", value = 0.00), silent = TRUE)
      try(updateSliderInput(session, "mv_r2_to1_seas1", value = 0.10), silent = TRUE)
      try(updateSliderInput(session, "mv_r2_to1_seas2", value = 0.00), silent = TRUE)
    })
    
    # --- Build backend arrays ---
    nR <- 2; nK <- 2; ns <- 2
    sfrac <- c(0.6, 0.4)
    
    mean_vals <- array(0, dim = c(nK, ns, nR, nR - 1))
    can_move  <- array(1, dim = c(nK, ns, nR, nR))
    must_move <- array(0, dim = c(nK, ns, nR))
    
    # natal homing: return to natal region in season 2
    must_move[,,2] <- 1
    
    mean_vals[1,1,1,1] <- 0.20  # Pop1, S1, R1→R2
    mean_vals[2,1,2,1] <- 0.10  # Pop2, S1, R2→R1
    
    out <- list(
      n_regions   = nR,
      n_stocks    = nK,
      n_seasons   = ns,
      season_frac = sfrac,
      dynamics    = "natal",
      pop_move    = c(TRUE, TRUE),
      separable   = c(TRUE, FALSE), # only pop1 separable
      can_move    = can_move,
      must_move   = must_move,
      mean_vals   = mean_vals,
      mean_model  = "season"
    )
    output_list_reactive(out)
    output$outputList <- renderPrint(out)
    
    # --- Figures ---
    output$seasonDiagram       <- renderPlot({ renderSeasonBar(nR, sfrac, 2) })
    output$seasonDiagram_final <- renderPlot({ renderSeasonBar(nR, sfrac, 2) })
    
    g <- create_graph() %>%
      add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") %>%
      add_global_graph_attrs(attr = "rankdir", value = "LR", attr_type = "graph") %>%
      add_node(label = "R1") %>% add_node(label = "R2") %>%
      add_edge(from = 1, to = 2) %>% add_edge(from = 2, to = 1)
    output$movementDiagram <- renderGrViz({ grViz(DiagrammeR::generate_dot(g)) })
    
    output$summary_text <- renderUI({
      HTML("<ul>
         <li><b>Dynamics:</b> Natal homing</li>
         <li><b>Regions:</b> 2 | <b>Stocks:</b> 2 | <b>Seasons:</b> 2</li>
         <li><b>Natal return season:</b> S2</li>
         </ul>")
    })
    
    showNotification("Loaded Natal Homing demo (2 regions).", type = "message")
  })
  
  # Close app (final step only)
  observeEvent(input$closeApp, {
    showModal(modalDialog(
      title = "Closing app",
      "Are you sure you want to close the wizard?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmClose", "Yes, close", class = "btn btn-danger")
      )
    ))
  })
  observeEvent(input$confirmClose, {
    stopApp()
  })
  
  # ============================================================================
  # End server
  # ============================================================================
}
# UI.R
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
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .nav-tabs .nav-item .nav-link {
        background-color: #2C3E50;
        color: white;
        font-weight: bold;
      }
      /* Highlight the active tab in green */
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
  
  titlePanel(paste0("Movement Model Builder (v", spasam_mse_version, ")")),
  
  tabsetPanel(
    id = "step_tabs",
    
    # STEP 1: Setup
    tabPanel("1) Setup",
             sidebarLayout(
               sidebarPanel(
                 h4("General setup"),
                 radioGroupButtons(
                   inputId = "dynamics",
                   label = "Dynamics type",
                   choices = c("Natal homing" = "natal", "Metapopulation" = "meta"),
                   justified = TRUE
                 ),
                 numericInput("n_regions", "Number of Regions:", min = 2, max = 12, value = 2),
                 numericInput("n_stocks", "Number of Populations:", min = 2, max = 12, value = 2),
                 numericInput("n_seasons", "Number of Seasons:", min = 1, max = 12, value = 4),
                 numericInput("fracyr_spawn", "Fraction of Year for Spawning:", 
                              min = 0, max = 1, value = 0.5, step = 0.05),
                 uiOutput("regionStockError"),
                 actionBttn("next_to_seasons", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 1: Define whether populations return to natal regions or form a metapopulation.
                    Specify the number of regions, stocks, and seasons.")
               )
             )
    ),
    
    # STEP 2: Seasons
    tabPanel("2) Seasons",
             sidebarLayout(
               sidebarPanel(
                 h4("Season settings"),
                 radioButtons("season_input_mode", "Season fraction entry:",
                              choices = c("Sliders", "Numeric"), inline = TRUE, selected = "Sliders"),
                 uiOutput("seasonFractionsUI"),
                 uiOutput("seasonFracSumUI"),
                 uiOutput("canMoveSeasonsUI"),
                 uiOutput("mustMoveSeasonsUI"),
                 uiOutput("seasonErrors"),
                 actionBttn("back_to_setup", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_populations", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 2: Specify the fraction of the year covered by each season. Fractions should sum to 1."),
                 plotOutput("seasonDiagram", height = 180)
               )
             )
    ),
    
    # STEP 3: Populations
    tabPanel("3) Populations",
             sidebarLayout(
               sidebarPanel(
                 h4("Population options"),
                 uiOutput("popMoveUI"),
                 uiOutput("separableUI"),
                 actionBttn("back_to_seasons", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_rates", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 3: Select which populations can move between regions."),
                 p("Note: 'Separable movement' means movement is applied sequentially after mortality. 
                    If unchecked, movement is treated as instantaneous with mortality.")
               )
             )
    ),
    
    # STEP 4: Movement Rates
    tabPanel("4) Movement Rates",
             sidebarLayout(
               sidebarPanel(
                 h4("Movement rates"),
                 selectInput("mean_model", "Model type",
                             choices = c("none","constant","season","population_constant","population_season"),
                             selected = "constant"),
                 radioButtons("rate_entry_mode", "Entry mode:", 
                              choices = c("Sliders","Numeric inputs"), 
                              selected = "Sliders", inline = TRUE),
                 actionBttn("back_to_populations", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_re", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 4: Define movement rates between regions, optionally varying by season or population."),
                 uiOutput("movementInputs")
               )
             )
    ),
    
    # STEP 5: Random Effects
    tabPanel("5) Random Effects",
             sidebarLayout(
               sidebarPanel(
                 h4("Random effects"),
                 fluidRow(
                   column(6, selectInput("age_re", "Age RE:", choices = c("none","iid","ar1"), selected = "none")),
                   column(6, selectInput("year_re", "Year RE:", choices = c("none","iid","ar1"), selected = "none"))
                 ),
                 uiOutput("sigmaScopeUI"),
                 uiOutput("sigmaUI"),
                 uiOutput("rhoAgeScopeUI"),
                 uiOutput("rhoAgeUI"),
                 uiOutput("rhoYearScopeUI"),
                 uiOutput("rhoYearUI"),
                 actionBttn("back_to_rates", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_priors", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 5: Configure random effects by population, season, or region.")
               )
             )
    ),
    
    # STEP 6: Priors
    tabPanel("6) Priors",
             sidebarLayout(
               sidebarPanel(
                 h4("Priors"),
                 checkboxInput("use_prior", "Use prior?", value = FALSE),
                 selectInput("prior_scope", "Prior sigma scope:",
                             choices = c("Global" = "single",
                                         "By season" = "by_season",
                                         "By population" = "by_stock",
                                         "By region" = "by_region",
                                         "By population × season" = "by_stock_season")),
                 uiOutput("priorSigmaInput"),
                 actionBttn("back_to_re", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_review", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 6: Optionally specify priors on movement parameters.")
               )
             )
    ),
    
    # STEP 7: Review & Export
    tabPanel("7) Review & Export",
             sidebarLayout(
               sidebarPanel(
                 h4("Export options"),
                 div(style="margin-bottom: 10px;", 
                     actionBttn("validate_finalize", "Validate & Finalize", 
                                style="material-flat", color="success")),
                 div(style="margin-bottom: 10px;", 
                     downloadButton("downloadMovementMatrix", "Download Movement List (.rds)")),
                 div(style="margin-bottom: 10px;", 
                     downloadButton("downloadMovementDiagram", "Download Diagram (.png)")),
                 div(style="margin-bottom: 10px;", 
                     actionBttn("start_over", "Start Over", style="material-flat", color="danger")),
                 div(style="margin-bottom: 10px;", 
                     actionBttn("back_to_priors", "◀ Back", style="material-flat", color="warning")),
                 br(),
                 h4("Demos"),
                 p("Load preconfigured demo setups for quick exploration."),
                 column(12,
                        actionButton("demo_meta", "Metapopulation Demo (2 Regions)",
                                     class = "btn btn-success btn-block")),
                 column(12,
                        actionButton("demo_natal", "Natal Homing Demo (2 Regions)",
                                     class = "btn btn-info btn-block")),
                 br(),
                 actionButton("closeApp", "Close", class = "btn btn-danger")
               ),
               mainPanel(
                 p("Step 7: Review your selections."),
                 uiOutput("summary_text"),
                 plotOutput("seasonDiagram_final", height = 180),
                 grVizOutput("movementDiagram"),
                 verbatimTextOutput("outputList"),
                 uiOutput("warning_final")  # warnings if rates > 1
               )
             )
    )
  )
)

# UI.R
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
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .nav-tabs .nav-item .nav-link {
        background-color: #2C3E50;
        color: white;
        font-weight: bold;
      }
      /* Highlight the active tab in green */
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
  
  titlePanel(paste0("Movement Model Builder (v", spasam_mse_version, ")")),
  
  tabsetPanel(
    id = "step_tabs",
    
    # STEP 1: Setup
    tabPanel("1) Setup",
             sidebarLayout(
               sidebarPanel(
                 h4("General setup"),
                 radioGroupButtons(
                   inputId = "dynamics",
                   label = "Dynamics type",
                   choices = c("Natal homing" = "natal", "Metapopulation" = "meta"),
                   justified = TRUE
                 ),
                 numericInput("n_regions", "Number of Regions:", min = 2, max = 12, value = 2),
                 numericInput("n_stocks", "Number of Populations:", min = 2, max = 12, value = 2),
                 numericInput("n_seasons", "Number of Seasons:", min = 1, max = 12, value = 4),
                 numericInput("fracyr_spawn", "Fraction of Year for Spawning:", 
                              min = 0, max = 1, value = 0.5, step = 0.05),
                 uiOutput("regionStockError"),
                 actionBttn("next_to_seasons", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 1: Define whether populations return to natal regions or form a metapopulation.
                    Specify the number of regions, stocks, and seasons.")
               )
             )
    ),
    
    # STEP 2: Seasons
    tabPanel("2) Seasons",
             sidebarLayout(
               sidebarPanel(
                 h4("Season settings"),
                 radioButtons("season_input_mode", "Season fraction entry:",
                              choices = c("Sliders", "Numeric"), inline = TRUE, selected = "Sliders"),
                 uiOutput("seasonFractionsUI"),
                 uiOutput("seasonFracSumUI"),
                 uiOutput("canMoveSeasonsUI"),
                 uiOutput("mustMoveSeasonsUI"),
                 uiOutput("seasonErrors"),
                 actionBttn("back_to_setup", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_populations", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 2: Specify the fraction of the year covered by each season. Fractions should sum to 1."),
                 plotOutput("seasonDiagram", height = 180)
               )
             )
    ),
    
    # STEP 3: Populations
    tabPanel("3) Populations",
             sidebarLayout(
               sidebarPanel(
                 h4("Population options"),
                 uiOutput("popMoveUI"),
                 uiOutput("separableUI"),
                 actionBttn("back_to_seasons", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_rates", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 3: Select which populations can move between regions."),
                 p("Note: 'Separable movement' means movement is applied sequentially after mortality. 
                    If unchecked, movement is treated as instantaneous with mortality.")
               )
             )
    ),
    
    # STEP 4: Movement Rates
    tabPanel("4) Movement Rates",
             sidebarLayout(
               sidebarPanel(
                 h4("Movement rates"),
                 selectInput("mean_model", "Model type",
                             choices = c("none","constant","season","population_constant","population_season"),
                             selected = "constant"),
                 radioButtons("rate_entry_mode", "Entry mode:", 
                              choices = c("Sliders","Numeric inputs"), 
                              selected = "Sliders", inline = TRUE),
                 actionBttn("back_to_populations", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_re", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 4: Define movement rates between regions, optionally varying by season or population."),
                 uiOutput("movementInputs")
               )
             )
    ),
    
    # STEP 5: Random Effects
    tabPanel("5) Random Effects",
             sidebarLayout(
               sidebarPanel(
                 h4("Random effects"),
                 fluidRow(
                   column(6, selectInput("age_re", "Age RE:", choices = c("none","iid","ar1"), selected = "none")),
                   column(6, selectInput("year_re", "Year RE:", choices = c("none","iid","ar1"), selected = "none"))
                 ),
                 uiOutput("sigmaScopeUI"),
                 uiOutput("sigmaUI"),
                 uiOutput("rhoAgeScopeUI"),
                 uiOutput("rhoAgeUI"),
                 uiOutput("rhoYearScopeUI"),
                 uiOutput("rhoYearUI"),
                 actionBttn("back_to_rates", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_priors", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 5: Configure random effects by population, season, or region.")
               )
             )
    ),
    
    # STEP 6: Priors
    tabPanel("6) Priors",
             sidebarLayout(
               sidebarPanel(
                 h4("Priors"),
                 checkboxInput("use_prior", "Use prior?", value = FALSE),
                 selectInput("prior_scope", "Prior sigma scope:",
                             choices = c("Global" = "single",
                                         "By season" = "by_season",
                                         "By population" = "by_stock",
                                         "By region" = "by_region",
                                         "By population × season" = "by_stock_season")),
                 uiOutput("priorSigmaInput"),
                 actionBttn("back_to_re", "◀ Back", style="material-flat", color="warning"),
                 actionBttn("next_to_review", "Next ▶", style="material-flat", color="primary")
               ),
               mainPanel(
                 p("Step 6: Optionally specify priors on movement parameters.")
               )
             )
    ),
    
    # STEP 7: Review & Export
    tabPanel("7) Review & Export",
             sidebarLayout(
               sidebarPanel(
                 h4("Export options"),
                 div(style="margin-bottom: 10px;", 
                     actionBttn("validate_finalize", "Validate & Finalize", 
                                style="material-flat", color="success")),
                 div(style="margin-bottom: 10px;", 
                     downloadButton("downloadMovementMatrix", "Download Movement List (.rds)")),
                 div(style="margin-bottom: 10px;", 
                     downloadButton("downloadMovementDiagram", "Download Diagram (.png)")),
                 div(style="margin-bottom: 10px;", 
                     actionBttn("start_over", "Start Over", style="material-flat", color="danger")),
                 div(style="margin-bottom: 10px;", 
                     actionBttn("back_to_priors", "◀ Back", style="material-flat", color="warning")),
                 br(),
                 h4("Demos"),
                 p("Load preconfigured demo setups for quick exploration."),
                 column(12,
                        actionButton("demo_meta", "Metapopulation Demo (2 Regions)",
                                     class = "btn btn-success btn-block")),
                 column(12,
                        actionButton("demo_natal", "Natal Homing Demo (2 Regions)",
                                     class = "btn btn-info btn-block")),
                 br(),
                 actionButton("closeApp", "Close", class = "btn btn-danger")
               ),
               mainPanel(
                 p("Step 7: Review your selections."),
                 uiOutput("summary_text"),
                 plotOutput("seasonDiagram_final", height = 180),
                 grVizOutput("movementDiagram"),
                 verbatimTextOutput("outputList"),
                 uiOutput("warning_final")  # warnings if rates > 1
               )
             )
    )
  )
)

# Run the app
shinyApp(ui = ui, server = server)
