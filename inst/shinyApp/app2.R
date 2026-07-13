library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DiagrammeR)

spasam_mse_version <- tryCatch(as.character(utils::packageVersion("SPASAM.MSE")),
                               error = function(e) "1.1.5")

ui <- fluidPage(
  useShinyjs(),
  titlePanel(paste0("Specify Movement Rates (Beta Version, v", spasam_mse_version, ")")),
  
  tags$head(tags$style(HTML("
    .picker-width { width: 100% !important; }
    .big-warning { color:#b30000; font-weight:600; }
    .muted { color:#666; }
    .section-title { margin-top: 12px; font-weight: 600; }
  "))),
  
  sidebarLayout(
    sidebarPanel(
      h4("Basic settings"),
      numericInput("n_stocks",  "Number of Stocks:",  min = 1, max = 50, value = 2),
      numericInput("n_seasons", "Number of Seasons:", min = 1, max = 12, value = 4),
      numericInput("n_regions", "Number of Regions:", min = 2, max = 12, value = 2),
      
      numericInput(
        "fracyr_spawn",
        "Fraction of Year for Spawning (for default season):",
        min = 0, max = 1, value = 0.5, step = 0.05
      ),
      
      div(class = "section-title", "Season fractions"),
      uiOutput("seasonFractionsUI"),
      uiOutput("seasonFracSumUI"),
      
      div(class = "section-title", "Movement dynamics"),
      radioButtons(
        "metapop", NULL,
        choices = list(
          "Natal homing (must move back)" = 0,
          "Metapopulation (no natal homing)" = 1
        ),
        selected = 0,
        inline = TRUE
      ),
      
      div(class = "section-title", "Stock-level options"),
      uiOutput("stockMoveUI"),
      uiOutput("separableUI"),
      
      div(class = "section-title", "Seasonal movement toggles"),
      uiOutput("canMoveSeasonsUI"),
      uiOutput("mustMoveSeasonsUI"),
      
      div(class = "section-title", "Mean model"),
      selectInput(
        "mean_model", NULL,
        choices = c("none", "constant", "season", "stock_constant", "stock_season"),
        selected = "constant"
      ),
      
      radioButtons(
        "rate_entry_mode",
        "Entry mode for movement rates:",
        choices = c("Sliders", "Numeric inputs"),
        selected = "Sliders",
        inline = TRUE
      ),
      
      div(class = "section-title", "Random effects"),
      fluidRow(
        column(6, selectInput("age_re",  "Age RE:",  choices = c("none", "iid", "ar1"), selected = "none")),
        column(6, selectInput("year_re", "Year RE:", choices = c("none", "iid", "ar1"), selected = "none"))
      ),
      
      uiOutput("sigmaScopeUI"),
      uiOutput("sigmaUI"),
      uiOutput("rhoAgeScopeUI"),
      uiOutput("rhoAgeUI"),
      uiOutput("rhoYearScopeUI"),
      uiOutput("rhoYearUI"),
      
      div(class = "section-title", "Priors on mean movement"),
      checkboxInput("use_prior", "Use prior?", value = FALSE),
      uiOutput("priorSigmaInput"),
      
      div(class = "section-title", "Movement rates"),
      uiOutput("movementInputs"),
      
      tags$hr(),
      actionButton("generate", "Generate Movement Input", class = "btn-primary"),
      downloadButton("downloadMovementMatrix", "Download Movement List (.rds)"),
      downloadButton("downloadMovementDiagram", "Download Diagram (.png)"),
      tags$hr(),
      actionButton("demoButton", "Demo"),
      actionButton("restartButton", "Restart"),
      actionButton("exitButton", "Exit")
    ),
    
    mainPanel(
      uiOutput("warning"),
      plotOutput("seasonDiagram", height = "320px"),
      grVizOutput("movementDiagram"),
      tags$hr(),
      verbatimTextOutput("outputList")
    )
  )
)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

`%||%` <- function(a, b) if (!is.null(a)) a else b

server <- function(input, output, session) {
  diagram_reactive <- reactiveVal(NULL)
  output_list_reactive <- reactiveVal(NULL)
  
  spawn_season <- reactive({
    max(1, min(input$n_seasons, ceiling(input$fracyr_spawn * input$n_seasons)))
  })
  
  home_region <- reactive({
    pmin(seq_len(input$n_stocks), input$n_regions)
  })
  
  output$seasonFractionsUI <- renderUI({
    ns <- input$n_seasons
    fracs <- lapply(seq_len(ns), function(s) {
      sliderInput(
        paste0("season_frac_", s),
        label = paste("Season", s, "fraction"),
        min = 0, max = 1, value = round(1 / ns, 2), step = 0.01, width = "100%"
      )
    })
    do.call(tagList, fracs)
  })
  
  season_frac_vec <- reactive({
    ns <- input$n_seasons
    
    vals <- lapply(seq_len(ns), function(s) {
      x <- input[[paste0("season_frac_", s)]]
      if (is.null(x) || length(x) == 0 || is.na(x)) {
        1 / ns
      } else {
        as.numeric(x)[1]
      }
    })
    
    unlist(vals, use.names = FALSE)
  })
  
  output$seasonFracSumUI <- renderUI({
    vals <- season_frac_vec()
    sm <- sum(as.numeric(vals), na.rm = TRUE)
    
    cls <- if (abs(sm - 1) < 1e-6) "muted" else "big-warning"
    
    div(
      class = cls,
      sprintf(
        "Sum of season fractions: %.2f%s",
        sm,
        if (abs(sm - 1) > 1e-6) " (will be normalized on Generate)" else ""
      )
    )
  })
  
  output$stockMoveUI <- renderUI({
    n <- input$n_stocks
    pickerInput(
      "stock_move", "Stocks allowed to move:",
      choices = setNames(1:n, paste0("Stock ", 1:n)),
      selected = 1:n, multiple = TRUE,
      options = list(`actions-box` = TRUE, size = 8),
      width = "100%"
    )
  })
  
  output$separableUI <- renderUI({
    n <- input$n_stocks
    pickerInput(
      "separable", "Stocks with separable movement from mortality:",
      choices = setNames(1:n, paste0("Stock ", 1:n)),
      selected = 1:n, multiple = TRUE,
      options = list(`actions-box` = TRUE, size = 8),
      width = "100%"
    )
  })
  
  output$canMoveSeasonsUI <- renderUI({
    ns <- input$n_seasons
    sel <- setdiff(1:ns, spawn_season())
    pickerInput(
      "canMoveSeasons", "Seasons where movement is allowed:",
      choices = 1:ns, selected = sel, multiple = TRUE,
      options = list(`actions-box` = TRUE, size = 10),
      width = "100%"
    )
  })
  
  output$mustMoveSeasonsUI <- renderUI({
    ns <- input$n_seasons
    pickerInput(
      "mustMoveSeasons", "Seasons with natal homing (must move):",
      choices = 1:ns, selected = spawn_season(), multiple = TRUE,
      options = list(`actions-box` = TRUE, size = 10),
      width = "100%"
    )
  })
  
  observeEvent(input$metapop, {
    if (as.integer(input$metapop) == 1) {
      updatePickerInput(session, "mustMoveSeasons", selected = character(0))
      shinyjs::disable("mustMoveSeasons")
    } else {
      shinyjs::enable("mustMoveSeasons")
      isolate({
        if (!length(input$mustMoveSeasons)) {
          updatePickerInput(
            session, "mustMoveSeasons",
            selected = as.character(spawn_season())
          )
        }
      })
    }
  }, ignoreInit = TRUE)
  
  output$priorSigmaInput <- renderUI({
    if (isTRUE(input$use_prior)) {
      numericInput(
        "priorSigma",
        "Prior sigma (on transformed scale):",
        value = 0.2, step = 0.05, min = 0
      )
    }
  })
  
  output$sigmaScopeUI <- renderUI({
    if (input$age_re %in% c("iid", "ar1") || input$year_re %in% c("iid", "ar1")) {
      selectInput(
        "sigma_scope", "Sigma scope:",
        choices = c("single", "by_season", "by_stock", "by_fromto", "by_stock_season"),
        selected = "single"
      )
    }
  })
  
  output$sigmaUI <- renderUI({
    if (!(input$age_re %in% c("iid", "ar1") || input$year_re %in% c("iid", "ar1"))) return(NULL)
    
    scope <- input$sigma_scope %||% "single"
    nK <- input$n_stocks
    nS <- input$n_seasons
    nR <- input$n_regions
    
    add_num <- function(id, lab) {
      sliderInput(id, lab, min = 0, max = 2, value = 0.5, step = 0.05)
    }
    
    inputs <- list()
    
    if (scope == "single") {
      inputs <- list(add_num("sig_global", "Sigma (all)"))
    } else if (scope == "by_season") {
      for (s in 1:nS) {
        inputs <- append(inputs, list(add_num(paste0("sig_seas_", s), paste("Sigma (Season", s, ")"))))
      }
    } else if (scope == "by_stock") {
      for (k in 1:nK) {
        inputs <- append(inputs, list(add_num(paste0("sig_stock_", k), paste("Sigma (Stock", k, ")"))))
      }
    } else if (scope == "by_fromto") {
      for (from in 1:nR) for (to in setdiff(1:nR, from)) {
        inputs <- append(inputs, list(add_num(paste0("sig_r", from, "_to", to), paste0("Sigma (", from, "→", to, ")"))))
      }
    } else if (scope == "by_stock_season") {
      for (k in 1:nK) for (s in 1:nS) {
        inputs <- append(inputs, list(add_num(paste0("sig_stock_", k, "_seas_", s), paste0("Sigma (Stock ", k, ", Season ", s, ")"))))
      }
    }
    
    do.call(tagList, inputs)
  })
  
  output$rhoAgeScopeUI <- renderUI({
    if (input$age_re == "ar1") {
      selectInput(
        "rhoA_scope", "Rho (age) scope:",
        choices = c("single", "by_season", "by_stock", "by_fromto", "by_stock_season"),
        selected = "single"
      )
    }
  })
  
  output$rhoAgeUI <- renderUI({
    if (input$age_re != "ar1") return(NULL)
    
    scope <- input$rhoA_scope %||% "single"
    nK <- input$n_stocks
    nS <- input$n_seasons
    nR <- input$n_regions
    
    add_num <- function(id, lab) {
      sliderInput(id, lab, min = -0.99, max = 0.99, value = 0.5, step = 0.01)
    }
    
    inputs <- list()
    
    if (scope == "single") {
      inputs <- list(add_num("rhoA_global", "rho_age (all)"))
    } else if (scope == "by_season") {
      for (s in 1:nS) {
        inputs <- append(inputs, list(add_num(paste0("rhoA_seas_", s), paste("rho_age (Season", s, ")"))))
      }
    } else if (scope == "by_stock") {
      for (k in 1:nK) {
        inputs <- append(inputs, list(add_num(paste0("rhoA_stock_", k), paste("rho_age (Stock", k, ")"))))
      }
    } else if (scope == "by_fromto") {
      for (from in 1:nR) for (to in setdiff(1:nR, from)) {
        inputs <- append(inputs, list(add_num(paste0("rhoA_r", from, "_to", to), paste0("rho_age (", from, "→", to, ")"))))
      }
    } else if (scope == "by_stock_season") {
      for (k in 1:nK) for (s in 1:nS) {
        inputs <- append(inputs, list(add_num(paste0("rhoA_stock_", k, "_seas_", s), paste0("rho_age (Stock ", k, ", Season ", s, ")"))))
      }
    }
    
    do.call(tagList, inputs)
  })
  
  output$rhoYearScopeUI <- renderUI({
    if (input$year_re == "ar1") {
      selectInput(
        "rhoY_scope", "Rho (year) scope:",
        choices = c("single", "by_season", "by_stock", "by_fromto", "by_stock_season"),
        selected = "single"
      )
    }
  })
  
  output$rhoYearUI <- renderUI({
    if (input$year_re != "ar1") return(NULL)
    
    scope <- input$rhoY_scope %||% "single"
    nK <- input$n_stocks
    nS <- input$n_seasons
    nR <- input$n_regions
    
    add_num <- function(id, lab) {
      sliderInput(id, lab, min = -0.99, max = 0.99, value = 0.5, step = 0.01)
    }
    
    inputs <- list()
    
    if (scope == "single") {
      inputs <- list(add_num("rhoY_global", "rho_year (all)"))
    } else if (scope == "by_season") {
      for (s in 1:nS) {
        inputs <- append(inputs, list(add_num(paste0("rhoY_seas_", s), paste("rho_year (Season", s, ")"))))
      }
    } else if (scope == "by_stock") {
      for (k in 1:nK) {
        inputs <- append(inputs, list(add_num(paste0("rhoY_stock_", k), paste("rho_year (Stock", k, ")"))))
      }
    } else if (scope == "by_fromto") {
      for (from in 1:nR) for (to in setdiff(1:nR, from)) {
        inputs <- append(inputs, list(add_num(paste0("rhoY_r", from, "_to", to), paste0("rho_year (", from, "→", to, ")"))))
      }
    } else if (scope == "by_stock_season") {
      for (k in 1:nK) for (s in 1:nS) {
        inputs <- append(inputs, list(add_num(paste0("rhoY_stock_", k, "_seas_", s), paste0("rho_year (Stock ", k, ", Season ", s, ")"))))
      }
    }
    
    do.call(tagList, inputs)
  })
  
  output$movementInputs <- renderUI({
    nR <- input$n_regions
    nS <- input$n_seasons
    nK <- input$n_stocks
    mm <- input$mean_model
    
    if (mm == "none") {
      return(tags$em("mean_model = 'none': no movement parameters to enter."))
    }
    
    asSlider <- identical(input$rate_entry_mode, "Sliders")
    
    newNum <- function(id, label) {
      if (asSlider) {
        sliderInput(id, label, min = 0, max = 1, value = 0, step = 0.01, width = "100%")
      } else {
        numericInput(id, label, value = 0, min = 0, max = 1, step = 0.01, width = "100%")
      }
    }
    
    inputs <- list()
    
    for (from in 1:nR) for (to in setdiff(1:nR, from)) {
      base <- paste0("From region ", from, " to ", to, ":")
      
      if (mm == "constant") {
        inputs <- append(inputs, list(newNum(paste0("mv_r", from, "_to", to), base)))
      } else if (mm == "season") {
        for (seas in 1:nS) {
          inputs <- append(inputs, list(newNum(paste0("mv_r", from, "_to", to, "_seas", seas), paste0(base, " (Season ", seas, ")"))))
        }
      } else if (mm == "stock_constant") {
        for (st in 1:nK) {
          inputs <- append(inputs, list(newNum(paste0("mv_r", from, "_to", to, "_stk", st), paste0(base, " (Stock ", st, ")"))))
        }
      } else if (mm == "stock_season") {
        for (st in 1:nK) for (seas in 1:nS) {
          inputs <- append(inputs, list(newNum(paste0("mv_r", from, "_to", to, "_stk", st, "_seas", seas), paste0(base, " (Stock ", st, ", Season ", seas, ")"))))
        }
      }
    }
    
    do.call(tagList, inputs)
  })
  
  output$seasonDiagram <- renderPlot({
    ns <- input$n_seasons
    sp <- spawn_season()
    
    op <- par(mar = c(2.5, 2, 3, 2), bg = "white", xpd = NA)
    on.exit(par(op))
    
    plot(
      NA, NA,
      xlim = c(0.5, ns + 0.5),
      ylim = c(0, 2.2),
      xaxt = "n", yaxt = "n",
      xlab = "", ylab = "",
      bty = "n"
    )
    
    fill_cols <- ifelse(1:ns == sp, "#7FCDBB", "#F3E39A")
    border_cols <- ifelse(1:ns == sp, "#2E8B57", "#C9A227")
    
    legend(
      "top",
      legend = c("Spawning season", "Off-spawning season"),
      fill = c("#7FCDBB", "#F3E39A"),
      border = c("#2E8B57", "#C9A227"),
      bty = "n",
      cex = 1.1,
      x.intersp = 0.6,
      y.intersp = 1.2,
      inset = c(0, -0.02)
    )
    
    box_half_width <- if (ns <= 6) 0.35 else if (ns <= 10) 0.28 else 0.22
    
    for (i in 1:ns) {
      rect(
        xleft   = i - box_half_width + 0.02,
        ybottom = 0.72 - 0.02,
        xright  = i + box_half_width + 0.02,
        ytop    = 1.18 - 0.02,
        col = rgb(0, 0, 0, 0.08),
        border = NA
      )
      
      rect(
        xleft   = i - box_half_width,
        ybottom = 0.72,
        xright  = i + box_half_width,
        ytop    = 1.18,
        col = fill_cols[i],
        border = border_cols[i],
        lwd = 2
      )
      
      text(
        x = i,
        y = 0.52,
        labels = i,
        cex = if (ns <= 8) 1.1 else if (ns <= 12) 1.0 else 0.9,
        col = "#4A4A4A",
        font = 2
      )
    }
    
    title(
      main = "Season Structure",
      cex.main = 1.8,
      font.main = 2,
      col.main = "#333333"
    )
  }, res = 120)
  
  observeEvent(input$demoButton, {
    updateNumericInput(session, "n_stocks", value = 3)
    updateNumericInput(session, "n_regions", value = 3)
    updateNumericInput(session, "n_seasons", value = 4)
    updateSelectInput(session, "mean_model", selected = "season")
    updateRadioButtons(session, "rate_entry_mode", selected = "Sliders")
    
    isolate({
      for (s in 1:input$n_seasons) {
        updateSliderInput(session, paste0("season_frac_", s), value = round(1 / input$n_seasons, 2))
      }
      
      for (from in 1:input$n_regions) for (to in setdiff(1:input$n_regions, from)) for (seas in 1:input$n_seasons) {
        id <- paste0("mv_r", from, "_to", to, "_seas", seas)
        if (!is.null(session$input[[id]])) {
          updateSliderInput(session, id, value = round(runif(1, 0, 0.25), 2))
        }
      }
    })
  })
  
  observeEvent(input$generate, {
    nK <- input$n_stocks
    nS <- input$n_seasons
    nR <- input$n_regions
    mm <- input$mean_model
    
    if (as.integer(input$metapop) == 0 && (!length(input$mustMoveSeasons))) {
      output$warning <- renderUI({
        div(class = "big-warning", "Natal homing selected: please specify at least one 'must move' season.")
      })
      return(invisible(NULL))
    } else {
      output$warning <- renderUI(NULL)
    }
    
    sfrac <- season_frac_vec()
    ssum <- sum(sfrac)
    
    if (abs(ssum - 1) > 1e-6) {
      sfrac <- sfrac / ssum
      output$warning <- renderUI({
        div(class = "big-warning", sprintf("Season fractions summed to %.2f and were normalized to 1.", ssum))
      })
    }
    
    mean_vals <- array(0, dim = c(nK, nS, nR, nR - 1))
    can_move  <- array(1, dim = c(nK, nS, nR, nR))
    must_move <- array(0, dim = c(nK, nS, nR))
    
    sm_vec <- rep(FALSE, nK)
    if (length(input$stock_move)) {
      sm_vec[as.integer(input$stock_move)] <- TRUE
    }
    
    sep_vec <- rep(FALSE, nK)
    if (length(input$separable)) {
      sep_vec[as.integer(input$separable)] <- TRUE
    }
    
    allowed_seasons <- if (length(input$canMoveSeasons)) as.integer(input$canMoveSeasons) else integer(0)
    
    for (k in 1:nK) for (t in 1:nS) {
      if (!(t %in% allowed_seasons) || !sm_vec[k]) {
        can_move[k, t, , ] <- 0
      }
    }
    
    if (as.integer(input$metapop) == 0) {
      hr <- home_region()
      for (k in 1:nK) for (t in as.integer(input$mustMoveSeasons)) for (r in 1:nR) {
        if (r != hr[k]) must_move[k, t, r] <- 1
      }
    } else {
      must_move[] <- 0
    }
    
    mean_model <- matrix(mm, nR, nR - 1)
    
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
            id <- paste0("mv_r", from, "_to", to)
            val <- get_val(id)
            mean_vals[, , from, k_idx] <- val
          } else if (mm == "season") {
            for (t in 1:nS) {
              id <- paste0("mv_r", from, "_to", to, "_seas", t)
              mean_vals[, t, from, k_idx] <- get_val(id)
            }
          } else if (mm == "stock_constant") {
            for (k in 1:nK) {
              id <- paste0("mv_r", from, "_to", to, "_stk", k)
              mean_vals[k, , from, k_idx] <- get_val(id)
            }
          } else if (mm == "stock_season") {
            for (k in 1:nK) for (t in 1:nS) {
              id <- paste0("mv_r", from, "_to", to, "_stk", k, "_seas", t)
              mean_vals[k, t, from, k_idx] <- get_val(id)
            }
          }
        }
      }
    } else {
      can_move[] <- 0
    }
    
    warn_msgs <- c()
    for (k in 1:nK) for (t in 1:nS) for (from in 1:nR) {
      sm <- sum(mean_vals[k, t, from, ], na.rm = TRUE)
      if (sm > 1 + 1e-8) {
        warn_msgs <- c(
          warn_msgs,
          sprintf("Stock %d, Season %d, Region %d rates sum to %.2f (>1).", k, t, from, sm)
        )
      }
    }
    
    if (length(warn_msgs)) {
      output$warning <- renderUI({
        HTML(paste0("<div class='big-warning'>", paste(warn_msgs, collapse = "<br>"), "</div>"))
      })
    }
    
    age_re  <- matrix(input$age_re,  nR, nR - 1)
    year_re <- matrix(input$year_re, nR, nR - 1)
    
    sigma_vals <- NULL
    if (input$age_re %in% c("iid", "ar1") || input$year_re %in% c("iid", "ar1")) {
      sigma_vals <- array(0, dim = c(nK, nS, nR, nR - 1))
      scope <- input$sigma_scope %||% "single"
      
      fill_sigma <- function(val, k = NULL, t = NULL, from = NULL, kidx = NULL) {
        if (is.null(k)) sigma_vals[,] <- val
        else if (is.null(t)) sigma_vals[k, , , ] <- val
        else if (is.null(from)) sigma_vals[k, t, , ] <- val
        else if (is.null(kidx)) sigma_vals[k, t, from, ] <- val
        else sigma_vals[k, t, from, kidx] <- val
      }
      
      if (scope == "single") {
        fill_sigma(as.numeric(input[["sig_global"]] %||% 0.5))
      } else if (scope == "by_season") {
        for (t in 1:nS) fill_sigma(as.numeric(input[[paste0("sig_seas_", t)]] %||% 0.5), t = t)
      } else if (scope == "by_stock") {
        for (k in 1:nK) fill_sigma(as.numeric(input[[paste0("sig_stock_", k)]] %||% 0.5), k = k)
      } else if (scope == "by_fromto") {
        for (from in 1:nR) {
          kidx <- 0
          for (to in 1:nR) if (to != from) {
            kidx <- kidx + 1
            fill_sigma(as.numeric(input[[paste0("sig_r", from, "_to", to)]] %||% 0.5), from = from, kidx = kidx)
          }
        }
      } else if (scope == "by_stock_season") {
        for (k in 1:nK) for (t in 1:nS) {
          fill_sigma(as.numeric(input[[paste0("sig_stock_", k, "_seas_", t)]] %||% 0.5), k = k, t = t)
        }
      }
    }
    
    cor_vals <- array(0, dim = c(nK, nS, nR, nR - 1, 2))
    
    if (input$age_re == "ar1") {
      scope <- input$rhoA_scope %||% "single"
      
      fill_rhoA <- function(val, k = NULL, t = NULL, from = NULL, kidx = NULL) {
        if (is.null(k)) cor_vals[,,,,1] <- val
        else if (is.null(t)) cor_vals[k,,, ,1] <- val
        else if (is.null(from)) cor_vals[k,t,, ,1] <- val
        else if (is.null(kidx)) cor_vals[k,t,from, ,1] <- val
        else cor_vals[k,t,from,kidx,1] <- val
      }
      
      if (scope == "single") {
        fill_rhoA(as.numeric(input[["rhoA_global"]] %||% 0.5))
      } else if (scope == "by_season") {
        for (t in 1:nS) fill_rhoA(as.numeric(input[[paste0("rhoA_seas_", t)]] %||% 0.5), t = t)
      } else if (scope == "by_stock") {
        for (k in 1:nK) fill_rhoA(as.numeric(input[[paste0("rhoA_stock_", k)]] %||% 0.5), k = k)
      } else if (scope == "by_fromto") {
        for (from in 1:nR) {
          kidx <- 0
          for (to in 1:nR) if (to != from) {
            kidx <- kidx + 1
            fill_rhoA(as.numeric(input[[paste0("rhoA_r", from, "_to", to)]] %||% 0.5), from = from, kidx = kidx)
          }
        }
      } else if (scope == "by_stock_season") {
        for (k in 1:nK) for (t in 1:nS) {
          fill_rhoA(as.numeric(input[[paste0("rhoA_stock_", k, "_seas_", t)]] %||% 0.5), k = k, t = t)
        }
      }
    }
    
    if (input$year_re == "ar1") {
      scope <- input$rhoY_scope %||% "single"
      
      fill_rhoY <- function(val, k = NULL, t = NULL, from = NULL, kidx = NULL) {
        if (is.null(k)) cor_vals[,,,,2] <- val
        else if (is.null(t)) cor_vals[k,,, ,2] <- val
        else if (is.null(from)) cor_vals[k,t,, ,2] <- val
        else if (is.null(kidx)) cor_vals[k,t,from, ,2] <- val
        else cor_vals[k,t,from,kidx,2] <- val
      }
      
      if (scope == "single") {
        fill_rhoY(as.numeric(input[["rhoY_global"]] %||% 0.5))
      } else if (scope == "by_season") {
        for (t in 1:nS) fill_rhoY(as.numeric(input[[paste0("rhoY_seas_", t)]] %||% 0.5), t = t)
      } else if (scope == "by_stock") {
        for (k in 1:nK) fill_rhoY(as.numeric(input[[paste0("rhoY_stock_", k)]] %||% 0.5), k = k)
      } else if (scope == "by_fromto") {
        for (from in 1:nR) {
          kidx <- 0
          for (to in 1:nR) if (to != from) {
            kidx <- kidx + 1
            fill_rhoY(as.numeric(input[[paste0("rhoY_r", from, "_to", to)]] %||% 0.5), from = from, kidx = kidx)
          }
        }
      } else if (scope == "by_stock_season") {
        for (k in 1:nK) for (t in 1:nS) {
          fill_rhoY(as.numeric(input[[paste0("rhoY_stock_", k, "_seas_", t)]] %||% 0.5), k = k, t = t)
        }
      }
    }
    
    use_prior <- array(0, dim = c(nK, nS, nR, nR - 1))
    prior_sigma <- NULL
    
    if (isTRUE(input$use_prior)) {
      use_prior[, 1, , ] <- 1
      if (!is.null(input$priorSigma)) {
        prior_sigma <- array(input$priorSigma, dim = c(nK, nS, nR, nR - 1))
      }
    }
    
    out <- list(
      stock_move  = as.logical(sm_vec),
      separable   = as.logical(sep_vec),
      must_move   = must_move,
      can_move    = can_move,
      mean_vals   = mean_vals,
      mean_model  = mean_model,
      year_re     = year_re,
      age_re      = age_re,
      cor_vals    = cor_vals,
      sigma_vals  = sigma_vals,
      use_prior   = use_prior,
      prior_sigma = prior_sigma,
      season_frac = sfrac
    )
    
    output_list_reactive(out)
    output$outputList <- renderPrint({ out })
    
    g <- create_graph() %>%
      add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") %>%
      add_global_graph_attrs(attr = "rankdir", value = "LR", attr_type = "graph")
    
    for (r in 1:nR) {
      g <- g %>% add_node(label = paste("Region", r))
    }
    
    for (from in 1:nR) {
      k_idx <- 0
      for (to in 1:nR) if (to != from) {
        k_idx <- k_idx + 1
        if (any(mean_vals[, , from, k_idx] > 0)) {
          g <- g %>% add_edge(from = from, to = to)
        }
      }
    }
    
    output$movementDiagram <- renderGrViz({
      grViz(DiagrammeR::generate_dot(g))
    })
    
    diagram_reactive(g)
  })
  
  output$downloadMovementMatrix <- downloadHandler(
    filename = function() "movement_list.rds",
    content  = function(file) saveRDS(output_list_reactive(), file)
  )
  
  output$downloadMovementDiagram <- downloadHandler(
    filename = function() "movement_diagram.png",
    content  = function(file) {
      export_svg(grViz(DiagrammeR::generate_dot(diagram_reactive()))) |>
        charToRaw() |>
        rsvg_png(file)
    }
  )
  
  observeEvent(input$restartButton, {
    session$reload()
  })
  
  observeEvent(input$exitButton, {
    stopApp()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
