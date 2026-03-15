Get_F_catch <- function(om, year, catch, Finit = 0.1, maxF = 10){
  rep = om$rep
  naa = rep$NAA[1,1,year,]
  Maa = rep$MAA[1,1,year,]
  sel_tot = rbind(rep$FAA[,year,]/max(exp(rep$log_FAA_tot[year,]))) #matrix nfleets x n_ages
  waa = rbind(om$input$data$waa[om$input$data$waa_pointer_fleets, year,]) #matrix nfleets x n_ages
  nfleets <- length(om$input$data$waa_pointer_fleets)
  get_catch = function(log_F, naa, sel, waa, Maa){
    Faa = exp(log_F) * sel_tot
    Zaa = Maa + apply(Faa,2,sum)
    Catch = 0
    for(a  in 1:length(naa)) for(f in 1:nfleets) Catch = Catch + waa[f,a] * naa[a] * Faa[f,a] *(1 - exp(-Zaa[a]))/Zaa[a];
    return(Catch)
  }
  obj = function(log_F) (catch - get_catch(log_F, naa, sel_tot, waa, Maa))^2
  opt = try(nlminb(log(Finit), obj))
  if(!is.character(opt)) Fsolve = exp(opt$par)[1] else Fsolve = maxF
  if(Fsolve>10) Fsolve = maxF
  print(paste0("Fsolve: ", Fsolve))
  return(Fsolve)
}

Update_F <- function(om, year, catch){
  rep <- om$rep #generate the reported values given the parameters
  year_ind <- year #index corresponding to year
  Fsolve <- Get_F_catch(om, year, catch) #find the F for the catch advice
  
  #have to be careful if more than one fleet
  FAA <- rbind(rep$FAA[,year_ind,]) #n_fleets x n_ages
  age_ind <- om$env$data$which_F_age[year_ind] #which age is used by wham to define total "full F"
  old_max_F <- apply(FAA,2,sum)[age_ind] # n_ages
  # selAA[[om$env$data$selblock_pointer_fleets[year_ind]]][year_ind,] #this only works when there is a single fleet
  selAA <- FAA/old_max_F #sum(selAA[i,]) = 1
  new_FAA <- Fsolve * selAA #updated FAA
  F_fleet_y <- apply(new_FAA, 1, max) #full F for each fleet
  return(new_FAA)
}

check_om_feedback_F <- function(mod, Finit = 0.1, maxF = 10,
                                save_pdf = TRUE,
                                main.dir = getwd(),
                                sub.dir = "Report",
                                pdf_name = "F_summary_table.pdf") {
  library(gridExtra)
  library(grid)
  
  om <- mod$om
  catch.adv <- do.call(rbind, mod$catch_advice)
  n_feedback_years <- nrow(catch.adv)
  n_om_years <- nrow(om$rep$pred_catch)
  first_feedback_year <- n_om_years - n_feedback_years + 1
  
  results <- data.frame(
    Year = integer(),
    OM_Catch = numeric(),
    Catch_Advice = numeric(),
    Diff = numeric(),
    True_Fbar = numeric(),
    Est_Fbar = numeric()
  )
  
  for (i in 1:n_feedback_years) {
    om_year <- first_feedback_year + i - 1
    
    user.catch <- catch.adv[i, ]
    om.catch <- om$rep$pred_catch[om_year, ]
    
    F_est <- Get_F_catch(om, om_year, user.catch)
    FAA_est <- Update_F(om, om_year, user.catch)
    Fbar_est <- mean(FAA_est[, om$env$data$which_F_age[om_year]])
    Fbar_true <- om$rep$Fbar[om_year, 3]
    
    results <- rbind(results, data.frame(
      Year = om_year,
      OM_Catch = round(om.catch, 3),
      Catch_Advice = round(user.catch, 3),
      Diff = round(om.catch - user.catch, 4),
      True_Fbar = round(Fbar_true, 4),
      Est_Fbar = round(Fbar_est, 4)
    ))
  }
  
  cat("\n==== Summary Table ====\n")
  print(results, row.names = FALSE)
  
  if (save_pdf) {
    dir.create(file.path(main.dir, sub.dir), recursive = TRUE, showWarnings = FALSE)
    pdf_path <- file.path(main.dir, sub.dir, pdf_name)
    pdf(file = pdf_path, width = 8.5, height = 15)
    grid.newpage()
    grid.draw(tableGrob(results))
    dev.off()
    cat(paste0("Summary table saved to: ", pdf_path, "\n"))
  }
  
  return(results)
}


plot_all_time_series_per_realization <- function(mods, main.dir, sub.dir,
                                                 width = 10, height = 7, dpi = 300,
                                                 col.opt = "D", new_model_names = NULL,
                                                 pdf_name = "All_TS_by_realization.pdf",
                                                 realizations_to_plot = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grDevices)
  
  # --- Detect single vs multiple realizations ---
  is.nsim <- is.list(mods[[1]]) && !is.null(mods[[1]][[1]]$om)
  
  if (is.nsim) {
    n_realizations <- length(mods)
    Years <- mods[[1]][[1]]$om$years
    n_models <- length(mods[[1]])
  } else {
    n_realizations <- 1
    Years <- mods[[1]]$om$years
    n_models <- length(mods)
    mods <- list(mods)  # Wrap single realization as list for consistency
  }
  
  # Default to all realizations
  if (is.null(realizations_to_plot)) {
    realizations_to_plot <- seq_len(n_realizations)
  }
  
  # Safety check
  if (any(realizations_to_plot > n_realizations | realizations_to_plot < 1)) {
    stop("One or more values in realizations_to_plot are out of bounds.")
  }
  
  # Create output directory if needed
  dir.create(file.path(main.dir, sub.dir), recursive = TRUE, showWarnings = FALSE)
  
  # Open multi-page PDF
  pdf(file = file.path(main.dir, sub.dir, pdf_name), width = width, height = height)
  
  for (r in realizations_to_plot) {
    # === SSB ===
    df_ssb <- lapply(seq_len(n_models), function(m) {
      data.frame(
        SSB = mods[[r]][[m]]$om$rep$SSB,
        Model = paste0("Model", m),
        Year = Years
      )
    }) %>% bind_rows()
    
    if (!is.null(new_model_names)) {
      df_ssb$Model <- factor(df_ssb$Model,
                             levels = paste0("Model", seq_len(n_models)),
                             labels = new_model_names)
    }
    
    df_ssb <- pivot_longer(df_ssb, cols = starts_with("SSB"), names_to = "Label", values_to = "SSB")
    
    p_ssb <- ggplot(df_ssb, aes(x = Year, y = SSB, color = Model, group = Model)) +
      geom_line(size = 0.8) +
      scale_color_viridis_d(option = col.opt) +
      facet_grid(Label ~ ., scales = "free") +
      theme_bw() +
      ggtitle(paste0("SSB from realization ", r)) +
      ylab("SSB")
    
    print(p_ssb)
    
    # === Fbar (Global) ===
    n_fleets <- mods[[r]][[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[r]][[1]]$om$input$data$n_regions[1]
    index_range <- n_fleets + n_regions + 1  # Global Fbar
    
    df_fbar <- lapply(seq_len(n_models), function(m) {
      tmp <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
      tmp <- as.data.frame(tmp)
      names(tmp) <- "Global"
      tmp$Model <- paste0("Model", m)
      tmp$Year <- Years
      tmp
    }) %>% bind_rows()
    
    if (!is.null(new_model_names)) {
      df_fbar$Model <- factor(df_fbar$Model,
                              levels = paste0("Model", seq_len(n_models)),
                              labels = new_model_names)
    }
    
    df_fbar <- pivot_longer(df_fbar, cols = starts_with("Global"), names_to = "Label", values_to = "Fbar")
    
    p_fbar <- ggplot(df_fbar, aes(x = Year, y = Fbar, color = Model, group = Model)) +
      geom_line(size = 0.8) +
      scale_color_viridis_d(option = col.opt) +
      facet_grid(Label ~ ., scales = "free") +
      theme_bw() +
      ggtitle(paste0("Fbar from realization ", r)) +
      ylab("Fbar")
    
    print(p_fbar)
    
    # === Catch ===
    df_catch <- lapply(seq_len(n_models), function(m) {
      data.frame(
        Catch = mods[[r]][[m]]$om$rep$pred_catch,
        Model = paste0("Model", m),
        Year = Years
      )
    }) %>% bind_rows()
    
    if (!is.null(new_model_names)) {
      df_catch$Model <- factor(df_catch$Model,
                               levels = paste0("Model", seq_len(n_models)),
                               labels = new_model_names)
    }
    
    df_catch <- pivot_longer(df_catch, cols = starts_with("Catch"), names_to = "Label", values_to = "Catch")
    
    p_catch <- ggplot(df_catch, aes(x = Year, y = Catch, color = Model, group = Model)) +
      geom_line(size = 0.8) +
      scale_color_viridis_d(option = col.opt) +
      facet_grid(Label ~ ., scales = "free") +
      theme_bw() +
      ggtitle(paste0("Catch from realization ", r)) +
      ylab("Catch")
    
    print(p_catch)
  }
  
  dev.off()
}


plot_NAA_tile_by_realization <- function(mods, type = "OM", em_index = NULL,
                                         main.dir, sub.dir,
                                         pdf_name = "NAA_tile_by_realization.pdf",
                                         width = NULL, height = NULL,
                                         fontfam = "", realizations_to_plot = NULL,
                                         new_model_names = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grDevices)
  
  is.nsim <- is.list(mods[[1]]) && !is.null(mods[[1]][[1]]$om)
  
  if (!is.nsim) stop("mods must be a list of realizations, each with multiple models.")
  
  n_realizations <- length(mods)
  n_models <- length(mods[[1]])
  
  if (is.null(realizations_to_plot)) {
    realizations_to_plot <- seq_len(n_realizations)
  }
  
  dir.create(file.path(main.dir, sub.dir), recursive = TRUE, showWarnings = FALSE)
  
  if(is.null(width) && is.null(height)) {
    if(type == "OM") {
      width = 15
      height = 5
    } else {
      width = 10
      height = 10
    }
  }
  pdf(file = file.path(main.dir, sub.dir, pdf_name), width = width, height = height)
  
  for (r in realizations_to_plot) {
    df.plot.all <- data.frame()
    
    if (type == "OM") {
      mod <- mods[[r]][[1]]$om
      dat <- mod$env$data
      rep <- mod$rep
      years <- mod$years
      n_ages <- dat$n_ages
      ages.lab <- if (!is.null(mod$ages.lab)) mod$ages.lab else 1:n_ages
      years_full <- if (dat$n_years_proj > 0) mod$years_full else years
      
      for (s in 1:dat$n_stocks) for (rn in 1:dat$n_regions) {
        df.tmp <- as.data.frame(rep$NAA_devs[s, rn, , ])
        colnames(df.tmp) <- paste0("Age_", 1:n_ages)
        df.tmp <- cbind.data.frame(
          Realization = paste0("R", r),
          Stock = mod$input$stock_names[s],
          Region = mod$input$region_names[rn],
          Year = years_full,
          df.tmp
        )
        df.tmp <- df.tmp %>%
          pivot_longer(starts_with("Age"), names_to = "Age", names_prefix = "Age_", values_to = "Deviation") %>%
          mutate(Age = factor(as.integer(Age), labels = ages.lab))
        df.plot.all <- rbind(df.plot.all, df.tmp)
      }
      
      p <- ggplot(df.plot.all, aes(x = Year, y = Age, fill = Deviation)) +
        geom_tile() +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +
        facet_grid(Realization ~ Stock + Region, labeller = label_both) +
        theme_bw() +
        ggtitle(paste0("OM NAA_devs for realization ", r)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        viridis::scale_fill_viridis()
      
      print(p)
    }
    
    if (type == "EM") {
      cat("Processing EM plots...\n")
      
      em_list_first_model <- mods[[r]][[1]]$em_list
      n_ems <- length(em_list_first_model)
      
      for (x in seq_len(n_ems)) {
        cat("  Realization", r, "- Assessment", x, "\n")
        df.plot.all <- data.frame()
        
        for (m in 1:n_models) {
          em_list <- mods[[r]][[m]]$em_list
          em_input_list <- mods[[r]][[m]]$em_input
          
          rep <- em_list[[x]]
          years_full <- em_input_list[[x]]$years_full
          n_ages <- dim(rep$NAA)[4]
          ages.lab <- 1:n_ages
          
          for (s in 1:dim(rep$NAA)[1]) for (rn in 1:dim(rep$NAA)[2]) {
            df.tmp <- as.data.frame(rep$NAA_devs[s, rn, , ])
            colnames(df.tmp) <- paste0("Age_", 1:n_ages)
            df.tmp <- cbind.data.frame(
              Realization = paste0("R", r),
              Model = paste0("Model", m),
              Assessment = paste0("Assessment", x),
              Stock = paste0("stock_", s),
              Region = paste0("region_", rn),
              Year = rep(years_full, each = n_ages),
              df.tmp
            )
            df.tmp <- df.tmp %>%
              pivot_longer(starts_with("Age"), names_to = "Age", names_prefix = "Age_", values_to = "Deviation") %>%
              mutate(Age = factor(as.integer(Age), labels = ages.lab)) %>%
              mutate(FacetLabel = paste0("Model", m, "_", Stock, "_", Region))
            df.plot.all <- rbind(df.plot.all, df.tmp)
          }
        }
        
        if (!is.null(new_model_names)) {
          df.plot.all$Model <- factor(df.plot.all$Model,
                                      levels = paste0("Model", seq_len(n_models)),
                                      labels = new_model_names)
        }
        
        p <- ggplot(df.plot.all, aes(x = Year, y = Age, fill = Deviation)) +
          geom_tile() +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_discrete(expand = c(0, 0)) +
          facet_wrap(~ FacetLabel, ncol = 2) +
          theme_bw() +
          ggtitle(paste0("Realization R", r, ", Assessment ", x, "\nModels ", paste(unique(df.plot.all$Model), collapse = ", "))) +
          theme(plot.title = element_text(hjust = 0.5)) +
          viridis::scale_fill_viridis()
        
        print(p)
      }
    }
    
    
    
  }
  
  dev.off()
}
