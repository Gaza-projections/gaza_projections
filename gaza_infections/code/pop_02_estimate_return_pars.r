#...............................................................................
### +++++++++++ RECONSTRUCTING POPULATION DENOMINATORS IN SOMALIA ++++++++++ ###
#...............................................................................

#...............................................................................
## ------ R CODE TO ESTIMATE PARAMETERS FOR THE FLOW OF IDP RETURNEES ------- ##
#...............................................................................

                          # LSHTM, SIMAD University (August 2023)
                          # francesco.checchi@lshtm_ac.uk 



#...............................................................................  
### Quantifying goodness of fit of various candidate parameter values
#...............................................................................

  #.........................................
  ## Prepare simulations

    # Draw a Latin hypercube sample of the parameters
    x <- improvedLHS(runs_est, length(strata) * 2, dup = 4)
    # x <- optimumLHS(runs_est, length(strata) * 2, 
      # maxSweeps = 4, eps = 0.01)
    pars <- matrix(nrow = nrow(x), ncol = ncol(x))
    pars[, 1] <- qunif(x[, 1], min = prop_stay_range[1], 
      max = prop_stay_range[2])
    pars[, 2] <- qunif(x[, 2], min = ret_time_range[1], 
      max = ret_time_range[2])
    pars[, 3] <- qunif(x[, 3], min = prop_stay_range[1], 
      max = prop_stay_range[2])
    pars[, 4] <- qunif(x[, 4], min = ret_time_range[1], 
      max = ret_time_range[2])
    pars[, 5] <- qunif(x[, 5], min = prop_stay_range[1], 
      max = prop_stay_range[2])
    pars[, 6] <- qunif(x[, 6], min = ret_time_range[1], 
      max = ret_time_range[2])
    
    # Initialise output
    out <- expand.grid(unique(pop_sources$worksheet), 1:runs_est, strata)
    colnames(out) <- c("pop_source", "run", "stratum")
    out[, c("prop_stay", "ret_time", "rmse", "ll", 
      "implausible_pop", "implausible_prop_idp")] <- NA
    out <- out[order(out$pop_source, out$run, out$stratum), ]

  #.........................................
  ## Run simulations and compute RMSE and log likelihood, for each pop. source
  for (i in unique(pop_sources$worksheet)) {
    
    # Progress statement
    print(paste("now working on population source...", i))
    
    # Run simulations
    for (j in 1:nrow(pars)) {
      
      # Progress so far
      print(paste("run", j, "of", nrow(pars)))
  
      # Run population reconstruction
      pars_i <- pars[j, ]
      pop_out <- f_pop(which_source_f = i)
      
      # Compute RMSE and log-likelihood
      out[which(out$pop_source == i & out$run == j), c("stratum", "prop_stay",
        "ret_time", "rmse", "ll", "implausible_pop", "implausible_prop_idp")] <- 
        f_gof()[, c("stratum", "prop_stay", "ret_time", "rmse", "ll", 
          "implausible_pop", "implausible_prop_idp")]
    }
  }

    # Save output
    write.csv(out, paste(dir_path, "output/som_pop_out_par_est_runs.csv", 
      sep = ""), row.names = FALSE)

    
#...............................................................................  
### Visualising and managing the output, and computing parameter best estimates
#...............................................................................
        
  #.........................................
  ## Visualise output
    
    # Read simulation output if starting from here
    if (! exists("out")) {
      out <- read.csv(paste(dir_path, "output/som_pop_out_par_est_runs.csv", 
        sep = ""))}
    
    # Check that simulations have evenly sampled across parameter space
    ggplot(data = out, aes(x = prop_stay)) + 
      geom_histogram(alpha = 0.5, fill = palette_cb[4]) + 
      scale_x_continuous("proportion of IDPs who never return") +
      scale_y_continuous("frequency of random values") +
      theme_bw()
    ggplot(data = out, aes(x = ret_time)) + 
      geom_histogram(alpha = 0.5, fill = palette_cb[6]) + 
      scale_x_continuous("mean time (months) to return") +
      scale_y_continuous("frequency of random values") +
      theme_bw()
    
    # Check whether implausible output correlates with certain pop'n sources
    ggplot(data = out, aes(y = implausible_pop, x = pop_source)) +
      geom_boxplot(colour = palette_cb[6]) +
      theme_bw()
    ggplot(data = out, aes(y = implausible_prop_idp, x = pop_source)) +
      geom_boxplot(colour = palette_cb[4]) +
      theme_bw()

    # Check whether implausible outputs correlate with poor fit
    ggplot(data = out, aes(y = ll, x = implausible_pop)) +
      geom_point(colour = palette_cb[6], alpha = 0.5) +
      geom_smooth(method = "gam") +
      theme_bw()
    ggplot(data = out, aes(y = ll, x = implausible_prop_idp)) +
      geom_point(colour = palette_cb[4], alpha = 0.5) +
      geom_smooth(method = "gam") +
      theme_bw()
      
    # Which parameter values are mostly associated with implausible output?
      # negative population
      plot1 <- ggplot(data = out, aes(x = prop_stay, y = ret_time, 
        colour = implausible_pop)) +
        geom_point(alpha = 0.5) +
        scale_x_continuous("proportion who never return") +
        scale_y_continuous("mean time to return (months)") +
        scale_colour_paletteer_c(
          "ggthemes::Red-Green-Gold Diverging", direction = -1) +
        theme_bw() +
        labs(colour = "proportion of district-month predictions") +
        theme(legend.position = "top") +
        facet_grid(stratum ~ pop_source, 
          labeller = labeller(pop_source = pop_labels))
      ggsave(paste(dir_path, "/output/som_out_pop_implausible_pop_runs.png", 
        sep =""), dpi = "print", units = "cm", height = 20, width = 30)

      # proportion of IDPs > 100%
      plot2 <- ggplot(data = out, aes(x = prop_stay, y = ret_time, 
        colour = implausible_prop_idp)) +
        geom_point(alpha = 0.5) +
        scale_x_continuous("proportion who never return") +
        scale_y_continuous("mean time to return (months)") +
        scale_colour_paletteer_c(
          "ggthemes::Red-Green-Gold Diverging", direction = -1) +
        theme_bw() +
        labs(colour = "proportion of district-month predictions") +
        theme(legend.position = "top") +
        facet_grid(stratum ~ pop_source, 
          labeller = labeller(pop_source = pop_labels))
      ggsave(paste(dir_path,"/output/som_out_pop_implausible_prop_idp_runs.png", 
        sep =""), dpi = "print", units = "cm", height = 20, width = 30)
      
      # combined plot
      plot <- ggarrange(plot1, plot2, nrow = 2, hjust = -0.1, labels = 
        c("Negative population", "IDPs < 0% or > 100% of total population") )
      ggsave(paste(dir_path,"/output/som_out_pop_implausible_runs_combi.png", 
        sep =""), dpi = "print", units = "cm", height = 40, width = 30)
      
      
  #.........................................
  ## Manage output

    # Change log-likelihood to negative log-likelihood
    out$negll <- - out$ll

    # Create a penalised negative log-likelihood to account for implausibility
      # of estimates around the minima and maxima of both parameters
    
      # set barrier thresholds
      prop_stay_bar_lo <- 0.10
      ret_time_bar_lo <- 6
      prop_stay_bar_hi <- 0.75
      ret_time_bar_hi <- 75
      
      # apply penalty and compute penalised negative log-likelihood
      out$negll_pen <- apply(out[, c("prop_stay", "ret_time")], 1, f_pen) + 
        out$negll

    
  #.........................................
  ## Plot RMSE and likelihood profiles for each parameter and pop. source
  for (i in c("rmse", "negll", "negll_pen")) {

    # For each population source...
    for (j in pop_sources$worksheet) {
      
      # call function
      x <- f_prof(pop_source_f = j, metric_f = i)
      
      # save plot
      plot <- x$plot
      ggsave(paste(dir_path, "/output/som_out_pop_profs_", j, "_", i, 
      ".png", sep = ""), dpi = "print", units = "cm", height = 20, width = 30)
    }       
  }
    
      
  #.........................................
  ## Smooth likelihood surfaces for each stratum and population source
    # just penalised negative log likelihood from this point
  
    # Initialise smoothing output dataframe
    out_smooth <- c()
    
    # Set grid for smoothing points
    grid <- expand.grid(prop_stay_grid, ret_time_grid)
    colnames(grid) <- c("prop_stay", "ret_time")
    
    # For each stratum...
    for (i in strata) {
      #...and for each population source...
      for (j in pop_sources$worksheet) {
        
        # new grid
        out_smooth_ij <- grid
        
        # subset simulation output
        out_ij <- subset(out, stratum == i & pop_source == j)

        # select variables and remove non-finite likelihoods
        out_ij <- out_ij[, c("run", "prop_stay", "ret_time", "negll_pen")]
        out_ij <- subset(out_ij, is.finite(negll_pen))
        
        # if there are no data, skip to next loop
        if (nrow(out_ij) == 0) {next}
        
        # smooth surface using a generalised additive model with tensor products
        fit_smooth <- gam(negll_pen ~ te(prop_stay, ret_time), data = out_ij)
        out_smooth_ij$smooth <- predict(fit_smooth, newdata = out_smooth_ij)
        
        # compute normalised running sum probability for later random sampling
        out_smooth_ij$prob <- 1 / out_smooth_ij$smooth
        out_smooth_ij <- out_smooth_ij[order(out_smooth_ij$prop_stay,
          out_smooth_ij$ret_time), ]
        out_smooth_ij$prob_cum <- cumsum(out_smooth_ij$prob)
        out_smooth_ij$prob_cum <- out_smooth_ij$prob_cum / 
          max(out_smooth_ij$prob_cum)
        
        # add the stratum and population source
        out_smooth_ij$stratum <- i
        out_smooth_ij$pop_source <- j

        # append to output
        out_smooth <- rbind(out_smooth, out_smooth_ij)
      }  
    }

    # Save output
    write.csv(out_smooth, 
      paste(dir_path, "output/som_pop_out_smooth.csv", sep = ""), 
      row.names = FALSE)

    # Plot likelihood surfaces
    plot <- ggplot(data = out_smooth, aes(x = prop_stay, y = ret_time, 
      z = smooth)) + 
      geom_raster(aes(fill = smooth), alpha = 0.8) +
      geom_contour(bins = 20, colour = "grey20") +
      scale_y_continuous("mean time to return (months)") +
      scale_x_continuous("proportion who never return") +
      scale_fill_paletteer_c(
        "ggthemes::Red-Green-Gold Diverging", direction = 1) +
      theme_bw() +
      theme(legend.position = "top") +
      facet_grid(stratum ~ pop_source, 
        labeller = labeller(pop_source = pop_labels)) +
      labs(fill = "penalised negative log likelihood (smoothed)")
    ggsave(paste(dir_path, "/output/som_out_pop_smooth_surfs.png", sep =""),
      dpi = "print", units = "cm", height = 20, width = 30)

    
  #.........................................
  ## Compute best estimates of the unknown parameters
    
    # Initialise output
    out_est <- data.frame(pop_source = NULL, stratum = NULL, prop_stay = NULL,
      ret_time = NULL)
    
    # For each population source...
    for (i in pop_sources$worksheet) {
      
      # ... and for each stratum...
      for (j in strata) {
        
        # select smoothed likelihood surface
        out_smooth_ij <- subset(out_smooth, pop_source == i & stratum == j)
        
        # find minimum and read corresponding parameter values
        x <- which.min(out_smooth_ij$smooth)
        out_ij <- c(i, j, 
          as.vector(out_smooth_ij[x, c("prop_stay", "ret_time")]))
        names(out_ij) <- c("pop_source", "stratum", "prop_stay", "ret_time")
        
        # update output
        out_est <- rbind(out_est, out_ij)
      }
    }  

    # Save estimates as table
    pop_labs <- data.frame(pop_source = pop_sources$worksheet, 
      pop_lab = pop_sources$label)
    out_est <- merge(out_est, pop_labs, by = "pop_source", all.x = TRUE)
    out_est <- out_est[, c("pop_lab", "stratum", "prop_stay", "ret_time")]
    x <- flextable(out_est)
    x <- autofit(x)
    flextable::save_as_docx(x, 
      path = paste(dir_path, "/output/som_out_pop_par_est.docx", sep=""))


    
#...............................................................................
### ENDS
#...............................................................................
  
  