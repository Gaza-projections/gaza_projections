#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ------------ R SCRIPT TO FIT MODEL OF WEIGHT LOSS VS INTAKE  ------------- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................  
### Visualising patterns in the data
#...............................................................................

  #...................................      
  ## Visualise distribution of outcome variable
  ggplot(aes(x = percent_wt_loss_mth, colour = type), data = df_wt) +
    geom_density() +
    theme_bw()
    
  #...................................      
  ## Visualise correlation of outcome variable with exposure
  ggplot(aes(y = percent_wt_loss_mth, x = intake_reduction, colour = type), 
    data = df_wt) +
    geom_point() +
    geom_smooth() +
    theme_bw()
    
    
#...............................................................................  
### Evaluating different models of weight loss as a function of Kcal intake
#...............................................................................

  # #...................................      
  # ## Fit a linear regression model of weight loss
  #   
  #   # Fit model
  #   fit <- lm(formula = percent_wt_loss_mth_log ~ intake_reduction + I(intake_reduction^2) + 
  #     bmi_baseline:duration + age + bmi_baseline, 
  #     weights = wt, data = df_wt_m )
  #   summary(fit)
  #   
  #   # Evaluate performance within sample
  #   x <- exp(predict(fit, type = "response"))
  #   df_wt_m[, "pred"] <- x
  # 
  #   ggplot(data = df_wt_m, aes(y = pred, x = percent_wt_loss_mth, size = wt)) +
  #     geom_point(alpha = 0.5, colour = palette_cb[6], fill = palette_cb[6]) +
  #     theme_bw() +
  #     scale_y_continuous("predicted", limits = c(0.0, 0.03), labels = percent) +
  #     scale_x_continuous("observed", limits = c(0.0, 0.03), labels = percent) +
  #     geom_abline(intercept = 0, slope = 1, colour = palette_cb[7])
  #     
    
  # #...................................      
  # ## Fit a beta regression model of weight loss
  #       
  #   # Fit model
  #   fit <- glmmTMB(percent_wt_loss ~ intake_reduction + 
  #     offset(log(duration)) + age + bmi_baseline:duration,
  #     weights = wt, data = df_wt_m, family = beta_family() )
  #   summary(fit)
  #   exp(coef(fit))
  # 
  #   # Evaluate performance within sample
  #   x <- predict(fit, type = "response")
  #   df_wt_m[, "pred"] <- x
  # 
  #   ggplot(data = df_wt_m, aes(y = pred, x = percent_wt_loss, size = wt)) +
  #     geom_point(alpha = 0.5, colour = palette_cb[6], fill = palette_cb[6]) +
  #     theme_bw() +
  #     scale_y_continuous("predicted", limits = c(0.05, 0.25), labels = percent) +
  #     scale_x_continuous("observed", limits = c(0.05, 0.25), labels = percent) +
  #     geom_abline(intercept = 0, slope = 1, colour = palette_cb[7])
  # 


  #...................................      
  ## Fit a generalised additive regression model of weight loss
    
    # Fit model
    fit <- mgcv::gam(formula = percent_wt_loss_mth_log ~ 
      s(intake_reduction, bs = "ps") + 
      bmi_baseline:duration + s(age) + s(bmi_baseline), 
      weights = wt, data = df_wt_m )
    summary(fit)
    
    # Evaluate performance within sample
    x <- exp(predict(fit, type = "response"))
    df_wt_m[, "pred"] <- x
  
    ggplot(data = df_wt_m, aes(y = pred, x = percent_wt_loss_mth, size = wt)) +
      geom_point(alpha = 0.5, colour = palette_cb[6], fill = palette_cb[6]) +
      theme_bw() +
      scale_y_continuous("predicted", limits = c(0.0, 0.03), labels = percent) +
      scale_x_continuous("observed", limits = c(0.0, 0.03), labels = percent) +
      geom_abline(intercept = 0, slope = 1, colour = palette_cb[7]) +
      theme(legend.position = "none")
      
    # Save model
    write_rds(fit, paste(dir_path, "outputs/", "wt_loss_model.rds", sep=""))      
            
#...............................................................................  
### Evaluating model predictive performance on LOOCV
#...............................................................................

  #...................................      
  ## Run LOOCV
  for (i in 1:nrow(df_wt_m)) {
    
    # Generate data without fold
    df_wt_i <- df_wt_m[-i, ]
    
    # Update model
    fit_i <- update(fit, data = df_wt_i)
    
    # Predict on fold and store the prediction
    df_wt_m[i, "pred_cv"] <- exp(predict(fit_i, newdata = df_wt_m[i, ], 
      type = "response"))
    
    # Predict on fold and store the standard error
    df_wt_m[i, "pred_cv_se"] <- predict(fit_i, newdata = df_wt_m[i, ], 
      se.fit = TRUE)[2]

  }

    # Compute upper and lower confidence intervals of LOOCV predictions
    df_wt_m$pred_cv_uci <- exp(log(df_wt_m$pred_cv) + 1.96 * df_wt_m$pred_cv_se)
    df_wt_m$pred_cv_lci <- exp(log(df_wt_m$pred_cv) - 1.96 * df_wt_m$pred_cv_se)
    
  #...................................      
  ## Visualise performance
    
    # scatter plot of predictions versus observations
    plot1 <- ggplot(data = df_wt_m, aes(y = pred_cv, x = percent_wt_loss_mth, size = wt)) +
      geom_point(alpha = 0.7, colour = palette_cb[6], fill = palette_cb[6],
        stroke = 1.5) +
      theme_bw() +
      scale_y_continuous("predicted monthly weight loss - LOOCV", 
        limits = c(0, 0.035), labels = percent) +
      scale_x_continuous("observed monthly weight loss", limits = c(0, 0.035), 
        labels = percent) +
      geom_abline(intercept = 0, slope = 1, colour = palette_cb[12], 
        alpha = 0.7, linewidth = 1.5) +
      theme(legend.position = "none")
    
    ggsave(paste(dir_path, "outputs/", "wt_loss_model_loocv1.png", sep=""),
      dpi = "print", units = "cm", height = 13, width = 13)  
    
    # dumbbell plot of predictions versus observations
    plot2 <- ggplot(data = df_wt_m, aes(y = reorder(authors, percent_wt_loss_mth)) ) +
      geom_segment(aes(x = pred_cv_lci, xend = pred_cv_uci,
        yend = authors), colour = palette_cb[6], linewidth = 2, alpha = 0.3) +
      geom_point(aes(x = pred_cv, y = authors), colour = palette_cb[6],
        size = 3, shape = 21, fill = palette_cb[6], alpha  = 0.5) +
      geom_point(aes(x = pred_cv_lci, y = authors), colour = palette_cb[6],
        size = 3, shape = 21, fill = "white", alpha  = 0.5) +
      geom_point(aes(x = pred_cv_uci, y = authors), colour = palette_cb[6],
        size = 3, shape = 21, fill = "white", alpha  = 0.5) +
      geom_point(aes(x = percent_wt_loss_mth, y = authors), shape = 22, size= 2,
        colour = palette_cb[9], fill = palette_cb[12], stroke = 2, 
        alpha = 0.5) +
      theme_bw() +
      scale_x_continuous("monthly weight loss", labels = percent,
        breaks = seq(0, 0.08, by = 0.01)) +
      scale_y_discrete("study")
    
    ggsave(paste(dir_path, "outputs/", "wt_loss_model_loocv2.png", sep=""),
      dpi = "print", units = "cm", height = 18, width = 12)  
    
  #...................................      
  ## Quantify relative bias (weighted for study size)
  weighted.mean((df_wt_m$pred_cv - df_wt_m$percent_wt_loss_mth) / 
      df_wt_m$percent_wt_loss_mth, df_wt_m$wt )
    # +17% relative bias
    

#...............................................................................  
### Observing dose-response effects
#...............................................................................

  #...................................      
  ## Caloric intake reduction by BMI level
    
    # Initialise output
    out <- expand.grid(reduction = seq(500, 1500, 100), bmi= seq(20, 45, 5))
    out[,  c("wt_loss_mth", "wt_loss_mth_lci", "wt_loss_mth_uci")] <- NA
    
    # Loop through each potential value of intake reduction
    for (i in 1:nrow(out) ) {
      
      # Change intake reduction in data
      df_wt_sim <- df_wt_m
      df_wt_sim$intake_reduction <- out[i, "reduction"]
      df_wt_sim$bmi_baseline <- out[i, "bmi"]
      
      # Predict weight loss and its 95% confidence interval, for each study
      x <- predict(fit, newdata = df_wt_sim, se.fit = TRUE)
      
      # Compute and store weighted mean predictions across all studies
      out[i, "wt_loss_mth"] <- weighted.mean(exp(x$fit), df_wt_sim$wt)
      out[i, "wt_loss_mth_lci"] <- weighted.mean(exp(x$fit - 1.96 * x$se.fit), 
        df_wt_sim$wt)
      out[i, "wt_loss_mth_uci"] <- weighted.mean(exp(x$fit + 1.96 * x$se.fit), 
        df_wt_sim$wt)
    }
  
    # Labels for plot facets
    out$bmi_level <- paste("BMI =", out$bmi, "Kg/m^2", sep = " ")
    
    # Plot dose-response relationship
    plot3 <- ggplot(data = out, aes(x = reduction, y = wt_loss_mth, fill = bmi_level,
      colour = bmi_level)) +
      geom_line() +
      geom_ribbon(aes(ymin = wt_loss_mth_lci, ymax = wt_loss_mth_uci),
        alpha = 0.3, linetype = "21") +
      theme_bw() +
      scale_y_continuous("monthly weight loss", labels = percent) +
      scale_x_continuous("caloric intake reduction (Kcal deficit)") +
      scale_fill_viridis_d() +
      scale_colour_viridis_d() +
      facet_wrap(. ~ bmi_level) +
      theme(legend.position = "none")
    
    ggsave(paste(dir_path, "outputs/", "wt_loss_dose_response.png", sep=""),
      dpi = "print", units = "cm", height = 12, width = 18)  

    
  #...................................      
  ## Caloric intake reduction by duration of reduced intake
    
    # Initialise output
    out <- expand.grid(duration = seq(1, 10, 1), bmi= seq(20, 45, 5))
    out[,  c("wt_loss_mth", "wt_loss_mth_lci", "wt_loss_mth_uci")] <- NA
    
    # Loop through each potential value of intake reduction
    for (i in 1:nrow(out) ) {
      
      # Change intake reduction in data
      df_wt_sim <- df_wt_m
      df_wt_sim$duration <- out[i, "duration"]
      df_wt_sim$bmi_baseline <- out[i, "bmi"]
      
      # Predict weight loss and its 95% confidence interval, for each study
      x <- predict(fit, newdata = df_wt_sim, se.fit = TRUE)
      
      # Compute and store weighted mean predictions across all studies
      out[i, "wt_loss_mth"] <- weighted.mean(exp(x$fit), df_wt_sim$wt)
      out[i, "wt_loss_mth_lci"] <- weighted.mean(exp(x$fit - 1.96 * x$se.fit), 
        df_wt_sim$wt)
      out[i, "wt_loss_mth_uci"] <- weighted.mean(exp(x$fit + 1.96 * x$se.fit), 
        df_wt_sim$wt)
    }
  
    # Labels for plot facets
    out$bmi_level <- paste("BMI =", out$bmi, "Kg/m^2", sep = " ")
    
    # Plot dose-response relationship
    plot4 <- ggplot(data = out, aes(x = duration, y = wt_loss_mth, fill = bmi_level,
      colour = bmi_level)) +
      geom_line() +
      geom_ribbon(aes(ymin = wt_loss_mth_lci, ymax = wt_loss_mth_uci),
        alpha = 0.3, linetype = "21") +
      theme_bw() +
      scale_y_continuous("monthly weight loss", labels = percent) +
      scale_x_continuous("months of exposure to caloric reduction") +
      scale_fill_viridis_d() +
      scale_colour_viridis_d() +
      facet_wrap(. ~ bmi_level) +
      theme(legend.position = "none")
    
    ggsave(paste(dir_path, "outputs/", "duration_dose_response.png", sep=""),
      dpi = "print", units = "cm", height = 12, width = 18)  
    
        
  #...................................      
  ## Combined plot for report
  ggarrange(ggarrange(plot1, plot2, ncol = 2, widths = c(1, 1), 
    labels = c("A", "B")), plot3, 
    heights = c(2, 2), nrow = 2, labels = c(NA, "C"))  
    
  ggsave(paste(dir_path, "outputs/", "wt_loss_combi.png", sep=""),
    dpi = "print", units = "cm", height = 25, width = 18)  
    

#...............................................................................  
### ENDS
#...............................................................................
     