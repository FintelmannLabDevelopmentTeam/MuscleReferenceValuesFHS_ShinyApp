library(gamlss)
library(tidyverse)

#Load data as models_anon
#List of 32 models stored as metric_sex_lvl (e.g., CSMA_Female_T5)
load("resources/models_anonymized_2022-12-10.RData")

choose_model <-
  function(metric = c("CSMA", "SMI", "SMRA", "SMG"),
           sex = c("Female", "Male"),
           lvl = c("T5", "T8", "T10", "L3")) {
    #' Extract correct model
    #'
    #' @param metric The muscle metric measured
    #' @param sex The participant sex
    #' @param lvl The vertebral level.
    #'
    #' @return the correct LMS model
    
    model_name <- paste(metric, sex, lvl, sep = "_")
    return(models_anon[[model_name]])
  }

get_z <-
  function(x,
           age,
           metric = c("CSMA", "SMI", "SMRA", "SMG"),
           sex = c("Female", "Male"),
           lvl = c("T5", "T8", "T10", "L3")) {
    #' Calculate z score of a given measurement.
    #'
    #' @param x A given measurement of an individual.
    #' @param age Age of the individual measured.
    #' @param metric The muscle metric measured
    #' @param sex The participant sex
    #' @param lvl The vertebral level.
    #'
    #' @return the z score.
    
    model <- choose_model(metric, sex, lvl)
    
    #Return NULL if age below 38 or above 80 - models were not created on data outside of this range.
    if (age < 38 | age > 80) {
      return(NULL)
    }
    
    z <- round(z.scores(model,  x = age, y = x)[1], 2)
    return(z)
  }

get_percentile <-
  function(x,
           age,
           metric = c("CSMA", "SMI", "SMRA", "SMG"),
           sex = c("Female", "Male"),
           lvl = c("T5", "T8", "T10", "L3")) {
    #' Calculate percentile of a given measurement.
    #'
    #' @param x A given measurement of an individual.
    #' @param age Age of the individual measured.
    #' @param metric The muscle metric measured
    #' @param sex The participant sex
    #' @param lvl The vertebral level.
    #'
    #' @return the percentile.
    
    model <- choose_model(metric, sex, lvl)
    
    #Return NULL if age below 38 or above 80 - models were not created on data outside of this range.
    if (age < 38 | age > 80) {
      return(NULL)
    }
    
    #Get distribution family:
    family <- model$family[1]
    #Get name of corresponding cumulative density function
    fname <- paste("p", family, sep = '')
    #Model parameters:
    pars <- model$par
    #Get predictions for the parameters at point x
    pred <- predict(model, newdata = age)
    
    #Create function call to cdf:
    if (length(pars) == 1) {
      fun_call <- call(fname, x, mu = pred$mu)
    } else if (length(pars) == 2) {
      fun_call <- call(fname, x, mu = pred$mu, sigma = pred$sigma)
      
    } else if (length(pars) == 3) {
      fun_call <- call(fname,
                       x,
                       mu = pred$mu,
                       sigma = pred$sigma,
                       nu = pred$nu)
    } else {
      fun_call <- call(
        fname,
        x,
        mu = pred$mu,
        sigma = pred$sigma,
        nu = pred$nu,
        tau = pred$tau
      )
    }
    
    centile <-
      eval(fun_call)
      
    centile <- round(centile, 2)
      
    return(centile)
  }

get_metric_label <- function(metric = c("CSMA", "SMI", "SMRA", "SMG")){
  #' Get ggplot label for each metric.
  #' 
  #' @param metric The muscle metric measured
  #' 
  #' @return The label.
  
  metrics_labs = list(
    CSMA=expression('Cross-Sectional Muscle Area (cm'^2 * ')'),
    SMI = expression('Skeletal Muscle Index (cm'^2 * '/m'^2*')'),
    SMRA = 'Skeletal Muscle Radio-Attenuation (HU)',
    SMG = expression('Skeletal Muscle Gauge (HU * cm'^2 * '/m'^2*')')
  )
  
  if(metric %in% names(metrics_labs)){
    return(metrics_labs[[metric]])
  }else{
    return(metric)
  }
}

plot_LMS_curves_with_indicator <-
  function(x,
           age,
           metric = c("CSMA", "SMI", "SMRA", "SMG"),
           sex = c("Female", "Male"),
           lvl = c("T5", "T8", "T10", "L3")) {
    #' Plot LMS centile curves with indicator.
    #'
    #' @param x A given measurement of an individual.
    #' @param age Age of the individual measured.
    #' @param metric The muscle metric measured
    #' @param sex The participant sex
    #' @param lvl The vertebral level.
    #'
    #' @return The plot object.
    
    plot <- ggplot(data.frame(x = seq(5, 10)), aes(x = x)) +
      geom_histogram()
    
    return(plot)
  }

plot_dist_at_age <-
  function(
           age,
           metric = c("CSMA", "SMI", "SMRA", "SMG"),
           sex = c("Female", "Male"),
           lvl = c("T5", "T8", "T10", "L3")) {
    #' Plot the LMS distribution at a given age.
    #' 
    #' @param age Age of the individual measured.
    #' @param metric The muscle metric measured
    #' @param sex The participant sex
    #' @param lvl The vertebral level.
    #'
    #' @return The plot.
    
    model <- choose_model(metric, sex, lvl)
    
    #Get distribution family:
    family <- model$family[1]
    #Get name of corresponding density function
    fname <- paste("d", family, sep = '')
    #and corresponding quantile function
    qfun <-  paste('q', family, sep='')
    #Model parameters:
    pars <- model$par
    #Get predictions for the parameters at point x
    pred <- predict(model, newdata = age)
    
    #Determine lower bound and upper bound at centiles of distribution:
    lb_cent <- .001
    ub_cent <- 1-lb_cent
    
    #Create stat function call based on amount of parameters required for the model family.
    #The stat function call can alter the output for each x in the later ggplot.
    #Also create quantile function call to determine bounds of histogram.
    if (length(pars) == 1) {
      stat_fun <-
        call(
          'stat_function',
          fun = fname,
          n = 101,
          args = list(mu = pred$mu)
        )
      newcall_lower_bound <- call(
        qfun,
        lb_cent,
        mu = pred$mu
      )
      newcall_upper_bound<- call(
        qfun,
        ub_cent,
        mu = pred$mu
      )
      
    } else if (length(pars) == 2) {
      stat_fun <-
        call(
          'stat_function',
          fun = fname,
          n = 101,
          args = list(mu = pred$mu, sigma = pred$sigma)
        )
      newcall_lower_bound <- call(
        qfun,
        lb_cent,
        mu = pred$mu, sigma = pred$sigma
      )
      newcall_upper_bound<- call(
        qfun,
        ub_cent,
        mu = pred$mu, sigma = pred$sigma
      )
    } else if (length(pars) == 3) {
      stat_fun <-
        call(
          'stat_function',
          fun = fname,
          n = 101,
          args = list(
            mu = pred$mu,
            sigma = pred$sigma,
            nu = pred$nu
          )
        )
      newcall_lower_bound <- call(
        qfun,
        lb_cent,
        mu = pred$mu, sigma = pred$sigma, nu = pred$nu
      )
      newcall_upper_bound<- call(
        qfun,
        ub_cent,
        mu = pred$mu, sigma = pred$sigma, nu = pred$nu
      )
    } else {
      stat_fun <-
        call(
          'stat_function',
          fun = fname,
          n = 101,
          args = list(
            mu = pred$mu,
            sigma = pred$sigma,
            nu = pred$nu,
            tau = pred$tau
          )
        )
      
      newcall_lower_bound <- call(
        qfun,
        lb_cent,
        mu = pred$mu,
        sigma = pred$sigma,
        nu = pred$nu,
        tau = pred$tau
      )
      newcall_upper_bound<- call(
        qfun,
        ub_cent,
        mu = pred$mu,
        sigma = pred$sigma,
        nu = pred$nu,
        tau = pred$tau
      )
    }
    
    #Define length of x axis of the plot.
    lower_bound <- eval(newcall_lower_bound)
    upper_bound <- eval(newcall_upper_bound)
    #Limit axis to 0 at the lower side
    lower_bound <- max(0, lower_bound)
    
    
    
    #higher_bound <- max(x, higher_bound) Would be nice, but forces the program to re-render upon every new entry of a measurement.
    #Function:
    plot <-
      ggplot(data.frame(x = c(lower_bound, upper_bound)), aes(x = x, fill =
                                                                 'blue')) +
      xlab(get_metric_label(metric)) +
      ylab('P(x)') +
      eval(stat_fun) 
    
    return(plot)
  }


get_plotting_dataframe <-
  function(model, metric,
           centiles = c(2.275, 10, 25, 50, 75, 90, 97.725)) {
    #' A helper function which will extract from a given model the dataframe needed to plot it in ggplot2.
    #' The functionality was taken out of the source code of the original centiles function.
    #'
    #'@param model The LMS model for which to plot the centiles.
    #'@param metric character The metric for which to gather the data.
    #'@param centiles numeric Which centiles should be plotted.
    #'
    #'@return The dataframe for plotting the model in ggplot2.
    
    xvar <- model$xvar
    #order xvar
    oxvar <- xvar[order(xvar)] #sort in ascending order
    
    
    #Get name of quantile function
    fname <- model$family[1] 
    qfun <- paste("q", fname, sep = "") 
    
    #Create dataframe with 3 columns: two linking age to predictions and the third to the corresponding percentile,
    #such that ggplot2 can later group by the percentile column
    centile_data <- data.frame()
    
    #Get function call for fitting centiles for a model based on amount of parameters
    lpar <- length(model$parameters)
    for (cen in centiles) {
      cent <- cen / 100
      #Get fitted data points for centile
      if (lpar == 1) {
        newcall <- call(qfun, cent, mu = fitted(model,
                                                "mu")[order(xvar)])
      }
      else if (lpar == 2) {
        newcall <- call(qfun,
                        cent,
                        mu = fitted(model,
                                    "mu")[order(xvar)],
                        sigma = fitted(model, "sigma")[order(xvar)])
      }
      else if (lpar == 3) {
        newcall <- call(
          qfun,
          cent,
          mu = fitted(model,
                      "mu")[order(xvar)],
          sigma = fitted(model, "sigma")[order(xvar)],
          nu = fitted(model, "nu")[order(xvar)]
        )
      }
      else {
        newcall <- call(
          qfun,
          cent,
          mu = fitted(model,
                      "mu")[order(xvar)],
          sigma = fitted(model, "sigma")[order(xvar)],
          nu = fitted(model, "nu")[order(xvar)],
          tau = fitted(model,
                       "tau")[order(xvar)]
        )
      }
      centile_fits <- eval(newcall)
      
      #effectively qBCTo(cent, mu=fitted, ...)
      #centile_data[[as.character(cen)]] <- centile_fits
      centile_df <-
        data.frame(Age = oxvar, Percentile = as.character(cen))
      centile_df[[metric]] <- centile_fits
      centile_data <- rbind(centile_data, centile_df)
    }
    centile_data$Percentile <-
      factor(centile_data$Percentile, levels = centiles)
    return(centile_data)
  }

get_LMS_curve_plot <- function(metric = c("CSMA", "SMI", "SMRA", "SMG"),
                               sex = c("Female", "Male"),
                               lvl = c("T5", "T8", "T10", "L3")) {
  #' Plot the LMS distribution at a given age.
  #'
  #' @param age Age of the individual measured.
  #' @param metric The muscle metric measured
  #' @param sex The participant sex
  #' @param lvl The vertebral level.
  #'
  #' @return The plot.
  
  centiles <- c(97, 85, 75, 50, 25, 15, 3)
  model <- choose_model(metric, sex, lvl)
  plotting_centile_data <- get_plotting_dataframe(model, metric, centiles)
  
  #Centile colors
  col.centiles <-
    c('#bdd7e7',
      '#6cafd8',
      '#3182bd',
      'orange',
      '#3182bd',
      '#6cafd8',
      '#bdd7e7')
  #Adjust for amount of centiles
  col.centiles <- col.centiles[1+floor((length(col.centiles)-length(centiles))/2):length(col.centiles)]
  
  #Plot
  plot <- ggplot(plotting_centile_data, aes_string(x='Age',
                                            y=metric,
                                            colour='Percentile')) +
    geom_line(size = .5) +
    labs(
      x = 'Age',
      y = get_metric_label(metric),
      colour = 'Percentile'
    ) +
    coord_cartesian(clip = 'off') +
    theme_gray() +
    theme(legend.position = 'right') +
    scale_colour_manual(values = col.centiles)
    
    return(plot)
}

mark_point <- function(x, y){
  #' Mark point described by x and y on ggplot.
  #' 
  #' @param x numeric The x coordinate.
  #' @param y numeric The y coordinate
  #'
  #' @return A marker to be added to a ggplot.
  
  marker <- geom_point(
    aes(x=x, y=y),size=4, color='red', shape=4)
  
  return(marker)
}

rowwise_z <- function(row){
  
  
  x <- as.numeric(row[["measurement"]])
  metric <- row[["metric"]]
  age <- as.numeric(row[["age"]])
  lvl <- row[["lvl"]]
  sex <- row[["sex"]]
  
  
  
  
  
  #Sanity checks:
  if(any(is.na(x), is.na(metric), is.na(lvl), is.na(sex), is.na(age))){return(NA)}
  if(x>0 & age >= 38 & age <= 80 & 
     metric %in% c("CSMA", "SMI", "SMRA", "SMG") &
     sex %in% c("Female", "Male") & 
     lvl %in% c("T5", "T8", "T10", "L3")){
        return(get_z(x,
             age,
             metric,
             sex,
             lvl))
  }
  else{
    return(NA)
  }
}

rowwise_percentile <- function(row){
  
  x <- as.numeric(row[["measurement"]])
  metric <- row[["metric"]]
  age <- as.numeric(row[["age"]])
  lvl <- row[["lvl"]]
  sex <- row[["sex"]]
  
  #Sanity checks:
  if(any(is.na(x), is.na(metric), is.na(lvl), is.na(sex), is.na(age))){return(NA)}
  if(x>0 & age >= 38 & age <= 80 & 
     metric %in% c("CSMA", "SMI", "SMRA", "SMG") &
     sex %in% c("Female", "Male") & 
     lvl %in% c("T5", "T8", "T10", "L3")){
    
    return(get_percentile(x,
                 age,
                 metric,
                 sex,
                 lvl))
  }
  else{
    return(NA)
  }
}

calc_z_scores_percentiles <- function(df){
  #' Calculate z scores and percentiles for a given dataframe.
  #' 
  #' @param df data.frame The dataframe. Must hold columns measurement, metric, age, lvl, sex.
  #' 
  #' @return The dataframe with additional columns z and p. The original dataframe if anything failed.
  
  #Sanity check dataframe:
  requirements <- c("measurement", "metric", "age", "lvl", "sex")
  
  for(req in requirements){
    if(!(req %in% colnames(df))){
      return(df)
    }
  }
  print("Test")
  
  #Calc z:
  z <- apply(df, 1, rowwise_z)
  print(z)
  df <- cbind(df, Z = z)
  
  #Calc percentile:
  p <- apply(df, 1, rowwise_percentile)
  df <- cbind(df, P = p)
  
  return(df)
}

