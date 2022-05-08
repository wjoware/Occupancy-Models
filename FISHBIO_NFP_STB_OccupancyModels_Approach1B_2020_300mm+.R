# File description
  # ---------------------------------------------------------------------------
# File-Name:        FISHBIO_NFP_STB_OccupancyModels_Approach1B_2020_300mm+
# Version:          v.
# Original Date:      27th Sep. 2021       
# Last Edit Date:     5th April 2022
# Author:           Will Ware
# Email(s):         (1) williamware@fishbio.net; (2) wware@ucsc.edu
# Description:      Prepares sampling of striped bass shorter than 300 mm
                      # fork lengths & associated variables for 
                      # occupancy models using data from 2019
# changes:          
  # ------
    # v.1 - a. data formatting building on objects imported from code sent by Matt
    #       b. attempting to align NAs for fish & associated variables by editing example code from Matt
    #       c. data formatting to prepare occupancy models
  
    # v.2 - a. formation of unmarked frame for occupancy models
    #       b. developing dynamic, multi-season occupancy models
    #       c. ranking dynamic multi-season occupancy models
    #       d. developing multi-season occupancy models that altered one parameter at a time
    #       e. ranking multi-season occupancy models to select covariate combinations for dynamic, multi-season occupancy models
    #       d. running & comparing dynamic, multi-season occupancy models
    
    # v.3 - a. formatting data to explore correlations between observation covariates
    #       b. binomial logistic regressions of observation covariate correlations
    #       c. variance inflation factor plots to visualize observation covariate correlations
    
    # v.4 - a. creating yearly a site covariate from sampling date from a list object
    #       b. re-running & ranking dynamic, multi-season occupancy models
      
    # v.5 - a. creating dataframes of compiled covariates for model predictions
    #       b. predicting occupancy model results with different dataframes
      
    # v.6 - a. attempting to plot model predictions & editing plots
    
    # v.7 - a. re-running & ranking dynamic, multi-season occupancy models based on Matt's new suggestions
      #     - b. predicting occupancy model results with a dataframe that Matt constructed for river kilometers, sampling days, & effort
      #     - c. visualizing model predictions & editing plots with Method I
      #     - d. Creating & visualizing model predictions with Method II
      #     - e. Adjusting model prediction plots to make them publication-quality
  
    # v.8 - a. adding updated code on calculating VIF values for covariates b/c prior code was inconsistent with how covariate NAs were aligned

  # Introduction
    # -------------------------------------------------------------------------
  # Approach 1B (as described by Matt) for 2019 striped bass
    # from the Stanislaus "Native Fish Plan" sampling
  
  # Note: striped bass are those above 300 mm fork length

  # Run occupancy models on annual basis as above with robust design 
    # accounted for. This is one of the correct ways to examine these data. 
    # For this we should be very clear about what are primary events vs. 
    # secondary events (NOTE that these will change depending on later 
    # approaches that we may use). For 1b, 2019 data will have 4 primary 
    # events (PE), each with 2 secondary events (SE). E.g., PE1 will have SE1 
    # and SE2, PE2 will have SE1 and SE2, etc.

  # Set Up
    # ---------------------------------------------------------------------------

  # clear items stored in global environment from prior code
    rm(list = ls())
    
  # load necessary packages
    library(sp)
    library(openxlsx) 
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(unmarked)
    library(AICcmodavg)
    library(xtable)
    library(reshape2)
    library(Hmisc)
    # (also placed where applicable below)

  # load R files from code developed by Matt & Tyler
    # ensure the file is downloaded onto your computer &
    # adjust reference folder for the load function, as needed
    load("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Capstone Project/Occupancy Models/Approach 1B/2020/OccupancyData_09082021.Rdata")

  # create the right reference data repository
    setwd("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Capstone Project/Occupancy Models/Approach 1B/2020")

  # Data formatting for striped bass less than 300 mm (fork length)
    # -------------------------------------------------------------
    
  # view detection data
    cap.list.2020.gtSC$STB
      # data not transformed to zeroes and ones
    
  # filter out unneeded columns
    stb.gt <- 
      cap.list.2020.gtSC$STB[, -c(1,2,3)] 
      # filter out the first 3 columns
        # from striped bass less than 300 mm
          # only want observations of striped bass
    
  # convert counts to 0s & 1s for detection
    stb.gt[c(1:39),] <-
      ifelse(stb.gt[c(1:39),] == 0,0,1)
    
    dimnames(stb.gt)[[2]] <- paste("y", 1:ncol(stb.gt), sep=".")
      # replace column names with "y.1" to "y.8" [8 columns total]
    sum(na.exclude(stb.gt))
      # calculate the total number of sampled striped bass = 64
        # equal to & longer than 300 mm fork lengths, excluding "NA" entries
    
    # ensure all "NA" entries align
      fishNAs <- which(is.na(stb.gt) == T)
        # take NAs from striped bass observations
    
    # make an object to store NAs from striped bass observations &
      # observation covariates
        allNAs <- unique(sort(c(fishNAs, 
                          # add striped bass NAs to the NA collection
                        envNAs1.2020, 
                          # add previously created object for flow NAs
                        envNAs2.2020, 
                          # add previously created object for temperature NAs
                        envNAs3.2020,
                          # add previously created object for effort NAs
                        envNAs4.2020,
                          # add previously created object for turbidity NAs
                        envNAs5.2020, 
                          # add previously created object for conductivity NAs
                        envNAs6.2020)))
                          # add previously created object for sampling day NAs
    
    # apply NAs from the compilation of all NA positions to each
      # observation covariate
        observation.covariates.2020.unscaled$flow[allNAs] <- NA
          # flow
        observation.covariates.2020.unscaled$temp[allNAs] <- NA
          # temperature
        observation.covariates.2020.unscaled$effort[allNAs] <- NA
          # sampling effort
        observation.covariates.2020.unscaled$turbidity[allNAs] <- NA
          # turbidity
        observation.covariates.2020.unscaled$conductivity[allNAs] <- NA
          # conductivity
        observation.covariates.2020.unscaled$day[allNAs] <- NA
          # sampling day
    
    # ensure striped bass data is a matrix
      str(stb.gt)
      stb.gt <- as.matrix(stb.gt)
      str(stb.gt)
        
    # apply all NA positions from each variable to striped bass observations
      stb.gt[allNAs] <- NA
    
    # check whether NAs are now aligned
      length(which(is.na(stb.gt))) # striped bass observations
      length(allNAs) # all NA locations
        # both are 42 values long, indicating that NAs from each are aligned
      
    # Assess variable correlations, quantified by 
      # variance inflation factors then visualize results
      # -------------------------------------------------------------------
      
      # compile dataframe of covariates
        flow <- observation.covariates.2021.unscaled$flow
        flow.avg <- apply(flow[, c(1:10)], 1, function(x) mean(x, na.rm = T))
        flow.means <- as.data.frame(flow.avg)
      
        effort <- observation.covariates.2021.unscaled$effort
        effort.avg <- apply(effort[, c(1:10)], 1, function(x) mean(x, na.rm = T))
        effort.means <- as.data.frame(effort.avg)
      
        day <- observation.covariates.2021.unscaled$day
        day.avg <- apply(day[, c(1:10)], 1, function(x) mean(x, na.rm = T))
        day.means <- as.data.frame(day.avg)
      
      # compile columns for row means of observation covariates with river km 
        covars <- cbind(flow.means, effort.means, day.means,
                      site.covariates.scaled$rkm)
      
      # rename columns for covariates
        colnames(covars)[1] <- "flow"
        colnames(covars)[2] <- "effort"
        colnames(covars)[3] <- "day"
        colnames(covars)[4] <- "rkm"
      
      # model flow against other variables (effort, day, & river km)
        cor_m1 <- lm(flow ~ effort+day+rkm, covars)
      # variance inflation value calculation
        library(performance)
        check1 <- check_collinearity(cor_m1)
        check1
      # visualize results
        library(see)
        p1 <- plot(check1)
      
      # model effort against other variables (flow, day, & river km)
        cor_m2 <- lm(effort ~ flow+day+rkm, covars)
      # variance inflation value calculation
        library(performance)
        check2 <- check_collinearity(cor_m2)
        check2
      # visualize results
        library(see)
        p2 <- plot(check2)
      
      # model sampling day against other variables (flow, effort, & river km)
        cor_m3 <- lm(day ~ flow+effort+rkm, covars)
      # variance inflation value calculation
        library(performance)
        check3 <- check_collinearity(cor_m3)
        check3
      # visualize results
        library(see)
        p3 <- plot(check3)

  # Developing occupancy model dataframe
    # ---------------------------------------------------------
  
    number.of.primary.periods <- 4
      # set number of primary sampling periods 
        # (sampling events in 2019, in this case)
    primaryperiods <- matrix(as.integer(rep(seq(1,number.of.primary.periods,1), 
                                          nrow(rkm.dat))),
                          ncol=number.of.primary.periods, 
                          byrow = F) # do not fill the matrix by row 
    
  # construct site covariates for occupancy models from sampling days
    # replace NAs for sampling date with column means 
      day <- observation.covariates.2020.unscaled$day
        # dataframe for sampling days
      day2.0 <- matrix(NA, nrow = 39, ncol = 8)
        # matrix for site covariate
      for (i in 1:8) {
        # i = 1 # use this line & pick a number from 1:8 to check whether
            # the for loop is correctly working
      v <- day[,i]
        # stand-in variable for column number
      avg <- mean(v,na.rm = T)
        # variable for column means
      v[which(is.na(v))] <- avg
        # replace NAs in each column with respective means
      day2.0[,i] <- v
        # 
    }
    
    # create a matrix that selects the primary sampling events
      # (first dates of each event)
        yearlysitecovs = list(date = matrix(c(day2.0[,1], 
                                          day2.0[,3], 
                                          day2.0[,5], 
                                          day2.0[,7]),
                                          nrow = 39, 
                                        byrow = F))
    # view result
      yearlysitecovs$date
        # same result as viewing "yearlysitecovs" since "date" is 
          # the only variable
    
  # model dataframe developed with "unmarkedMultFrame"
    stb.gt.UMF <- unmarkedMultFrame(y = stb.gt,
                  # observed detections (initial occupancy)
                  siteCovs = site.covariates.scaled,
                  # use environmental variables - 
                    # river km, sinuosity, max, stratification, depth, cv
                  obsCovs = observation.covariates.2020.unscaled,
                  yearlySiteCovs = yearlysitecovs,
                  numPrimary = number.of.primary.periods)
    summary(stb.gt.UMF)

  # Compare submodels that alter parameters for multi-season occupancy models
    # ------------------------------------------------------------------------
  
  # Explanation
    # SEPARATELY alter initial occupancy, colonization, local 
      # extinction/persistence, & detection probabilities SEPARATELY by 
      # combinations of site & observation covariates to determine which 
      # ones produce the best functioning model FOR STRIPED BASS SMALLER THAN 
      # 300 MM FORK LENGTH
    # Note: 
      # first-year occupancy can only be altered by site covariates
      # colonization & extinction/persistence can only be altered by
        # covariates that vary by site and season
      # detection can be altered by covariates that vary by site, season, &
        # observations
      
# ------------------------------------------------------------------------
  # Adjustments on Matt's suggested code for 
      # dynamic, multi-season occupancy models
      
    # models
      dm0 <- colext( ~1, ~1, ~1, ~1, stb.gt.UMF)
        # null model
      dm1 <- colext( ~ 1, ~ 1, ~ 1, ~ flow, stb.gt.UMF)
        # alter detection probability by flow
      dm2 <- colext( ~ 1, ~ 1, ~ 1, ~ effort, stb.gt.UMF)
        # alter detection probability by effort
      dm3 <- colext( ~ 1, ~ 1, ~ 1, ~ flow + effort, stb.gt.UMF)
        # alter detection probability by flow & effort
      dm4 <- colext( ~ 1, ~ date, ~ 1, ~ flow, stb.gt.UMF)
        # alter colonization probability by sampling date &
          # detection probability by flow
      dm5 <- colext( ~ 1, ~ date, ~ 1, ~ effort, stb.gt.UMF)
        # alter colonization probability by sampling date &
          # detection probability by sampling effort
      dm6 <- colext( ~ 1, ~ date, ~ 1, ~ flow + effort, stb.gt.UMF)
        # alter colonization probability by sampling date &
          # detection probability by flow + sampling effort
      dm7 <- colext( ~ rkm, ~ date, ~ 1, ~ flow, stb.gt.UMF)
        # alter initial occupancy probability by river km,
          # colonization probability by sampling date, &
          # detection probability by flow
      dm8 <- colext( ~ rkm, ~ date, ~ 1, ~ effort, stb.gt.UMF)
        # alter initial occupancy probability by river km,
          # colonization probability by sampling date, &
          # detection probability by sampling effort
      dm9 <- colext( ~ rkm, ~ date, ~ 1, ~ flow + effort, stb.gt.UMF)
        # alter initial occupancy probability by river km,
          # colonization probability by sampling date, &
          # detection probability by flow + sampling effort
      dm10 <- colext( ~ rkm, ~ date, ~ date, ~ flow, stb.gt.UMF)
        # alter initial occupancy probability by river km,
          # colonization probability by sampling date, 
          # extinction probability by sampling date, &
          # detection probability by flow
      dm11 <- colext( ~ rkm, ~ date, ~ date, ~ effort, stb.gt.UMF)
        # alter initial occupancy probability by river km,
          # colonization probability by sampling date, 
          # extinction probability by sampling date, &
          # detection probability by sampling effort
      dm12 <- colext( ~ rkm, ~ date, ~ date, ~ flow + effort, stb.gt.UMF)
        # alter initial occupancy probability by river km,
          # colonization probability by sampling date, 
          # extinction probability by sampling date, &
          # detection probability by flow + sampling effort
      
      modnames =
        c('Null', # null model
          'det~flow', # model 1 
          'det~effort', # model 2 
          'det~flow+effort', # model 3 
          'col~date, det~flow', # model 4 
          'col~date, det~effort', # model 5 
          'col~date, det~flow+effort', # model 6
          'psi~rkm, col~date, det~flow', # model 7 
          'psi~rkm, col~date, det~effort', # model 8
          'psi~rkm, col~date, det~flow+effort', # model 9
          'psi~rkm, col~date, ext~date, det~flow', # model 10
          'psi~rkm, col~date, ext~date, det~effort', # model 11
          'psi~rkm, col~date, ext~date, det~flow+effort') # model 12
      
      models = c(dm0, dm1, dm2, dm3, dm4, dm5, dm6,
                  dm7, dm8, dm9, dm10, dm11, dm12)
          # store all of the models & 
            # their respective names as a list object
      
    # develop the output that shows model comparisons
      library(AICcmodavg)  
      mod.table <-
        aictab(models, # list of models being compared
               modnames, # names to reference models
               second.ord = F, # use 1st-order AIC rather than 2nd-order
               nobs = NULL, # calculate AICs by the total sample size
               sort = T, # order models by AIC
               c.hat = T) # show value to indicate overdispersion
      mod.table <- as.data.frame(mod.table)
      mod.table
      
      # Export dataframe of method 2 model results to Excel file
        write.xlsx(mod.table,
                 file = "C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Capstone Project/Occupancy Models/Approach 1B/2019/2019_STB<300mmFL_OccupancyModel_Results.xlsx",
                 sheetName = "Model Comparisons - Method 2",
                 overwrite = T)
      
        dm2020_STBlt300_List <- fitList(fits = models)
          # convert the list object to a "fitList" object for
            # model comparisons
      
      # Extract values of interest from model summaries
        coef(dm2020_STBlt300_List) # coefficients 
        SE(dm2020_STBlt300_List) # standard errors
      
      # Model-averaged prediction
        predict(dm2020_STBlt300_List, type="psi")
          # predict initial occupancy probabilities by 
            # averaging estimates across all models
        predict(dm2020_STBlt300_List, type="col")
          # predict colonization probabilities by 
            # averaging estimates across all models
        predict(dm2020_STBlt300_List, type="ext")
          # predict extinction probabilities by 
            # averaging estimates across all models
        predict(dm2020_STBlt300_List, type="det")
          # predict detection probabilities by 
            # averaging estimates across all models
        
      # Model selection
      AICtable_2020_STBlt300 = modSel(dm2020_STBlt300_List, nullmod="Null")
      AICtable_2020_STBlt300
  
# ------------------------------------------------------------------------
  
# Visualize model results
    
  # Plot initial occupancy probability  & colonization probabilities across years
    # develop dataset for "predictions" of initial occupancy probability 
  
    # use predict function get initial occupancy, colonization, &
      # extinction estimates
      library(stats)
      
      # form dataframe for model predictions
        nd = data.frame(rkm = seq(0, 65, length.out = 100),
                      # kilometers of the lower Stanislaus River
                        date = seq(60, 180, length.out = 100),
                      # sampling dates from March - May
                        effort = seq(100, 1500, length.out = 100))
                      # sampling effort range
                        
        E.occ  <- predict(dm8, type = "psi", nd)
        E.col <- predict(dm8, type = "col", nd)
        E.det <- predict(dm8, type = "det", nd)
        
  # Ploting method I
    # large plots of initial occupancy, colonization, & detection probabilities
    # -------------------------------------------------------------------------
        
    # plotting settings  
      graphics.off() # don't plot
      windows(7,6) # plotting window size
      par(mar=c(4,4.8,1.5,2.1), # maximum plot margins
        # adjusts bottom, left, top, & right margins (in order)
          mfrow = c(1,1), # 1 rows & 1 column
          mgp = c(1.5,0.2,0)) # axis labels relative to 
                                # the edge of inner plot window
        
      library(Hmisc) # package for plotting minor tick marks
                        # not used for below code
                          # here in case later desired
        
    # initial occupancy probability
      with(E.occ, 
             {
               plot(nd$rkm, 
                    Predicted,
                    type = 'l',
                    pch = 1, 
                    xaxt = "n", 
                    xlab = "River kilometer",
                    main = "2020 striped bass (Morone saxatilis) longer than 300 mm fork length",
                    ylab = expression(hat(psi)),
                    las = 1,
                    ylim = c(0,1), font.lab = 2, tck = 0.03, 
                    cex.axis = 0.75)
               axis(1, at = nd$rkm, 
                    labels = round(seq(0, 65, length.out = 100),0),
                    tck = 0.03, 
                    cex.axis = 0.75)
               arrows(nd$rkm, lower, nd$rkm, upper, code = 3, angle = 90,
                      length = 0.03, col=rgb(0,0,0,0.2))
             })
        
      # colonization probability
        with(E.col, 
             {
               plot(nd$date, 
                    Predicted, 
                    type = 'l',
                    xaxt = "n", 
                    xlab = "Sampling day",
                    main = "2020 striped bass (Morone saxatilis) longer than 300 mm fork length",
                    ylab = expression(hat(gamma)),
                    las = 1,
                    ylim = c(0,1), font.lab = 2, tck = 0.01, cex.axis = 0.75)
               axis(1, at = nd$date, 
                    labels = round(seq(60, 180, length.out = 100),0),
                    tck = 0.01, cex.axis = 0.75)
               arrows(nd$date, lower, nd$date, upper, code = 3, angle = 90,
                      length = 0.03, col=rgb(0,0,0,0.2))
             })
        
      # detection probability
        with(E.det, 
             {
               plot(nd$effort, 
                    Predicted,
                    type = 'l',
                    xaxt = "n", 
                    xlab = "Electrofishing effort (s)",
                    main = "2020 striped bass (Morone saxatilis) longer than 300 mm fork length",
                    ylab = expression(hat(p)),
                    las = 1,
                    ylim = c(0,1), font.lab = 2, tck = 0.03, 
                    cex.axis = 0.75)
               axis(1, at = nd$effort, 
                    labels = round(seq(100, 1500, length.out = 100),0),
                    tck = 0.03 , cex.axis = 0.65)
               arrows(nd$effort, lower, nd$effort, upper, code = 3, angle = 90,
                      length = 0.03, col = rgb(0,0,0,0.2))
             })
        
  # Plotting method II
    # small plots of initial occupancy, colonization, & detection probabilities
      # followed by a plot of "naive" & estimated "actual" occupancy
      # -----------------------------------------------------------------------
        
      # mean predicted occupancy among sites
        boot_mod.gt20 <- nonparboot(dm8, B = 5)
          # 5x boostrap for the most parsimonious model for SE calculation
            pred.gt20 <- data.frame( event = 1:4, 
                    # initial occupancy before each primary sampling event
                              smoothed_occ = smoothed(dm8)[2,], 
                                # smooth the curve for model estimates
                                  SE = boot_mod.gt20@smoothed.mean.bsse[2,] )
                                    # extract basis set supersition error from 
                                      # smoothed model estimates
        
      # make prediction plots here w/ respect to covariates of top model AND
        # provide estimates and uncertainty of occupancy
          par(mfrow = c(2,2), # 2 rows & 2 columns
            mar = c(4,4,0.5,0.5)) # plot margin size settings
              # adjusts bottom, left, top, & right margins (in order)
        
        # predicted initial occupancy probability by river kilometer
          plot(nd$rkm, 
             E.occ$Predicted, 
             type = 'l', 
             xaxt = "n",
             ann = F,
             ylim = c(0,1),
             # plot probabilites along the y-axis from 0 to 1
             tck = 0.01
              )
          axis(1, at = nd$rkm, 
             labels = round(seq(0, 65, length.out = 100),0),
             tck = 0.008, 
             cex.axis = 0.75)
          title(xlab = 'River kilometer', 
              ylab = expression(hat(psi)))
          lines(nd$rkm, E.occ$lower, lty = 2)
          lines(nd$rkm, E.occ$upper, lty = 2)
        
        # predicted colonization probability by sampling day
          plot(nd$date, 
             E.col$Predicted, 
             type = 'l', 
             xaxt = "n",
             ann = F,
             ylim = c(0,1),
             # plot probabilites along the y-axis from 0 to 1
             tck = 0.01
              )
          axis(1, at = nd$date, 
             labels = round(seq(60, 180, length.out = 100),0),
             tck = 0.008, 
             cex.axis = 0.75)
          title(xlab = "Sampling date",
              ylab = expression(hat(gamma)))
          lines(nd$date, E.col$lower, lty = 2)
          lines(nd$date, E.col$upper, lty = 2)
        
        # predicted detection probability by sampling effort
          plot( nd$effort, 
              E.det$Predicted, 
              type = 'l', 
              xaxt = "n",
              ann = F,
              ylim = c(0,1), 
              # plot probabilites along the y-axis from 0 to 1
              tck = 0.01
              )
          axis(1, at = nd$effort, 
             labels = round(seq(100, 1500, length.out = 100),0),
             tck = 0.01, 
             cex.axis = 0.75)
          title(xlab = 'Electrofishing effort (s)',
                ylab = expression(hat(p)))
          lines( nd$effort, E.det$lower, lty = 2)
          lines( nd$effort, E.det$upper, lty = 2)
        
  # comparison of predicted occupancy & 
    # occupancy based on detections, or "naive" occupancy
          
      # adjust plot margins
        par(oma = c(0, 0, 0, 7),
            # adjusts outer margins
              # bottom, left, top, & right margins (in order)
              mar = c(4, 4, 2, 2),
                # adjusts inner margins
                  # bottom, left, top, & right margins (in order)
              mfrow = c(2,1))
                # 2 rows, 1 column
          
      # plotting
        plot(pred.gt20$event, 
          pred.gt20$smoothed_occ, 
          ylim = c(0,1), 
            # plot probabilites along the y-axis from 0 to 1
          tck = 0.01,
          type = 'n', 
          las = 1, 
          xlab = "Primary sampling event", 
          ylab = "Occupancy",
          main = "Large (FL ≥ 300 mm) striped bass occupancy in 2020", 
          axes = F)
          
        # place box around plot area
          box('plot')
          
        # naive estimate of occupancy #
          # calculate proportions of occupied sites
            nums20gt <- apply(cap.list.2020.gtSC$STB[, -c(1:3)], 
                            2, function(x) length(which( x > 0)))
            ns20gt <- apply(cap.list.2020.gtSC$STB[, -c(1:3)], 
                          2, function(x) 39 - length(which( is.na(x))))
            props20gt <- nums20gt / ns20gt
          
        # plot points depicting estimates of actual & "naive" 
          # occupancy at the start of each sampling event
          # "naive" occupancy (doesn't account for detection %)
            points(pred.gt20$event, 
                 c(mean(props20gt[1:2]), 
                   mean(props20gt[3:4]), 
                   mean(props20gt[5:6]), 
                   mean(props20gt[7:8])), 
                 pch = 21, bg = 'grey80', cex = 2.1 )
          
          # predicted occupancy, based on detections
            points(pred.gt20$event, 
                 pred.gt20$smoothed_occ,
                 pch = 21, 
                 bg = 'grey50', 
                 cex = 2.1 )
          
          # confidence intervals for "naive" estimates
            segments(pred.gt20$event, 
                   # specify where upper limit will be sourced
                  pred.gt20$smoothed_occ + pred.gt20$SE,
                   # upper
                  pred.gt20$event, 
                   # specify where lower limit will be sourced
                  pred.gt20$smoothed_occ - pred.gt20$SE,
                   # lower
                  lwd = 2, lend = 2)
                    # line thickness & line end
          
          # axes
            # x-axis
              axis(side = 1, at = 1:4, 
                # bottom of plot & locations at x1 to x4
               tck = 0.01, 
                # position of x-axis tick marks
               labels = 1:4) 
                # primary sampling events
          
          # y-axis
            axis(side = 2, at = seq(0, 1, 0.2), 
               # left of plot at locations y0 to y1 by 0.2 increments
              tck = 0.01,
               # position of y-axis tick marks
              labels = seq(0,1,0.2), las = 1) 
                # probability axis set (see above comment); using solid line
          
          # legend
            legend(x = 3.95, y = 1.6,
                 legend = c( 'Naive Estimate', 'Model Estimate'), 
                 pch = 21, 
                 pt.cex = 2, 
                 pt.bg = c( 'grey80', 'grey50'), 
                 bty = 'n',
                 xpd = NA)
          
  # Obselete code (all code below this line)
    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------
          
    # NA handling
      # Replace "NA" entries for observation & site covariates with columns means
      # ------------------------------------------------------
          
      # Align NAs from different dataframes for striped bass 
        # -----------------------------------------------------
          # store NA entries for striped bass shorter than 
            # 300 mm fork length as an object
              stb.lt.NAs <- 
                which(is.na(stb.lt) == T)
          
          # Match NA entries for striped bass observations & covariates
            flow2[stb.lt.NAs] <- NA
            effort2[stb.lt.NAs] <- NA
            day2[stb.lt.NAs] <- NA 
          
          # flow (cfs)
          flow <- observation.covariates.2019.unscaled$flow
          # save variable as dataframe object
          apply(flow, 2, function(x)length(which(is.na(x))))
          # count "NA" entries in each column
          flow_means <- apply(flow, 2, function(x)mean(x, na.rm = T))
          flow_means
          # calculate column means
          # column 1 mean = 4268.108
          # column 2 mean = 4549.31
          # column 3 mean = 4623.158
          # column 4 mean = 4637.368
          # column 5 mean = 3174.211
          # column 6 mean = 3014.571
          # column 7 mean = 1323.333
          # column 8 mean = 1434.737 
          
          flow2 <- matrix(NA, nrow = 39, ncol = 8)
          for (i in 1:8) {
            # i = 1 
            # (sequentially pick 1-8)
            v <- flow[,i]
            avg <- mean(v,na.rm = T)
            v[which(is.na(v))] <- avg
            flow2[,i] <- v
          }
          
          colnames(flow2)[1:8] <- c("flow1", "flow2","flow3", "flow4", 
                                    "flow5", "flow6", "flow7", "flow8")
          # rename dataframe columns
          
          # sampling effort
          effort <- observation.covariates.2019.unscaled$effort
          # save variable as dataframe object
          apply(effort, 2, function(x)length(which(is.na(x))))
          # count "NA" entries in each column
          effort_means <- apply(effort, 2, function(x)mean(x, na.rm = T))
          effort_means
          # calculate column means
          # column 1 mean = 433.8176
          # column 2 mean = 469.6236
          # column 3 mean = 497.1053
          # column 4 mean = 510.3224
          # column 5 mean = 598.2895
          # column 6 mean = 562.3257
          # column 7 mean = 567.3077
          # column 8 mean = 569.1053  
          
          effort2 <- matrix(NA, nrow = 39, ncol = 8)
          for (i in 1:8) {
            # i = 1 
            # (sequentially pick 1-8)
            v <- effort[,i]
            avg <- mean(v,na.rm = T)
            v[which(is.na(v))] <- avg
            effort2[,i] <- v
          } 
          
          colnames(effort2)[1:8] <- c("effort1", "effort2","effort3", "effort4", 
                                      "effort5", "effort6", "effort7", "effort8")
          # rename dataframe columns
          
          # sampling day
          day <- observation.covariates.2019.unscaled$day
          # save variable as dataframe object
          apply(day, 2, function(x)length(which(is.na(x))))
          # count "NA" entries in each column
          day_means <- apply(day, 2, function(x)mean(x, na.rm = T))
          day_means
          # calculate column means
          # column 1 mean = 66.40541
          # column 2 mean = 67.96774
          # column 3 mean = 94.31579
          # column 4 mean = 95.31579
          # column 5 mean = 122.52632
          # column 6 mean = 123.45714
          # column 7 mean = 157.23077
          # column 8 mean = 158.47368  
          
          day2.0 <- matrix(NA, nrow = 39, ncol = 8)
          for (i in 1:8) {
            # i = 1 
            # (sequentially pick 1-8)
            v <- day[,i]
            avg <- mean(v,na.rm = T)
            v[which(is.na(v))] <- avg
            day2.0[,i] <- v
          }
          
          colnames(day2.0)[1:8] <- c("day1", "day2","day3", "day4", 
                                     "day5", "day6", "day7", "day8")
          # rename dataframe columns
          
          # water temperature
          wtemp <- observation.covariates.2019.unscaled$temp
          # another object already named "temp", so 
          # save variable as dataframe object named "wtemp"
          apply(wtemp, 2, function(x)length(which(is.na(x))))
          # count "NA" entries in each column
          wtemp_means <- apply(wtemp, 2, function(x)mean(x, na.rm = T))
          wtemp_means
          # calculate column means
          # column 1 mean = 11.05135
          # column 2 mean = 10.84516
          # column 3 mean = 11.41053
          # column 4 mean = 11.35789
          # column 5 mean = 12.38947
          # column 6 mean = 12.46571
          # column 7 mean = 16.36154
          # column 8 mean = 16.10526 
          
          wtemp2 <- matrix(NA, nrow = 39, ncol = 8)
          for (i in 1:8) {
            # i = 1 
            # (sequentially pick 1-8)
            v <- wtemp[,i]
            avg <- mean(v,na.rm = T)
            v[which(is.na(v))] <- avg
            wtemp2.0[,i] <- v
          }
          
          colnames(wtemp2.0)[1:8] <- c("temp1", "temp2","temp3", "temp4", 
                                       "temp5", "temp6", "temp7", "temp8")
          # rename dataframe columns
          
          # river kilometer
          rkm <- as.data.frame(site.covariates.unscaled$rkm)
          # save variable as dataframe object
          apply(rkm, 2, function(x)length(which(is.na(x))))
          # count "NA" entries in each column - no "NA" entries
          colnames(rkm)[1] <- "rkm"
          # rename the column header of "rkm" to river kilometer
          
          # combine all 2019 observation covariates together
          observationCovs <-
            cbind(flow2, effort2, wtemp2, rkm)
          
          observationCovs2 <- 
            observationCovs[-c(17:24)]
          
    # Striped bass observation data formatting
      # ------------------------------------------------------
        
          View(stb.lt)
          # notice how observed are recorded with counts
          
          colnames(stb.lt)[1:8] <- c("detections1", "detections2","detections3", 
                                     "detections4", "detections5", "detections6", 
                                     "detections7", "detections8")
                                       # rename dataframe columns
          
    # Combine the dataframes of detections & covariates (observation & site)
      # with "NA" entries replaced by column means together
      # -----------------------------------------------------------------------
          mod_dat <- cbind(stb.lt, flow2, day2.0, effort2, rkm)
          
    # Evaluate collinearity between model covariates
      # -----------------------------------------------------------------------
          
      # create a dataframe for observation covariates from 2019
          obscovs.2019 <- 
            as.data.frame(observation.covariates.2019.unscaled)
      # select variables of interest from specific columns
          obscovs.lt.2019 <- select(obscovs.2019, 
                                    1:8, # flow
                                    17:24, # effort in seconds
                                    41:48) # sampling day
          # this will also represent yearly site covariate "date"
            # from the model dataframes since that is every 
              # 2nd sampling day
          
          # call object storing observation & site covariates of interest
          mod_dat
          
          # add a column representing the sampling site number
          mod_dat$site <- c(13,18,23,28,33,38,45,50,56,61,67,79,84,89,
                            94,99,104,109,114,119,124,129,139,
                            144,149,154,159,164,169,174,186,191,
                            196,201,206,211,216,221,226)
          # 39 sites sampled striped bass in 2019
          # add a column representing the river bank at the sampling site
          mod_dat$bank <- c("left","right","left","right","left","right","left",
                            "right","left","right","left","right","left","right",
                            "left","right","left","right","left","right","left",
                            "right","left","right","left","right","left","right",
                            "left","right","left","right","left","right","left",
                            "right","left","right","left")
          
          # format observation and site covariates for linear models
          # detail - convert data from "wide" to "long" format
          
          # flow
          # select columns representing flow from 
          # parent dataframe "mod_dat"
          flow.2019 <- select(mod_dat, 1:8)
          # stack flow observations on top of one another
          library(reshape2)
          flow.2.2019 <- melt(flow.2019)
          # effort
          # select columns representing effort from 
          # parent dataframe "mod_dat"
          effort.2019 <- select(mod_dat, 17:24)
          # stack effort observations on top of one another
          library(reshape2)
          effort.2.2019 <- melt(effort.2019)
          # sampling day ("day")
          # select columns representing sampling day from 
          # parent dataframe "mod_dat"
          day.2019 <- select(obscovs.2019, 41:48)
          # stack sampling day observations on top of one another
          library(reshape2)
          day.2.2019 <- melt(day.2019)
          # river kilometer ("rkm")
          # stack river km observations on top of one another
          # parent dataframe "mod_dat"
          rkm2 <- rep(rkm, 8)
          
          # format sampled detections of striped bass in long format
          stb.lt.2 <- melt(stb.lt)
          
          # combine stacked datasets ("long format") together
          mod_dat2 <- cbind(stb.lt.2, flow.2.2019,
                            effort.2.2019,day.2.2019,rkm.2.2019)
          # rename columns containing values of interest
          colnames(mod_dat2)[2] <- "observations"
          colnames(mod_dat2)[4] <- "flow"
          colnames(mod_dat2)[6] <- "effort"
          colnames(mod_dat2)[8] <- "day"
          colnames(mod_dat2)[9] <- "rkm"
          # select columns of interest & overwrite the dataframe
          # usually overwritting is not a good practice, but
          # the parent dataframe is undesirable & is overwritten
          # to reduce potential confusion
          mod_dat3 <- select(mod_dat2,2,4,6,8,9)
          # long-formatted data that will be used for linear
          # models to assess variance inflation factors of variables
          # used for occupancy models
          
          # add sampling site & event names to the long-formatted 
          # dataframe of observed detection/non-detection & covariates
          mod_dat3$site <- rep(site.covariates.unscaled$unit_name, 8)
          # extract & replicate sample site names 8x
          mod_dat3$event <- rep(seq(1:8),39)
          
          # remove "NA" entries from presence/absence records in "mod_dat3"
          # and save as a new dataframe
          mod_dat4 <- mod_dat3[!is.na(mod_dat3$observations), ]
          
          # calculate the proportion of striped bass presences & absences
          # at all sampling sites
          site_props <- 
            as.data.frame(prop.table(table(mod_dat4$observations, # observed presences
                                           mod_dat4$site))) # by sampling site 
          
          # rename column headers
          colnames(site_props)[1] <- "detections"
          colnames(site_props)[2] <- "sites"
          colnames(site_props)[3] <- "proportions"
          
    # Exploratory data analyses
          
      # plot the relationship between the proportion of observed presence 
        # sampling site & select variables used for occupancy models (covariates)
        #-------------------------------------------------------------------------------------
          
          # step 1: count & remove "NA" entries
          length(which(is.na(mod_dat4$flow)))
          # count
          mod_dat5 <- mod_dat4[!is.na(mod_dat4$flow), ]
          # remove rows of NA observations
          
          # step 2: unique(site_props$site)
          # notice how 39 sample sites remain, so observations 
          # at each site are still included despite removal of NA values
          # 27 sampling sites of the 39 total sampling sites have presences
          # 104L 109R 114L 119R 124L 129R 139L 144R 149L 154R 159L 
          # 169L 174R 18R  191R 201R 206L 211R 216L 221R 226L 56L
          # 61R  67L  84L 89R  94L  99R
          
          
          # step 3: change the variable "day" from a factor to a number to allow
          # calculation of the mean
          mod_dat5$day <- as.numeric(mod_dat5$day)
          
          # step 4: calculate mean values of covariates for each of the 39 sample sites
          # site 104L
          site104L <- filter(mod_dat5, site == "104L")
          mean(site104L$flow) # 3443.75
          mean(site104L$effort) # 111
          mean(site104L$rkm) # 36.9
          mean(site104L$day) # 14.75
          
          # site 109R
          site109R <- filter(mod_dat5, site == "109R")
          mean(site109R$flow) # 3911.667
          mean(site109R$effort) # 100.6667
          mean(site109R$rkm) # 35.4
          mean(site109R$day) # 12.33333
          
          # site 114L
          site114L <- filter(mod_dat5, site == "114L")
          mean(site114L$flow) # 3473.75
          mean(site114L$effort) # 110.75
          mean(site114L$rkm) # 33.9
          mean(site114L$day) # 14.5
          
          # site 119R
          site119R <- filter(mod_dat5, site == "119R")
          mean(site119R$flow) # 4008.333
          mean(site119R$effort) # 99.66667
          mean(site119R$rkm) # 32.4
          mean(site119R$day) # 11.66667
          
          # site 124L
          site124L <- filter(mod_dat5, site == "124L")
          mean(site124L$flow) # 3497.5
          mean(site124L$effort) # 109
          mean(site124L$rkm) # 30.9
          mean(site124L$day) # 13.25
          
          # site 129R
          site129R <- filter(mod_dat5, site == "129R")
          mean(site129R$flow) # 3497.5
          mean(site129R$effort) # 109
          mean(site129R$rkm) # 29.4
          mean(site129R$day) # 13.25
          
          # site 139L
          site139L <- filter(mod_dat5, site == "139L")
          mean(site139L$flow) # 3578.75
          mean(site139L$effort) # 110
          mean(site139L$rkm) # 26.4
          mean(site139L$day) # 13.25
          
          # site 13L
          site13L <- filter(mod_dat5, site == "13L")
          mean(site13L$flow) # 3201.25
          mean(site13L$effort) # 107.25
          mean(site13L$rkm) # 64.2
          mean(site13L$day) # 11.5
          
          # site 144R
          site144R <- filter(mod_dat5, site == "144R")
          mean(site144R$flow) # 3578.75
          mean(site144R$effort) # 110
          mean(site144R$rkm) # 24.9
          mean(site144R$day) # 13.75
          
          # site 149L
          site149L <- filter(mod_dat5, site == "149L")
          mean(site149L$flow) # 3506.25
          mean(site149L$effort) # 113
          mean(site149L$rkm) # 23.4
          mean(site149L$day) # 15.5
          
          # site 154R
          site154R <- filter(mod_dat5, site == "154R")
          mean(site154R$flow) # 3506.25
          mean(site154R$effort) # 113
          mean(site154R$rkm) # 21.9
          mean(site154R$day) # 15.5
          
          # site 159L
          site159L <- filter(mod_dat5, site == "159L")
          mean(site159L$flow) # 3506.25
          mean(site159L$effort) # 113
          mean(site159L$rkm) # 20.4
          mean(site159L$day) # 15.5
          
          # site 164R
          site164R <- filter(mod_dat5, site == "164R")
          mean(site164R$flow) # 3272.857
          mean(site164R$effort) # 119
          mean(site164R$rkm) # 18.9
          mean(site164R$day) # 16.71429
          
          # site 169L
          site169L <- filter(mod_dat5, site == "169L")
          mean(site169L$flow) # 3506.25
          mean(site169L$effort) # 113
          mean(site169L$rkm) # 17.4
          mean(site169L$day) # 15.5
          
          # site 174R
          site174R <- filter(mod_dat5, site == "174R")
          mean(site174R$flow) # 3506.25
          mean(site174R$effort) # 113
          mean(site174R$rkm) # 15.9
          mean(site174R$day) # 15.5
          
          # site 186L
          site186L <- filter(mod_dat5, site == "186L")
          mean(site186L$flow) # 3332.5
          mean(site186L$effort) # 113.5
          mean(site186L$rkm) # 12.3
          mean(site186L$day) # 15
          
          # site 18R
          site18R <- filter(mod_dat5, site == "18R")
          mean(site18R$flow) # 3201.25
          mean(site18R$effort) # 107.25
          mean(site18R$rkm) # 62.7
          mean(site18R$day) # 11.5
          
          # site 191R
          site191R <- filter(mod_dat5, site == "191R")
          mean(site191R$flow) # 3617.143
          mean(site191R$effort) # 111
          mean(site191R$rkm) # 10.8
          mean(site191R$day) # 14.85714
          
          # site 196L
          site196L <- filter(mod_dat5, site == "196L")
          mean(site196L$flow) # 3431.25
          mean(site196L$effort) # 114.5
          mean(site196L$rkm) # 9.3
          mean(site196L$day) # 16.5
          
          # site 201R
          site201R <- filter(mod_dat5, site == "201R")
          mean(site201R$flow) # 3431.25
          mean(site201R$effort) # 114.5
          mean(site201R$rkm) # 7.8
          mean(site201R$day) # 16.5
          
          # site 206L
          site206L <- filter(mod_dat5, site == "206L")
          mean(site206L$flow) # 3531.429
          mean(site206L$effort) # 112.7143
          mean(site206L$rkm) # 6.3
          mean(site206L$day) # 16
          
          # site 211R
          site211R <- filter(mod_dat5, site == "211R")
          mean(site211R$flow) # 3531.429
          mean(site211R$effort) # 112.7143
          mean(site211R$rkm) # 4.8
          mean(site211R$day) # 16
          
          # site 216L
          site216L <- filter(mod_dat5, site == "216L")
          mean(site216L$flow) # 3531.429
          mean(site216L$effort) # 112.7143
          mean(site216L$rkm) # 3.3
          mean(site216L$day) # 16
          
          # site 221R
          site221R <- filter(mod_dat5, site == "221R")
          mean(site221R$flow) # 2888
          mean(site221R$effort) # 129.6
          mean(site221R$rkm) # 1.8
          mean(site221R$day) # 19.8
          
          # site 226L
          site226L <- filter(mod_dat5, site == "226L")
          mean(site226L$flow) # 3680
          mean(site226L$effort) # 112.5
          mean(site226L$rkm) # 0.3
          mean(site226L$day) # 16
          
          # site 23L
          site23L <- filter(mod_dat5, site == "23L")
          mean(site23L$flow) # 3201.25
          mean(site23L$effort) # 107.25
          mean(site23L$rkm) # 61.2
          mean(site23L$day) # 11.5
          
          # site 28R
          site28R <- filter(mod_dat5, site == "28R")
          mean(site28R$flow) # 3201.25
          mean(site28R$effort) # 107.25
          mean(site28R$rkm) # 59.7
          mean(site28R$day) # 11.5
          
          # site 33L
          site33L <- filter(mod_dat5, site == "33L")
          mean(site33L$flow) # 3201.25
          mean(site33L$effort) # 107.25
          mean(site33L$rkm) # 58.2
          mean(site33L$day) # 11.5
          
          # site 38R
          site38R <- filter(mod_dat5, site == "38R")
          mean(site38R$flow) # 2945
          mean(site38R$effort) # 117
          mean(site38R$rkm) # 56.7
          mean(site38R$day) # 13.5
          
          # site 45L
          site45L <- filter(mod_dat5, site == "45L")
          mean(site45L$flow) # 3508.333
          mean(site45L$effort) # 107
          mean(site45L$rkm) # 54.6
          mean(site45L$day) # 11.83333
          
          # site 50R
          site50R <- filter(mod_dat5, site == "50R")
          mean(site50R$flow) # 3613.333
          mean(site50R$effort) # 107.3333
          mean(site50R$rkm) # 53.1
          mean(site50R$day) # 12.16667
          
          # site 56L
          site56L <- filter(mod_dat5, site == "56L")
          mean(site56L$flow) # 3204.286
          mean(site56L$effort) # 114.2857
          mean(site56L$rkm) # 51.3
          mean(site56L$day) # 3.71429
          
          # site 61R
          site61R <- filter(mod_dat5, site == "61R")
          mean(site61R$flow) # 3220
          mean(site61R$effort) # 113
          mean(site61R$rkm) # 49.8
          mean(site61R$day) # 13.16667
          
          # site 67L
          site67L <- filter(mod_dat5, site == "67L")
          mean(site67L$flow) # 3371.429
          mean(site67L$effort) # 102.1429
          mean(site67L$rkm) # 48
          mean(site67L$day) # 11.71429
          
          # site 79R
          site79R <- filter(mod_dat5, site == "79R")
          mean(site79R$flow) # 3101.25
          mean(site79R$effort) # 109.25
          mean(site79R$rkm) # 44.4
          mean(site79R$day) # 13.5
          
          # site 84L
          site84L <- filter(mod_dat5, site == "84L")
          mean(site84L$flow) # 3101.25
          mean(site84L$effort) # 109.25
          mean(site84L$rkm) # 42.9
          mean(site84L$day) # 13.5
          
          # site 89R
          site89R <- filter(mod_dat5, site == "89R")
          mean(site89R$flow) # 3093.75
          mean(site89R$effort) # 110.5
          mean(site89R$rkm) # 41.4
          mean(site89R$day) # 14.25
          
          # site 94L
          site94L <- filter(mod_dat5, site == "94L")
          mean(site94L$flow) # 3062.857
          mean(site79R$effort) # 109.25
          mean(site79R$rkm) # 44.4
          mean(site79R$day) # 13.5
          
          # site 99R
          site99R <- filter(mod_dat5, site == "99R")
          mean(site99R$flow) # 3025.714
          mean(site99R$effort) # 116.8571
          mean(site99R$rkm) # 38.4
          mean(site99R$day) # 15.71429
          
          # step 5: align mean values for the "flow" 
          # covariate to each proportion
          # (use the mean value 2x for each site to account for presences & absences)
          site_props$flow <- c(3443.75, 3443.75, 3911.667, 3911.667, 3473.75, 3473.75, 
                               4008.333, 4008.33, 3497.5, 3497.5, 3497.5, 3497.5,
                               3578.75, 3578.5, 3201.25, 3201.25, 3578.75, 3578.75, 
                               3506.25, 3506.25, 3506.25, 3506.25, 3506.25, 3506.25,
                               3272.857, 3272.857, 3506.25, 3506.25, 3332.5, 3332.5, 
                               3201.25, 3201.25, 3201.25, 3201.25, 3201.25, 3201.25, 
                               3617.143, 3617.143, 3431.25, 3431.25,3431.25, 3431.25, 
                               3531.429, 3531.429, 331.429, 331.429, 3531.429, 3531.429, 
                               2888, 2888, 3680, 3680, 3201.25, 3201.25, 3201.25, 3201.25, 
                               2945, 2945, 3508.333, 3508.33, 3613.333, 3613.333, 
                               3204.286, 3204.286, 3220, 3220, 3371.429, 3371.429, 
                               3101.25, 3101.25, 3101.25, 3101.25, 3093.75, 3093.75, 
                               3062.857, 3062.857, 3025.714, 3025.714)
          
          # step 6: align mean values for the "effort" 
          # covariate to each proportion
          # (use the mean value 2x for each site to account for presences & absences)
          site_props$effort <- c(111, 111, 100.6667, 100.6667, 110.75, 110.75, 
                                 99.66667, 99.66667, 109, 109, 109, 109, 110, 110, 
                                 107.25, 107.25, 110, 110, 113, 113, 113, 113, 113, 113,
                                 119, 119, 113, 113, 113, 113, 113.5, 113.5, 107.25, 107.25,
                                 111, 111, 114.5, 114.5, 114.5, 114.5, 112.7143, 112.7143, 
                                 112.7143, 112.7143, 112.7143, 112.7143, 129.6, 129.6, 
                                 112.5, 112.5, 107.25, 107.25, 107.25, 107.25, 107.25, 107.25,
                                 117, 117, 107, 107, 107.3333, 107.3333, 114.2857, 114.2857, 
                                 113, 113, 102.1429, 102.1429, 109.25, 109.25, 109.25, 109.25,
                                 110.5, 110.5, 109.25, 109.25, 116.8571, 116.8571)
          
          # step 7: align mean values for the river km or "rkm" 
          # covariate to each proportion
          # (use the mean value 2x for each site to account for presences & absences)
          site_props$rkm <- c(36.9, 36.9, 35.4, 35.4, 33.9, 33.9, 32.4, 32.4, 30.9, 30.9, 
                              29.4, 29.4, 26.4, 26.4, 64.2, 64.2, 24.9, 24.9, 23.4, 23.4, 
                              21.9, 21.9, 20.4, 20.4, 18.9, 18.9, 17.4, 17.4, 15.9, 15.9, 
                              12.3, 12.3, 62.7, 62.7, 10.8, 10.8, 9.3, 9.3, 7.8, 7.8, 
                              6.3, 6.3, 4.8, 4.8, 3.3, 3.3, 1.8, 1.8, 0.3, 0.3, 61.2, 61.2, 
                              59.7, 59.7, 58.2, 58.2, 56.7, 56.7, 54.6, 54.6, 53.1, 53.1, 
                              51.3, 51.3, 49.8, 49.8, 48, 48, 44.4, 44.4, 42.9, 42.9, 
                              41.4, 41.4, 44.4, 44.4, 38.4, 38.4)
          
          # step 8: align mean values for the "day" 
          # covariate to each proportion
          # (use the mean value 2x for each site to account for presences & absences)
          site_props$day <- c(14.75, 14.75, 12.3333, 12.3333, 14.5, 14.5, 11.66667, 11.66667,
                              13.25, 13.25, 13.25, 13.25, 13.25, 13.25, 11.5, 11.5, 
                              13.75, 13.75, 15.5, 15.5, 15.5, 15.5, 15.5, 15.5, 16.71429, 16.71429, 
                              15.5, 15.5, 15, 15, 11.5, 11.5, 14.85714, 14.85714, 16.5, 16.5, 
                              16.5, 16.5, 16, 16, 16, 16, 16, 16, 19.8, 19.8, 16, 16, 11.5, 11.5, 
                              11.5, 11.5, 11.5, 11.5, 13.5, 13.5, 11.83333, 11.83333, 12.16667, 12.16667, 
                              3.71429, 3.71429, 13.16667, 13.16667, 11.71429, 11.71429, 13.5, 13.5, 13.5, 
                              13.5, 13.5, 13.5, 14.25, 14.25, 13.5, 13.5, 15.71429, 15.71429)
          
          # step 9: model relationships between proportions of presence by
          # site and covariates
          # develop binomial logistic regressions
          # use raw observations of detections and non-detections
          # of striped bass
          logit1 <- glm (proportions ~ flow, 
                         family = binomial, data = site_props)
          # detections/non-detections by flow
          summary(logit1)
          # AIC = 6.0129
          
          logit2 <- glm(proportions ~ effort, 
                        family = binomial, data = site_props)
          # detections/non-detections by sampling effort
          summary(logit2)
          # AIC = 6.013
          
          logit3 <- glm(proportions ~ day, 
                        family = binomial, data = site_props)
          # detections/non-detections by sampling day
          summary(logit3)
          # AIC 6.0129
          
          logit4 <- glm(proportions ~ rkm, 
                        family = binomial, data = site_props)
          # detections/non-detections by river km
          summary(logit4)
          # AIC = 6.013
          
          # step 10: plot relationships between proportions of presences by 
          # site and covariates
          p1 <- ggplot(site_props, aes(x = flow, y = proportions)) + 
            geom_point() + stat_smooth(method = "glm", 
                                       family = "binomial") +
            coord_cartesian(ylim = c(0, 0.1)) +
            theme_classic() +
            ylab("proportions of presence by site") +
            xlab("flow (cfs)")
          
          p2 <- ggplot(site_props, aes(x = effort, y = proportions)) + 
            geom_point() + stat_smooth(method = "glm", 
                                       family = "binomial") +
            coord_cartesian(ylim = c(0, 0.1)) +
            theme_classic() +
            ylab("proportions of presence by site") +
            xlab("sampling effort (s)")
          
          p3 <- ggplot(site_props, aes(x = rkm, y = proportions)) + 
            geom_point() + stat_smooth(method = "glm", 
                                       family = "binomial") +
            coord_cartesian(ylim = c(0, 0.1)) +
            theme_classic() +
            ylab("proportions of presence by site") +
            xlab("river kilometer")
          
          p4 <- ggplot(site_props, aes(x = day, y = proportions)) + 
            geom_point() + stat_smooth(method = "glm", 
                                       family = "binomial") +
            coord_cartesian(ylim = c(0, 0.1)) +
            theme_classic() +
            ylab("proportions of presence by site") +
            xlab("sampling day")
          
          library(gridExtra)
          grid.arrange(p1, p2, p3, p4, nrow = 2, 
                       top = "Presence proportions of striped bass < 300 mm FL")
          
    # assess variable correlations, quantified by 
      # variance inflation factors then visualize results
      # -------------------------------------------------------------------
          
          # model flow against other variables (effort, day, & river km)
          cor_m1 <- lm(flow ~ effort+day+rkm, mod_dat3)
          # variance inflation value calculation
          library(performance)
          check1 <- check_collinearity(cor_m1)
          # visualize results
          library(see)
          p1 <- plot(check1)
          
          # model effort against other variables (flow, day, & river km)
          cor_m2 <- lm(effort ~ flow+day+rkm, mod_dat3)
          # variance inflation value calculation
          library(performance)
          check2 <- check_collinearity(cor_m2)
          # visualize results
          library(see)
          p2 <- plot(check2)
          
          # model sampling day against other variables (flow, effort, & river km)
          cor_m3 <- lm(day ~ flow+effort+rkm, mod_dat3)
          # variance inflation value calculation
          library(performance)
          check3 <- check_collinearity(cor_m3)
          # visualize results
          library(see)
          p3 <- plot(check3)
          
          # model river km against other variables (flow, effort, & sampling day)
          cor_m4 <- lm(rkm ~ flow+effort+day, mod_dat3)
          # variance inflation value calculation
          library(performance)
          check4 <- check_collinearity(cor_m4)
          # visualize results
          library(see)
          p4 <- plot(check4)
          
    # Compare observed detections and non-detections of striped bass
      # with the four model covariates of concern 
      # (e.g., flow, effort, day, river km)
      # -------------------------------------------------------------------
          
          # View distribution of variables with a histogram
          
          # select long-formatted data for environmental covariates
          library(dplyr)
          var_dat <- select(mod_dat3, 2:5)
          # select flow, sampling effort, sampling day, & river km
          
          # place 4 plots in one window
          par(mfrow = c(2,2))
          # plot 2 rows & 2 columns in one window
          par(mar=c(3,3,1,1))
          # change maximum margins to allow room for plots
          # the order of the arguments is "bottom," "left," "top," & "right"
          
          str(mod_dat3$event)
          mod_dat3$event <- as.factor(mod_dat3$event)
          str(mod_dat3$event)
          
          str(mod_dat3$site)
          mod_dat3$site <- as.factor(mod_dat3$site)
          str(mod_dat3$site)
          
          str(mod_dat3$day)
          mod_dat3$day <- as.factor(mod_dat3$day)
          str(mod_dat3$day)
          
          for(i in 1:4){
            hist(var_dat[,i], 
                 main = colnames(var_dat)[i],
                 xlab = colnames(var_dat)[i], 
                 col = 'light blue')
          }
          
          
          h1 <- rnorm( 100, 400, 100)
          h2 <- rnorm( 200, 300, 50)
          breaks = seq( 0, 1000, 25)
          xlim = c(min(breaks), max(breaks))
          ylim = c( 0, 50)
          hist( h1, xlim = xlim, ylim = ylim, breaks = breaks, las = 1, xaxs = "i", yaxs = "i" )
          hist( h2, xlim = xlim, ylim = ylim, breaks = breaks, las = 1, xaxs = "i", yaxs = "i" )
          
  # 2-step formation of occupancy models
    # ------------------------------------------------------------------------
          
      # keep all probabilities constant
        null <- colext (~1, ~1, ~1, ~1, stb.lt.UMF)
          
      # alter initial occupancy probability by a site covariate 
        # (river kilometer ["rkm"])   
          occ_lt.rkm <-
            colext(~rkm, 
                   # alter first-year occupancy probability by river kilometer
                   ~1, 
                   # constant colonization probability
                   ~1, 
                   # constant extinction probability
                   ~1, 
                   # constant detection probability
                   stb.lt.UMF) # dataframe
          summary(occ_lt.rkm)
            # use "plogis()" on logit estimates for model paramters to
              # get the values in probabilities
          plogis(-7.34)
            # initial occupancy estimate ~ 6.49e04
          
          # compare AICs of all models that alter occupancy probability -------------
          library(AICcmodavg)
          occ.lt.AICs <- 
            data.frame(Model = c("null","rkm"), 
                       AIC = c(null@AIC, occ_lt.rkm@AIC))
          occ.lt.AICs
          # lowest for model with initial occupancy altered by river km
          
          # compile AIC, delta AIC, AIC weight, cumulative weight, & log-likelihoods
          # into one object to compare how covariates shape each colonization
          # probability
          mlist1 <- c(null, # simple model, all parameters constant, 
                      # except detection (varies by river km & day)
                      occ_lt.rkm) # occupancy altered by river kilometer
          
          # create a vector of names corresponding to models
          # that compare covariates for 
          mnames1 <- c("simple",
                       "occ by rkm")
          
          # develop the output that shows model comparisons
          library(AICcmodavg)  
          mtable1 <-
            aictab(mlist1, # list of models being compared
                   mnames1, # names to reference models
                   second.ord = F, # use 1st-order AIC rather than 2nd-order
                   nobs = NULL, # calculate AICs by the total sample size
                   sort = T, # order models by AIC
                   c.hat = T) # show value to indicate overdispersion
          mtable1
          
          # alter colonization & extinction/persistence probability
          # colonization probability -------------------------------------------
          
          # by a site covariate ("rkm")
          col_lt.rkm <-
            colext(~1,
                   ~rkm,
                   # alter colonization probability by rkm
                   ~1,
                   ~1,
                   stb.lt.UMF)
          
          # by a yearly site covariate
          col_lt.date <-
            colext(~1,
                   ~date,
                   # alter colonization probability by sampling day ["date"]
                   ~1,
                   ~1,
                   stb.lt.UMF)
          
          # compare AICs of all models that alter colonization probability
          col.lt.AICs <- 
            data.frame(Model = c("null", "rkm", "date"), 
                       AIC = c(null@AIC, col_lt.rkm@AIC, col_lt.date@AIC))
          col.lt.AICs
          # lowest AIC for model with colonization altered by river km
          
          # compile AIC, delta AIC, AIC weight, cumulative weight, 
          # & log-likelihoods into one object to compare how covariates 
          # shape colonization probability
          mlist2 <- c(null, # null model
                      col_lt.rkm,
                      col_lt.date) # colonization altered by date
          
          # create a vector of names corresponding to models
          # that compare covariates for 
          mnames2 <- c("null", 
                       "rkm",
                       "col by date")
          
          # develop the output that shows model comparisons
          library(AICcmodavg)  
          mtable2 <-
            aictab(mlist2, # list of models being compared
                   mnames2, # names to reference models
                   second.ord = F, # use 1st-order AIC rather than 2nd-order
                   nobs = NULL, # calculate AICs by the total sample size
                   sort = T, # order models by AIC
                   c.hat = T) # show value to indicate overdispersion
          mtable2
          
          summary(col_lt.date)
          # view results of most parsimonious model to know estimates
          # Note: estimates of parameters are on logit scale
          # use "plogis()" on logit estimates for model parameters to
          # get the values in probabilities
          plogis(0.0235)
          # colonization probability estimate = 0.5058747
          
          # extinction probability 
          # ---------------------------------------------------------------
          
          # alter extinction probability by sample date ("date")
          ext_lt.date <-
            colext(~1, 
                   # constant first-year occupancy probability
                   ~1,
                   # constant colonization probability 
                   ~date,
                   # alter extinction probability by date
                   ~1,
                   # constant detection probability
                   stb.lt.UMF) # dataframe
          
          # compare models that alter extinction probability
          ext.lt.AICs <- 
            data.frame(Model = c("null","date"), 
                       AIC = c(null@AIC, ext_lt.date@AIC))
          ext.lt.AICs
          # determine the lowest AIC for constant extinction probability
          # lowest for extinction altered by every 2nd sampling day ("date")
          
          # compile AIC, delta AIC, AIC weight, cumulative weight, 
          # & log-likelihoods into one object to compare how covariates 
          # shape colonization probability
          mlist3 <- c(null, # null model
                      ext_lt.date) # colonization altered by date
          
          # create a vector of names corresponding to models
          # that compare covariates for 
          mnames3 <- c("null",
                       "ext by date")
          
          # develop the output that shows model comparisons
          library(AICcmodavg)  
          mtable3 <-
            aictab(mlist3, # list of models being compared
                   mnames3, # names to reference models
                   second.ord = F, # use 1st-order AIC rather than 2nd-order
                   nobs = NULL, # calculate AICs by the total sample size
                   sort = T, # order models by AIC
                   c.hat = T) # show value to indicate overdispersion
          mtable3
          
          summary(ext_lt.date)
          # estimates are on logit scale so must be 
          # converted to probability
          # use "plogis()" on logit estimates for model paramters to
          # get the values in probabilities
          plogis(-0.358)
          # constant extinction probability estimate = 0.4114438
          
          # alter detection probability by site and observation covariates
          # -------------------------------------------------------------------------
          
          # detection altered by flow [site covariate]
          det_lt.flow <-
            colext(~1, 
                   # constant first-year occupancy probability
                   ~1,
                   # constant colonization probability
                   ~1, 
                   # constant extinction probability
                   ~flow,
                   # alter detection probability by flow
                   stb.lt.UMF) # dataframe
          summary(det_lt.flow)
          
          # detection altered by effort [observation covariate]
          det_lt.effort <-
            colext(~1, 
                   # constant first-year occupancy probability
                   ~1,
                   # constant colonization probability
                   ~1, 
                   # constant extinction probability
                   ~effort,
                   # alter detection probability by effort (seconds)
                   stb.lt.UMF) # dataframe
          summary(det_lt.effort)
          
          # detection altered by flow & effort
          det_lt.flow_effort <-
            colext(~1, 
                   # constant first-year occupancy probability
                   ~1,
                   # constant colonization probability
                   ~1, 
                   # constant extinction probability
                   ~flow+effort,
                   # alter detection probability by flow & sampling effort (sec)
                   stb.lt.UMF) # dataframe
          summary(det_lt.flow_effort)
          
          # detection altered by site covariates
          # river kilometer ("rkm")
          det_lt.rkm <-
            colext(~1, 
                   # constant first-year occupancy probability
                   ~1,
                   # constant colonization probability
                   ~1, 
                   # constant extinction probability
                   ~rkm,
                   # alter detection probability by rkm
                   stb.lt.UMF) # dataframe
          summary(det_lt.rkm) 
          
          # detection altered by site and observation covariates combined
          # river kilometer & flow ("rkm")
          det_lt.rkm_flow <-
            colext(~1, 
                   # constant first-year occupancy probability
                   ~1,
                   # constant colonization probability
                   ~1, 
                   # constant extinction probability
                   ~rkm+flow,
                   # alter detection probability by flow & rkm
                   stb.lt.UMF) # dataframe
          summary(det_lt.rkm_flow) 
          
          # detection altered by river kilometer ("rkm") & effort
          det_lt.rkm_effort <-
            colext(~1, 
                   # constant first-year occupancy probability
                   ~1,
                   # constant colonization probability
                   ~1, 
                   # constant extinction probability
                   ~rkm+effort,
                   # alter detection probability by flow & rkm
                   stb.lt.UMF) # dataframe
          summary(det_lt.rkm_effort) 
          
          #  Compare the models with altered detection probability by 
          # using AIC (not necessary for other altered parameters because
          # these were only altered by river kilometer ("rkm"))
          det.lt.AICs <-
            data.frame(Model = 
                         c("null",
                           "flow",
                           "effort",
                           "flow & effort",
                           "rkm", 
                           "rkm & flow",
                           "rkm & effort"),
                       AIC = 
                         c(null@AIC,
                           det_lt.flow@AIC, 
                           det_lt.effort@AIC,
                           det_lt.flow_effort@AIC,
                           det_lt.rkm@AIC,
                           det_lt.rkm_flow@AIC,
                           det_lt.rkm_effort@AIC))
          det.lt.AICs
          # altering detection probability by river km ("rkm") & 
          # effort yielded the lowest AIC
          
          # Compile AIC, delta AIC, AIC weight, cumulative weight, & log-likelihoods
          # into one object to compare how covariates shape detection probability
          
          mlist4 <- c(null, # null model = all parameters constant
                      det_lt.flow, # detection altered by flow
                      det_lt.effort, # detection altered by effort
                      det_lt.flow_effort, # detecton altered by flow & effort
                      det_lt.rkm, # detection altered by rkm
                      det_lt.rkm_flow, # detection altered by rkm & flow
                      det_lt.rkm_effort) # detection altered by rkm & effort
          
          # create a vector of names corresponding to models
          # that compare covariates for 
          mnames4 <- c("null",
                       "det by flow", 
                       "det by effort",
                       "det by flow & effort",
                       "det by rkm", 
                       "det by rkm & flow", 
                       "det by rkm & effort")
          
          # develop the output that shows model comparisons
          library(AICcmodavg)  
          mtable4 <-
            aictab(mlist4, # list of models being compared
                   mnames4, # names to reference models
                   second.ord = F, # use 1st-order AIC rather than 2nd-order
                   nobs = NULL, # calculate AICs by the total sample size
                   sort = T, # order models by AIC
                   c.hat = T) # show value to indicate overdispersion
          mtable4
          
          summary(det_lt.flow_effort) 
          # estimates of parameters are on a logit scale, so 
          # convert to probabilities
          # use "plogis()" on logit estimates for model paramters to
          # get the values in probabilities
          plogis(0.00287)
          # initial occupancy estimate = 0.5007175
          
          # -------------------------------------------------------------------------
          # Develop & compare dynamic, multi-season occupancy models
          # -------------------------------------------------------------------------    
          library(unmarked) # if not loaded previously
          
          # simple model
          m1.lt <- colext(~1, ~1, ~date, ~flow+effort, stb.lt.UMF)
          # view model results (estimates as logit values)
          summary(m1.lt)
          
          # alter initial occupancy probability by river km
          m2.lt <- colext(~rkm, ~1, ~date, ~flow+effort, stb.lt.UMF)
          # view model results (estimates as logit values)
          summary(m2.lt)
          
          # alter colonization probability by river km 
          m3.lt <- colext(~1, ~rkm, ~date, ~flow+effort, stb.lt.UMF)
          # view model results (estimates as logit values)
          summary(m3.lt)
          
          # alter colonization probability by date
          m4.lt <- colext(~1, ~date, ~date, ~flow+effort, stb.lt.UMF)
          # view model results (estimates as logit values)
          summary(m4.lt)
          
          # alter initial occupancy probability by river km &
          # alter colonization probability by river km
          m5.lt <- colext(~rkm, ~rkm, ~date, ~flow+effort, stb.lt.UMF)
          # view model results (estimates as logit values)
          summary(m5.lt)
          
          # alter initial occupancy probability by river km &
          # alter colonization probability by date
          m6.lt <- colext(~rkm, ~date, ~date, ~flow+effort, stb.lt.UMF)
          # view model results (estimates as logit values)
          summary(m6.lt)   
          
          # select best model
          # Method 1 [long] --------------------------------
          # place models in an object for reference
          dyn.lt.mod <-
            c(m1.lt, m2.lt, m3.lt, m4.lt, m5.lt, m6.lt)
          
          # place abbreviated explanations of models as names by order
          # in an object
          dyn.lt.names <-
            c("simple", "occ by rkm", "col by rkm", "col by date", 
              "occ & col by rkm", "occ by rkm & col by date")
          
          # develop the output that shows model comparisons
          library(AICcmodavg)  
          dyn.lt.table <-
            aictab(dyn.lt.mod, # list of models being compared
                   dyn.lt.names, # names to reference models
                   second.ord = F, # use 1st-order AIC rather than 2nd-order
                   nobs = NULL, # calculate AICs by the total sample size
                   sort = T, # order models by AIC
                   c.hat = T) # show value to indicate overdispersion
          dyn.lt.table <- as.data.frame(dyn.lt.table)
          
          # Export dataframe of method 2 model results to Excel file
          write.xlsx(dyn.lt.table,
                     file = "C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Capstone Project/Occupancy Models/Approach 1B/2019/2019_STB<300mmFL_OccupancyModel_Results.xlsx",
                     sheetName = "Model Comparisons - Method 1",
                     overwrite = T)
          
          # Method 2 [short] -------------------------------------
          mlist <- fitList(Null = m1.lt, A. = m2.lt, B. = m3.lt, 
                           C. = m4.lt, D. = m5.lt, E. = m6.lt)
          coef(mlist)
          SE(mlist)
          mselect <- modSel(mlist, nullmod = "Null")
          mselect
          # AIC values slightly vary from those in Method 1
          
  # mapping occupancy model results
    # setting bounds for map
          minX <- min(spp.dat$XMin)
          maxX <- max(spp.dat$XMin)
          minY <- min(spp.dat$YMin)
          maxY <- max(spp.dat$YMin)
          
          xy1 <- data.frame(-121.2374, -120.8891)
          xy2 <- data.frame(37.66444, 37.7721)
          library(proj4)
          proj4string <- "+proj=utm +zone=10 +north +ellps=WGS84 +datum=NAD83 +units=m +no_defs"
          
          pj1 <- project(xy1, proj4string, inverse = T)
          pj2 <- project(xy2, proj4string, inverse = T)
          
          latlon1 <- data.frame(lat=pj1$y, lon=pj1$x)
          latlon2 <- data.frame(lat=pj2$y, lon=pj2$x)
          
          print(latlon1)
          # (lat, long) = (-0.001090343, -127.4898)
          print(latlon2)
          # (lat, long) = (0.0003406809, ) 
          
          
          
          
          
          
          
          