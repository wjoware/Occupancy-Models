#____________________________________________________________________________
  # Introduction
#____________________________________________________________________________

# Approach 1B (as described by Matt) for 2019 striped bass
  # from the Stanislaus "Native Fish Plan" sampling
  # Note: striped bass are divided into those above 
    # and below 300 mm fork length

  # Run occupancy models on annual basis as above with robust design 
  # accounted for. This is one of the correct ways to examine these data. 
  # For this we should be very clear about what are primary events vs. 
  # secondary events (NOTE that these will change depending on later 
  # approaches that we may use). For 1b, 2019 data will have 4 primary 
  # events (PE), each with 2 secondary events (SE). E.g., PE1 will have SE1 
  # and SE2, PE2 will have SE1 and SE2, etc.

#_____________________________________________________________________________
  # Set Up
#_____________________________________________________________________________

# load necessary packages
library(sp)
library(openxlsx) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(unmarked)
library(AICcmodavg)
library(xtable)
# (also placed where applicable below)

# load R files from code developed by Matt & Tyler
# ensure the file is downloaded onto your computer &
# adjust reference folder for the load function, as needed
load("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Capstone Project/Occupancy Models/OccupancyData_09082021.Rdata")

# create the right reference data repository
setwd("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Capstone Project/Occupancy Models")

# ___________________________________________________________________________________
      # Data formatting
# ___________________________________________________________________________________
# view detection data for striped bass less than 
# and greater than 300 mm (fork length)

# less than 300 mm fork length
cap.list.2019.ltSC$STB
  # data not transformed to zeroes and ones
  # greater than 300 mm fork length
cap.list.2019.gtSC$STB
  # data not transformed to zeroes and ones

stb.lt <- 
  cap.list.2019.ltSC$STB[, 
    -c(1,2,3)] # filter out the first 3 columns
                  # from striped bass less than 300 mm

stb.gt <- 
  cap.list.2019.gtSC$STB[, 
    -c(1,2,3)] # filter out the first 3 columns
                  # from striped bass greater than 300 mm

# -----------------------------------------------------------------------------
    # Align NAs from different dataframes for striped bass 
# -----------------------------------------------------------------------------

# smaller than 300 mm fork length --------------------------------------------
  stb.lt.NAs <- 
    which(is.na(stb.lt) == T)
  all.lt.NAs <- 
    unique(sort(c(stb.lt.NAs, envNAs1.2019, envNAs2.2019, envNAs3.2019, 
                   envNAs4.2019, envNAs5.2019, envNAs6.2019)))

# match NAs for observation covariates
  observation.covariates.2019.unscaled$flow[all.lt.NAs] <- NA
  observation.covariates.2019.unscaled$temp[all.lt.NAs] <- NA
  observation.covariates.2019.unscaled$effort[all.lt.NAs] <- NA
  observation.covariates.2019.unscaled$turbidity[all.lt.NAs] <- NA
  observation.covariates.2019.unscaled$conductivity[all.lt.NAs] <- NA
  observation.covariates.2019.unscaled$day[all.lt.NAs] <- NA

  length(which(is.na(observation.covariates.2019.unscaled))) 
    # should be 0, yet selecting specific variables will show NA counts

# match observations of striped bass smaller than 
  # 300 mm in fork length to the other covariates
  stb.lt[is.na(observation.covariates.2019.unscaled$
        temp)] # use the temperature variable, 
                  # which has NAs matching all other variables

# match NAs for site covariates with those for observation covariates
  site.covariates.scaled$rkm[is.na(observation.covariates.2019.unscaled$
                                   temp)] # using temperature

# striped bass larger than 300 mm fork length ---------------------------------

# repeat the prior 24 lines of codes and comments
  stb.gt.NAs <- 
    which(is.na(stb.gt) == T)
  all.gt.NAs <- 
    unique(sort(c(stb.gt.NAs, envNAs1.2019, envNAs2.2019, envNAs3.2019, 
                   envNAs4.2019, envNAs5.2019, envNAs6.2019)))

# match NAs for observation covariates
  observation.covariates.2019.unscaled$flow[all.gt.NAs] <- NA
  observation.covariates.2019.unscaled$temp[all.gt.NAs] <- NA
  observation.covariates.2019.unscaled$effort[all.gt.NAs] <- NA
  observation.covariates.2019.unscaled$turbidity[all.gt.NAs] <- NA
  observation.covariates.2019.unscaled$conductivity[all.gt.NAs] <- NA
  observation.covariates.2019.unscaled$day[all.gt.NAs] <- NA

# match observations of striped bass larger than 
  # 300 mm in fork length to the other covariates
  stb.gt[is.na(observation.covariates.2019.unscaled$
        temp)] # use the temperature variable, 
                  # which has NAs matching all other variables

# match NAs for site covariates with those for observation covariates
  site.covariates.scaled$rkm[is.na(observation.covariates.2019.unscaled$
                                   temp)] # using temperature


length(which(is.na(site.covariates.scaled))) 
  # should be 0

# -----------------------------------------------------------------------------
  # Striped bass observation data formatting
# -----------------------------------------------------------------------------

# striped bass under 300 mm fork length ---------------------------------------
  View(stb.lt)
  # notice how observed are recorded with counts

stb.lt[c(1:39),] <-
  ifelse(stb.lt[c(1:39),]==0,0,1)
# convert counts to
# 0s & 1s for detection

# striped bass over 300 mm fork length
View(stb.gt)
  # notice how observed are recorded with counts

stb.gt[c(1:39),] <-
  ifelse(stb.gt[c(1:39),]==0,0,1)
  # convert counts to
    # 0s & 1s for detection

#----------------------------------------------------------------------------
# Will's adjustment of Matt's Code for developing occupancy model dataframe
  # for striped bass less than 300 mm in fork length
#----------------------------------------------------------------------------

number.of.primary.periods <- 4
# set number of primary sampling periods (years in this case --> 2019 -2021)
primaryperiods <- matrix(as.integer(rep( seq(1,number.of.primary.periods,1) , 
                                          nrow(rkm.dat))),
                          ncol=number.of.primary.periods, 
                          byrow = F) # do not fill the matrix by row 

# striped bass less than 300 mm fork length --------------------------------- 

    siteCovs <- 
      # create an object to store...
    observation.covariates.2019.unscaled[["day"]][seq(1,312,2)]
      # every second day from the sequence of sampling days in 2019
    siteCovs2 <- as.data.frame(siteCovs)
      # create a dataframe from "siteCovs2"
    names(siteCovs2)[1] <- "date"
      # name the select sampling days "date," not to be confused with "day"
    which(is.na(siteCovs2))
      # 19 "NA" entries in the dataframe
    siteCovs3 <- siteCovs2[!is.na(siteCovs2$date),1]
      # remove 'NAs" from "siteCovs2" & save as another object
        # to calculate the mean of
          # Note: 19 rows removed from "siteCovs" with 156 rows = 137 rows 
    mean(siteCovs3)
      # average is ~ 111 for the sampling day
    siteCovs2[is.na(siteCovs2)] <- 111
      # replace "NA" entries in "siteCovs2" with the mean value
        # advised by Matt (30th Sept.)
    
  # model dataframe developed with "unmarkedMultFrame"
    stb.lt.UMF <- unmarkedMultFrame(y = stb.lt,
                  # observed detections (initial occupancy)
                  siteCovs = site.covariates.scaled,
                  # use environmental variables - 
                    # river km, sinuosity, max, stratification, depth, cv
                  obsCovs = observation.covariates.2019.unscaled,
                  yearlySiteCovs = siteCovs2,
                  numPrimary = number.of.primary.periods )
summary(stb.lt.UMF)

# striped bass greater than 300 mm fork length --------------------------------- 
stb.gt.UMF <- unmarkedMultFrame(y = stb.gt,
                                # observed detections (initial occupancy)
                                siteCovs = site.covariates.scaled,
                                # use environmental variables - 
                                # river km, sinuosity, max, stratification, 
                                # depth, cv
                                obsCovs = observation.covariates.2019.scaled,
                                numPrimary = number.of.primary.periods )
summary(stb.gt.UMF)

# ----------------------------------------------------------------------------
    # Dynamic occupancy models (approach 1B)
      # for striped bass less than 300 mm in fork length
# ----------------------------------------------------------------------------

# simplest model for striped bass under 300 mm fork length 
  m1 <-
    colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~1, # constant detection probability
         stb.lt.UMF) # dataframe

summary(m1) # results are on a logit-scale
# back-transofrmation needed

#  Backtransform estimates from logistic to  probability  scale

backTransform(m1, type =  "psi")   # First-year occupancy 
# 2.02e-05% chance of initial occupancy (SE = 0.000888)
backTransform(m1, type =  "col")   # Colonization probability 
# 0.205% chance of colonization (SE = 0.0595)
backTransform(m1, type =  "ext")   # Extinction probability 
# 1.7e04% chance of extinction (SE = 0.00479)
backTransform(m1, type =  "det")   # Detection probability
# 0.343% chance of detection (SE = 0.102)

# -----------------------------------------------------------------------------
  # Develop & compare mult-season occupancy models
  
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
# -----------------------------------------------------------------------------
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
               AIC = c(m1@AIC, occ_lt.rkm@AIC))
  occ.lt.AICs
    # lowest for model with occupancy altered by detection probability
  
  # alter colonization & extinction/persistence probability -----------------
    # colonization probability ---------------------------------
      # by a site covariate
      col_lt.rkm <-
        colext(~1,
           # constant first-year occupancy
           ~rkm,
           # alter colonization probability by river kilometer
           ~1, 
           # constant extinction probability
           ~1, 
           # constant detection probability
           stb.lt.UMF) # dataframe
      summary(col_lt.rkm)
      
      # by a yearly site covariate
      col_lt.date <-
        colext(~1,
               ~date,
               # alter colonization probability by sampling day ["date"]
               ~1,
               ~1,
               stb.lt.UMF)
      summary(col_lt.date)
        # estimates of parameters are on logit scale
          # use "plogis()" on logit estimates for model paramters to
            # get the values in probabilities
              plogis(0.0235)
                # initial occupancy estimate = 0.5058747
      
      # by river kilometer and sampling date
      col_lt.date_rkm <-
        colext(~1,
               ~date+rkm,
               # alter colonization probability by sampling day ["date"] & 
                # river kilometer ["rkm"]
               ~1,
               ~1,
               stb.lt.UMF)    
      
    # compare AICs of all models that alter colonization probability
      col.lt.AICs <- 
        data.frame(Model = c("null","rkm", "date",  "date+rkm"), 
                   AIC = c(m1@AIC, col_lt.rkm@AIC, col_lt.date@AIC, 
                           col_lt.date_rkm@AIC))
      col.lt.AICs
        # lowest AIC for model with colonization altered by sample date
      
    # extinction probability --------------------------------------------------
      # alter extinction probability by river kilometer ("rkm")
        ext_lt.rkm <-
          colext(~1, 
            # constant first-year occupancy probability
         ~1,
            # constant colonization probability 
         ~rkm,
            # alter extinction probability by river kilometer
         ~1,
            # constant detection probability
         stb.lt.UMF) # dataframe
        
        summary(ext_lt.rkm)
          # estimates are on logit scale so must be 
            # converted to probability
              # use "plogis()" on logit estimates for model paramters to
                # get the values in probabilities
                  plogis(1.07)
                    # initial occupancy estimate = 0.7445969
      
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
 
      # alter extinction probability by river kilometer ("rkm") &
        # sample date ('"date")
       ext_lt.rkm_date <-
          colext(~1, 
               # constant first-year occupancy probability
               ~1,
               # constant colonization probability 
               ~rkm+date,
               # alter extinction probability by river kilometer and sample date
               ~1,
               # constant detection probability
               stb.lt.UMF) # dataframe
       
       # compare models that alter extinction probability
        ext.lt.AICs <- 
          data.frame(Model = c("null","rkm", "date",  "date+rkm"), 
                    AIC = c(m1@AIC, ext_lt.rkm@AIC, ext_lt.date@AIC, 
                            ext_lt.rkm_date@AIC))
        ext.lt.AICs
          # lowest AIC for constant extinction probability
  
  # alter detection probability by site and observation covariates ----------
    # flow [site covariate]
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
  
    # effort [observation covariate]
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
  
    # day [observation covariate]
    det_lt.day <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~day,
             # alter detection probability by sampling day
             stb.lt.UMF) # dataframe
    summary(det_lt.day)
    
    # combination of observation covariates
      # flow & day
      det_lt.flow_day <-
        colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~flow+day,
             # alter detection probability by sampling day
             stb.lt.UMF) # dataframe
    summary(det_lt.flow_day)
    
    # flow & effort
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
    
    # effort & day
    det_lt.effort_day <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~effort+day,
             # alter detection probability by sampling day & effort 9sec)
             stb.lt.UMF) # dataframe
    summary(det_lt.effort_day)
    
    # flow, day, & effort
    det_lt.flow_day_effort <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~flow+day+effort,
             # alter detection probability by flow, 
              # sampling day, & effort
             stb.lt.UMF) # dataframe
      summary(det_lt.flow_day_effort)
    
    # site covariates
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
    
    # site and observation covariates combined
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
    
    # river kilometer ("rkm") & effort
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
    
    # river kilometer ("rkm") & day
    det_lt.rkm_day <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~rkm+day,
             # alter detection probability by rkm & day
             stb.lt.UMF) # dataframe
    summary(det_lt.rkm_day) 
      # estimates of parameters are on a logit scale, so 
        # convert to probabilities
          # use "plogis()" on logit estimates for model paramters to
            # get the values in probabilities
              plogis(0.0533)
                # initial occupancy estimate = 0.5133218
    
    # flow, day, effort, & river kilometer ("rkm")
    det_lt.flow_day_effort_rkm <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~flow+day+effort+rkm,
             # alter detection probability by flow, 
             # sampling day, & effort
             stb.lt.UMF) # dataframe
    summary(det_lt.flow_day_effort_rkm)
    
  #  Compare the models with altered detection probability by 
    # using AIC (not necessary for other altered parameters because
    # these were only altered by river kilometer ("rkm"))
    #---------------------------------------------------------------------
    det.lt.AICs <-
      data.frame(Model = 
                   c("null","flow","day","effort","flow & day",
                     "flow & effort","effort & day","flow, effort, & day",
                     "rkm", "rkm & day", "rkm & effort", "rkm & flow",
                     "flow, day, effort, & rkm"),
                 AIC = 
                   c(m1@AIC,
                     det_lt.flow@AIC, 
                     det_lt.day@AIC, 
                     det_lt.effort@AIC,
                     det_lt.flow_day@AIC,
                     det_lt.flow_effort@AIC,
                     det_lt.effort_day@AIC,
                     det_lt.flow_day_effort@AIC,
                     det_lt.rkm@AIC,
                     det_lt.rkm_day@AIC,
                     det_lt.rkm_effort@AIC,
                     det_lt.rkm_flow@AIC,
                     det_lt.flow_day_effort_rkm@AIC))
    det.lt.AICs
    # altering occupancy by river kilometer ("rkm") & 
      # flow yielded the lowest AIC

# Compile AIC, delta AIC, AIC weight, cumulative weight, & log-likelihoods
    # into one object to compare how covariates shape each the probability
    # of each model parameter (initial occupancy, colonization, extinction,
    # & detection)
    # -----------------------------------------------------------------------
      mod_list <- c(m1, # null model = all parameters constant
              occ_lt.rkm, # occupancy altered by river kilometer
              col_lt.rkm, # colonization altered by river kilometer ("rkm")
              col_lt.date, # colonization altered by sampling date ("date")
              col_lt.date_rkm, # colonization altered by rkm & date
              ext_lt.rkm, # extinction altered by rkm
              ext_lt.date, # extinction altered by date
              ext_lt.rkm_date, # extinction altered by rkm & date
              det_lt.flow, # detection altered by flow
              det_lt.day, # detection altered by day
              det_lt.effort, # detection altered by effort
              det_lt.flow_day, # detection altered by flow & date
              det_lt.flow_effort, # detecton altered by flow & effort
              det_lt.effort_day, # detection altered by effort & date
              det_lt.flow_day_effort, # detection altered by flow, 
                                        # date, & effort
              det_lt.rkm, # detection altered by rkm
              det_lt.rkm_day, # detection altered by rkm & date
              det_lt.rkm_effort, # detection altered by rkm & effort
              det_lt.rkm_flow, # detection altered by flow
              det_lt.flow_day_effort_rkm) # detection altered by flow, day,
                                              # effort, & rkm

  # create a vector of names corresponding to each model, 
    # in the order listed above
  mod_names <- c("null",
               "occ by rkm", 
               "col by rkm", 
               "col by date", 
               "col by date & rkm", 
               "ext by rkm", 
               "ext by date", 
               "ext by river km & date", 
               "det by flow", 
               "det by day", 
               "det by effort",
               "det by flow & day", 
               "det by flow & effort", 
               "det by effort & day", 
               "det by flow, day, & effort", 
               "det by rkm", 
               "det by rkm & day", 
               "det by rkm & effort", 
               "det by rkm & flow", 
               "det by flow, day, effort, & rkm")

  # develop the output that shows model comparisons
  library(AICcmodavg)  
  lt.submodel.table <-
      aictab(mod_list, # list of models being compared
       mod_names, # names to reference models
       second.ord = F, # use 1st-order AIC rather than 2nd-order
       nobs = NULL, # calculate AICs by the total sample size
       sort = T, # order models by AIC
       c.hat = T) # show value to indicate overdispersion
  lt.submodel.table
    
  # turn the output into a table
    library(xtable)
    print(xtable(lt.submodel.table,
                 caption = "Submodel selection table for covariates",
                 label = "tab:selection"),
          include.rownames = FALSE,
          caption.placement = "top")
    # didn't  appear as desired

# -------------------------------------------------------------------------
  # Develop & compare dynamic multi-season occupancy models
# -------------------------------------------------------------------------    
  library(unmarked) # if not loaded previously
  
  # null model - all probabilities constant
      m1.lt <- colext(~1, ~1, ~1, ~1, stb.lt.UMF)
    # view model results (estimates as logit values)
      summary(m1.lt)
    # convert estimates from logit to probability value
      plogis(0.0648)
    # estimated colonization probability is 0.5161943
  
  # alter initial occupancy probability by river km
      m2.lt <- colext(~rkm, ~1, ~1, ~rkm+day, stb.lt.UMF)
    # view model results (estimates as logit values)
      summary(m2.lt)
    # convert estimates from logit to probability value
      plogis(-7.03)
        # estimated initial occupancy probability is 8.841494e04
  
  # alter colonization probability by river km
    m3.lt <- colext(~1, ~rkm, ~1, ~rkm+day, stb.lt.UMF)
    # view model results (estimates as logit values)
      summary(m3.lt)
    # convert estimates from logit to probability value
    plogis(0.0648)
    # estimated colonization probability is 0.5161943
  
  # alter initial occupancy probability &
    # alter colonization probability by river km
      m4.lt <- colext(~rkm, ~rkm, ~1, ~rkm+day, stb.lt.UMF)
    # view model results (estimates as logit values)
      summary(m4.lt)
    # convert estimates from logit to probability values
        plogis(-0.604)
      # estimated initial occupancy probability is 0.3534291
        plogis(0.139)
        # estimated colonization probability is 0.5346942
    
  # alter initial occupancy by river km & 
    # alter colonization probability by river km & every 2nd sampling day
      m5.lt <- colext(~rkm, ~rkm+date, ~1, ~rkm+day, stb.lt.UMF)
      # view model results (estimates as logit values)
        summary(m5.lt)
      # convert estimates from logit to probability values
          plogis(-0.704)
        # estimated initial occupancy probability is 0.330926
          plogis(0.0492)
          # estimated colonization probability is 0.5122975
  
  # place models in an object for reference
  stb.lt.mod <-
    c(m1.lt, m2.lt, m3.lt, m4.lt, m5.lt)
  
  # place abbreviated explanations of models as names by order
    # in an object
  stb.lt.names <-
    c("null", "occu by rkm", "col by rkm", "occu & col by rkm", 
      "col by rkm & date")
  
  # develop the output that shows model comparisons
  library(AICcmodavg)  
  lt.model.table <-
    aictab(stb.lt.mod, # list of models being compared
           stb.lt.names, # names to reference models
           second.ord = F, # use 1st-order AIC rather than 2nd-order
           nobs = NULL, # calculate AICs by the total sample size
           sort = T, # order models by AIC
           c.hat = T) # show value to indicate overdispersion
  lt.model.table
  
  
# Quantitatively evaluate model fit
  library(AICcmodavg)
  gof_lt.mod <- mb.gof.test(dyn_lt.model, 
                            nsim = 1000, plot.hist = F, 
                          plot.seasons = T, report = 1)
  gof_lt.mod
    # 4 seasons with separate Chi-square statistics calculated for them
    # 1000 bootstrapped sampled
      # Quantiles
        # 0% = 2.8
        # 25% = 10.1
        # 50% = 14.5
        # 75% = 20.9
        # 100% = 177.0
        # c-hat estimate is 1.37, indicating low overdispersion
          # overdispersion = higher variance than expected
          # which would likely be due to missing covariates for the models
  
# ------------------------------------------------------------------------
  # Evaluate collinearity between model covariates
    obscovs.lt.2019 <- 
      as.data.frame(observation.covariates.2019.unscaled)
    obscovs.lt.2019 <- select(obscovs.lt.2019, 1:8,17:24,39:48)
    sitecovs.lt.2019<-as.data.frame(rkm.dat$RKM)
    mod_dat <- cbind(obscovs.lt.2019,sitecovs.lt.2019)
    vif(lm())
    
#---------------------------------------------------------------------------    
#------ Procedures for Striped Bass Larger than 300 mm fork length ---------
# --------------------------------------------------------------------------
    # Alter initial occupancy, colonization, local extinction/persistence, 
      # and detection probabilities SEPARATELY by combinations of site and 
      # observation covariates to determine which ones produce the best 
      # functioning model FOR STRIPED BASS LARGER THAN 300 MM FORK LENGTH
        # Note: 
          # first-year occupancy can only be altered by site covariates
          # colonization & extinction/persistence can only be altered by
          # covariates that vary by site and season
          # detection can be altered by covariates that vary by site, season, 
          # and observations
# -----------------------------------------------------------------------------
    
# alter initial occupancy probability by site covariates ---------------------
    occ_gt.rkm <-
      colext(~rkm, 
             # alter first-year occupancy probability by river kilometer
             ~1, 
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~1, 
             # constant detection probability
             stb.gt.UMF) # dataframe
    summary(occ_gt.rkm)
    
# alter colonization & extinction/persistence probability -----------------
    # by site covariates
    col_gt.rkm <-
      colext(~1,
             # constant first-year occupancy
             ~rkm,
             # alter colonization probability by river kilometer
             ~1, 
             # constant extinction probability
             ~1, 
             # constant detection probability
             stb.gt.UMF) # dataframe
    summary(col_gt.rkm)
    
    # extinction probability
    ext_gt.rkm <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability 
             ~rkm,
             # alter extinction probability by river kilometer
             ~1,
             # constant detection probability
             stb.gt.UMF) # dataframe
    summary(ext_gt.rkm)
    
# alter detection probability by site and observation covariates ----------
    # flow [site covariate]
    det_gt.flow <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~flow,
             # alter detection probability by flow
             stb.gt.UMF) # dataframe
    summary(det_gt.flow)
    
    # effort [observation covariate]
    det_gt.effort <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~effort,
             # alter detection probability by effort (seconds)
             stb.gt.UMF) # dataframe
    summary(det_gt.effort)
    
    # day [observation covariate]
    det_gt.day <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~day,
             # alter detection probability by sampling day
             stb.gt.UMF) # dataframe
    summary(det_gt.day)
    
# combination of observation covariates -----------------------------------
    # flow & day
    det_gt.flow_day <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~flow+day,
             # alter detection probability by sampling day
             stb.gt.UMF) # dataframe
    summary(det_gt.flow_day)
    
    # flow & effort
    det_gt.flow_effort <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~flow+effort,
             # alter detection probability by flow & sampling effort (sec)
             stb.gt.UMF) # dataframe
    summary(det_gt.flow_effort)
    
    # effort & day
    det_gt.effort_day <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~effort+day,
             # alter detection probability by sampling day & effort 9sec)
             stb.gt.UMF) # dataframe
    summary(det_gt.effort_day)
    
    # flow, day, & effort
    det_gt.flow_day_effort <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~flow+day+effort,
             # alter detection probability by flow, 
             # sampling day, & effort
             stb.gt.UMF) # dataframe
    summary(det_gt.flow_day_effort)
    
# site covariates ----------------------------------------------------------
    # river kilometer ("rkm")
    det_gt.rkm <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~rkm,
             # alter detection probability by rkm
             stb.gt.UMF) # dataframe
    summary(det_gt.rkm) 
    
# site and observation covariates combined ---------------------------------
    # river kilometer & flow ("rkm")
    det_gt.rkm_flow <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~rkm+flow,
             # alter detection probability by flow & rkm
             stb.gt.UMF) # dataframe
    summary(det_gt.rkm_flow) 
    
    # river kilometer ("rkm") & effort
    det_gt.rkm_effort <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~rkm+effort,
             # alter detection probability by flow & rkm
             stb.gt.UMF) # dataframe
    summary(det_gt.rkm_effort) 
    
    # river kilometer ("rkm") & day
    det_gt.rkm_day <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~rkm+day,
             # alter detection probability by rkm & day
             stb.gt.UMF) # dataframe
    summary(det_gt.rkm_day) 
    
    # flow, day, effort, & river kilometer ("rkm")
    det_gt.flow_day_effort_rkm <-
      colext(~1, 
             # constant first-year occupancy probability
             ~1,
             # constant colonization probability
             ~1, 
             # constant extinction probability
             ~flow+day+effort+rkm,
             # alter detection probability by flow, 
             # sampling day, & effort
             stb.gt.UMF) # dataframe
    summary(det_gt.flow_day_effort_rkm)
    
#  Compare the models with altered detection probability by 
    # using AIC -------------------------------------------------------------
    det.gt.AICs <-
      data.frame(Model = 
                   c("flow","day","effort","flow & day",
                     "flow & effort","effort & day","flow, effort, & day",
                     "rkm", "rkm & flow", "rkm & effort", "rkm & day",
                     "flow, day, effort, & rkm"),
                 AIC = 
                   c(det_gt.flow@AIC, 
                     det_gt.day@AIC, 
                     det_gt.effort@AIC,
                     det_gt.flow_day@AIC,
                     det_gt.flow_effort@AIC,
                     det_gt.effort_day@AIC,
                     det_gt.flow_day_effort@AIC,
                     det_gt.rkm@AIC,
                     det_gt.rkm_day@AIC,
                     det_gt.rkm_effort@AIC,
                     det_gt.rkm_flow@AIC,
                     det_gt.flow_day_effort_rkm@AIC))
    det.gt.AICs
    # altering occupancy by river kilometer ("rkm") & 
      # flow yielded the lowest AIC


# -----------------------------------------------------------------------------
  # Assess model performance for striped bass greater 
    # than 300 mm fork length then view results
    # for the best-performing model
# -----------------------------------------------------------------------------
  #  Generate a list of to compare AIC from each model
  AICs <-
  data.frame(Model = 
               c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8","m9", "m10"),
             AIC = 
               c(m1@AIC, m2@AIC, m3@AIC, m4@AIC, m5@AIC, m6@AIC, m7@AIC, 
                 m8@AIC, m9@AIC, m10@AIC))
  
  # model 6 performed the best, based on its AIC value
    # it alters detection by sampling day and river kilometer
    m6 <-
      colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+rkm, # vary detection probability by 
         # sampling day & river kilometer
         stb.gt.UMF) # dataframe
    summary(m6)
  
  #  Backtransform estimates from logistic to  probability  scale 
    backTransform(m6, type =  "psi")   # First-year occupancy 
      # 0.828% chance of initial occupancy (SE = 0.231)
    backTransform(m6, type =  "col")   # Colonization probability 
      # 0.274% chance of colonization (SE = 0.387)
    backTransform(m6, type =  "ext")   # Extinction probability 
      # 2.58e04% chance of extinction (SE = 0.00703)
    # detection probability cannot be back-transformed to a probability 
      # scale from a logit scale like the other model parameters
      # so use the below steps to get the detection estimate
      lc <- linearComb(m3, c(1,4), type = "det")
        # make an object that stores detection from the model ("m3")
      backTransform(lc)
        # back transform the logistic estimate to a probability estimate
          # detection probability = 0.134% (0.209 standard error)

  # Quantitatively evaluate model fit
      library(AICcmodavg)
      gof <- mb.gof.test(m6, nsim = 1000, plot.hist = F, plot.seasons = T, report = 1)
      gof
      # 4 seasons with separate Chi-square (X^2) statistics calculated for them
        # Season 1 X^2  = 13.7132
        # Season 2 X^2  = 5.0576
        # Season 3 X^2  = 12.6557
        # Season 4 X^2  = 8.1636
      # 1000 bootstrapped sampled
        # 0% quantile = 2.2
        # 25% quantile = 5.9
        # 50% quantile = 8.4
        # 75% quantile = 11.8
        # 100% quantile = 63.3
      # c-hat estimate is 3.89, indicating large overdispersion
        # overdispersion = higher variance than expected
        # this is likely due to missing covariates for the models

