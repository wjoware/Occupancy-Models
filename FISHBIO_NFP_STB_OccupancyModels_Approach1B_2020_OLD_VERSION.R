#____________________________________________________________________________
  # Introduction
#____________________________________________________________________________

# Approach 1B (as described by Matt) for 2020 striped bass
  # from the Stanislaus "Native Fish Plan" sampling
  # Note: striped bass are divided into those above 
    # and below 300 mm fork length

  # Run occupancy models on annual basis as above with robust design 
  # accounted for. This is one of the correct ways to examine these data. 
  # For this we should be very clear about what are primary events vs. 
  # secondary events (NOTE that these will change depending on later 
  # approaches that we may use). For 1b, 2020 data will have 4 primary 
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
# (also placed where applicable below)

# load R files from code developed by Matt & Tyler
# ensure the file is downloaded onto your computer &
# adjust reference folder for the load function, as needed
load("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Summer2021 _STB_Tasks/Task 2/FISHBIO_StanislausRiver_NativeFishPredation/FISHBIO_StanislausRiver_NFP_RCode/OccupancyData_09082021.Rdata")

# create the right reference data repository
setwd("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Capstone Project/Occupancy Models")

# ___________________________________________________________________________________
      # Data formatting
# ___________________________________________________________________________________
# view detection data for striped bass less than 
# and greater than 300 mm (fork length)

# less than 300 mm fork length
cap.list.2020.ltSC$STB
# data not transformed to zeroes and ones
# greater than 300 mm fork length
cap.list.2020.gtSC$STB
# data not transformed to zeroes and ones

stb.lt <- 
  cap.list.2020.ltSC$STB[, 
                         -c(1,2,3)] 
# filter out the first 3 columns
  # from striped bass less than 300 mm

stb.gt <- 
  cap.list.2020.gtSC$STB[, 
                         -c(1,2,3)] 
# filter out the first 3 columns
  # from striped bass greater than 300 mm

# -----------------------------------------------------------------------------
  # Align NAs from different dataframes for striped bass 
# -----------------------------------------------------------------------------

# smaller than 300 mm fork length --------------------------------------------
stb.lt.NAs <- 
  which( is.na(stb.lt) == T )
all.lt.NAs <- 
  unique( sort(c(stb.lt.NAs, envNAs1.2020, envNAs2.2020, envNAs3.2020, 
                   envNAs4.2020, envNAs5.2020, envNAs6.2020)))

# match NAs for observation covariates
observation.covariates.2020.unscaled$flow[all.lt.NAs] <- NA
observation.covariates.2020.unscaled$temp[all.lt.NAs] <- NA
observation.covariates.2020.unscaled$effort[all.lt.NAs] <- NA
observation.covariates.2020.unscaled$turbidity[all.lt.NAs] <- NA
observation.covariates.2020.unscaled$conductivity[all.lt.NAs] <- NA
observation.covariates.2020.unscaled$day[all.lt.NAs] <- NA

length(which(is.na(observation.covariates.2020.unscaled))) 
# should be 0, yet selecting specific variables will show NA counts

# match observations of striped bass smaller than 
  # 300 mm in fork length to the other covariates
  stb.lt[is.na(observation.covariates.2020.unscaled$
               temp)] # use the temperature variable, 
               # which has NAs matching all other variables

# match NAs for site covariates with those for observation covariates
  site.covariates.scaled$rkm[is.na(observation.covariates.2020.unscaled$
                                   temp)] # using temperature

# striped bass larger than 300 mm fork length ---------------------------------

# repeat the prior 24 lines of codes and comments
stb.gt.NAs <- 
  which( is.na(stb.gt) == T )
all.gt.NAs <- 
  unique(sort(c(stb.gt.NAs, envNAs1.2020, envNAs2.2020, envNAs3.2020, 
                   envNAs4.2020, envNAs5.2020, envNAs6.2020)))

# match NAs for observation covariates
observation.covariates.2020.unscaled$flow[all.gt.NAs] <- NA
observation.covariates.2020.unscaled$temp[all.gt.NAs] <- NA
observation.covariates.2020.unscaled$effort[all.gt.NAs] <- NA
observation.covariates.2020.unscaled$turbidity[all.gt.NAs] <- NA
observation.covariates.2020.unscaled$conductivity[all.gt.NAs] <- NA
observation.covariates.2020.unscaled$day[all.gt.NAs] <- NA

# match observations of striped bass larger than 
  # 300 mm in fork length to the other covariates
  stb.gt[is.na(observation.covariates.2020.unscaled$
               temp)] # use the temperature variable, 
                        # which has NAs matching all other variables

# match NAs for site covariates with those for observation covariates
  site.covariates.scaled$rkm[is.na(observation.covariates.2020.unscaled$
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

dimnames(stb.lt)[[2]] <- paste( "y", 1:ncol(stb.lt), sep=".")
# rename the columns by "y.n" convention
  # this code seems ancillary (Will's opinion)

number.of.primary.periods <- 4
# set number of primary sampling periods (years in this case --> 2019 -2021)
primaryperiods <- matrix(as.integer(rep( seq(1,number.of.primary.periods,1) , 
                                          nrow(rkm.dat))),
                          ncol=number.of.primary.periods, 
                          byrow = F) # do not fill the matrix by row 
obsCovs <- list(x = data.frame(observation.covariates.2020.unscaled))
# striped bass less than 300 mm fork length --------------------------------- 
stb.lt.UMF <- unmarkedMultFrame(y = stb.lt,
                                # observed detections (initial occupancy)
                                siteCovs = site.covariates.scaled,
                                # use environmental variables - 
                                # river km, sinuosity, max, stratification, 
                                # depth, cv
                                obsCovs = observation.covariates.2020.unscaled,
                                numPrimary = number.of.primary.periods )
summary(stb.lt.UMF)

# striped bass greater than 300 mm fork length --------------------------------- 
stb.gt.UMF <- unmarkedMultFrame(y = stb.gt,
                                # observed detections (initial occupancy)
                                siteCovs = site.covariates.scaled,
                                # use environmental variables - 
                                # river km, sinuosity, max, stratification, 
                                # depth, cv
                                obsCovs = observation.covariates.2020.unscaled,
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
# 6.47-05% chance of initial occupancy (SE = 0.00224)
backTransform(m1, type =  "col")   # Colonization probability 
# 0.266% chance of colonization (SE = 0.145)
backTransform(m1, type =  "ext")   # Extinction probability 
# 2.92e04% chance of extinction (SE = 0.0112)
backTransform(m1, type =  "det")   # Detection probability
# 0.129% chance of detection (SE = 0.0657)

# -------- altering detection probability by ... 
# observation covariates that alter detection probability
# by flow 
m2 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow, # vary detection probability by flow
         stb.lt.UMF) # dataframe
summary(m2)

# by effort
m3 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~effort, # vary detection probability by sampling effort (seconds)
         stb.lt.UMF) # dataframe
summary(m3)

# by day
m4 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day, # vary detection probability by day
         stb.lt.UMF) # dataframe
summary(m4)

# site covariates that alter detection probability
# by river kilometer ("rkm")
m5 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~rkm, # vary detection probability by day
         stb.lt.UMF) # dataframe
summary(m5) # results in logit scale

# by combinations of site & observation covariates
# by sampling day & river kilometer
m6 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+rkm, # vary detection probability by 
         # sampling day & river kilometer
         stb.lt.UMF) # dataframe
summary(m6) # results in logit scale

# by sampling day & sampling effort
m7 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+effort, # vary detection probability by 
         # sampling day & sampling effort (seconds)
         stb.lt.UMF) # dataframe
summary(m7) # results in logit scale

# by sampling day, sampling effort, and river kilometer
m8 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+effort+rkm, # vary detection probability by 
         # sampling day, sampling effort 
         # (in seconds), and river kilometers
         stb.lt.UMF) # dataframe
summary(m8) # results in logit scale

# by flow and river kilometer
m9 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow+rkm, # vary detection probability by flow &
         # river kilometer
         stb.lt.UMF) # dataframe
summary(m9) # results in logit scale

# by flow and effort
m10 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow+effort, # vary detection probability by flow & effort
         stb.lt.UMF) # dataframe
summary(m10) # results in logit scale

# -----------------------------------------------------------------------------
  # Assess model performance for striped bass less 
    # than 300 mm in fork length then view results
    # of the best performing model
# -----------------------------------------------------------------------------
#  Compare the  models  using AIC 
  AICs <-
    data.frame(Model = 
               c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8","m9", "m10"),
             AIC = 
               c(m1@AIC, m2@AIC, m3@AIC, m4@AIC, m5@AIC, m6@AIC, m7@AIC, 
                 m8@AIC, m9@AIC, m10@AIC))
  # model 3 (for striped bass less than 300 mm fork length) 
    # has the lowest AIC
    # recall model 3 changes detection by sampling effort
    m3 <-
      colext(~1, # constant first-year occupancy probability
             ~1, # constant colonization probability
             ~1, # constant extinction probability
             ~effort, # vary detection probability by sampling effort (seconds)
             stb.lt.UMF) # dataframe
      summary(m3)
      
    # Backtransform model 3 estimates from logistic to  probability  scale 
      backTransform(m3, type =  "psi")   # First-year occupancy 
        # 4.98e04% chance of initial occupancy (SE = 0.00547)
      backTransform(m3, type =  "col")   # Colonization probability 
        # 0.15% chance of colonization (SE = 0.0932)
      backTransform(m3, type =  "ext")   # Extinction probability 
        # 5.35e03% chance of extinction (SE = 0.0533)
      # detection probability cannot be back-transformed to a probability 
        # scale from a logit scale like the other model parameters
        # so use the below steps to get the detection estimate
      lc <- linearComb(m3, c(1,4), type = "det")
        # make an object that stores detection from the model ("m3")
      backTransform(lc)
        # back transform the logistic estimate to a probability estimate
          # 0.134
  
  # Quantitatively evaluate model fit
      library(AICcmodavg)
      gof <- mb.gof.test(m3, nsim = 1000, plot.hist = F, plot.seasons = T, report = 1)
      gof
        # 4 seasons with separate Chi-square statistics calculated for them
        # 1000 bootstrapped sampled
        # c-hat estimate is 3.89, indicating large overdispersion
          # overdispersion = higher variance than expected
          # this is likely due to missing covariates for the models

# ----------------------------------------------------------------------------
    # Dynamic occupancy models (approach 1B)
      # for striped bass greater than 300 mm in fork length
# ----------------------------------------------------------------------------

# simplest model for striped bass under 300 mm fork length 
  m1 <-
    colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~1, # constant detection probability
         stb.gt.UMF) # dataframe

    summary(m1) 
    # results are on a logit-scale
              # back-transofrmation needed

#  Backtransform estimates from logistic to  probability  scale 
  
  backTransform(m1, type =  "psi")   # First-year occupancy 
    # 0.489% chance of initial occupancy (SE = 0.127)
  backTransform(m1, type =  "col")   # Colonization probability 
    # 0.302% chance of colonization (SE = 0.105)
  backTransform(m1, type =  "ext")   # Extinction probability 
    # 1.64e03% chance of extinction (SE = 0.0305)
  backTransform(m1, type = "det")
    # 0.338% chance of detection (SE = 0.0411)
  
# -------- altering detection probability by ... 
# observation covariates that alter detection probability
  # by flow 
    m2 <-
      colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow, # vary detection probability by flow
         stb.gt.UMF) # dataframe
    summary(m2) 
    # results on a logit scale

  # by effort
    m3 <-
      colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~effort, # vary detection probability by sampling effort (seconds)
         stb.gt.UMF) # dataframe
    summary(m3)
    # results on a logit scale

# by day
    m4 <-
      colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day, # vary detection probability by day
         stb.gt.UMF) # dataframe
    summary(m4)
    # results on a logit scale

# site covariates that alter detection probability
  # by river kilometer ("rkm")
    m5 <-
      colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~rkm, # vary detection probability by day
         stb.gt.UMF) # dataframe
    summary(m5)
    # results on a logit scale

# by combinations of site & observation covariates
  # specifically, by sampling day & river kilometer
    m6 <-
      colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+rkm, # vary detection probability by 
         # sampling day & river kilometer
         stb.gt.UMF) # dataframe
    summary(m6)
    # results on a logit scale

# by sampling day & sampling effort
    m7 <-
      colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+effort, # vary detection probability by 
         # sampling day & sampling effort (seconds)
         stb.gt.UMF) # dataframe
    summary(m7)
    # results on a logit scale

# by sampling day, sampling effort, and river kilometer
    m8 <-
     colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+effort+rkm, # vary detection probability by 
         # sampling day, sampling effort 
         # (in seconds), and river kilometers
         stb.gt.UMF) # dataframe
    summary(m8)
    # results on a logit scale

  # by flow and river kilometer
    m9 <-
      colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow+rkm, # vary detection probability by flow &
         # river kilometer
         stb.gt.UMF) # dataframe
    summary(m9)
# results on a logit scale

  # by flow and effort
    m10 <-
      colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow+effort, # vary detection probability by flow & effort
         stb.gt.UMF) # dataframe
    summary(m10)
    # results on a logit scale

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

