# Approach 1B (as described by Matt)
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
                         -c(1,2,3)] 
  # filter out the first 3 columns
  # from striped bass less than 300 mm

stb.gt <- 
  cap.list.2019.gtSC$STB[, 
                         -c(1,2,3)] 
  # filter out the first 3 columns
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
#----------------------------------------------------------------------------

# run 2019 estimates for STB < 300 mm

number.of.primary.periods <- 4
# set number of primary sampling periods (years in this case --> 2019 -2021)
primaryperiods <- matrix(as.integer(rep( seq(1,number.of.primary.periods,1) , 
                                          nrow(rkm.dat))),
                          ncol=number.of.primary.periods, 
                          byrow= T ) # fill the matrix by row 

# striped bass less than 300 mm fork length --------------------------------- 
stbUMF <- unmarkedMultFrame(y = stb.lt,
                             # observed detections (initial occupancy)
                             siteCovs = site.covariates.scaled,
                             # use environmental variables - 
                             # river km, sinuosity, max, stratification, 
                             # depth, cv
                             obsCovs = observation.covariates.2019.unscaled,
                             numPrimary = number.of.primary.periods )
summary(stbUMF)

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

# ----------------------------------------------------------------------
# Dynamic occupancy models (approach 2)
# ----------------------------------------------------------------------
# simplest model for striped bass under 300 mm fork length 
m1 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~1, # constant detection probability
         stbUMF) # dataframe

summary(m1) # results are on a logit-scale
# back-transofrmation needed

#  Backtransform estimates from logistic to  probability  scale 
  backTransform(m1, type =  "psi")   # First-year occupancy 
  # 2.02e-05% chance of initial occupancy (SE = 0.000888)
  backTransform(m1, type =  "col")   # Colonization probability 
  # 0.205% chance of colonization (SE = 0.0595)
  backTransform(m1, type =  "ext")   # Extinction probability 
  # 1.7e-04% chance of extinction (SE = 0.00479)
  backTransform(m1, type =  "det")   # Detection probability
  # 0.343% chance of detection (SE = 0.102)

# -------- altering detection probability by ... 
# observation covariates that alter detection probability
# by flow 
  m2 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow, # vary detection probability by flow
         stbUMF) # dataframe
summary(m2)

# by effort
m3 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~effort, # vary detection probability by sampling effort (seconds)
         stbUMF) # dataframe
summary(m3)

# by day
m4 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day, # vary detection probability by day
         stbUMF) # dataframe
summary(m4)

# site covariates that alter detection probability
# by river kilometer ("rkm")
m5 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~rkm, # vary detection probability by day
         stbUMF) # dataframe
summary(m5)
  # AIC: 133.3147

# by combinations of site & observation covariates
# by sampling day & river kilometer
m6 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+rkm, # vary detection probability by 
         # sampling day & river kilometer
         stbUMF) # dataframe
summary(m6)
  # AIC: 120.149

# by sampling day & sampling effort
m7 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+effort, # vary detection probability by 
         # sampling day & sampling effort (seconds)
         stbUMF) # dataframe
summary(m7)
  # AIC: 124.7349

# by sampling day, sampling effort, and river kilometer
m8 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+effort+rkm, # vary detection probability by 
         # sampling day, sampling effort 
         # (in seconds), and river kilometers
         stbUMF) # dataframe
summary(m8)
  # AIC: 122.2153

# by flow and river kilometer
m9 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow+rkm, # vary detection probability by flow &
         # river kilometer
         stbUMF) # dataframe
summary(m9)
  # AIC: 141.2969

# by flow and effort
m10 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow+effort, # vary detection probability by flow & effort
         stbUMF) # dataframe
summary(m10)
  # AIC: 126.0187




