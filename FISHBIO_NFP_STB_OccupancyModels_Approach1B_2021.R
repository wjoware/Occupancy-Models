#____________________________________________________________________________
  # Introduction
#____________________________________________________________________________

# Approach 1B (as described by Matt) for 2021 striped bass
  # from the Stanislaus "Native Fish Plan" sampling
  # Note: striped bass are divided into those above 
    # and below 300 mm fork length

  # Run occupancy models on annual basis as above with robust design 
  # accounted for. This is one of the correct ways to examine these data. 
  # For this we should be very clear about what are primary events vs. 
  # secondary events (NOTE that these will change depending on later 
  # approaches that we may use). For 1b, 2021 data will have 5 primary 
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
load("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Summer2021 _STB_Tasks/Task 2/FISHBIO_StanislausRiver_NativeFishPredation/FISHBIO_StanislausRiver_NFP_RCode/OccupancyData_09082021.Rdata")

# create the right reference data repository
setwd("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Capstone Project/Occupancy Models")


# ___________________________________________________________________________________
# Data formatting
# ___________________________________________________________________________________
# view detections data for striped bass less than 
# and greater than 300 mm (fork length)

# less than 300 mm fork length
cap.list.2021.ltSC$STB
# data not transformed to zeroes and ones
# greater than 300 mm fork length
cap.list.2021.gtSC$STB
# data not transformed to zeroes and ones

stb.lt <- 
  cap.list.2021.ltSC$STB[, 
                         -c(1,2,3)] 
# filter out the first 3 columns
  # from striped bass less than 300 mm

stb.gt <- 
  cap.list.2021.gtSC$STB[, 
                         -c(1,2,3)] 
# filter out the first 3 columns
  # from striped bass greater than 300 mm

# -----------------------------------------------------------------------------
  # align NAs from different dataframes for striped bass 
# -----------------------------------------------------------------------------

# smaller than 300 mm fork length --------------------------------------------
stb.lt.NAs <- 
  which( is.na(stb.lt) == T )
all.lt.NAs <- 
  unique( sort(c(stb.lt.NAs, envNAs1.2021, envNAs2.2021, envNAs3.2021, 
                   envNAs4.2021, envNAs5.2021, envNAs6.2021)))

# match NAs for observation covariates
observation.covariates.2021.unscaled$flow[all.lt.NAs] <- NA
observation.covariates.2021.unscaled$temp[all.lt.NAs] <- NA
observation.covariates.2021.unscaled$effort[all.lt.NAs] <- NA
observation.covariates.2021.unscaled$turbidity[all.lt.NAs] <- NA
observation.covariates.2021.unscaled$conductivity[all.lt.NAs] <- NA
observation.covariates.2021.unscaled$day[all.lt.NAs] <- NA

length(which(is.na(observation.covariates.2021.unscaled))) 
  # should be 0, yet selecting specific variables will show NA counts

# match observations of striped bass smaller than 
  # 300 mm in fork length to the other covariates
stb.lt[is.na(observation.covariates.2021.unscaled$
               temp)] # use the temperature variable, 
               # which has NAs matching all other variables

# match NAs for site covariates with those for observation covariates
  site.covariates.scaled$rkm[is.na(observation.covariates.2021.unscaled$
                                     temp)] # using temperature

# striped bass larger than 300 mm fork length ---------------------------------

# repeat the prior 24 lines of codes and comments
stb.gt.NAs <- 
  which(is.na(stb.gt) == T)
all.gt.NAs <- 
  unique(sort(c(stb.gt.NAs, envNAs1.2021, envNAs2.2021, envNAs3.2021, 
                   envNAs4.2021, envNAs5.2021, envNAs6.2021)))

# match NAs for observation covariates
observation.covariates.2021.unscaled$flow[all.gt.NAs] <- NA
observation.covariates.2021.unscaled$temp[all.gt.NAs] <- NA
observation.covariates.2021.unscaled$effort[all.gt.NAs] <- NA
observation.covariates.2021.unscaled$turbidity[all.gt.NAs] <- NA
observation.covariates.2021.unscaled$conductivity[all.gt.NAs] <- NA
observation.covariates.2021.unscaled$day[all.gt.NAs] <- NA

# match observations of striped bass larger than 
  # 300 mm in fork length to the other covariates
  stb.gt[is.na(observation.covariates.2021.unscaled$
               temp)] # use the temperature variable, 
                        # which has NAs matching all other variables
  
# match NAs for site covariates with those for observation covariates
  site.covariates.scaled$rkm[is.na(observation.covariates.2021.unscaled$
                                     temp)] # using temperature


length(which(is.na(site.covariates.scaled))) # should be 0 

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

number.of.primary.periods <- 5
# set number of primary sampling periods (years in this case --> 2019 -2021)
primaryperiods <- matrix(as.integer(rep( seq(1,number.of.primary.periods,1) , 
                                          nrow(rkm.dat))),
                          ncol=number.of.primary.periods, 
                          byrow = T) # fill the matrix by row 

# striped bass less than 300 mm fork length --------------------------------- 
stb.lt.UMF <- unmarkedMultFrame(y = stb.lt,
                                # observed detections (initial occupancy)
                                siteCovs = site.covariates.scaled,
                                # use environmental variables - 
                                # river km, sinuosity, max, stratification, 
                                # depth, cv
                                obsCovs = observation.covariates.2019.unscaled,
                                numPrimary = number.of.primary.periods )
summary(stb.lt.UMF)

# striped bass greater than 300 mm fork length --------------------------------- 
stb.gt.UMF <- unmarkedMultFrame(y = stb.gt,
                                # observed detections (initial occupancy)
                                siteCovs = site.covariates.scaled,
                                # use environmental variables - 
                                # river km, sinuosity, max, stratification, 
                                # depth, cv
                                obsCovs = observation.covariates.2019.unscaled,
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
# back-transformation needed

#  Backtransform estimates from logistic to  probability  scale

backTransform(m1, type =  "psi")   # First-year occupancy 
# % chance of initial occupancy (SE = )
backTransform(m1, type =  "col")   # Colonization probability 
# % chance of colonization (SE = )
backTransform(m1, type =  "ext")   # Extinction probability 
# % chance of extinction (SE = )
backTransform(m1, type =  "det")   # Detection probability
# % chance of detection (SE = )

# -------- altering detection probability by ... 
# observation covariates that alter detection probability
# by flow 
m2 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow, # vary detection probability by flow
         stb.lt.UMF) # dataframe
summary(m2) # results on logistic scale

# by effort
m3 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~effort, # vary detection probability by sampling effort (seconds)
         stb.lt.UMF) # dataframe
summary(m3) # results on logistic scale

# by day
m4 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day, # vary detection probability by day
         stb.lt.UMF) # dataframe
summary(m4) # results on logistic scale

# site covariates that alter detection probability
  # by river kilometer ("rkm")
m5 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~rkm, # vary detection probability by day
         stb.lt.UMF) # dataframe
summary(m5) # results on logistic scale

# by combinations of site & observation covariates
# by sampling day & river kilometer
m6 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+rkm, # vary detection probability by 
         # sampling day & river kilometer
         stb.lt.UMF) # dataframe
summary(m6) # results on logistic scale

# by sampling day & sampling effort
m7 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+effort, # vary detection probability by 
         # sampling day & sampling effort (seconds)
         stb.lt.UMF) # dataframe
summary(m7) # results on logistic scale

# by sampling day, sampling effort, and river kilometer
m8 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~day+effort+rkm, # vary detection probability by 
         # sampling day, sampling effort 
         # (in seconds), and river kilometers
         stb.lt.UMF) # dataframe
summary(m8) # results on logistic scale

# by flow and river kilometer
m9 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow+rkm, # vary detection probability by flow &
         # river kilometer
         stb.lt.UMF) # dataframe
summary(m9) # results on logistic scale

# by flow and effort
m10 <-
  colext(~1, # constant first-year occupancy probability
         ~1, # constant colonization probability
         ~1, # constant extinction probability
         ~flow+effort, # vary detection probability by flow & effort
         stb.lt.UMF) # dataframe
summary(m10) # results on logistic scale

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
      # 8.5e04% chance of initial occupancy (SE = 0.00883)
      backTransform(m3, type =  "col")   # Colonization probability 
      # 0.288% chance of colonization (SE = 0.159)
      backTransform(m3, type =  "ext")   # Extinction probability 
      # 2.49e03% chance of extinction (SE = 0.0301)
      backTransform(m3, type =  "det")   # Detection probability
      # 0.129% chance of detection (SE = 0.0657)
      

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

summary(m1) # results are on a logit-scale
              # back-transofrmation needed

#  Backtransform estimates from logistic to  probability  scale 
  
  backTransform(m1, type =  "psi")   # First-year occupancy 
    # 0.192% chance of initial occupancy (SE = 0.0952)
  backTransform(m1, type =  "col")   # Colonization probability 
    # 0.479% chance of colonization (SE = 0.121)
  backTransform(m1, type =  "ext")   # Extinction probability 
    # .254% chance of extinction (SE = 0.137)
  backTransform(m1, type =  "det")   # Detection probability
    # % chance of detection (SE = )

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
# by sampling day & river kilometer
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
  
  # model 5 performed the best, based on its AIC value
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
    # 0.891% chance of initial occupancy (SE = 0.179)
  backTransform(m6, type =  "col")   # Colonization probability 
    # 0.049% chance of colonization (SE = 0.553)
  backTransform(m6, type =  "ext")   # Extinction probability 
    # 6.52e03% chance of extinction (SE = 0.0367)
  backTransform(m6, type =  "det")   # Detection probability
    # % chance of detection (SE = )



