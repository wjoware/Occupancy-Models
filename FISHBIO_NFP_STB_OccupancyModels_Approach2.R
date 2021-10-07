# Approach 2 for occupancy models (as described by Matt) 

  # treat years as primary events and the monthly*** sampling events as secondary events. 
  # This approach would mean that there would be 3 primary events and a total of 
  # 4 + 4 + 5 (13) secondary events w/ 4 from 2019 and 2020 and 5 from 2021. 
  # This would require data to be collapsed to reflect this different data structure. 

  # *** NOTE that in some cases, especially in 2021, that there will be 2 
    # secondary events in April, so that this data was not collected on a 
    # true 'monthly' basis.

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
# align NAs from different dataframes

  # view detections data
    cap.list.2019.ltSC$STB 
      # (not transformed to zeroes and ones)
  stb.y <- 
    cap.list.2019.ltSC$STB[, 
            -c(1,2,3)] # filter out the first 3 columns
                          # detection data not included in these
                              
  # ensure data is consistent with respect to NAs #
  fishNAs <- 
    which( is.na(stb.y) == T )
  allNAs <- 
    unique( sort( c( fishNAs, envNAs1.2019, envNAs2.2019, envNAs3.2019, 
                     envNAs4.2019, envNAs5.2019, envNAs6.2019 )))
  observation.covariates.2019.scaled$flow[allNAs] <- NA
  observation.covariates.2019.scaled$temp[allNAs] <- NA
  observation.covariates.2019.scaled$effort[allNAs] <- NA
  observation.covariates.2019.scaled$turbidity[allNAs] <- NA
  observation.covariates.2019.scaled$conductivity[allNAs] <- NA
  observation.covariates.2019.scaled$day[allNAs] <- NA
  length( which( is.na( site.covariates.scaled ))) # should be 0
  
  # make a new fish dataset to dump into "unmarked" frame for data
    fish <- 
      as.matrix(cap.list.2019.ltSC$STB[, -c(1,2,3)]) #
      # same as the next line (commented out), 
        # but using code that Matt sent via Slack
          # Code: stb.y <- cap.list.2019.ltSC$STB[, -c(1,2,3)]
    
  # NAs from fish dataset derived from captures
    fish[ allNAs ] <- NA
    # NAs in correct spot from index
      # "allNAs" applies to fish and environmental data
  
  fish <- data.frame( cap.list.2019.ltSC$STB[ , c(1,2,3)], fish )
    # adjusted NAs into functions
  fish
  # view result
  
  # may need to specify fish dataset going into occ model
  
  # N-mixture model
   # bb.mix <- mix.model(count.df = fish,
                      # kval = 200,
                      # n.primary.periods=5,  # note 2019, 2020 = 4; 2021 = 5 #
                      # design.matrix = mod.set,
                      # obs.covs.list = observation.covariates.2021.scaled,
                      # site.covs.df=site.covariates.scaled,
                      # mix="NB")
  

# ------------------------------------------------------------
  # Will's code for data formating
# ------------------------------------------------------------

STB19_under300 <- as.data.frame(cap.list.2019.ltSC$STB)
# convert to dataframe

Det <- stb.y
# remove the 1st to 3rd columns

Det[c(1:39),] <-
  ifelse(Det[c(1:39),]==0,0,1)
# convert counts of sampled STB to
# 0s & 1s for detections

# add a column to "Det" indicating the sampling site
# Det$site <- rep(c(1:39), 1)

#----------------------------------------------------------------------------
# Matt's Code for developing occupancy model dataframe
#----------------------------------------------------------------------------

# run 2019 estimates for STB < 300 mm

View(Det) 
  # review the detection data (or skip this step if already done)

dimnames(Det)[[2]] <- paste( "y", 1:ncol(Det), sep=".")
  # rename the columns by "y.n" convention

number.of.primary.periods <- 4
  # set number of primary sampling periods (years in this case --> 2019 -2021)

primaryperiods <- matrix( as.integer(rep( seq(1,number.of.primary.periods,1) , 
                                          nrow(rkm.dat))),
                          ncol=number.of.primary.periods, 
                          byrow= T ) # fill the matrix by row 
stbUMF <- unmarkedMultFrame( y = Det,
                             # observed detections (initial occupancy)
                             siteCovs = site.covariates.scaled,
                            # use environmental variables - 
                              # river km, sinuosity, max, stratification, 
                              # depth, cv
                             obsCovs = observation.covariates.2019.unscaled,
                             numPrimary = number.of.primary.periods )
summary(stbUMF)

# ----------------------------------------------------------------------
  # Dynamic occupancy models (approach 2)
# ----------------------------------------------------------------------

# simplest model  -----------
m1 <-
    colext(~1, # constant first-year occupancy probability
           ~1, # constant colonization probability
           ~1, # constant extinction probability
           ~1, # constant detection probability
           stbUMF) # dataframe

summary(m1) # results are on a logit-scale
                          # back-transofrmation needed

#  Backtransform estimates to from logistic to probability  scale 
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
        
        # by sampling day & sampling effort
        m7 <-
          colext(~1, # constant first-year occupancy probability
                 ~1, # constant colonization probability
                 ~1, # constant extinction probability
                 ~day+effort, # vary detection probability by 
                                # sampling day & sampling effort (seconds)
                 stbUMF) # dataframe
        summary(m7)
        
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
        
        # by flow and river kilometer
        m9 <-
          colext(~1, # constant first-year occupancy probability
                 ~1, # constant colonization probability
                 ~1, # constant extinction probability
                 ~flow+rkm, # vary detection probability by flow &
                              # river kilometer
                 stbUMF) # dataframe
        summary(m9)
        
        # by flow and effort
        m10 <-
          colext(~1, # constant first-year occupancy probability
                 ~1, # constant colonization probability
                 ~1, # constant extinction probability
                 ~flow+effort, # vary detection probability by flow & effort
                 stbUMF) # dataframe
        summary(m10)
        
# -----------------------------------------------------------------------------
        # Assess model performance
# -----------------------------------------------------------------------------
#  Generate a fit  list  and  compare the  models  using AIC 
    
        models <- fitList( 
          'psi(.)gam(.)eps(.)p(.)'  = m1, 
          'psi(.)gam(.)eps(.)p(Y)'  = m2, 
          'psi(.)gam(.)eps(.)p(Y)'  = m3, 
          'psi(.)gam(.)eps(.)p(Y)'  = m4, 
          'psi(.)gam(.)eps(.)p(Y)'  = m5, 
          'psi(.)gam(.)eps(.)p(Y)'  = m6, 
          'psi(.)gam(.)eps(.)p(Y)'  = m7, 
          'psi(.)gam(.)eps(.)p(Y)'  = m8,
          'psi(.)gam(.)eps(.)p(Y)'  = m9,
          'psi(.)gam(.)eps(.)p(Y)'  = m10) 
        
        (ms <-  modSel(models)) 
