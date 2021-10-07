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
load("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Capstone Project/Occupancy Models/OccupancyData_09082021.Rdata")

# create the right reference data repository
setwd("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Capstone Project/Occupancy Models")

# ___________________________________________________________________________________
    # Data formatting
# ___________________________________________________________________________________

# Cleave unneccessary columns -------------------------------------------------------
  # import data for observations of striped bass
    # smaller than 300 mm in fork length
      cap.list.2019.ltSC$STB 
        # dataset of interest 
          # (not transformed to zeroes and ones)
      stb.lt <- 
        cap.list.2019.ltSC$STB[, 
            -c(1,2,3)] # filter out the first 3 columns because
                          # detection data not included in these
      
    # larger than 300 mm in fork length
      cap.list.2019.gtSC$STB
        # dataset of interest
          # (not transformed to zeroes and ones)
      stb.gt <-
        cap.list.2019.gtSC$STB[, 
            -c(1,2,3)] # filter out the first 3 columns because
                          # detection data not included in these

# -----------------------------------------------------------------------------                              
  # Align NAs from different dataframes for striped bass
# -----------------------------------------------------------------------------
      
  # ----------------- striped bass smaller than 300 mm in fork length ---------
        fishNAs <- 
          which( is.na(stb.lt) == T )
        allNAs <- 
          unique( sort( c( fishNAs, envNAs1.2019, envNAs2.2019, envNAs3.2019, 
                     envNAs4.2019, envNAs5.2019, envNAs6.2019 )))
        observation.covariates.2019.scaled$flow[allNAs] <- NA
        observation.covariates.2019.scaled$temp[allNAs] <- NA
        observation.covariates.2019.scaled$effort[allNAs] <- NA
        observation.covariates.2019.scaled$turbidity[allNAs] <- NA
        observation.covariates.2019.scaled$conductivity[allNAs] <- NA
        observation.covariates.2019.scaled$day[allNAs] <- NA
  
        length(which(is.na(observation.covariates.2019.scaled))) 
          # should be 0, but will not be if specific columns are selected
  
      # match observations of striped bass larger than 
        # 300 mm in fork length to the other covariates
        stb.lt[is.na(observation.covariates.2019.unscaled$
                 temp)] # use the temperature variable, which
                            # has NAs matching all other variables
  
      # match NAs for site covariates with those for observation covariates
        site.covariates.scaled$rkm[is.na(observation.covariates.2020.unscaled$
                 temp)] # using temperature
        
  # ------------- striped bass larger than 300 mm in fork length ------------
        fishNAs <- 
          which(is.na(stb.gt) == T)
        allNAs <- 
          unique(sort(c(fishNAs, envNAs1.2019, envNAs2.2019, envNAs3.2019, 
                           envNAs4.2019, envNAs5.2019, envNAs6.2019)))
        observation.covariates.2019.scaled$flow[allNAs] <- NA
        observation.covariates.2019.scaled$temp[allNAs] <- NA
        observation.covariates.2019.scaled$effort[allNAs] <- NA
        observation.covariates.2019.scaled$turbidity[allNAs] <- NA
        observation.covariates.2019.scaled$conductivity[allNAs] <- NA
        observation.covariates.2019.scaled$day[allNAs] <- NA
        
        length(which(is.na(observation.covariates.2019.scaled))) 
        # should be 0, but will not be if specific columns are selected
        
        # match observations of striped bass larger than 
          # 300 mm in fork length to the other covariates
            stb.gt[is.na(observation.covariates.2019.unscaled$
                       temp)] # use the temperature variable, which
                                # has NAs matching all other variables
        
        # match NAs for site covariates with those for observation covariates
          site.covariates.scaled$rkm[is.na(observation.covariates.2020.unscaled$
                       temp)] # using temperature
  
#----------------------------------------------------------------------------
  # Will's adjustment of Matt's Code for developing occupancy model dataframe
#----------------------------------------------------------------------------

# occupancy model for striped bass smaller than 300 mm in fork length
  # -----------------------------------------------------------------

View(stb.lt) 
    # review the detection data for striped bass smaller than 300 mm
      # in fork length (or skip this step if already done)

# set number of primary sampling periods (years in this case --> 2019 -2021)
  number.of.primary.periods <- 4
  primaryperiods <- matrix( as.integer(rep( seq(1,number.of.primary.periods,1) , 
                                          nrow(rkm.dat))),
                          ncol=number.of.primary.periods, 
                          byrow= T ) # fill the matrix by row 
                                        # make the above line (byrow = F)
                                          # if code fails to run 
                                              # (same result either way)
  stb.lt.UMF <- unmarkedMultFrame(y = stb.lt,
                             # observed detections (initial occupancy)
                             siteCovs = site.covariates.scaled,
                            # use environmental variables - 
                              # river km, sinuosity, max, stratification, 
                              # depth, cv
                             obsCovs = observation.covariates.2019.unscaled,
                             numPrimary = number.of.primary.periods )
  summary(stb.lt.UMF)
  
  # occupancy model for striped bass larger than 300 mm in fork length
  # -----------------------------------------------------------------
  
  View(stb.lt) 
  # review the detection data for striped bass larger than 300 mm
    # in fork length (or skip this step if already done)
  
  # set number of primary sampling periods (years in this case --> 2019 -2021)
  number.of.primary.periods <- 4
  primaryperiods <- matrix( as.integer(rep( seq(1,number.of.primary.periods,1) , 
                                            nrow(rkm.dat))),
                            ncol=number.of.primary.periods, 
                            byrow= T ) # fill the matrix by row 
  # make the above line (byrow = F)
  # if code fails to run 
  # (same result either way)
  stb.gt.UMF <- unmarkedMultFrame(y = stb.gt,
                                  # observed detections (initial occupancy)
                                  siteCovs = site.covariates.scaled,
                                  # use environmental variables - 
                                  # river km, sinuosity, max, stratification, 
                                  # depth, cv
                                  obsCovs = observation.covariates.2019.unscaled,
                                  numPrimary = number.of.primary.periods )
  summary(stb.gt.UMF)

# _____________________________________________________________________________
  # Dynamic occupancy models (approach 2)
# _____________________________________________________________________________

# Striped bass smaller than 300 mm in fork length -----------------------------
  # simplest model
    m1 <-
      colext(~1, # constant first-year occupancy probability
           ~1, # constant colonization probability
           ~1, # constant extinction probability
           ~1, # constant detection probability
           stb.lt.UMF) # dataframe

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
      summary(m5)

    # by combinations of site & observation covariates
      # by sampling day & river kilometer
        m6 <-
          colext(~1, # constant first-year occupancy probability
               ~1, # constant colonization probability
               ~1, # constant extinction probability
               ~day+rkm, # vary detection probability by 
                          # sampling day & river kilometer
               stb.lt.UMF) # dataframe
        summary(m6)
        
        # by sampling day & sampling effort
        m7 <-
          colext(~1, # constant first-year occupancy probability
                 ~1, # constant colonization probability
                 ~1, # constant extinction probability
                 ~day+effort, # vary detection probability by 
                                # sampling day & sampling effort (seconds)
                 stb.lt.UMF) # dataframe
        summary(m7)
        
        # by sampling day, sampling effort, and river kilometer
        m8 <-
          colext(~1, # constant first-year occupancy probability
                 ~1, # constant colonization probability
                 ~1, # constant extinction probability
                 ~day+effort+rkm, # vary detection probability by 
                                    # sampling day, sampling effort 
                                      # (in seconds), and river kilometers
                 stb.lt.UMF) # dataframe
        summary(m8)
        
        # by flow and river kilometer
        m9 <-
          colext(~1, # constant first-year occupancy probability
                 ~1, # constant colonization probability
                 ~1, # constant extinction probability
                 ~flow+rkm, # vary detection probability by flow &
                              # river kilometer
                 stb.lt.UMF) # dataframe
        summary(m9)
        
        # by flow and effort
        m10 <-
          colext(~1, # constant first-year occupancy probability
                 ~1, # constant colonization probability
                 ~1, # constant extinction probability
                 ~flow+effort, # vary detection probability by flow & effort
                 stb.lt.UMF) # dataframe
        summary(m10)
        
# Assess model performance for striped bass smaller than 300 mm in fork length
        # --------------------------------------------------------------------
  #  Generate a list  and  compare the  models  using AIC 
        #  Compare the  models  using AIC 
          AICs <-
            data.frame(Model = 
                       c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8","m9", "m10"),
                     AIC = 
                       c(m1@AIC, m2@AIC, m3@AIC, m4@AIC, m5@AIC, m6@AIC, m7@AIC, 
                         m8@AIC, m9@AIC, m10@AIC))
          AICs
          
      # model 6 performed the best, based on its AIC value,
          # it alters detection by sampling day and river kilometer
          m6 <-
            colext(~1, # constant first-year occupancy probability
                   ~1, # constant colonization probability
                   ~1, # constant extinction probability
                   ~day+rkm, # vary detection probability by 
                   # sampling day & river kilometer
                   stb.lt.UMF) # dataframe
          summary(m6)
          
          #  Backtransform estimates from logistic to  probability  scale 
            backTransform(m6, type =  "psi")   # First-year occupancy 
              # 3.85e03% chance of initial occupancy (SE = 0.144)
            backTransform(m6, type =  "col")   # Colonization probability 
              # 0.29% chance of colonization (SE = 0.0892)
            backTransform(m6, type =  "ext")   # Extinction probability
              # 1.69e04 chance of extinction (SE = 0.00605)
          # detection probability cannot be back-transformed to a probability 
              # scale from a logit scale like the other model parameters
              # so use the below steps to get the detection estimate
            lc <- linearComb(m6, c(1,4), type = "det")
              # make an object that stores detection from the model ("m3")
            backTransform(lc)
              # back transform the logistic estimate to a probability estimate
                # detection probability = 0.134% (0.209 standard error)
            
  # Striped bass smaller than 300 mm in fork length -----------------------------
        # simplest model
            m1 <-
              colext(~1, # constant first-year occupancy probability
                     ~1, # constant colonization probability
                     ~1, # constant extinction probability
                     ~1, # constant detection probability
                     stb.gt.UMF) # dataframe
            
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
                     stb.gt.UMF) # dataframe
            summary(m2)
            
            # by effort
            m3 <-
              colext(~1, # constant first-year occupancy probability
                     ~1, # constant colonization probability
                     ~1, # constant extinction probability
                     ~effort, # vary detection probability by sampling effort (seconds)
                     stb.gt.UMF) # dataframe
            summary(m3)
            
            # by day
            m4 <-
              colext(~1, # constant first-year occupancy probability
                     ~1, # constant colonization probability
                     ~1, # constant extinction probability
                     ~day, # vary detection probability by day
                     stb.gt.UMF) # dataframe
            summary(m4)
            
            # site covariates that alter detection probability
            # by river kilometer ("rkm")
            m5 <-
              colext(~1, # constant first-year occupancy probability
                     ~1, # constant colonization probability
                     ~1, # constant extinction probability
                     ~rkm, # vary detection probability by day
                     stb.gt.UMF) # dataframe
            summary(m5)
            
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
            
            # by sampling day & sampling effort
            m7 <-
              colext(~1, # constant first-year occupancy probability
                     ~1, # constant colonization probability
                     ~1, # constant extinction probability
                     ~day+effort, # vary detection probability by 
                     # sampling day & sampling effort (seconds)
                     stb.gt.UMF) # dataframe
            summary(m7)
            
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
            
            # by flow and river kilometer
            m9 <-
              colext(~1, # constant first-year occupancy probability
                     ~1, # constant colonization probability
                     ~1, # constant extinction probability
                     ~flow+rkm, # vary detection probability by flow &
                     # river kilometer
                     stb.gt.UMF) # dataframe
            summary(m9)
            
            # by flow and effort
            m10 <-
              colext(~1, # constant first-year occupancy probability
                     ~1, # constant colonization probability
                     ~1, # constant extinction probability
                     ~flow+effort, # vary detection probability by flow & effort
                     stb.gt.UMF) # dataframe
            summary(m10)
            
# Assess model performance for striped bass larger than 300 mm in fork length
            # ----------------------------------------------------------------
  #  Generate a list  and  compare the  models  using AIC 
            
    #  Compare the  models  using AIC 
      AICs <-
          data.frame(Model = 
          c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8","m9", "m10"),
          AIC = c(m1@AIC, m2@AIC, m3@AIC, m4@AIC, m5@AIC, m6@AIC, m7@AIC, 
                  m8@AIC, m9@AIC, m10@AIC))
      AICs
            
            # model 8 performed the best, based on its AIC value,
              # it alters detection by sampling day, sampling effort, 
              # and river kilometer
        m8 <-
          colext(~1, # constant first-year occupancy probability
               ~1, # constant colonization probability
               ~1, # constant extinction probability
               ~day+effort+rkm, # vary detection probability by 
               # sampling day, sampling effort 
               # (in seconds), and river kilometers
               stb.gt.UMF) # dataframe
        summary(m8)
            
            #  Backtransform estimates from logistic to  probability  scale 
            backTransform(m8, type =  "psi")   # First-year occupancy 
              # 0.252% chance of initial occupancy (SE = 0.153)
            backTransform(m8, type =  "col")   # Colonization probability 
              # 0.933% chance of colonization (SE = 0.864)
            backTransform(m8, type =  "ext")   # Extinction probability
              # 0.0322 chance of extinction (SE = 0.623)
            # detection probability cannot be back-transformed to a probability 
              # scale from a logit scale like the other model parameters
              # so use the below steps to get the detection estimate
            lc <- linearComb(m8, c(1,4), type = "det")
            # make an object that stores detection from the model ("m3")
            backTransform(lc)
            # back transform the logistic estimate to a probability estimate
            # detection probability = % ( standard error)
            
        