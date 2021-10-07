#_______________________________________________________
# Set Up
#_______________________________________________________

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
setwd("C:/Users/wjowa/OneDrive - ucsc.edu/FISHBIO/Summer2021 _STB_Tasks/Task 2/FISHBIO_StanislausRiver_NativeFishPredation/FISHBIO_StanislausRiver_NFP_RCode")

#___________________________________________________________________________
                  # Prepare data for occupancy models
#___________________________________________________________________________
# -------- -----------------------------------------------------------------
  # Align NAs from striped bass observation data ("Det") &
  # corresponding environmental and sampling data ("Cov")
#---------------------------------------------------------------------------
# Environmental & sampling data NAs
  envNAs1.2019 # list of NAs for flow
  envNAs2.2019 # list of NAs for sampling effort (in seconds)
  envNAs3.2019
  envNAs4.2019
  # ------  note how both NAs have identical placement, as
      # indicated by their numbers in the list -----------
  
# Striped bass observation data NAs
  DetNAs <- which(is.na(cap.list.2019.ltSC$STB)==T)
  DetNAs
  # ---- note how NAs vary from those for environmental/sampling data -----
  
    # common NAs b/w env/sampling data ("Cov") & STB obs data ("Det")
     # record #: 38, 39 46, 47, 48, 49, 54, 55, 77, 78, 109, 148, 187, 226, 230
    # varied NAs b/w env/sampling data ("Cov") & STB obs data ("Det")
     # record #: 123, 205, 227, 231, 232, 233, 241, 252, 254, 237, 312
  
  # align NAs between env/sampling data ("Cov") & STB obs data ("Det") 
  
    allNAs <- unique(sort(c(envNAs1.2019, envNAs2.2019, 
                            envNAs3.2019, envNAs4.2019,
                            DetNAs)))
      # create a list of each NA from environmental variables (flow), 
        # effort, & observed striped bass occurrence
  DetNAs[allNAs] <- NA
    
  observation.covariates.2019.scaled$flow[allNAs] <- NA
      # replace values for flow that do not align with
        # NAs for other variables with NAs
          # Note - stacks the 8 flow columns into 1 single column
    observation.covariates.2019.scaled$effort[allNAs] <- NA
      # replace values for effort that do not align with
        # NAs for other variables with NAs
          # Note - stacks the 8 effort columns into 1 single column
    observation.covariates.2019.scaled$day[allNAs] <- NA
      # replace values for sample days (events) that do not align with
        # NAs for other variables with NAs
          # Note - stacks the 8 sample day (event) columns into 1 single column
    
    length(which(is.na(site.covariates.scaled)))
      # check whether each "NA" (italicized) were replaced with "NA" (standard font)
        # by checking the number of "NA" (italicized) entries
    length(which(is.na(observation.covariates.2019.scaled)))
      # ditto
    
 # ---------------- observed striped bass occurrence
    
  # ------------------- STB < 300 mm TL in 2019
    
    STB19_under300 <- as.data.frame(cap.list.2019.ltSC$STB)
    # convert to dataframe
    
    Det <- STB19_under300[,-c(1:3)]
    # remove the 1st to 3rd columns
    
    Det[c(1:39),] <-
      ifelse(Det[c(1:39),]==0,0,1)
    # convert counts of sampled STB to
    # 0s & 1s for detections
    
    # add a column to "Det" indicating the sampling site
      # Det$site <- rep(c(1:39), 1)
     
    # stack observed striped bass occurrences (or detections)
      # by the 8 sampling events
      library(tidyr)
      Det2 <- gather(Det, event, detection)
      
    # remove unnecessary rows
      Det2 <- Det2[-c(313:351),]
    
# ----------------- scaled environmental & sampling data
    
    # 2019
    covariates_19scaled <- 
      as.data.frame(observation.covariates.2019.scaled)
    
    covariates_19scaled <- 
      covariates_19scaled[c(1:312), 
          # recall that the 8 recorded flow, effort, & sampling days for 
            # each sample event stacked for the 39 sample sites
            # (8 recorded variables x 39 sample sites = 312 rows of data)
          c(1,10,27)]
          # the 8 columns the 3 variables were compiled in 3 columns
    # remove covariates that are not of interest
    # (e.g., turbidity, temperature, conductivity, DO)
    
    Cov <- covariates_19scaled
    # store 2019 scaled covariates in the object
    # "Cov" which will be used for other occupancy
    # models for data
  
#----------------------------------------------------------------------------
  # Format data
#----------------------------------------------------------------------------
    
# combine dataframes for env/sampling data ("Cov") & STB obs data ("Det")
    comb_dat <- 
      cbind.data.frame(Det2, # 39 rows & 8 columns of unique data
                                        # Note: repeats 8x to align with
                                        # observations from ("Cov")
                       Cov) # 312 rows and 3 columns of unique data
    
#----------------------------------------------------------------------------
    # Matt's Code for developing occupancy models
#----------------------------------------------------------------------------

# run 2019 estimates for STB < 300 mm
    cap.list.2019.ltSC$STB
    stb.y <- cap.list.2019.ltSC$STB[,-c(1,2,3)]
    dimnames(stb.y)[[2]] <- paste( "y", 1:ncol(stb.y), sep=".")
    number.of.primary.periods <- 4
    primaryperiods <- matrix( as.integer(rep( seq(1,number.of.primary.periods,1) , 
                                              nrow(rkm.dat))),
                              ncol=number.of.primary.periods, byrow=T )
    stbUMF <- unmarkedFrameOccu( y = stb.y,
                                 siteCovs = site.covariates.scaled,
                                 obsCovs = observation.covariates.2019.unscaled ),
                                 numPrimary= number.of.primary.periods,
                                 primaryPeriod=primaryperiods )
    summary(stbUMF)
    # ensure data is consistent with respect to NAs #
    fishNAs <- which( is.na(stb.y) == T )
    allNAs <- unique( sort( c( fishNAs, envNAs1.2019, envNAs2.2019, envNAs3.2019, envNAs4.2019, envNAs5.2019, envNAs6.2019 )))
    observation.covariates.2019.scaled$flow[allNAs] <- NA
    observation.covariates.2019.scaled$temp[allNAs] <- NA
    observation.covariates.2019.scaled$effort[allNAs] <- NA
    observation.covariates.2019.scaled$turbidity[allNAs] <- NA
    observation.covariates.2019.scaled$conductivity[allNAs] <- NA
    observation.covariates.2019.scaled$day[allNAs] <- NA
    length( which( is.na( site.covariates.scaled ))) # should be 0

# _____________________________________________________________________________
                    # Develop occupancy models 
#______________________________________________________________________________

# install.packages("unmarked")
library(unmarked)
  # install & load "unmarked" package, if not performed earlier

# Approach 1A ----------------------------------------------
    
   y <- matrix(comb_dat[,2])
cov1 <- matrix(comb_dat[,3])
cov2 <- matrix(comb_dat[,4])
cov3 <- matrix(comb_dat[,5])

# dataframe for occupancy models
m_dat <- unmarkedMultFrame(y = y, 
                           numPrimary = 4,
                           obsCovs = 
                                   list(cov1 = cov1,
                                        cov2 = cov2,
                                        cov3 = cov3))

# Simplest occupancy model
occ_m1 <- occu(~1 ~1, # Constant detectability & occupancy 
               dat = occu2019dat)
summary(occ_m1) # view results

# Approach 1B -----------------------------------------------
library(dplyr)

# obsolete code (as of 13th Sept. 2021) ---------------------
  # siteCovs19 <- dplyr::select(covariates_19scaled,
                      # flow.flow.1:effort.effort.8)

  # yrCovs19 <- dplyr::select(covariates_19scaled,
                      # starts_with("day"))

  # dat_1B_2019 <- 
    # unmarkedMultFrame(y = STB19_under300,
                    # siteCovs = siteCovs19,
                    # yearlySiteCovs = yrCovs19,
                    # numPrimary = 4)

model_dat <- unmarkedMultFrame(y = stb_obs,
                  numPrimary = 4,
                  obsCovs = list(env_cond, samp_dates))

fm0 <- colext(~1, ~1, ~1, ~1, model_dat)
summary(fm0)

as.matrix(siteCovs19)
fm1 <- colext(~1, ~1, ~1, ~yrCovs19, dat_1A_2019)

# reformat data (unneeded code) ----------------------------
# currently, the above code leaves data formatted 
# in the correct manner for occupancy models
## STB19_under300 <- t(STB19_under300)
# rearrange the data to have columns for each sample site

## STB19_under300 <- STB19_under300[-c(2,3),]
# remove the second and third row

# make the first row the column header
# method 1
# install.packages("janitor")
# janitor::row_to_names(STB19_under300, 1)
# method 2
# names(STB19_under300) <- STB19_under300[1,]
# both methods failed

# rename column headers
## colnames(STB19_under300) <- 
## as.character(STB19_under300[1,])
# make row 1 (SamplingUnitNo) the column header
## STB19_under300 <- STB19_under300[-1,]
# remove row 1
## STB19_under300 <-
## ifelse(STB19_under300==0,0,1)

# 2020
## cap.list.2020.ltSC$STB
# 2021
## cap.list.2021.ltSC$STB

# -------------------- STB > 300 mm TL ----------------------
cap.list.2019.gtSC$STB # 2019
cap.list.2020.gtSC$STB # 2020
cap.list.2021.gtSC$STB # 2021

# covariates
# all covariates
occ_covar <- as.data.frame(observation.covariates)

# sample day
# unscaled
observation.covariates.2019.unscaled$day # 2019
observation.covariates.2020.unscaled$day # 2020
observation.covariates.2021.unscaled$day # 2021

# scaled (mean centering for maximum likelihood calculations)
observation.covariates.2019.scaled$day # 2019
observation.covariates.2020.scaled$day # 2020
observation.covariates.2021.scaled$day # 2021

# flow
# unscaled
observation.covariates.2019.unscaled$flow # 2019
observation.covariates.2020.unscaled$flow # 2020
observation.covariates.2021.unscaled$flow # 2021

# scaled (mean centering for maximum likelihood calculations)
observation.covariates.2019.scaled$flow # 2019
observation.covariates.2020.scaled$flow # 2020
observation.covariates.2021.scaled$flow # 2021   

# effort
# unscaled
observation.covariates.2019.unscaled$effort # 2019
observation.covariates.2020.unscaled$effort # 2020
observation.covariates.2021.unscaled$effort # 2021

# scaled (mean centering for maximum likelihood calculations)
observation.covariates.2019.scaled$effort # 2019
observation.covariates.2020.scaled$effort # 2020
observation.covariates.2021.scaled$effort # 2021
