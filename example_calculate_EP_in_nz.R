###################################################################################################
### Calculate Environmental Prevalence Index (EP)
###################################################################################################

source(".\\Environmental-prevalence\\faster_functions_EnvironmentalPrevalenceIndex.R")
source(".\\Environmental-prevalence\\functions_EnvironmentalPrevalenceIndex.R")

library(schoolmath)
library(doFuture)
library(foreach)
library(plyr)

### Setup for multi-core use
registerDoFuture()  ## tells foreach futures should be used
plan(multisession)  ## specifies what type of futures

### Load climate data
load("Y:\\Scores_Acaena_landcover5km.data")
load("Y:\\Scores_LGM_mainisland_worldclim1_5km.data")

climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")


##########################################################################
###### EP of current climates under current conditions; EPcc
###########################################################################

### EP within Whole NZ
ep.i <- multicore_calc_EPcc_within_whole_target_areas(data1 = scores,
                  climateNames = climateNames,
                  coordinateNames = c("x","y")
                  )
save(ep.i, file = "EPcc_NZ_4var_test.data")


### EP within i km neighbourhood
for(n in c(20,50,100
           )){
  
  ep.i <- multicore_calc_EPcc_within_neighbourhood_areas(data1 = scores,
                    climateNames = climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = n
                    )
  # Save
  save(ep.i, file = paste("EPcc_5km_", n,"kmNeighbourhood.data", sep=""))
  
}


############################################################################
###### EP of the LGM climates under LGM conditions; EPll
############################################################################

# LGM climate data for Zealandia
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
climateNames <- c("bi1", "bi6", "bi12", "bi15")

# EP within Whole NZ
ep.i <- multicore_calc_EPcc_within_whole_target_areas(scores.lgm, 
                  climateNames,
                  coordinateNames = c("x","y")
                  )
# Save
save(ep.i, file = "EPll_4var.data")

# EP within i km neighbourhood
for(i in c(20,50,100)){
  ### i km neighbourhood window
  ep.i <- multicore_calc_EPcc_within_neighbourhood_areas(scores.lgm,
                    climateNames = climateNames,
                    coordinateNames = c("x","y"),
                    neighbourhood.size = i
  )
  # Save
  save(ep.i, file = paste("EPll_5km_", i,"kmNeighbourhood.data", sep=""))
  
}


############################################################################
###### EP of current climates under LGM conditions; EPcl
############################################################################

### Load climate data
load(".\\Scores_Acaena_landcover5km.data")
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
colnames(scores.lgm) <- gsub("bi","bioclim", colnames(scores.lgm))
climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15") 



### EPcl within Whole NZ
ep.i <- multicore_calc_EPcl_within_whole_target_areas(scores,
                  scores.lgm,
                  climateNames,
                  coordinateNames = c("x","y")
                  )
save(ep.i, file = "EPcl_NZ_4var.data")

### EPcl within i km neighbourhood
for(i in c(20,50,100)){
  
  ep.i <- multicore_calc_EPcl_within_neighbourhood_areas(data1 = scores,
                  data2 = scores.lgm,
                  climateNames = climateNames,
                  coordinateNames = c("x","y"),
                  neighbourhood.size = i
  )
  # Save
  save(ep.i, file = paste("EPcl_", i,"kmNeighbourhood.data", sep=""))
  
}