###################################################################################################
### Calculate Environmental Prevalence Index (EP)
###################################################################################################

source(".\\GitHub\\Environmental-prevalence\\functions_EnvironmentalPrevalenceIndex.R")


##########################################################################
###### EP of current climates under current conditions; EPcc
###########################################################################

### Load current cliamte data at 5km resolution
load(".\\Scores_Acaena_landcover5km.data")
climateNames <- c("bioclim1", "bioclim6", "bioclim12")

### EP within Whole NZ
ep.i <- calc_EP(data1 = scores,
                  data2 = scores,
                  climateNames = climateNames,
                  coordinateNames = c("x","y")
                  )
save(ep.i, file = "EPcc_NZ_3var.data")

### Check the map of EP
ggplot(ep.i) +
  geom_raster(aes_string(x = "x", y = "y", fill="EP"))


### EP within i km neighbourhood
for(n in c(20,50,100
           )){
  
  ep.i <- calc_EP(data1 = scores,
                    data2 = scores,
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
ep.i <- calc_EP(scores.lgm, 
                  scores.lgm, 
                  climateNames,
                  coordinateNames = c("x","y")
                  )
# Save
save(ep.i, file = "EPll_4var.data")

# EP within i km neighbourhood
for(i in c(20,50,100)){
  ### i km neighbourhood window
  ep.i <- calc_EP(scores.lgm,
                    scores.lgm,
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
ep.i <- calc_EP(scores,
                  scores.lgm,
                  climateNames,
                  coordinateNames = c("x","y")
                  )
save(ep.i, file = "EPcl_NZ_4var.data")

### EPcl within i km neighbourhood
for(i in c(20,50,100)){
  
  ep.i <- calc_EP(data1 = scores,
                  data2 = scores.lgm,
                  climateNames = climateNames,
                  coordinateNames = c("x","y"),
                  neighbourhood.size = i
  )
  # Save
  save(ep.i, file = paste("EPcl_", i,"kmNeighbourhood.data", sep=""))
  
}