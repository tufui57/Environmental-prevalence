

####################################################################################################
source(".\\GitHub\\Environmental-prevalence\\functions_EnvironmentalPrevalenceIndex.R")
### Load climate data
load(".\\Scores_Acaena_landcover5km.data")
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")



###################################################################################################
### Identify cells with analogous conditions in two climate variables
###################################################################################################

sapply_analogousCells_in_multi_ranges <- function(p, # a target grid cell
                                           data2, # grid cells to search analogous climates for.
                                           ranges, # result of get_range_breadth()
                                           climateNames # column name of climate variable
){
  
  data2$id <- row.names(data2) 
  # Identify grid cells with values within (j -1)*10 % range of each variable
  analogCells.x.Percent.of.a.variable <- sapply(1:length(climateNames),
                                                function(i){
                                                  
                                                  # Analogous cells at 100% range must be 100% of grid cells in data2.
                                                  # Therefore, this function skips the analogous cell search for 100% range to save time.
                                                  lapply(2:(length(ranges[[1]]) - 1), function(j){
                                                    analogousCells_in_single_range(p, data2,
                                                                                   a1 = ranges[[i]][j], 
                                                                                   climateName = climateNames[i])
                                                  }
                                                  )
                                                }, simplify = F
  )
  
  # Find grid cells which appear within (j -1)*10 % range of all variables
  analogCells.size <- sapply(1:length(analogCells.x.Percent.of.a.variable[[1]]), function(i){
    lapply(analogCells.x.Percent.of.a.variable, function(x){ x[[i]][, "id"]}) %>% 
      Reduce(intersect, .) %>% 
      length
    
  }, simplify = F
  )
  
  #analogCells.size <- append(append(list(0), analogCells.size), list(nrow(data2)))
  
  return(analogCells.size)
  
}

###################################################################################################
### Comapre lapply and for
###################################################################################################


source(".\\GitHub\\Environmental-prevalence\\functions_EnvironmentalPrevalenceIndex.R")
library(dplyr)
### Load climate data
load(".\\Scores_Acaena_landcover5km.data")
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

p = scores[1000,]
data1=scores
data2=scores
ranges<-lapply(climateNames, get_range_breadth, dat=data2)

system.time(
  {test <- list()
  for(i in 1:100){
    test2 <- sapply_analogousCells_in_multi_ranges(data1[i, ], data2, 
                                          ranges, climateNames = climateNames)
  }
  } 
)


system.time(
  test.l <- lapply(1:100, function(i){sapply_analogousCells_in_multi_ranges(data1[i,], data2 =  data2, 
                                               ranges = ranges, climateNames = climateNames)
    }
  )
  )

system.time(
  test.s <- sapply(1:100, function(i){sapply_analogousCells_in_multi_ranges(data1[i,], data2 =  data2, 
                                                           ranges = ranges, climateNames = climateNames)
  }, simplify = F
  )
)


system.time(
  {test <- list()
  for(i in 1:100){
    test2 <- analogousCells_in_multi_ranges(data1[i, ], data2, 
                                                   ranges, climateNames = climateNames)
  }
  } 
)



