##################################################################################################
### Try to make faster function for EP calculation
###################################################################################################

library(schoolmath)

calc_EPcc_within_whole_target_areas <- function(
  data1, # data.frame containing data of grid cells to calcualte EP for.
  climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
  coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
  climateRangeStep = 10 # Integer. Breaks of cliamte range breaths.

){
  
  ###################################################################################### 
  ### Check missing values in data1 and data2
  ###################################################################################### 
  # The data cannot have missing values for EP calculation.
  
  if(any(is.na(data1[, climateNames]))){
    warning('EP cannot be calcualted for grid cells with NA values. \nThe returned data frame does not have those grid cells.')
  }
  data1 <- data1[complete.cases(data1[, c(coordinateNames, climateNames)]), ]

  ##########################################################################################################################
  ### Show Warning messages, if the unit of neighbourhood area seems different from the units of coordinates
  ########################################################################################################################## 
  
  if(any(is.decimal(data1[, coordinateNames[1]])) && is.null(neighbourhood.size) == FALSE ){
    warning('Units of your coordinates might not be kilo meter. \nIf so, the returned EP values are wrong. \nConvert your coordinates into km and re-run.')
  }
  
  ###################################################################################### 
  ### Calculate EP
  ###################################################################################### 
  ep <- list()
  
  # Climate range breadths
  ranges <- lapply(climateNames, get_range_breadth, dat = data1)
  names(ranges) <- climateNames
    
  for(i in 1:nrow(data1)){

    ###################################################################################### 
    ### EP calculation
    ###################################################################################### 
    ep[i] <- EP(data1[i,], # a target grid cell
                data1, # grid cells within the neighbourhood
                ranges, # result of get_range_breadth() 
                climateNames # column names of climate variables
    )

  }
  
  # Combine climate data (data1) with EP
  ep.d <- cbind(data1[, c(coordinateNames, climateNames)], unlist(ep))
  colnames(ep.d)[ncol(ep.d)] <- "EP"
  
  return(ep.d)
}


### Testing the modified function
load(".\\Scores_Acaena_landcover5km.data")
climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

source(".\\GitHub\\Environmental-prevalence\\functions_EnvironmentalPrevalenceIndex.R")


system.time(
  test <- calc_EPcc_within_whole_target_areas(data1 = scores,
                  climateNames = climateNames,
                  coordinateNames = c("x","y")
  )
)
# user  system elapsed 
# 8.20    0.08    8.37


system.time(
  test2 <- calc_EP(data1 = scores, data2 = scores,
                                              climateNames = climateNames,
                                              coordinateNames = c("x","y")
  )
)

# user  system elapsed 
# 9.73    0.04    9.80



##################################################################################################
### Calculate EP of current climate in the past within whole target areas
###################################################################################################

calc_EPcl_within_whole_target_areas <- function(
  data1, # data.frame containing data of grid cells to calcualte EP for.
  data2, # data.frame containing data of grid cells to search analogous climates from.
  climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
  coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
  climateRangeStep = 10 # Integer. Breaks of cliamte range breaths.

){
  
  ###################################################################################### 
  ### Check missing values in data1 and data2
  ###################################################################################### 
  # The data cannot have missing values for EP calculation.
  
  if(any(is.na(data1[, climateNames]))){
    warning('EP cannot be calcualted for grid cells with NA values. \nThe returned data frame does not have those grid cells.')
  }
  data1 <- data1[complete.cases(data1[, c(coordinateNames, climateNames)]), ]
  data2 <- data2[complete.cases(data2[, c(coordinateNames, climateNames)]), ]
  
  ##########################################################################################################################
  ### Show Warning messages, if the unit of neighbourhood area seems different from the units of coordinates
  ########################################################################################################################## 
  
  if(any(is.decimal(data1[, coordinateNames[1]])) && is.null(neighbourhood.size) == FALSE ){
    warning('Units of your coordinates might not be kilo meter. \nIf so, the returned EP values are wrong. \nConvert your coordinates into km and re-run.')
  }
  
  ###################################################################################### 
  ### Calculate EP
  ###################################################################################### 
  ep <- list()
  
  for(i in 1:nrow(data1)){

    ######################################################################################
    # Prepare the neighbourhood square of the target cell
    ######################################################################################

        # To get a set of climate range breadths, combine cells of data1 and data2 within the neighbourhood
        neighbour.window.data2_p <- rbind(data2[c(coordinateNames, climateNames)], 
                                          data1[i, c(coordinateNames, climateNames)])
        ranges <- lapply(climateNames, get_range_breadth, dat = neighbour.window.data2_p)
        names(ranges) <- climateNames
 
    ###################################################################################### 
    ### EP calculation
    ###################################################################################### 
    ep[i] <- EP(data1[i, ], # a target grid cell
                data2, # grid cells within the neighbourhood
                ranges, # result of get_range_breadth() or ranges_without_outliers()
                climateNames # column names of climate variables
    )
  }
  
  # Combine climate data (data1) with EP
  ep.d <- cbind(data1[, c(coordinateNames, climateNames)], unlist(ep))
  colnames(ep.d)[ncol(ep.d)] <- "EP"
  
  return(ep.d)
}



### Load climate data
load(".\\Scores_Acaena_landcover5km.data")
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")
colnames(scores.lgm) <- gsub("bi","bioclim", colnames(scores.lgm))
climateNames <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15") 

system.time(
  test <- calc_EPcl_within_whole_target_areas(data1 = scores[1:300,],
                                              data2 = scores.lgm,
                                              climateNames = climateNames,
                                              coordinateNames = c("x","y")
  )
)


system.time(
  test2 <- calc_EP(scores[1:300,],
                   scores.lgm,
                   climateNames,
                   coordinateNames = c("x","y")
  )
  
)



##################################################################################################
### Calculate EP of current climate in the current time within user-defined neighbourhood areas
###################################################################################################

calc_EPcc_within_neighbourhood_areas <- function(
  
    data1, # data.frame containing data of grid cells to calcualte EP for.
    climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
    coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
    climateRangeStep = 10, # Integer. Breaks of cliamte range breaths.
    neighbourhood.size = NULL # Size of neighbourhood area to search analogous climates. 
    # neighbourhood.size corresponds to length of one side of square and must be given in km.
    
  ){
    
    ###################################################################################### 
    ### Check missing values in data1 and data2
    ###################################################################################### 
    # The data cannot have missing values for EP calculation.
    
    if(any(is.na(data1[, climateNames]))){
      warning('EP cannot be calcualted for grid cells with NA values. \nThe returned data frame does not have those grid cells.')
    }
    data1 <- data1[complete.cases(data1[, c(coordinateNames, climateNames)]), ]

    ##########################################################################################################################
    ### Show Warning messages, if the unit of neighbourhood area seems different from the units of coordinates
    ########################################################################################################################## 
    
    if(any(is.decimal(data1[, coordinateNames[1]])) && is.null(neighbourhood.size) == FALSE ){
      warning('Units of your coordinates might not be kilo meter. \nIf so, the returned EP values are wrong. \nConvert your coordinates into km and re-run.')
    }
    
    
    ###################################################################################### 
    ### Calculate EP
    ###################################################################################### 
    ep <- list()
    
    for(i in 1:nrow(data1)){
      
      ######################################################################################
      # Prepare the neighbourhood square of the target cell
      ######################################################################################

          # A neighbourhood square to search analogous climate from.
          # IF neighbourhood.size = 20 (km), the radius of the neighbourhood square, "a", is 10000 m (= 10km = 20km/2)
          dat.x <- analogousCells_in_single_range(data1[i, ], data1, 
                                                  neighbourhood.size * 1000 / 2, coordinateNames[1])
          neighbour.window <- analogousCells_in_single_range(data1[i, ], dat.x,
                                                             neighbourhood.size * 1000 / 2, coordinateNames[2])
          
        ranges <- lapply(climateNames, get_range_breadth, dat = neighbour.window)
        names(ranges) <- climateNames

      
      
      ###################################################################################### 
      ### EP calculation
      ###################################################################################### 
      ep[i] <- EP(data1[i, ], # a target grid cell
                  neighbour.window, # grid cells within the neighbourhood
                  ranges, # result of get_range_breadth() or ranges_without_outliers()
                  climateNames # column names of climate variables
      )
      
    }
    
    # Combine climate data (data1) with EP
    ep.d <- cbind(data1[, c(coordinateNames, climateNames)], unlist(ep))
    colnames(ep.d)[ncol(ep.d)] <- "EP"
    
    return(ep.d)
  }
  

system.time(
  test <- calc_EPcc_within_neighbourhood_areas (data1 = scores[1:300,],
                                              climateNames = climateNames,
                                              coordinateNames = c("x","y"),
                                              neighbourhood.size = 20
  )
)
user  system elapsed 
8.97    0.03    9.20

system.time(
  test2 <- calc_EP(scores[1:300,],
                   scores[1:300,],
                   climateNames,
                   coordinateNames = c("x","y"),
                   neighbourhood.size = 20
  )
  
)

user  system elapsed 
9.97    0.06   10.05 


##################################################################################################
### Calculate EP of current climate in the past within user-defined neighbourhood areas
###################################################################################################

calc_EPcl_within_neighbourhood_areas <- function(
  
  data1, # data.frame containing data of grid cells to calcualte EP for.
  data2,
  climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
  coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
  climateRangeStep = 10, # Integer. Breaks of cliamte range breaths.
  neighbourhood.size = NULL # Size of neighbourhood area to search analogous climates. 
  # neighbourhood.size corresponds to length of one side of square and must be given in km.
  
){
  
  ###################################################################################### 
  ### Check missing values in data1 and data2
  ###################################################################################### 
  # The data cannot have missing values for EP calculation.
  
  if(any(is.na(data1[, climateNames]))){
    warning('EP cannot be calcualted for grid cells with NA values. \nThe returned data frame does not have those grid cells.')
  }
  data1 <- data1[complete.cases(data1[, c(coordinateNames, climateNames)]), ]
  data2 <- data2[complete.cases(data2[, c(coordinateNames, climateNames)]), ]
  
  ##########################################################################################################################
  ### Show Warning messages, if the unit of neighbourhood area seems different from the units of coordinates
  ########################################################################################################################## 
  
  if(any(is.decimal(data1[, coordinateNames[1]])) && is.null(neighbourhood.size) == FALSE ){
    warning('Units of your coordinates might not be kilo meter. \nIf so, the returned EP values are wrong. \nConvert your coordinates into km and re-run.')
  }
  
  
  
  ###################################################################################### 
  ### Calculate EP
  ###################################################################################### 
  ep <- list()
  
  for(i in 1:nrow(data1)){
    
    ######################################################################################
    # Prepare the neighbourhood square of the target cell
    ######################################################################################
    # Prepare a neighbourhood square to search analogous climate from.
        # IF neighbourhood.size = 20 (km), the radius of the neighbourhood square, "a", is 10000 m (= 10km = 20km/2)
        dat.x <- analogousCells_in_single_range(data1[i, ], data2, 
                                                neighbourhood.size * 1000 / 2, coordinateNames[1])
        neighbour.window <- analogousCells_in_single_range(data1[i, ], dat.x, 
                                                           neighbourhood.size * 1000 / 2, coordinateNames[2])
        
        # To get a set of climate range breadths, combine cells within the neighbourhood and the target point.
        neighbour.window.data2_p <- rbind(neighbour.window[c(coordinateNames, climateNames)], 
                                          data1[i, c(coordinateNames, climateNames)])
        ranges <- lapply(climateNames, get_range_breadth, dat = neighbour.window.data2_p)
        names(ranges) <- climateNames

    
    ###################################################################################### 
    ### EP calculation
    ###################################################################################### 
    ep[i] <- EP(data1[i, ], # a target grid cell
                neighbour.window, # grid cells within the neighbourhood
                ranges, # result of get_range_breadth() 
                climateNames # column names of climate variables
                )
    
        }
  
  # Combine climate data (data1) with EP
  ep.d <- cbind(data1[, c(coordinateNames, climateNames)], unlist(ep))
  colnames(ep.d)[ncol(ep.d)] <- "EP"
  
  return(ep.d)
}

system.time(
  test <- calc_EPcl_within_neighbourhood_areas(
    scores[1:300,],
    scores.lgm,
    climateNames = climateNames,
    coordinateNames = c("x","y"),
    neighbourhood.size = 20
  )
)

system.time(
  test <- calc_EP(
    scores[1:300,],
    scores.lgm,
    climateNames = climateNames,
    coordinateNames = c("x","y"),
    neighbourhood.size = 20
  )
)


