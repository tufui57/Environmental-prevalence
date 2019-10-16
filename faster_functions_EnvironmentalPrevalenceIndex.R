##################################################################################################
### Faster functions for EP calculation
###################################################################################################


##################################################################################################
### Calculate EP of current climate in the current time within whole target areas
###################################################################################################

multicore_calc_EPcc_within_whole_target_areas <- function(
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
  
  
  # Climate range breadths
  ranges <- lapply(climateNames, get_range_breadth, dat = data1)
  names(ranges) <- climateNames
  
  ep <- llply(1:nrow(data1), function(i){
    
    ###################################################################################### 
    ### EP calculation
    ###################################################################################### 
   return(EP(data1[i,], # a target grid cell
                data1, # grid cells within the neighbourhood
                ranges, # result of get_range_breadth() 
                climateNames # column names of climate variables
    ))
    
  }, .parallel = TRUE)
  
  # Combine climate data (data1) with EP
  ep.d <- cbind(data1[, c(coordinateNames, climateNames)], unlist(ep))
  colnames(ep.d)[ncol(ep.d)] <- "EP"
  
  return(ep.d)
}



##################################################################################################
### Calculate EP of current climate in the past within whole target areas
###################################################################################################

multicore_calc_EPcl_within_whole_target_areas <- function(
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
  ep <- llply(1:nrow(data1), function(i){

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
    return(
      EP(data1[i, ], # a target grid cell
                data2, # grid cells within the neighbourhood
                ranges, # result of get_range_breadth() or ranges_without_outliers()
                climateNames # column names of climate variables
    )
    )
  }, .parallel = TRUE)
  
  # Combine climate data (data1) with EP
  ep.d <- cbind(data1[, c(coordinateNames, climateNames)], unlist(ep))
  colnames(ep.d)[ncol(ep.d)] <- "EP"
  
  return(ep.d)
}

##################################################################################################
### Calculate EP of current climate in the past within user-defined neighbourhood areas
###################################################################################################

multicore_calc_EPcl_within_neighbourhood_areas <- function(
  data1, # data.frame containing data of grid cells to calcualte EP for.
  data2, # data.frame containing data of grid cells to search analogous climates from.
  climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
  coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
  climateRangeStep = 10, # Integer. Breaks of cliamte range breaths.
  neighbourhood.size = NULL 
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
  ep <- llply(1:nrow(data1), function(i){
    
    ######################################################################################
    # Prepare the neighbourhood square of the target cell
    ######################################################################################
    #### Prepare a neighbourhood square to search analogous climate from.
    # IF neighbourhood.size = 20 (km), the radius of the neighbourhood square, "a", is 10000 m (= 10km = 20km/2)
    #a <- neighbourhood.size * 1000 / 2
    dat.x <- analogousCells_in_single_range(data1[i,], data2, (neighbourhood.size * 1000 / 2), coordinateNames[1])
    neighbour.window <- analogousCells_in_single_range(data1[i,], dat.x, (neighbourhood.size * 1000 / 2), coordinateNames[2])
    
    # To get a set of climate range breadths, combine cells within the neighbourhood and the target point.
    neighbour.window.data2_p <- rbind(neighbour.window[, c(coordinateNames, climateNames)], 
                                      data1[i, c(coordinateNames, climateNames)])
    ranges <- lapply(climateNames, get_range_breadth, dat = neighbour.window.data2_p)
    names(ranges) <- climateNames
    
    ###################################################################################### 
    ### EP calculation
    ###################################################################################### 
    return(
      EP(data1[i, ], # a target grid cell
         neighbour.window, # grid cells within the neighbourhood
         ranges, # result of get_range_breadth() or ranges_without_outliers()
         climateNames # column names of climate variables
      )
    )
  }, .parallel = TRUE
  )
  
  # Combine climate data (data1) with EP
  ep.d <- cbind(data1[, c(coordinateNames, climateNames)], unlist(ep))
  colnames(ep.d)[ncol(ep.d)] <- "EP"
  
  return(ep.d)
}

##################################################################################################
### Calculate EP of current climate in the current time within user-defined neighbourhood areas
###################################################################################################

multicore_calc_EPcc_within_neighbourhood_areas <- function(
  
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
    ep <- llply(1:nrow(data1), function(i){
      
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
      return(
        EP(data1[i, ], # a target grid cell
                  neighbour.window, # grid cells within the neighbourhood
                  ranges, # result of get_range_breadth() or ranges_without_outliers()
                  climateNames # column names of climate variables
      )
      )
      
    }, .parallel = TRUE
    )
    
    # Combine climate data (data1) with EP
    ep.d <- cbind(data1[, c(coordinateNames, climateNames)], unlist(ep))
    colnames(ep.d)[ncol(ep.d)] <- "EP"
    
    return(ep.d)
  }
  

