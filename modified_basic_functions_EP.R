######################################################################################
### Environmental Prevalence Index (EP) 
######################################################################################

# This file was created and modified by Miki Nomura.

### Steps for the EP calculation
# 1. Prepare a x a km2 neighbourhood square for a target grid cell. 
# For the cells along coastlines, the neighbourhood can overlap with the sea.
# 2. Prepare 11 climate range breadth steps (0, 10%, ..., 90%, 100% of the total range breadth of cells within the neighbourhood).
# At 100% range step, the climate range, (climate condition of target cell - the range) - (climate condition of target cell + the range), 
# must cover all climate conditions of any grid cells in NZ.
# 3. Identify the cells within the neighbourhood, cells with analogous climates, for each climate range step and variable.
# 6. Calculate the proportion of the cells with analogous climates over all grid cells in NZ.
# 7. Draw a graph which its x axis is k (0, 10%, ..., 90%, 100%) and the y axis is the proportion.
# 8. Calculate Area Under the Curve. The AUC is EP for the target grid cell.

######################################################################################
# Breadth of environmental variable * 0, 10%, ..., 90%, 100%
######################################################################################

get_range_breadth <- function(dat, # data.frame
                              climateName, # a column name of climate variable
                              climateRangeStep = 10 # Integer. Percentage of climate range breadths.
                              ){
  
  # NOTE; the 100% range breath MUST cover the total range of climate variables.
  # that is, the 100% range, from (the target cell - 100% range breath) to (the target cell + 100% range breath),
  # must cover climate values of all grid cells.
  return((max(dat[,climateName]) - min(dat[,climateName]) ) * (c(0, 1:climateRangeStep) / climateRangeStep)) 

}

######################################################################################
### Identify grid cells with analogous conditions in a climate variable
######################################################################################

analogousCells_in_single_range <- function(p, # a target grid cell
                                           data2, # grid cells to search analogous climates for.
                                           a1, # a range of climate variable or coordinate 
                                           climateName # column name of climate variable
                                           ){
  # Identify grid cells of data2 which meet the following condition; (p- a1) <= data2 <= (p + a1)
  dat.plus <- data2[(data2[, climateName] <= (p[, climateName] + a1)), ]
  return(
    dat.plus[(dat.plus[, climateName] >= (p[, climateName] - a1)), ]
         )

}


###################################################################################################
### Identify cells with analogous conditions in two climate variables
###################################################################################################

analogousCells_in_multi_ranges <- function(p, # a target grid cell
                                           data2, # grid cells to search analogous climates for.
                                           ranges, # result of get_range_breadth()
                                           climateNames # column name of climate variable
                                           ){
    
    data2$id <- row.names(data2) 
    # Identify grid cells with values within (j -1)*10 % range of each variable
    analogCells.x.Percent.of.a.variable <- lapply(1:length(climateNames),
                           function(i){
                             
                             lapply(2:length(ranges[[1]]), function(j){
                               analogousCells_in_single_range(p, data2,
                                                              a1 = ranges[[i]][j], 
                                                              climateName = climateNames[i])
                              }
                             )
                           }
                         )
    
    # Find grid cells which appear within (j -1)*10 % range of all variables
    analogCells.size <- sapply(1:length(analogCells.x.Percent.of.a.variable[[1]]), function(i){
      lapply(analogCells.x.Percent.of.a.variable, "[[", i, "[", "id") %>% 
        Reduce(function(...) merge(..., by='id'), .) %>% 
        # Get just the number of grid cells with analogous climates
        nrow
      }
    )
  
  return(analogCells.size)

}

test <-analogCells.x.Percent.of.a.variable[[1]]
res<-list()
res[[1]] <- test[[1]]
for(i in 1:(length(test) - 1)){
 res[[i + 1]] <- merge(res[[i]], test[[i + 1]], by=c("id", "x","y"))
}

analogCells.size <- sapply(1:length(analogCells.x.Percent.of.a.variable[[1]]), function(i){
  
  test <- lapply(1:length(climateNames), function(x){analogCells.x.Percent.of.a.variable[[x]][[i]]})
  res<-list()
  res[[1]] <- test[[1]]
  for(i in 1:(length(test) - 1)){
    res[[i + 1]] <- merge(res[[i]], test[[i + 1]][,"id"], by="id")
  }
  
  return(nrow(res[[length(test)]]))
}
)


###################################################################################################
### Identify cells with analogous conditions in two climate variables
### Original
###################################################################################################

slow_analogousCells_in_multi_ranges <- function(p, # a target grid cell
                                           data2, # grid cells to search analogous climates for.
                                           ranges, # result of get_range_breadth()
                                           climateNames # column name of climate variable
){
  
  ### If just one variable is given 
  if(length(climateNames) == 1){
    
    neighbours.size <- list()
    
    for(x in 2:length(ranges[[1]])){
      
      # Get cells within x * 10 % range of variable 1
      neighbours.size[[x]] <- analogousCells_in_single_range(p, data2,
                                                             a1 = ranges[[1]][x], 
                                                             climateNames)
    }
    
  }else{
    
    ### If more than one variables are given 
    neighbours.size <- list()
    
    for(j in 2:length(ranges[[1]])){
      
      neighbours <- list()
      
      # Get cells within (j -1)*10 % neighbourhood of variable 1
      neighbours[[1]] <- analogousCells_in_single_range(p, data2,
                                                        a1 = ranges[[1]][j], 
                                                        climateNames[1])
      
      for(i in 1:(length(climateNames)-1)){
        
        # Get cells within (j -1)*10 % neighbourhood of variable i+1
        neighbours[[i+1]] <- analogousCells_in_single_range(p, neighbours[[i]],
                                                            a1 = ranges[[i+1]][j], 
                                                            climateNames[i+1])
      }
      # Cells within (j -1)*10 % neighbourhood of all the variables
      neighbours.size[[j]] <- neighbours[[length(climateNames)]]
    }
  }
  
  
  return(neighbours.size)
}


library(dplyr)

system.time(
  slow_analogousCells_in_multi_ranges(p, data2, ranges, climateNames) %>% 
sapply(., nrow)
)

system.time(
  {
  options(warn = -1)
  analogousCells_in_multi_ranges(p, data2, ranges, climateNames)
  options(warn = 0)
  }
)

######################################################################################
### Function to calcualte AUC
######################################################################################

# This function is copied from the following GitHub repository; https://rdrr.io/cran/MESS/src/R/auc.R
auc <-
  function(x, y, from = min(x), to = max(x), type=c("linear", "spline"), absolutearea=FALSE, ...){
    type <- match.arg(type)
    
    if (length(x) != length(y))
      stop("x and y must have the same length")
    if (length(unique(x)) < 2)
      return(NA)
    
    if (type=="linear") {
      
      ## Boost all y's to be non-negative
      if (absolutearea)
        y <- y - min(y)
      
      values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
      res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
      
      ## Remove the rectangle we artificially introduced above
      if (absolutearea)
        res <- res - min(y)*(max(x) - min(x))
      
    } else {
      if (absolutearea)
        myfunction <- function(z) { abs(splinefun(x, y, method="natural")(z)) }
      else
        myfunction <- splinefun(x, y, method="natural")
      
      res <- integrate(myfunction, lower=from, upper=to)$value
    }
    
    res
  }

###################################################################################################
### EP
###################################################################################################
# EP of the time at the time (e.g. EP of the current climate at the current time)

EP <- function(p, # a target grid cell
               data2, # grid cells to search analogous climates for.
               ranges, # result of get_range_breadth()
               climateNames # column name of climate variable
               ){
  
  neighbours.size <- analogousCells_in_multi_ranges(
    p, data2, ranges, climateNames
    )
  
  # Calculate percentage of area within the neighbourhood over NZ
  ratio <- lapply(2:length(ranges[[1]]),
                  function(j){
                    # Find points of a group within neighbourhood of another group of points 
                    nrow(neighbours.size[[j]]) / nrow(data2)
                  }
  )
  
  ### Calculate AUC (area under curve) for each current grid cells
  res <- auc(c(0, 1:10)*0.1, c(0, ratio), type="spline")
  
  return(res)
}



##################################################################################################
### Calculate EP of current climate in the current climate at 5km resolution
###################################################################################################
library(schoolmath)

calc_EP <- function(
  data1, # data.frame containing data of grid cells to calcualte EP for.
  data2, # data.frame containing data of grid cells to search analogous climates from.
  climateNames, # Vector. Column names of environmental variables to search for analogous environment conditions
  coordinateNames, # Vector with two elements. Column names of coodinate in data1 & data2
  climateRangeStep = 10, # Integer. Breaks of cliamte range breaths.
  neighbourhood.size = NULL, # Size of neighbourhood area to search analogous climates. 
                             # neighbourhood.size corresponds to length of one side of square and must be given in km.
  standardize = F, # Standardize climate data
  ordination = F, # use of ordination axes instead of climate variables
  outlierRemoval = F, # remove climate outliers from data to search analogous climates from.
  outlierPercent = 0

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
  
  ########################################################################################### 
  # Check if the use of ordination axes instead of climate variables makes differnce in EP.
  ###########################################################################################
  if(ordination == TRUE){
    # get env. corrdinates (PCA axes)
    pca <- prcomp(data1[, climateNames],
                  center = TRUE,
                  scale. = TRUE)
    data1 <- data.frame(data1[, c(climateNames, "x", "y")], pca$x[, 1:4])
  }
  
  ###################################################################################### 
  ### Check if standerdization makes differnce in EP.
  ###################################################################################### 
  if(standardize == TRUE){
    data1 <- cbind(data1[, !(colnames(data1) %in% climateNames)], scale(data1[, climateNames]))
  }
  
  
  ###################################################################################### 
  ### Calculate EP
  ###################################################################################### 
  ep <- list()
  
  for(i in 1:nrow(data1)){
    
    # The target grid cell
    p <- data1[i, ]
    
    ######################################################################################
    # Prepare the neighbourhood square of the target cell
    ######################################################################################
    
    ## If data1 and data2 are the same 
    if(identical(data1, data2)){
      
      if(is.null(neighbourhood.size) == FALSE){
        
        # A neighbourhood square to search analogous climate from.
        # IF neighbourhood.size = 20 (km), the radius of the neighbourhood square, "a", is 10000 m (= 10km = 20km/2)
        a <- neighbourhood.size * 1000 / 2
        dat.x <- analogousCells_in_single_range(p, data2, a, coordinateNames[1])
        neighbour.window <- analogousCells_in_single_range(p, dat.x, a, coordinateNames[2])
        
      }else{
        
        #  Cells to be searched for analogous climates
        neighbour.window <- data2
      }
      
      ranges <- lapply(climateNames, get_range_breadth, dat = neighbour.window)
      names(ranges) <- climateNames
      
    }else{

      ## If data1!= data2
      if(is.null(neighbourhood.size)){

        #  Cells to be searched for analogous climates
        neighbour.window <- data2

        # To get a set of climate range breadths, combine cells of data1 and data2 within the neighbourhood
        neighbour.window.data2_p <- rbind(neighbour.window[c(coordinateNames, climateNames)], 
                                          p[c(coordinateNames, climateNames)])
        ranges <- lapply(climateNames, get_range_breadth, dat = neighbour.window.data2_p)
        names(ranges) <- climateNames
      

      }else{
        
        # Prepare a neighbourhood square to search analogous climate from.
        # IF neighbourhood.size = 20 (km), the radius of the neighbourhood square, "a", is 10000 m (= 10km = 20km/2)
        a <- neighbourhood.size * 1000 / 2
        dat.x <- analogousCells_in_single_range(p, data2, a, coordinateNames[1])
        neighbour.window <- analogousCells_in_single_range(p, dat.x, a, coordinateNames[2])
        
        # To get a set of climate range breadths, combine cells within the neighbourhood and the target point.
        neighbour.window.data2_p <- rbind(neighbour.window[c(coordinateNames, climateNames)], 
                                          p[c(coordinateNames, climateNames)])
        ranges <- lapply(climateNames, get_range_breadth, dat = neighbour.window.data2_p)
        names(ranges) <- climateNames
      }
      
      
    }
    
    
    ###################################################################################### 
    ### EP calculation
    ###################################################################################### 
    ep[i] <- EP(p, # a target grid cell
                  neighbour.window, # grid cells within the neighbourhood
                  ranges, # result of get_range_breadth() or ranges_without_outliers()
                  climateNames # column names of climate variables
    )
    
    print(paste("finished the cell number", i))
  }
  
  # Combine climate data (data1) with EP
  ep.d <- cbind(data1[, c(coordinateNames, climateNames)], unlist(ep))
  colnames(ep.d)[ncol(ep.d)] <- "EP"
  
  return(ep.d)
}
