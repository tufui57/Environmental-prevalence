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
                             
                             # Analogous cells at 100% range must be 100% of grid cells in data2.
                             # Therefore, this function skips the analogous cell search for 100% range to save time.
                             lapply(2:(length(ranges[[1]]) - 1), function(j){
                               analogousCells_in_single_range(p, data2,
                                                              a1 = ranges[[i]][j], 
                                                              climateName = climateNames[i])
                              }
                             )
                           }
                         )
    
    # Find grid cells which appear within (j -1)*10 % range of all variables
    analogCells.size <- lapply(1:length(analogCells.x.Percent.of.a.variable[[1]]), function(i){
      lapply(analogCells.x.Percent.of.a.variable, function(x){ x[[i]][, "id"]}) %>% 
        Reduce(intersect, .) %>% 
        length
      
    }
    )
    
    #analogCells.size <- append(append(list(0), analogCells.size), list(nrow(data2)))
  
  return(analogCells.size)

}

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
  
  analogCells.size <- analogousCells_in_multi_ranges(
    p, data2, ranges, climateNames
    )
          
  # Calculate percentage of area within the neighbourhood over NZ
  ratio <- unlist(analogCells.size) / nrow(data2)
  
  ### Calculate AUC (area under curve) for each current grid cells
  res <- auc(c(0, 1:10)*0.1, c(0, ratio, 1), type="spline")
  
  return(res)
}
# 
# p= scores[1,]
# data2=scores
# ranges<-lapply(climateNames, get_range_breadth, dat=data2)
