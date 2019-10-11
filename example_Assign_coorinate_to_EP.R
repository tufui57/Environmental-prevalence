load(".//EPcc_world.data") # this loads EP values, "ep.world"
data1 <- read.csv(".//worldclim.csv") # wold climate data

climateNames = paste("bio", c(1,6,12,15), sep="")
coordinateNames = c("x","y")

### Remove NA values
data.noNA <- data1[complete.cases(data1[, c(coordinateNames, climateNames)]), ] 

### Assign coordinates to EP
res <- data.frame(cbind(wor2, unlist(ep.world)))
colnames(res)[ncol(res)] <- "EP"

### Check the map of EP
library(ggplot2)
ggplot(res) +
  geom_raster(aes_string(x = "x", y = "y", fill="EP"))


                 