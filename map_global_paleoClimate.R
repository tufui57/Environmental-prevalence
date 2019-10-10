#################################################################################
### Maps of past global climates
#################################################################################

library(ggplot2)
library(gridExtra)
library(grid)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)

########################################
### Data preparation
########################################


headers <- read.csv("Y:\\Writings\\Thesis\\3rd chapter\\Publication\\Climate data\\CRU_biocli_global 0fields.txt") 
climate <- read.table("Y:\\Writings\\Thesis\\3rd chapter\\Publication\\Climate data\\cru_biocli_global_0ka_RO")

colnames(climate) <- c("x","y", paste("bio", c(1,6,12,15), sep=""))

climateNames = 


###############################################################
# Prepare a polygon of LGM terrestrial area
###############################################################

map_globalclimate <- function(i){
  
  ggplot(data= wor2) +
  geom_raster(aes_string(x = "x", y = "y", fill = paste("bio", i, sep=""))) +
  scale_fill_gradientn(colours = c("blue", "white", "red"), 
                         guide = "colorbar", na.value = "black") +
  # legend position inside plot at topleft
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        plot.title = element_text(family = "Times New Roman", size = 20),
        panel.background =  element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )
}

i = 1
png(paste("Y://Climate_map_world_bio", i, ".png", sep=""), width = 1200, height = 650)
map_globalclimate(i)
dev.off()
i = 6
png(paste("Y://Climate_map_world_bio", i, ".png", sep=""), width = 1200, height = 650)
map_globalclimate(i)
dev.off()

i = 12
png(paste("Y://Climate_map_world_bio", i, ".png", sep=""), width = 1200, height = 650)

ggplot(data= wor2) +
  geom_raster(aes_string(x = "x", y = "y", fill = paste("bio", i, sep=""))) +
  scale_fill_continuous(low="white", high="blue", 
                        guide = "colorbar", na.value = "black") +
  # legend position inside plot at topleft
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        plot.title = element_text(family = "Times New Roman", size = 20),
        panel.background =  element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

dev.off()

i = 15
png(paste("Y://Climate_map_world_bio", i, ".png", sep=""), width = 1200, height = 650)

ggplot(data= wor2) +
  geom_raster(aes_string(x = "x", y = "y", fill = paste("bio", i, sep=""))) +
  scale_fill_continuous(low="green", high="red", 
                        guide = "colorbar", na.value = "black") +
  # legend position inside plot at topleft
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        plot.title = element_text(family = "Times New Roman", size = 20),
        panel.background =  element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

dev.off()


