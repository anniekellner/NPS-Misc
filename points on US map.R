# points on US map.R

# initial code to plot NPVuln results on US map

# v1 26 May 2021

#  Needs:
#    - to categorize scores into 2 and 3 categories
#    - check that NAs aren't causing some strange proglem(s)
#    - get good color ramps for continuous variables, pref. 508 compliant - like green, yellow, red for low-high
#    - way to set map size and place legend in suitable location
#    - do ggsave thing - good size for putting on pptx slides, reasonable resolution
#    - make state boundaries lighter (usmap package)


library(ggplot2)
library(tidyverse)
library(usmap)
library(rgdal)
library(colorspace)

rm(list=ls())

load('./Data/mapPoints.RData')

NPSDir <- '/Volumes/Seagate1_Blue2TB/GIS/NPS Units/nps_boundary_centroids'
outDir <- './Output'
PUnits <- readOGR(NPSDir, "nps_boundary_centroids")
vulnScores <- read_csv("/Volumes/Seagate1_Blue2TB/_NPVuln/Data/Final Data 2021/NPVuln_Park_Database for external review.csv")

PUnitsDF <- data.frame(data.frame(coordinates(PUnits), code = PUnits$UNIT_CODE, state=PUnits$STATE,
                    NPS_reg=PUnits$REGION, regNum = as.numeric(PUnits$Unified_Re,pkName=PUnits$PARKNAME)))

names(PUnitsDF)[1:2] <- c("long", "lat")
PUnitsDF$group = 1

PkTrans <- usmap_transform(PUnitsDF)    # Albers Equal Area transform
PkPts <- PkTrans %>% filter(long < -0, lat > 0)   # limit to N Hemisphere, delete Samoa
PkPts <- PkTrans %>% filter(long < -67, lat > 0)  # delete Virgin Island,Puerto Rica area & S hemisphere pks
  # make sure mapping works
BMap <- plot_usmap(color = "gray80")
BMap
BMap + geom_point(data=PkPts, aes(x=long.1, y=lat.1), color = "blue")
BMap + geom_point(data=PkPts, aes(x=long.1, y=lat.1, color = NPS_reg))
    
PltData <- PkTrans %>% full_join(vulnScores, by="code")    # keeps ALL points - Samoa, etc.
USData <- PltData  %>% filter(long < -67, lat > 0)         # map is wierd if you keep all points. Drops Virgin, Samoa, etc.


#####################################################################################################################################################
# AKD start here

library(sf)
library(tmap)
library(viridis)
library(colorspace)
library(tmap)

####  COLORS #########

# - the text below is from CCRP's plotting scripts. These are the hex codes for the 508-compliant colors we use: 
# All 508-compliant color scheme -- navy (hot wet), light blue (warm wet), pink (warm dry), red (hot dry)
# colors5 <-   c("white","#12045C","#9A9EE5","#F3D3CB","#E10720")


# For more color options for use with ggplot, I would recommend either RColorbrewer or colorspace
# In the two examples below, I include two scale_color options, one for a continuous scale and one for a discrete scale, in which the vuln scores are divided into neat breaks. 

# COLOR BREWER: https://colorbrewer2.org is a good place to discover palettes. Viridis is an oft-used palette for its visual appeal and robustness to color-blindness.
# ColorBrewer example:

TerrTotVulnMap <- BMap + 
  geom_point(data=USData, aes(x=long.1, y=lat.1, color = Ter.tot.vul.score)) +
  #scale_color_gradientn(colors = viridis(4), na.value = NA) + # used for continuous scale. 'n' refers to an n-color gradient. 
  scale_color_fermenter(type = "seq", n.breaks = 4, palette = "BuPu", direction = 1, na.value = NA) + # can be used for discrete bins of vuln scores. n.breaks = n bins. 
  ggtitle("Terrestrial Living total score")  

# na.value can be set to NA if you don't want parks w/o data included. They can also be grey. 


# COLORSPACE: 

choose_palette() # This allows you to choose colors manually and record hex codes. 

# The package also comes with pre-loaded palettes you can find here: http://colorspace.r-forge.r-project.org/articles/ggplot2_color_scales.html

TerrTotVulnMap <- BMap + 
  geom_point(data=USData, aes(x=long.1, y=lat.1, color = Ter.tot.vul.score)) +
  #scale_color_continuous_sequential(palette = "Viridis") +
  scale_color_binned_sequential(palette = "Hawaii", n.breaks = 4) + 
  ggtitle("Terrestrial Living total score") 
  
  
 #### Coloring states by management region and manipulating data using tmap. 


# I REALLY like tmap and find it easier to use than ggplot for mapping. 
# I added tmap for fun because I know you are enjoying tidy. Tmap + tidy = love
# Note: There are entries in VA and NC that are causing those two states to be mislabeled. Can be fixed easily.

# I am using the US States shapefile available from the NPS SharePoint: 
# https://doimspp.sharepoint.com/sites/NPS-CCRP-FCScienceAdaptation/Shared%20Documents/Forms/AllItems.aspx?viewid=54c972dc%2D7b2e%2D4eb7%2Da737%2D42792988c0b3&id=%2Fsites%2FNPS%2DCCRP%2DFCScienceAdaptation%2FShared%20Documents%2FRCF%2Fscript%20rewrites%2Fdata%2Ezip&parent=%2Fsites%2FNPS%2DCCRP%2DFCScienceAdaptation%2FShared%20Documents%2FRCF%2Fscript%20rewrites

states <- st_read('./Data/data/spatial-data/State_Shapefile/Contig_US_Albers.shp') # reads in as sf object. Sf is a spatial package that works with tidy syntax.

NPS.region <- USData %>% 
  select(State, NPS.region) %>% # Get NPS mgmt region from USData df
  rename(STATE_ABBR = State) # Change column name to match states object. Syntax is new_name = old_name.
  
regions <- states %>% left_join(NPS.region) 

USData.sf <- st_as_sf(USData, coords = c("long", "lat"), crs = 4326) # turn USData dataframe into an sf object. 4326 is lat/long.
USData.sf <- st_transform(USData.sf, crs = st_crs(regions)) # Reproject from lat/long to same crs as other data

# In tmap, tmaptools:palette_explorer() can help you choose colors

tmaptools::palette_explorer()

tmap_mode("plot") # can also use "view" mode which is interactive

tm_shape(regions) + 
  tm_polygons("NPS.region", palette = "Greys", title = "NPS Region") + 
  tm_shape(USData.sf %>% drop_na(Aqu.tot.vul.score)) + # I excluded NA's but you can also include NA's and specify a color and text with colorNA() and textNA().
  # Above is also where you could select various scores if you only wanted to plot scores above a certain level, etc. by using the filter() function
  tm_symbols(col = "Aqu.tot.vul.score", palette = "viridis", breaks = c(1,2,3), labels = c("Low", "High"), title.col = "Aquatic Vulnerability Score") + # you could also specify discrete breaks like this or labels like this 
  tm_layout(legend.stack = "horizontal") 

# If you google any of these functions you can learn more about how to manipulate the map to show what you want.
# Legend-specific functions can be found 
  # (1) within the functions themselves (i.e. tm_symbols) 
  # (2) tm_layout for where on the plot you want the legend and 
  # (3) tm_add_legend which is SUPER handy for customizing your own legends


########## John's code #################################################

NLTotVulnMap <- BMap + geom_point(data=USData, aes(x=long.1, y=lat.1, color = Nliv.tot.vul.score))  # Non-living

TerrTotVulnMap
AquTotVulnMap
NLTotVulnMap
rm(TerrTotVulnMap, AquTotVulnMap,NLTotVulnMap)

BMap + geom_point(data=USData, aes(x=long.1, y=lat.1, color = NPS_reg))

   # clean up
rm(PUnits, PUnitsDF, vulnScores, PkPts, PkTrans)

setwd(outDir)
save.image(file="mapPoints.RData")

# EOF - anything below here is junk code



