
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(sf)
library(leaflet)
library(leafpop)
library(RColorBrewer)


# Load the data
load("../Data/IHD_data.Rdata")


S <- nrow(counts)
T <- ncol(counts)-1


t.from <- 1999
t.to <- 2021


# Add population in 2021 to the cartography
carto$pop2021 <- pop$`2021`



###########################################
# Figure 1: rural/urban areas and regions #
###########################################
centroid <- st_centroid(carto)

pnt = st_as_sf(data.frame(x=st_coordinates(centroid)[,1], y=st_coordinates(centroid)[,2]),
               coords = c("x", "y"),
               crs = 4326)

states <- aggregate(carto[, "STATEFP"], by = list(ID = carto$STATEFP), 
                    FUN = unique, dissolve = T)

regions <- aggregate(carto[, "Region"], by = list(ID = carto$Region), 
                     FUN = unique, dissolve = T)


# Color palette for Rural/Medium/Large metros
col.metros <- c("#a8ddb5", "#4eb3d3", "#0868ac")
val.metros <- c(0,49999,999999, max(carto$pop2021))

pal.metros <- colorBin(col.metros, domain=carto$pop2021, bins=val.metros)


# Color palette for West/Midwest/South/Northeast regions
col.reg <- c("#fd8d3c", "#de77ae", "#fed976", "#66c2a5")
val.reg <- c(1,2,3,4,5)

pal.reg <- colorBin(col.reg, domain=carto$Region, bins=val.reg)


# Map population
map.metros <- leaflet(carto) %>% addTiles() %>% 
  addPolygons(fillColor = ~pal.metros(pop2021), weight = 1, 
              opacity = 1, color = "black", fillOpacity = 0.7) %>% 
  addLegend("bottomright", colors=col.metros,
            labels = c("Rural","Medium","Large"),
            title = "Metros in 2021", opacity = 1) %>% 
  addPolylines(data = states, color = "white", opacity = 1, weight = 1) %>% 
  addPolylines(data = regions, color = "#c2a5cf", opacity = 1, weight = 4) %>% 
print(map.metros)


# Map regions
map.regions <- leaflet(carto) %>% addTiles() %>% 
  addPolygons(fillColor = ~pal.reg(Region), weight = 1, 
              opacity = 1, color = "black", fillOpacity = 0.7) %>% 
  addLegend("bottomright", colors=col.reg,
            labels = c("West", "Midwest", "South", "Northeast"),
            title = "", opacity = 1) %>% 
  addPolylines(data = states, color = "white", opacity = 1, weight = 1) %>% 
  addPolylines(data = regions, color = "#c2a5cf", opacity = 1, weight = 4) %>% 
print(map.regions)



