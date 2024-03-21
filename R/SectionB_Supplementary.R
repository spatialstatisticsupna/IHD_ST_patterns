
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



##################################################
# Number of missing observations for each county #
##################################################
x <- rowSums(is.na(counts))
Tab <- data.frame(GEOID=counts$GEOID, X=x)


# Percentage of total observations with NAs
(sum(Tab$X)/(S*T))*100



##################################################
# Counties with at least one missing observation #
##################################################
IDs23 <- Tab$GEOID[Tab$X==23]
IDs15to23 <- Tab$GEOID[Tab$X<23 & Tab$X>=15]
IDs15 <- Tab$GEOID[Tab$X<15 & Tab$X>0]


# 0="no NAs", 1="<15 NAs", 2=[15,23) NAs", 3="23 NAs"
carto$NAs <- rep(0, 3105)
carto$NAs[carto$GEOID %in% IDs23] <- 3
carto$NAs[carto$GEOID %in% IDs15to23] <- 2
carto$NAs[carto$GEOID %in% IDs15] <- 1



############################
# Figure B1: number of NAs #
############################
centroid <- st_centroid(carto)

pnt = st_as_sf(data.frame(x=st_coordinates(centroid)[,1], y=st_coordinates(centroid)[,2]),
               coords = c("x", "y"), crs = 4326)


states <- aggregate(carto[, "STATEFP"], by = list(ID = carto$STATEFP), 
                    FUN = unique, dissolve = T)

regions <- aggregate(carto[, "Region"], by = list(ID = carto$Region), 
                     FUN = unique, dissolve = T)



col.NAs <- c("#f0f0f0", "#66c2a5", "#fed976", "#fd8d3c")
val.NAs <- c(0,1,2,3,4)

pal.NAs <- colorBin(col.NAs, domain=carto$NAs, bins=val.NAs)



map <- leaflet(carto) %>% addTiles() %>% 
  addPolygons(fillColor = ~pal.NAs(NAs), weight = 1, 
              opacity = 1, color = "black", fillOpacity = 0.7) %>% 
  addLegend("bottomright", colors=col.NAs,
            labels = c("0", "[1, 14]", "[15, 22]", "23"),
            title = "Number of NAs", opacity = 1) %>% 
  addPolylines(data = states, color = "white", opacity = 1, weight = 1) %>% 
  addPolylines(data = regions, color = "#c2a5cf", opacity = 1, weight = 4)

print(map)



####################################################################
# Contingency table: number of missing observations in each region #
####################################################################
counts <- counts[, -1]
carto$num.NAs <- rowSums(is.na(counts))


cont.table <- with(carto, table(num.NAs, Region))
cont.table.obs <- cont.table*seq(0,23,1)

obs.NAs <- c(colSums(cont.table.obs), sum(is.na(counts)))
obs.total <- c(table(carto$Region)*T, S*T)

obs.noNAs <- obs.total-obs.NAs


Table <- rbind(obs.noNAs, obs.NAs, obs.total)
colnames(Table) <- c("West", "Midwest", "South", "Northeast", "Total")
rownames(Table) <- c("Observed", "NAs", "Total")
print(Table)



################################################################
# Table B1: percentage of missing observations for each region #
################################################################
perc.NAs <- obs.NAs/obs.total*100
perc.noNAs <- obs.noNAs/obs.total*100
perc.total <- obs.total/obs.total*100


TableB1 <- rbind(perc.noNAs, perc.NAs, perc.total)
colnames(TableB1) <- c("West", "Midwest", "South", "Northeast", "Total")
rownames(TableB1) <- c("Observed", "NAs", "Total")
TableB1 <- round(TableB1, digits=4)
print(TableB1)





