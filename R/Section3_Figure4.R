
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(INLA)
library(sf)
library(spdep)
library(RColorBrewer)
library(tmap)


# Load the results
load("ST_models_ICAR_partitioned.Rdata")
rm(TypeI.ICAR, TypeIII.ICAR, TypeIV.ICAR)

model <- TypeII.ICAR


# Load the data and cartography
load("../Data/IHD_data.Rdata")


S <- 3105
T <- 23



####################################
# Identify counties with NA counts #
####################################
x <- rowSums(is.na(counts))
Tab <- data.frame(GEOID=counts$GEOID, X=x)
ID.NAs <- Tab$GEOID[Tab$X>0]



############################################################################
# Figure 4: relative risks estimates in 1999, 2004, 2009, 2014, 2019, 2021 #
############################################################################
RR <- as.data.frame(matrix(model$summary.fitted.values$`0.5quant`, S, T, byrow=F))
rownames(RR) <- model$.args$data$Area[1:S]


# Reorder the relative risks in the same order as the cartography
RR.new <- RR[carto$GEOID, ]
colnames(RR.new) <- paste0("year", seq(1999, 2021, 1))
RR.new$GEOID <- rownames(RR.new)


# Select the years 1999, 2004, 2009, 2014, 2019, 2021
RR.new <- RR.new[, c("GEOID", "year1999", "year2004", "year2009",
                     "year2014", "year2019", "year2021")]


# Merge the data and the cartography
carto <- merge(carto, RR.new, by="GEOID")


# Cartography for the states
carto.states <- aggregate(carto[,"geometry"], by=list(ID.group=carto$STATEFP), head)


# Cartography of the four regions
carto.reg <- aggregate(carto[,"geometry"], by=list(ID.group=carto$Region), head)


# Suppress the RR of the counties with at least one missing
carto[carto$GEOID %in% ID.NAs, c("year1999", "year2004", "year2009",
                                 "year2014", "year2019", "year2021")] <- NA



summary(RR.new)

paleta <- brewer.pal(9,"YlOrRd")
values <- c(0.2,0.5,0.75,0.9,1,1.1,1.25,1.5,2,4.59)


RR.pattern <- tm_shape(carto) +
  tm_polygons(col=c("year1999", "year2004", "year2009", "year2014", "year2019", "year2021"), 
              palette=paleta, border.alpha=1, title="",
              leyend.show=T, legend.reverse=T, style="fixed", breaks=values, interval.closure="left",
              border.col = "black", 
              colorNA = "#bdbdbd", # color of missing data
              textNA = "Insufficient data") +
  tm_layout(main.title="", main.title.position="center", legend.text.size=1,
            panel.labels=c("Relative risks 1999", "Relative risks 2004", "Relative risks 2009", 
                           "Relative risks 2014", "Relative risks 2019", "Relative risks 2021"), 
            panel.label.bg.color="lightskyblue",
            legend.outside=T, legend.outside.position="right", legend.frame=F,
            panel.label.size=2, panel.label.height=1, outer.margins=0) + 
  tm_shape(carto.states) + tm_borders(col="white", lwd=1) + 
  tm_shape(carto.reg) + tm_borders(col="#c2a5cf", lwd=3) +
  tm_facets(ncol=2, nrow=3)
print(RR.pattern)
tmap_save(RR.pattern, width=12, height=10, file="Figure4.png", dpi=1000)




