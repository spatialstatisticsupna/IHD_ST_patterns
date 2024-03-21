
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(INLA)
library(sf)
library(leaflet)
library(leafpop)
library(ggplot2)
library(data.table)
library(zoo)
library(plotly)
library(htmlwidgets)


# Load the results
load("ST_models_ICAR_partitioned.Rdata")
rm(TypeI.ICAR, TypeIII.ICAR, TypeIV.ICAR)

model <- TypeII.ICAR


# Load the data and cartography
load("../Data/IHD_data.Rdata")


# Add population in 2021 to the cartography
carto$pop2021 <- pop$`2021`


S <- 3105
T <- 23

t.from <- 1999
t.to <- 2021



####################################
# Identify counties with NA counts #
####################################
x <- rowSums(is.na(counts))
Tab <- data.frame(GEOID=counts$GEOID, X=x)
ID.NAs <- Tab$GEOID[Tab$X>0]


# Assign 0 value to the population of counties with NAs
carto$pop2021[carto$GEOID %in% ID.NAs] <- 0


carto.noNAs <- carto[carto$pop2021>0, ]
carto.NAs <- carto[carto$pop2021==0, ]



###########################################################
# Relative risks estimates: medians, 95% CI and P(rr>1|Y) #
###########################################################
RR.median <- as.data.frame(matrix(model$summary.fitted.values$`0.5quant`, S, T, byrow=F))
rownames(RR.median) <- model$.args$data$Area[1:S]
colnames(RR.median) <- seq(1999, 2021, 1)
RR.median <- RR.median[carto$GEOID, ]


RR.CIlow <- as.data.frame(matrix(model$summary.fitted.values$`0.025quant`, S, T, byrow=F))
rownames(RR.CIlow) <- model$.args$data$Area[1:S]
colnames(RR.CIlow) <- paste0("CIlow", seq(1999, 2021, 1))
RR.CIlow <- RR.CIlow[carto$GEOID, ]


RR.CIup <- as.data.frame(matrix(model$summary.fitted.values$`0.975quant`, S, T, byrow=F))
rownames(RR.CIup) <- model$.args$data$Area[1:S]
colnames(RR.CIup) <- paste0("CIup", seq(1999, 2021, 1))
RR.CIup <- RR.CIup[carto$GEOID, ]


probs <- as.data.frame(matrix(1-model$summary.linear.predictor$`0 cdf`, nrow=S, ncol=T, byrow=F))
rownames(probs) <- model$.args$data$Area[1:S]
colnames(probs) <- paste0("Probs", seq(1999, 2021, 1))
probs <- probs[carto$GEOID, ]



#################################################################
# Plot the temporal trends of the relative risks of each county #
#################################################################
minimum <- min(RR.CIlow[carto$GEOID[!(carto$GEOID %in% ID.NAs)], ])
maximum <- max(RR.CIup[carto$GEOID[!(carto$GEOID %in% ID.NAs)], ])



color <- c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027")
prob.levels <- Hmisc::cut2(unlist(c(0.05,0.15,0.5,0.85,0.95)), c(0,0.1,0.2,0.8,0.9,1))


graph <- lapply(carto$GEOID[!(carto$GEOID %in% ID.NAs)], function(ID){
  
  fitted.val <- data.frame(r_it=unlist(RR.median[ID, ]),
                           q1=unlist(RR.CIlow[ID, ]),
                           q2=unlist(RR.CIup[ID, ]),
                           PRP_it=Hmisc::cut2(unlist(probs[ID, ]), c(0,0.1,0.2,0.8,0.9,1)),
                           Year=seq(t.from, t.to, 1))
  
  fitted.val$group <- rleid(fitted.val$PRP_it)  
  fitted.val$PRP_it2 <- as.character(fitted.val$PRP_it)
  
  fitted.val.plot <- head(do.call(rbind, by(fitted.val, fitted.val$group, rbind, NA)), -1)
  fitted.val.plot[,c("group","PRP_it2")] <- lapply(fitted.val.plot[,c("group","PRP_it2")], na.locf)
  fitted.val.plot[] <- lapply(fitted.val.plot, na.locf, fromLast = TRUE)
  
  p <- ggplot(fitted.val.plot, aes(x=Year, y=r_it)) + 
    geom_line(size=1, color="black") +
    geom_ribbon(aes(ymin=q1, ymax=q2, fill=PRP_it2, group=group), alpha=0.7) +
    scale_fill_manual(values=c("#a6d96a", "#ffffbf", "#fee08b", "#fdae61", "#d73027"),
                      breaks=prob.levels) +
    geom_hline(yintercept=1, linetype="dashed", color = "black") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
          text = element_text(size = 20),
          panel.spacing = unit(1.3, "lines"),
          strip.background = element_rect(fill="lightskyblue"),
          legend.position = "none") + 
    scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
    scale_y_continuous(breaks=seq(1,5,1), limits=c(0,5.5)) +
    xlab("Year") + ylab(expression(r[it])) + labs(colour = NULL) +
    ggtitle(paste0(carto$NAME[carto$GEOID==ID]," county, ", carto$State.Name[carto$GEOID==ID], " (", ID, ")"))
  print(p)
  recordPlot()
})



###################
# Interactive map #
###################
centroid <- st_centroid(carto)

pnt = st_as_sf(data.frame(x=st_coordinates(centroid)[,1], y=st_coordinates(centroid)[,2]),
               coords = c("x", "y"),
               crs = 4326)


states <- aggregate(carto[, "STATEFP"], by = list(ID = carto$STATEFP), 
                    FUN = unique, dissolve = T)

regions <- aggregate(carto[, "Region"], by = list(ID = carto$Region), 
                     FUN = unique, dissolve = T)



paleta <- c("#bdbdbd", "#a8ddb5", "#4eb3d3", "#0868ac")
values <- c(0,1,49999,999999,max(pop[, 2:24]))


pal <- colorBin(paleta, domain=carto$pop2021, bins=values)


pal.noNAs <- colorBin(paleta, domain=carto.noNAs$pop2021, bins=values)
pal.NAs <- colorBin(paleta, domain=carto.NAs$pop2021, bins=values) 



map <- leaflet(carto) %>% addTiles() %>% 
  addPolygons(data=carto.noNAs, fillColor = ~pal.noNAs(pop2021), weight = 1,
              opacity = 1, color = "black", fillOpacity = 0.7,
              label = paste0(carto.noNAs$NAME, " county, ", carto.noNAs$State.Name),
              popup=popupGraph(graph, width = 700, height = 600)) %>%
  addPolygons(data=carto.NAs, fillColor = ~pal.NAs(pop2021), weight = 1,
              opacity = 1, color = "black", fillOpacity = 0.7,
              label = paste0(carto.NAs$NAME, " county, ", carto.NAs$State.Name)) %>%
  addPolylines(data=states, color = "white", opacity = 1, weight = 1) %>%
  addPolylines(data = regions, color = "#c2a5cf", opacity = 1, weight = 4) %>%
  addLegend("bottomright", colors=paleta,
            labels = c("Insufficient data", "Rural","Medium","Large"),
            title = "Metro", opacity = 1) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addMiniMap(tiles = providers$Esri.WorldStreetMap,
             toggleDisplay = TRUE, position = "topright")
print(map)


saveWidget(map, file="Interactive_map.html")




