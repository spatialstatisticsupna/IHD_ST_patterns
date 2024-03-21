
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(spdep)
library(INLA)
library(bigDM)


# Load the data after imputing the missing counts
load("../Imputed_data/IHD_imputed_data.Rdata")


S <- nrow(counts)
T <- ncol(counts)-1


# Prepare the data
Year <- unlist(lapply(seq(1999,2021, 1), function(x) rep(x, S)))

Data <- data.frame(GEOID=rep(carto$GEOID, T), ID.year=as.integer(Year), 
                   O=unlist(c(counts[, 2:(T+1)])), E=unlist(c(exp[, 2:(T+1)])))
rownames(Data) <- seq(1, S*T)



######################################################
# Partitioned spatio-temporal models with ICAR prior #
######################################################
TypeI.ICAR <- STCAR_INLA(carto=carto, data=Data,
                     ID.area="GEOID", ID.year="ID.year", O="O", E="E", ID.group="STATEFP2",
                     spatial="intrinsic", temporal="rw1", interaction="TypeI",
                     model="partition", k=1, compute.fitted.values=TRUE,
                     plan="sequential", workers=NULL, inla.mode="compact")


TypeII.ICAR <- STCAR_INLA(carto=carto, data=Data,
                     ID.area="GEOID", ID.year="ID.year", O="O", E="E", ID.group="STATEFP2",
                     spatial="intrinsic", temporal="rw1", interaction="TypeII",
                     model="partition", k=1, compute.fitted.values=TRUE,
                     plan="sequential", workers=NULL, inla.mode="compact")


TypeIII.ICAR <- STCAR_INLA(carto=carto, data=Data,
                     ID.area="GEOID", ID.year="ID.year", O="O", E="E", ID.group="STATEFP2",
                     spatial="intrinsic", temporal="rw1", interaction="TypeIII",
                     model="partition", k=1, compute.fitted.values=TRUE,
                     plan="sequential", workers=NULL, inla.mode="compact")


TypeIV.ICAR <- STCAR_INLA(carto=carto, data=Data,
                     ID.area="GEOID", ID.year="ID.year", O="O", E="E", ID.group="STATEFP2",
                     spatial="intrinsic", temporal="rw1", interaction="TypeIV",
                     model="partition", k=1, compute.fitted.values=TRUE,
                     plan="sequential", workers=NULL, inla.mode="compact")


# Save results
save(TypeI.ICAR, TypeII.ICAR, TypeIII.ICAR, TypeIV.ICAR, 
     file = "ST_models_ICAR_partitioned.Rdata")



######################################################
# Partitioned spatio-temporal models with BYM2 prior #
######################################################
TypeI.BYM2 <- STCAR_INLA(carto=carto, data=Data,
                          ID.area="GEOID", ID.year="ID.year", O="O", E="E", ID.group="STATEFP2",
                          spatial="BYM2", temporal="rw1", interaction="TypeI",
                          model="partition", k=1, compute.fitted.values=TRUE,
                          plan="sequential", workers=NULL, inla.mode="compact")


TypeII.BYM2 <- STCAR_INLA(carto=carto, data=Data,
                     ID.area="GEOID", ID.year="ID.year", O="O", E="E", ID.group="STATEFP2",
                     spatial="BYM2", temporal="rw1", interaction="TypeII",
                     model="partition", k=1, compute.fitted.values=TRUE,
                     plan="sequential", workers=NULL, inla.mode="compact",
                     save.models=TRUE)


TypeIII.BYM2 <- STCAR_INLA(carto=carto, data=Data,
                      ID.area="GEOID", ID.year="ID.year", O="O", E="E", ID.group="STATEFP2",
                      spatial="BYM2", temporal="rw1", interaction="TypeIII",
                      model="partition", k=1, compute.fitted.values=TRUE,
                      plan="sequential", workers=NULL, inla.mode="compact")


TypeIV.BYM2 <- STCAR_INLA(carto=carto, data=Data,
                     ID.area="GEOID", ID.year="ID.year", O="O", E="E", ID.group="STATEFP2",
                     spatial="BYM2", temporal="rw1", interaction="TypeIV",
                     model="partition", k=1, compute.fitted.values=TRUE,
                     plan="sequential", workers=NULL, inla.mode="compact")


# Save results
save(TypeI.BYM2, TypeII.BYM2, TypeIII.BYM2, TypeIV.BYM2, 
     file = "ST_models_BYM2_partitioned.Rdata")



##########################################
# Table 2: goodness of fit of the models #
##########################################
MODELS <- list(TypeI.ICAR, TypeII.ICAR, TypeIII.ICAR, TypeIV.ICAR, 
               TypeI.BYM2, TypeII.BYM2, TypeIII.BYM2, TypeIV.BYM2)


fun.DIC.WAIC <- function(x){
  data.frame(mean.deviance=x$dic$mean.deviance, ## posterior mean deviance
             pD=x$dic$p.eff,                    ## effective number of parameters
             DIC=x$dic$dic,                     ## Deviance Information Criterion
             WAIC=x$waic$waic)}                 ## Watanabe-Akaike information criterion


Table.DIC.WAIC <- do.call(rbind, lapply(MODELS, fun.DIC.WAIC))
rownames(Table.DIC.WAIC) <- c("TypeI ICAR", "TypeII ICAR", "TypeIII ICAR", "TypeIV ICAR", 
                              "TypeI BYM2", "TypeII BYM2", "TypeIII BYM2", "TypeIV BYM2")

Table.DIC.WAIC <- round(Table.DIC.WAIC, 4)


Table.DIC.WAIC[, 3] <- Table.DIC.WAIC[, 3]-min(Table.DIC.WAIC[, 3])
Table.DIC.WAIC[, 4] <- Table.DIC.WAIC[, 4]-min(Table.DIC.WAIC[, 4])

print(Table.DIC.WAIC)




