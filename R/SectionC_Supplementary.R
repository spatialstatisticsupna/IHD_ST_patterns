
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(INLA)



# Load the results: partitioned ST models
load("ST_models_ICAR_partitioned.Rdata")
MODELS.bigDM <- list(TypeI.ICAR, TypeII.ICAR, TypeIII.ICAR, TypeIV.ICAR)

rm(TypeI.ICAR, TypeII.ICAR, TypeIII.ICAR, TypeIV.ICAR)


# Load the results: global ST models
load("ST_TypeI_ICAR_global.Rdata")
load("ST_TypeII_ICAR_global.Rdata")
load("ST_TypeIII_ICAR_global.Rdata")
load("ST_TypeIV_ICAR_global.Rdata")

MODELS.INLA <- list(TypeI.ICAR, TypeII.ICAR, TypeIII.ICAR, TypeIV.ICAR)

rm(TypeI.ICAR, TypeII.ICAR, TypeIII.ICAR, TypeIV.ICAR)



###########################################
# Table C1: goodness of fit of the models #
###########################################
fun.DIC.WAIC <- function(x){
  data.frame(mean.deviance=x$dic$mean.deviance, ## posterior mean deviance
             pD=x$dic$p.eff,                    ## effective number of parameters
             DIC=x$dic$dic,                     ## Deviance Information Criterion
             WAIC=x$waic$waic)}                 ## Watanabe-Akaike information criterion


Table.bigDM <- do.call(rbind,lapply(MODELS.bigDM, fun.DIC.WAIC))
rownames(Table.bigDM) <- c("TypeI ICAR", "TypeII ICAR", "TypeIII ICAR", "TypeIV ICAR")


Table.INLA <- do.call(rbind,lapply(MODELS.INLA, fun.DIC.WAIC))
rownames(Table.INLA) <- c("TypeI ICAR", "TypeII ICAR", "TypeIII ICAR", "TypeIV ICAR")


Tab <- rbind(Table.bigDM, Table.INLA)
Tab <- round(Tab, 4)

Tab[, 3] <- Tab[, 3]-min(Tab[, 3])
Tab[, 4] <- Tab[, 4]-min(Tab[, 4])


Tab.final <- cbind(Tab[1:4, ], Tab[5:8, ])
print(Tab.final)

