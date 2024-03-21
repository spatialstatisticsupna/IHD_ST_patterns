
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(sf)
library(INLA)
library(spdep)


# Load the data
load("../Data/IHD_data.Rdata")


S <- nrow(counts)
T <- ncol(counts)-1



##############################################################
# Compute the overall rates and expected cases for each year #
##############################################################
O.total <- colSums(counts[, -1], na.rm=TRUE)
Pop.total <- colSums(pop[, -1], na.rm=TRUE)

Tab.rate <- data.frame(year=colnames(counts)[-1], O.total, Pop.total, 
                       Rate=O.total/Pop.total)


expected <- data.frame(GEOID=pop$GEOID)

for (i in 2:ncol(pop)){
  print(colnames(pop)[i])
  expected <- cbind(expected, Tab.rate$Rate[i-1]*pop[, i])
  colnames(expected)[i] <- paste0("exp", colnames(pop)[i])
}

rownames(expected) <- expected$GEOID



#######################################
# Define the spatial structure matrix #
#######################################
nb.list <- poly2nb(carto)
nb.mat <- nb2mat(poly2nb(carto), style ="B")
nbs <- unlist(lapply(poly2nb(carto), function(x) length(x)))

Q.xi <- diag(nbs)-nb.mat



###########################################
# Prepare the data for the spatial models #
###########################################
data <- merge(counts, expected, by="GEOID")
rownames(data) <- data$GEOID
data <- data[carto$GEOID, ]

data$ID.area <- seq(1, nrow(data))



#######################################################################
# Fit a spatial model for each year from 1999 to 2021 with BYM2 prior #
#######################################################################
f.Spat.bym2 <- O ~ 1 + f(ID.area, model="bym2", graph=Q.xi) 


Spat.bym2 <- vector("list", T)

for(i in 1:T){
  print(i)
  name <- colnames(counts)[i+1]
  print(name)
  data$O <- data[, name]
  data$E <- data[, paste0("exp", name)]
  
  Spat.bym2[[i]] <- inla(f.Spat.bym2, family="poisson", data=data, E=E,
                    control.predictor=list(compute=TRUE, cdf=c(log(1))), 
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE,
                                         return.marginals.predictor=TRUE))
  data$O <- NULL
  data$E <- NULL
}



###########################################
# Posterior medians of the relative risks #
###########################################
median.RR <- function(marginal){
  inla.qmarginal(c(0.5), marginal)
}


RR.bym2.median <- vector("list", T)

for(i in 1:T){
  print(i)
  model <- Spat.bym2[[i]]
  marg <- lapply(model$marginals.linear.predictor, function(x) inla.tmarginal(function(x) exp(x), x))
  RR.bym2.median[[i]] <- unlist(lapply(marg, median.RR))
}



#########################
# Impute missing counts #
#########################
imputed.counts <- data.frame(GEOID=counts$GEOID)

for(i in 2:ncol(counts)){
  O <- counts[, i]
  E <- expected[, i]
  rr <- RR.bym2.median[[i-1]]
  for (j in which(is.na(O))){
    O[j] <- round(E[j]*rr[j])
    O[j] <- ifelse(O[j]>9, 9, O[j])
  }
  imputed.counts <- cbind(imputed.counts, O)
  colnames(imputed.counts)[i] <- colnames(counts)[i]
}



##################################################################################
# Compute expected cases for the descriptive analysis and spatio-temporal models #
##################################################################################

# Replace with NA the population if the count is missing 
pop.NAs <- pop

for (i in 2:ncol(counts)){
  pos <- which(is.na(counts[i]))
  pop.NAs[pos, i] <- NA
}


# Overall rate for all the years (without imputed data)
O.total <- sum(colSums(counts[, -1], na.rm=TRUE))
Pop.total <- sum(colSums(pop.NAs[, -1], na.rm=TRUE))

Rate <- O.total/Pop.total


# Compute the expected cases
rm(expected)

expected <- Rate*pop[, -1]
summary(expected)

exp <- cbind(pop.NAs$GEOID, expected)
colnames(exp)[1] <- "GEOID"
rownames(exp) <- exp$GEOID

exp <- exp[carto$GEOID, ]



##########################################################
# Save the imputed counts dataset and the expected cases #
##########################################################
counts <- imputed.counts

save(counts, pop, exp, carto, file="../Imputed_data/IHD_imputed_data.Rdata")




