
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(INLA)
library(sf)


# Load the results
load("ST_models_ICAR_partitioned.Rdata")
rm(TypeI.ICAR, TypeIII.ICAR, TypeIV.ICAR)

model <- TypeII.ICAR


# Load the data and cartography
load("../Data/IHD_data.Rdata")


S <- 3105
T <- 23


# Load capitals information
capitals <- read.table("../Data/Capitals.txt", header=TRUE, sep=",", 
                       colClasses = c("character", "character", "character", "character"))



################################################################
# Relative risks estimates of the capitals: medians and 95% CI #
################################################################
RR.median <- as.data.frame(matrix(model$summary.fitted.values$`0.5quant`, S, T, byrow=F))
rownames(RR.median) <- model$.args$data$Area[1:S]
colnames(RR.median) <- seq(1999, 2021, 1)
RR.median$GEOID <- rownames(RR.median)
RR.median <- RR.median[, c(24, 1:23)]

RR.median.cap <- RR.median[capitals$GEOID, c("1999", "2004", "2009", "2014", "2019", "2021")]


RR.CIlow <- as.data.frame(matrix(model$summary.fitted.values$`0.025quant`, S, T, byrow=F))
rownames(RR.CIlow) <- model$.args$data$Area[1:S]
colnames(RR.CIlow) <- paste0("CIlow", seq(1999, 2021, 1))
RR.CIlow$GEOID <- rownames(RR.CIlow)
RR.CIlow <- RR.CIlow[, c(24, 1:23)]

RR.CIlow.cap <- RR.CIlow[capitals$GEOID, c("CIlow1999", "CIlow2004", "CIlow2009", "CIlow2014", 
                                           "CIlow2019", "CIlow2021")]


RR.CIup <- as.data.frame(matrix(model$summary.fitted.values$`0.975quant`, S, T, byrow=F))
rownames(RR.CIup) <- model$.args$data$Area[1:S]
colnames(RR.CIup) <- paste0("CIup", seq(1999, 2021, 1))
RR.CIup$GEOID <- rownames(RR.CIup)
RR.CIup <- RR.CIup[, c(24, 1:23)]

RR.CIup.cap <- RR.CIup[capitals$GEOID, c("CIup1999", "CIup2004", "CIup2009", "CIup2014", 
                                           "CIup2019", "CIup2021")]



################################################################################
# Table D1: posterior medians and 95% CI of the relative risks of the capitals #
################################################################################
TableD1 <- cbind(RR.median.cap$`1999`, RR.CIlow.cap$CIlow1999, RR.CIup.cap$CIup1999,
                 RR.median.cap$`2004`, RR.CIlow.cap$CIlow2004, RR.CIup.cap$CIup2004,
                 RR.median.cap$`2009`, RR.CIlow.cap$CIlow2009, RR.CIup.cap$CIup2009,
                 RR.median.cap$`2014`, RR.CIlow.cap$CIlow2014, RR.CIup.cap$CIup2014,
                 RR.median.cap$`2019`, RR.CIlow.cap$CIlow2019, RR.CIup.cap$CIup2019,
                 RR.median.cap$`2021`, RR.CIlow.cap$CIlow2021, RR.CIup.cap$CIup2021)

TableD1 <- as.data.frame(TableD1)
TableD1$Capitals <- capitals$Capital
rownames(TableD1) <- rownames(RR.median.cap)
colnames(TableD1) <- c("1999", "CIlow1999", "CIup1999","2004", "CIlow2004", "CIup2004", 
                       "2009", "CIlow2009", "CIup2009", "2014", "CIlow2014", "CIup2014", 
                       "2019", "CIlow2019", "CIup2019", "2021", "CIlow2021", "CIup2021",
                       "Capital")

TableD1[, 1:(ncol(TableD1)-1)] <- round(TableD1[, 1:(ncol(TableD1)-1)], digits=4)
TableD1 <- TableD1[, c(ncol(TableD1), 1:(ncol(TableD1)-1))]
print(TableD1)



#######################################
# Detect high-low risks areas in 2021 #
#######################################
RR.CIlow.2021 <- RR.CIlow[, c(1, 24)]
RR.CIup.2021 <- RR.CIup[, c(1, 24)]


ID.high.hotspot <- RR.CIlow.2021$GEOID[RR.CIlow.2021$CIlow2021>1]
ID.low.hotspot <- RR.CIup.2021$GEOID[RR.CIup.2021$CIup2021<1]



####################################################################################################
# Population in 2021 of the counties classified as high-low risks areas avoiding counties with NAs #
####################################################################################################
counts <- counts[, c("GEOID", "1999", "2004", "2009", "2014", "2019", "2021")]
x <- rowSums(is.na(counts))
Tab <- data.frame(GEOID=counts$GEOID, X=x)
ID.NAs <- Tab$GEOID[Tab$X>0]


pop <- pop[, c("GEOID", "2021")]


# Population of the high risk areas
pop.high <- pop[pop$GEOID %in% ID.high.hotspot, ]
pop.high <- pop.high[!(pop.high$GEOID %in% ID.NAs), ]
pop.high <- pop.high[order(pop.high$`2021`, decreasing=TRUE), ]


# Population of the low risk areast
pop.low <- pop[pop$GEOID %in% ID.low.hotspot, ]
pop.low <- pop.low[!(pop.low$GEOID %in% ID.NAs), ]
pop.low <- pop.low[order(pop.low$`2021`, decreasing=TRUE), ]



#################################################################
# Select the high-low risk counties to show in Tables D2 and D3 #
#################################################################
HIGH.IDs <- pop.high$GEOID[c(1:10, (nrow(pop.high)-9):nrow(pop.high))]
LOW.IDs <- pop.low$GEOID[c(1:10, (nrow(pop.low)-9):nrow(pop.low))]



###################################################################################
# Table D2: posterior medians and 95% CI of the relative risks of high-risk areas #
###################################################################################
RR.median.HIGH <- RR.median[HIGH.IDs, c(1,2,7,12,17,22,24)]
RR.CIlow.HIGH <- RR.CIlow[HIGH.IDs, c(1,2,7,12,17,22,24)]
RR.CIup.HIGH <- RR.CIup[HIGH.IDs, c(1,2,7,12,17,22,24)]


TableD2 <- cbind(RR.median.HIGH$`1999`, RR.CIlow.HIGH$CIlow1999, RR.CIup.HIGH$CIup1999,
                 RR.median.HIGH$`2004`, RR.CIlow.HIGH$CIlow2004, RR.CIup.HIGH$CIup2004,
                 RR.median.HIGH$`2009`, RR.CIlow.HIGH$CIlow2009, RR.CIup.HIGH$CIup2009,
                 RR.median.HIGH$`2014`, RR.CIlow.HIGH$CIlow2014, RR.CIup.HIGH$CIup2014,
                 RR.median.HIGH$`2019`, RR.CIlow.HIGH$CIlow2019, RR.CIup.HIGH$CIup2019,
                 RR.median.HIGH$`2021`, RR.CIlow.HIGH$CIlow2021, RR.CIup.HIGH$CIup2021)

TableD2 <- as.data.frame(TableD2)
rownames(TableD2) <- rownames(RR.median.HIGH)
colnames(TableD2) <- c("1999", "CIlow1999", "CIup1999","2004", "CIlow2004", "CIup2004", 
                       "2009", "CIlow2009", "CIup2009", "2014", "CIlow2014", "CIup2014", 
                       "2019", "CIlow2019", "CIup2019", "2021", "CIlow2021", "CIup2021")


county.names <- read.table("../Data/GEOID_vs_CountyNames.txt", header=TRUE, sep=",",
                           colClasses = c("character", "character"))
names.HIGH <- county.names[county.names$GEOID %in% HIGH.IDs, ]
rownames(names.HIGH) <- names.HIGH$GEOID
names.HIGH <- names.HIGH[rownames(RR.median.HIGH), ]

TableD2$County <- names.HIGH$COUNTY.NAME
TableD2[, 1:(ncol(TableD2)-1)] <- round(TableD2[, 1:(ncol(TableD2)-1)], digits=4)
TableD2 <- TableD2[, c(ncol(TableD2), 1:(ncol(TableD2)-1))]
print(TableD2)



##################################################################################
# Table D3: posterior medians and 95% CI of the relative risks of low-risk areas #
##################################################################################
RR.median.LOW <- RR.median[LOW.IDs, c(1,2,7,12,17,22,24)]
RR.CIlow.LOW <- RR.CIlow[LOW.IDs, c(1,2,7,12,17,22,24)]
RR.CIup.LOW <- RR.CIup[LOW.IDs, c(1,2,7,12,17,22,24)]


TableD3 <- cbind(RR.median.LOW$`1999`, RR.CIlow.LOW$CIlow1999, RR.CIup.LOW$CIup1999,
                 RR.median.LOW$`2004`, RR.CIlow.LOW$CIlow2004, RR.CIup.LOW$CIup2004,
                 RR.median.LOW$`2009`, RR.CIlow.LOW$CIlow2009, RR.CIup.LOW$CIup2009,
                 RR.median.LOW$`2014`, RR.CIlow.LOW$CIlow2014, RR.CIup.LOW$CIup2014,
                 RR.median.LOW$`2019`, RR.CIlow.LOW$CIlow2019, RR.CIup.LOW$CIup2019,
                 RR.median.LOW$`2021`, RR.CIlow.LOW$CIlow2021, RR.CIup.LOW$CIup2021)

TableD3 <- as.data.frame(TableD3)
rownames(TableD3) <- rownames(RR.median.LOW)
colnames(TableD3) <- c("1999", "CIlow1999", "CIup1999","2004", "CIlow2004", "CIup2004", 
                       "2009", "CIlow2009", "CIup2009", "2014", "CIlow2014", "CIup2014", 
                       "2019", "CIlow2019", "CIup2019", "2021", "CIlow2021", "CIup2021")


names.LOW <- county.names[county.names$GEOID %in% LOW.IDs, ]
rownames(names.LOW) <- names.LOW$GEOID
names.LOW <- names.LOW[rownames(RR.median.LOW), ]

TableD3$County <- names.LOW$COUNTY.NAME
TableD3[, 1:(ncol(TableD3)-1)] <- round(TableD3[, 1:(ncol(TableD3)-1)], digits=4)
TableD3 <- TableD3[, c(ncol(TableD3), 1:(ncol(TableD3)-1))]
print(TableD3)



