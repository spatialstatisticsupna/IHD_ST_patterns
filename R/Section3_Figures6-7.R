
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load packages
library(INLA)
library(ggplot2)
library(latex2exp)


# Load the results
load("ST_models_ICAR_partitioned.Rdata")
rm(TypeI.ICAR, TypeIII.ICAR, TypeIV.ICAR)

model <- TypeII.ICAR


# Load the data and cartography
load("../Data/IHD_data.Rdata")


S <- 3105
T <- 23

t.from <- 1999
t.to <- 2021



############################
# Relative risks estimates #
############################
RR.median <- as.data.frame(matrix(model$summary.fitted.values$`0.5quant`, S, T, byrow=F))
rownames(RR.median) <- model$.args$data$Area[1:S]
colnames(RR.median) <- seq(1999, 2021, 1)
RR.median <- RR.median[rownames(pop), ]

RR.median.mat <- as.matrix(RR.median)



#########################################################
# Multiply relative risks estimates with the population #
#########################################################
pop$GEOID <- NULL
pop.mat <- as.matrix(pop)


RR.pop <- pop.mat*RR.median.mat
RR.pop <- as.data.frame(RR.pop)



##########################################################################################
# Weighted relative risks estimates and population in West, Midwest, Northeast and South #
##########################################################################################
ID.West <- carto$GEOID[carto$Region==1]
ID.Midwest <- carto$GEOID[carto$Region==2]
ID.South <- carto$GEOID[carto$Region==3]
ID.Northeast <- carto$GEOID[carto$Region==4]


RR.West <- RR.pop[rownames(RR.pop) %in% ID.West, ]
RR.Midwest <- RR.pop[rownames(RR.pop) %in% ID.Midwest, ]
RR.South <- RR.pop[rownames(RR.pop) %in% ID.South, ]
RR.Northeast <- RR.pop[rownames(RR.pop) %in% ID.Northeast, ]


pop.West <- pop.mat[rownames(pop.mat) %in% ID.West, ]
pop.Midwest <- pop.mat[rownames(pop.mat) %in% ID.Midwest, ]
pop.South <- pop.mat[rownames(pop.mat) %in% ID.South, ]
pop.Northeast <- pop.mat[rownames(pop.mat) %in% ID.Northeast, ]



##################################################
# Weighted average relative risks of each region #
##################################################
RR.pop.US <- colSums(RR.pop)/colSums(pop)
RR.pop.West <- colSums(RR.West)/colSums(pop.West)
RR.pop.Midwest <- colSums(RR.Midwest)/colSums(pop.Midwest)
RR.pop.South <- colSums(RR.South)/colSums(pop.South)
RR.pop.Northeast <- colSums(RR.Northeast)/colSums(pop.Northeast)



##################################################
# Figure 6: mean temporal risk trends by regions #
##################################################
RR.mean.all <- data.frame(Region=c(rep("US",T), rep("West",T), rep("Midwest",T), 
                                   rep("South",T), rep("Northeast",T)),
                          Year=rep(seq(1999,2021,1), 5), 
                          RR=c(RR.pop.US, RR.pop.West, RR.pop.Midwest, RR.pop.South, 
                               RR.pop.Northeast))

RR.mean.all$Region <- factor(RR.mean.all$Region, levels=c("US","West","Midwest","South","Northeast"))
                          


pdf("Figure6.pdf", width=8, height=6)
ggplot(RR.mean.all, aes(x=Year, y=RR, colour=Region)) + 
  geom_line(size=1.3) +
  scale_color_manual(values=c("grey", "#fd8d3c", "#de77ae", "#fed976", "#66c2a5")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines")) + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(0.8,1.8,0.2), limits=c(0.6,1.85)) +
  xlab("Year") + ylab(TeX('$\\bar{r}_{t}$')) + labs(colour = NULL)
dev.off()



############################################################
# Urbanization groups in 2021: Large (>=1 million people), #
# Medium (50000-999999 people), Rural (<50000 people)      #
############################################################
pop$GEOID <- rownames(pop)
pop.2021 <- pop[, c("GEOID","2021")]


ID.Large <- pop.2021$GEOID[pop.2021$`2021`>=1000000]
ID.Medium <- pop.2021$GEOID[pop.2021$`2021`<1000000 & pop.2021$`2021`>=50000]
ID.Rural <- pop.2021$GEOID[pop.2021$`2021`<50000]



########
# West #
########
RR.West.Large <- RR.West[rownames(RR.West) %in% ID.Large, ]
RR.West.Medium <- RR.West[rownames(RR.West) %in% ID.Medium, ]
RR.West.Rural <- RR.West[rownames(RR.West) %in% ID.Rural, ]


pop.West.Large <- pop.West[rownames(pop.West) %in% ID.Large, ]
pop.West.Medium <- pop.West[rownames(pop.West) %in% ID.Medium, ]
pop.West.Rural <- pop.West[rownames(pop.West) %in% ID.Rural, ]


RR.pop.West <- colSums(RR.West)/colSums(pop.West)
RR.pop.West.Large <- colSums(RR.West.Large)/colSums(pop.West.Large)
RR.pop.West.Medium <- colSums(RR.West.Medium)/colSums(pop.West.Medium)
RR.pop.West.Rural <- colSums(RR.West.Rural)/colSums(pop.West.Rural)


RR.West.all <- data.frame(Population=c(rep("All",T), rep("Rural",T), rep("Medium",T), 
                                       rep("Large",T)),
                          Year=rep(seq(1999,2021,1),4), 
                          RR=c(RR.pop.West, RR.pop.West.Rural, RR.pop.West.Medium, RR.pop.West.Large))

RR.West.all$Population <- factor(RR.West.all$Population, levels=c("All", "Rural", "Medium", "Large"))



###########
# Midwest #
###########
RR.Midwest.Large <- RR.Midwest[rownames(RR.Midwest) %in% ID.Large, ]
RR.Midwest.Medium <- RR.Midwest[rownames(RR.Midwest) %in% ID.Medium, ]
RR.Midwest.Rural <- RR.Midwest[rownames(RR.Midwest) %in% ID.Rural, ]


pop.Midwest.Large <- pop.Midwest[rownames(pop.Midwest) %in% ID.Large, ]
pop.Midwest.Medium <- pop.Midwest[rownames(pop.Midwest) %in% ID.Medium, ]
pop.Midwest.Rural <- pop.Midwest[rownames(pop.Midwest) %in% ID.Rural, ]


RR.pop.Midwest <- colSums(RR.Midwest)/colSums(pop.Midwest)
RR.pop.Midwest.Large <- colSums(RR.Midwest.Large)/colSums(pop.Midwest.Large)
RR.pop.Midwest.Medium <- colSums(RR.Midwest.Medium)/colSums(pop.Midwest.Medium)
RR.pop.Midwest.Rural <- colSums(RR.Midwest.Rural)/colSums(pop.Midwest.Rural)



RR.Midwest.all <- data.frame(Population=c(rep("All",T), rep("Rural",T), rep("Medium",T), 
                                          rep("Large",T)),
                             Year=rep(seq(1999,2021,1),4), 
                             RR=c(RR.pop.Midwest, RR.pop.Midwest.Rural, RR.pop.Midwest.Medium, RR.pop.Midwest.Large))

RR.Midwest.all$Population <- factor(RR.Midwest.all$Population, levels=c("All", "Rural", "Medium", "Large"))



#############
# Northeast #
#############
RR.Northeast.Large <- RR.Northeast[rownames(RR.Northeast) %in% ID.Large, ]
RR.Northeast.Medium <- RR.Northeast[rownames(RR.Northeast) %in% ID.Medium, ]
RR.Northeast.Rural <- RR.Northeast[rownames(RR.Northeast) %in% ID.Rural, ]


pop.Northeast.Large <- pop.Northeast[rownames(pop.Northeast) %in% ID.Large, ]
pop.Northeast.Medium <- pop.Northeast[rownames(pop.Northeast) %in% ID.Medium, ]
pop.Northeast.Rural <- pop.Northeast[rownames(pop.Northeast) %in% ID.Rural, ]


RR.pop.Northeast <- colSums(RR.Northeast)/colSums(pop.Northeast)
RR.pop.Northeast.Large <- colSums(RR.Northeast.Large)/colSums(pop.Northeast.Large)
RR.pop.Northeast.Medium <- colSums(RR.Northeast.Medium)/colSums(pop.Northeast.Medium)
RR.pop.Northeast.Rural <- colSums(RR.Northeast.Rural)/colSums(pop.Northeast.Rural)


RR.Northeast.all <- data.frame(Population=c(rep("All",T), rep("Rural",T), rep("Medium",T), 
                                            rep("Large",T)),
                               Year=rep(seq(1999,2021,1),4), 
                               RR=c(RR.pop.Northeast, RR.pop.Northeast.Rural, RR.pop.Northeast.Medium, RR.pop.Northeast.Large))

RR.Northeast.all$Population <- factor(RR.Northeast.all$Population, levels=c("All", "Rural", "Medium", "Large"))



#########
# South #
#########
RR.South.Large <- RR.South[rownames(RR.South) %in% ID.Large, ]
RR.South.Medium <- RR.South[rownames(RR.South) %in% ID.Medium, ]
RR.South.Rural <- RR.South[rownames(RR.South) %in% ID.Rural, ]


pop.South.Large <- pop.South[rownames(pop.South) %in% ID.Large, ]
pop.South.Medium <- pop.South[rownames(pop.South) %in% ID.Medium, ]
pop.South.Rural <- pop.South[rownames(pop.South) %in% ID.Rural, ]


RR.pop.South <- colSums(RR.South)/colSums(pop.South)
RR.pop.South.Large <- colSums(RR.South.Large)/colSums(pop.South.Large)
RR.pop.South.Medium <- colSums(RR.South.Medium)/colSums(pop.South.Medium)
RR.pop.South.Rural <- colSums(RR.South.Rural)/colSums(pop.South.Rural)


RR.South.all <- data.frame(Population=c(rep("All",T), rep("Rural",T), rep("Medium",T), 
                                        rep("Large",T)),
                           Year=rep(seq(1999,2021,1),4), 
                           RR=c(RR.pop.South, RR.pop.South.Rural, RR.pop.South.Medium, RR.pop.South.Large))

RR.South.all$Population <- factor(RR.South.all$Population, levels=c("All", "Rural", "Medium", "Large"))



########################################################################
# Figure 7: mean temporal risk trends by regions and urban/rural areas #
########################################################################
RR.final <- rbind(RR.West.all, RR.Midwest.all, RR.South.all, RR.Northeast.all)
RR.final$Region <- c(rep("West",4*T), rep("Midwest",4*T), rep("South",4*T), rep("Northeast",4*T))

RR.final$Region <- factor(RR.final$Region, levels=c("West","Midwest","South","Northeast"))



pdf("Figure7.pdf", width=10, height=8)
ggplot(RR.final, aes(x=Year, y=RR, colour=Population)) + 
  facet_wrap(. ~ Region, ncol=2, scales="free") +
  geom_line(size=1.3) +
  scale_color_manual(values=c("grey", "#a8ddb5", "#4eb3d3","#0868ac" )) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9),
        text = element_text(size = 20),
        panel.spacing = unit(1.3, "lines"),
        strip.background = element_rect(fill="lightskyblue")) + 
  scale_x_continuous(breaks=c(1999,2004,2009,2014,2019,2021)) +
  scale_y_continuous(breaks=seq(0.6,1.8,0.2), limits=c(0.55,1.94)) +
  xlab("Year") + ylab(TeX('$\\bar{r}_{t}$')) + labs(colour = NULL) 
dev.off()



